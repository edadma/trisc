package io.github.edadma.trisc

import io.github.edadma.toml.{TomlParser, TomlValue}

/** Source of a locked package. `None` (in `LockedPackage.source`) means the
 *  package is "local" — i.e. the project root itself or a workspace member —
 *  and lives next to the lock file. Path deps point to a resolved absolute
 *  filesystem location; git deps pin a resolved SHA plus the ref kind/name
 *  the manifest asked for, so `sysl fetch` can match a manifest entry to a
 *  lock entry without re-resolving the ref. */
sealed trait LockedSource:
  /** Cargo-style serialized form. Examples:
   *
   *    path+/Users/ed/dev/parsyl
   *    git+https://example/foo.git?branch=main#deadbeef...
   *    git+https://example/foo.git?tag=v1.0#deadbeef...
   *    git+https://example/foo.git?rev=deadbeef...#deadbeef... */
  def encoded: String
object LockedSource:
  case class Path(absolutePath: String) extends LockedSource:
    def encoded: String = s"path+$absolutePath"

  case class Git(url: String, refKind: GitRefKind, refName: String, sha: String) extends LockedSource:
    def encoded: String = s"git+$url?${GitRefKind.label(refKind)}=$refName#$sha"

/** One row in `[[package]]`. `dependencies` is a list of `"<name> <version>"`
 *  strings, sorted, mirroring Cargo.lock. */
case class LockedPackage(
    name: String,
    version: String,
    source: Option[LockedSource],
    dependencies: List[String],
)

case class SyslLock(version: Int, packages: List[LockedPackage])

object SyslLock:

  /** Bump on incompatible schema changes. v1 = path-only deps; later git +
   *  registry deps will keep v1 readable but write at the new version. */
  val Version = 1

  val LockFile = "sysl.lock"

  /** Build a lock graph from a resolved dep tree.
   *
   *  *Local* packages — the project root in single-package mode, or each
   *  workspace member in workspace mode — get `source = None`. Every other
   *  package in the resolved set is reached via a path dep and is recorded
   *  with its absolute on-disk location. Entries are sorted by `(name, version)`. */
  def fromResolved(io: FileOps, resolved: ResolvedDeps): SyslLock =
    val localDirs = computeLocalDirs(io, resolved)
    val rows = resolved.manifests.toList.flatMap {
      case (dir, m: PackageManifest) =>
        val pkg = m.p
        val deps = pkg.deps.toList.flatMap { case (alias, _) =>
          // Source-kind-agnostic: we only need the resolved dep directory
          // to find its [package] table. depResolutions handles path/git
          // uniformly; pre-chunk-5 fallback uses the path-only walker so
          // call sites that don't populate the map keep working.
          val depDir = resolved.depResolutions.getOrElse(
            (dir, alias),
            SyslResolver.resolveDepPath(io, dir, pkg.deps(alias) match {
              case SyslDep.Path(p) => p
              case _               => "" // git: must be in depResolutions
            }),
          )
          resolved.manifests.get(depDir).flatMap(_.pkg).map(d => s"${d.name} ${d.version}")
        }.sorted
        val source =
          if localDirs.contains(dir) then None
          else
            resolved.gitSources.get(dir) match
              case Some(g) => Some(LockedSource.Git(g.url, g.refKind, g.refName, g.sha))
              case None    => Some(LockedSource.Path(io.absolutePath(dir)))
        Some(LockedPackage(pkg.name, pkg.version, source, deps))
      case _ => None
    }
    SyslLock(Version, rows.sortBy(p => (p.name, p.version)))

  /** Where the lock file should live for this resolution. None when there's
   *  no project (legacy mode); some callers skip writing in that case. */
  def lockPathFor(io: FileOps, resolved: ResolvedDeps): Option[String] =
    resolved.projectRoot.map(io.joinPath(_, LockFile))

  /** Sync the on-disk lock with `resolved`, if needed.
   *
   *  Writes `<projectRoot>/sysl.lock` only when its content would actually
   *  change — byte-comparing the rendered form against any existing file
   *  keeps test snapshots stable and avoids touching mtimes on no-op runs.
   *
   *  Skipped silently when (a) there's no project root, (b) the only entry
   *  would be a synthesized marker-only package with no real deps (the
   *  trisc repo's own SLIX-config sysl.toml falls into this case — writing
   *  a noise lock there pollutes the source tree), or (c) the rendering
   *  would produce zero packages.
   *
   *  Returns Some(true) if the file was written, Some(false) if it was
   *  already up to date, and None if writing was skipped. */
  def writeIfChanged(io: FileOps, resolved: ResolvedDeps): Option[Boolean] =
    if isMarkerOnlyRoot(resolved) then return None
    val lock = fromResolved(io, resolved)
    if lock.packages.isEmpty then return None
    lockPathFor(io, resolved) match
      case None => None
      case Some(path) =>
        val rendered = render(lock)
        val current =
          if io.exists(path) then
            try io.readFile(path) catch case _: Throwable => ""
          else ""
        if current == rendered then Some(false)
        else
          io.writeFile(path, rendered)
          Some(true)

  /** True when the resolved project root is a marker-only sysl.toml (no
   *  [package], no [workspace]) and no real deps would be locked. */
  private def isMarkerOnlyRoot(resolved: ResolvedDeps): Boolean =
    resolved.projectRoot.exists { root =>
      resolved.manifests.get(root) match
        case Some(PackageManifest(p)) => p.synthetic && p.deps.isEmpty
        case _ => false
    }

  /** Render the canonical TOML form. Stable across runs given the same input;
   *  callers can byte-compare to existing content to detect "no change". */
  def render(lock: SyslLock): String =
    val sb = new StringBuilder
    sb ++= "# This file is automatically @generated by sysl. Do not edit manually.\n"
    sb ++= s"version = ${lock.version}\n"
    for p <- lock.packages do
      sb += '\n'
      sb ++= "[[package]]\n"
      sb ++= s"name = ${tomlString(p.name)}\n"
      sb ++= s"version = ${tomlString(p.version)}\n"
      p.source.foreach(s => sb ++= s"source = ${tomlString(s.encoded)}\n")
      if p.dependencies.nonEmpty then
        sb ++= "dependencies = [\n"
        for d <- p.dependencies do sb ++= s"    ${tomlString(d)},\n"
        sb ++= "]\n"
    sb.toString

  /** Parse lock TOML. Used by `sysl update` (and future inspection commands)
   *  to confirm the on-disk lock is well-formed before overwriting. */
  def parse(content: String, path: String): Either[String, SyslLock] =
    TomlParser.parse(content) match
      case Left(msg) => Left(s"$path: $msg")
      case Right(doc) =>
        val v = doc.getInt("version").map(_.toInt).getOrElse(0)
        if v <= 0 then Left(s"$path: missing or invalid `version` field")
        else
          decodePackages(doc, path).map(pkgs => SyslLock(v, pkgs))

  private def decodePackages(doc: io.github.edadma.toml.TomlDocument, path: String): Either[String, List[LockedPackage]] =
    doc.getArr("package") match
      case None => Right(Nil)
      case Some(arr) =>
        val out = List.newBuilder[LockedPackage]
        var err: Option[String] = None
        for v <- arr if err.isEmpty do v match
          case TomlValue.Obj(fields) =>
            val name = fields.get("name").collect { case TomlValue.Str(s) => s }
            val version = fields.get("version").collect { case TomlValue.Str(s) => s }
            (name, version) match
              case (Some(n), Some(ver)) =>
                val src = fields.get("source").collect { case TomlValue.Str(s) => s }.flatMap(decodeSource)
                val deps = fields.get("dependencies") match
                  case Some(TomlValue.Arr(items)) =>
                    items.collect { case TomlValue.Str(s) => s }.toList
                  case _ => Nil
                out += LockedPackage(n, ver, src, deps)
              case _ => err = Some(s"$path: each [[package]] entry needs `name` and `version`")
          case _ => err = Some(s"$path: [[package]] entries must be tables")
        err match
          case Some(e) => Left(e)
          case None => Right(out.result())

  private def decodeSource(s: String): Option[LockedSource] =
    if s.startsWith("path+") then Some(LockedSource.Path(s.stripPrefix("path+")))
    else if s.startsWith("git+") then decodeGitSource(s.stripPrefix("git+"))
    else None

  /** Decode `<url>?<kind>=<name>#<sha>`. Returns None on any malformed shape
   *  rather than throwing — callers (currently only `parse`) treat an
   *  unrecognized source as no-source so an upgrade path can introduce new
   *  schemes without hard-failing existing locks. */
  private def decodeGitSource(rest: String): Option[LockedSource.Git] =
    val hashIdx = rest.lastIndexOf('#')
    if hashIdx < 0 then return None
    val sha = rest.substring(hashIdx + 1)
    val urlAndQuery = rest.substring(0, hashIdx)
    val qIdx = urlAndQuery.lastIndexOf('?')
    if qIdx < 0 then return None
    val url = urlAndQuery.substring(0, qIdx)
    val query = urlAndQuery.substring(qIdx + 1)
    val eqIdx = query.indexOf('=')
    if eqIdx < 0 then return None
    val kindLabel = query.substring(0, eqIdx)
    val refName = query.substring(eqIdx + 1)
    GitRefKind.fromLabel(kindLabel).map(k => LockedSource.Git(url, k, refName, sha))

  /** Read sysl.lock (if any) at the project root and decode it. Used by
   *  `sysl fetch` so the resolver can short-circuit ref-resolution when the
   *  pinned sha is still satisfiable. Missing or malformed lock → None;
   *  callers fall back to a from-scratch resolve. */
  def loadFrom(io: FileOps, projectRoot: String): Option[SyslLock] =
    val path = io.joinPath(projectRoot, LockFile)
    if !io.exists(path) then None
    else
      val content = try io.readFile(path) catch case _: Throwable => return None
      parse(content, path).toOption

  /** Build the `(url, refKindLabel, refName) -> sha` pin map the resolver
   *  consumes. Path-source rows contribute nothing — the lock there exists
   *  only to record the resolved location, not to pin it. */
  def pinsFor(lock: SyslLock): Map[(String, String, String), String] =
    lock.packages.flatMap(_.source).collect {
      case LockedSource.Git(url, kind, name, sha) =>
        (url, GitRefKind.label(kind), name) -> sha
    }.toMap

  /** Convenience: read `<root>/sysl.lock` (if any) and return its pin map.
   *  Returns the empty map on missing or malformed lock so call sites can
   *  use it unconditionally. */
  def loadPins(io: FileOps, projectRoot: String): Map[(String, String, String), String] =
    loadFrom(io, projectRoot).map(pinsFor).getOrElse(Map.empty)

  /** Compute the set of "local" directories — packages that live next to the
   *  lock file rather than being pulled in as path/git deps. */
  private def computeLocalDirs(io: FileOps, resolved: ResolvedDeps): Set[String] =
    resolved.projectRoot match
      case None => Set.empty
      case Some(root) =>
        resolved.manifests.get(root) match
          case Some(WorkspaceManifest(ws)) =>
            ws.members.map(m => SyslResolver.resolveDepPath(io, root, m)).toSet
          case Some(PackageManifest(_)) => Set(root)
          case _ => Set.empty

  /** TOML basic-string with the minimal escapes we need. The values we emit
   *  (package names, semver versions, absolute paths, dep strings) never
   *  contain control chars; we only need to handle backslashes and quotes. */
  private def tomlString(s: String): String =
    val sb = new StringBuilder
    sb += '"'
    for c <- s do c match
      case '"'  => sb ++= "\\\""
      case '\\' => sb ++= "\\\\"
      case other => sb += other
    sb += '"'
    sb.toString
