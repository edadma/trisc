package io.github.edadma.trisc

import io.github.edadma.toml.{TomlDocument, TomlParser, TomlValue}

import scala.collection.immutable.VectorMap

/** Decoded `sysl.toml` for a single project. */
case class SyslPackage(
    name: String,
    version: String,
    deps: Map[String, SyslDep],
)

/** Decoded `[workspace]` table — a top-level `sysl.toml` that lists member
 *  projects but defines no `[package]` of its own. */
case class SyslWorkspace(members: List[String])

/** A `sysl.toml` is either a single-project manifest or a workspace root.
 *  Both forms may appear in the same file (workspace root that is itself a
 *  package), but v1 keeps them mutually exclusive. */
sealed trait SyslManifest:
  def pkg: Option[SyslPackage]
  def workspace: Option[SyslWorkspace]

case class PackageManifest(p: SyslPackage) extends SyslManifest:
  def pkg: Option[SyslPackage] = Some(p)
  def workspace: Option[SyslWorkspace] = None

case class WorkspaceManifest(w: SyslWorkspace) extends SyslManifest:
  def pkg: Option[SyslPackage] = None
  def workspace: Option[SyslWorkspace] = Some(w)

/** A dependency declared in `[dependencies]`. */
sealed trait SyslDep
object SyslDep:
  /** `dep = { path = "../sister" }` — resolved against the manifest's directory. */
  case class Path(path: String) extends SyslDep

  /** `dep = { git = "...", rev = "abc" }` (and tag/branch variants). Resolution
   *  is deferred — the resolver currently rejects this with a clear error. */
  case class Git(url: String, ref: GitRef) extends SyslDep

sealed trait GitRef
object GitRef:
  case class Rev(s: String) extends GitRef
  case class Tag(s: String) extends GitRef
  case class Branch(s: String) extends GitRef

/** Parsing result. Errors are user-facing strings, formatted by the caller. */
object SyslManifest:

  /** Parse a `sysl.toml`'s contents. The `path` argument is purely for error
   *  message context. */
  def parse(content: String, path: String): Either[String, SyslManifest] =
    TomlParser.parse(content) match
      case Left(msg) => Left(s"$path: $msg")
      case Right(doc) => decode(doc, path)

  private def decode(doc: TomlDocument, path: String): Either[String, SyslManifest] =
    val hasPackage = doc.getTable("package").isDefined
    val hasWorkspace = doc.getTable("workspace").isDefined
    (hasPackage, hasWorkspace) match
      case (false, false) =>
        // Not a dep-resolution-aware manifest. Synthesize an empty package
        // so the resolver still finds and walks this directory; the existing
        // Step 3b behaviour of treating sysl.toml only as a project marker
        // is preserved.
        Right(PackageManifest(SyslPackage(name = inferName(path), version = "0.0.0", deps = Map.empty)))
      case (true, true) =>
        Left(s"$path: a sysl.toml may declare either [package] or [workspace], not both")
      case (false, true) =>
        decodeWorkspace(doc, path).map(WorkspaceManifest.apply)
      case (true, false) =>
        decodePackage(doc, path).map(PackageManifest.apply)

  private def inferName(path: String): String =
    val parent = parentDir(path)
    val name = parent.split('/').filter(_.nonEmpty).lastOption.getOrElse(parent)
    if name.isEmpty || name == "." then "_unnamed" else name

  private def parentDir(path: String): String =
    val i = path.lastIndexOf('/')
    if i < 0 then "." else path.substring(0, i)

  private def decodeWorkspace(doc: TomlDocument, path: String): Either[String, SyslWorkspace] =
    doc.getArr("workspace.members") match
      case None => Left(s"$path: [workspace] requires a `members = [...]` field")
      case Some(arr) =>
        val sb = List.newBuilder[String]
        var err: Option[String] = None
        for v <- arr if err.isEmpty do v match
          case TomlValue.Str(s) => sb += s
          case other => err = Some(s"$path: workspace member entries must be strings (got ${TomlValue.typeLabel(other)})")
        err match
          case Some(msg) => Left(msg)
          case None => Right(SyslWorkspace(sb.result()))

  private def decodePackage(doc: TomlDocument, path: String): Either[String, SyslPackage] =
    doc.getString("package.name") match
      case None => Left(s"$path: [package] requires `name = \"...\"`")
      case Some(name) =>
        val version = doc.getString("package.version").getOrElse("0.0.0")
        decodeDeps(doc, path).map(deps => SyslPackage(name, version, deps))

  private def decodeDeps(doc: TomlDocument, path: String): Either[String, Map[String, SyslDep]] =
    doc.getTable("dependencies") match
      case None => Right(Map.empty)
      case Some(tab) =>
        val mb = Map.newBuilder[String, SyslDep]
        var err: Option[String] = None
        for (k, v) <- tab if err.isEmpty do
          decodeDep(k, v, path) match
            case Right(d) => mb += (k -> d)
            case Left(e) => err = Some(e)
        err match
          case Some(e) => Left(e)
          case None => Right(mb.result())

  private def decodeDep(name: String, v: TomlValue, path: String): Either[String, SyslDep] =
    v match
      case TomlValue.Str(_) =>
        Left(s"$path: bare-version dependency `$name = \"...\"` is not yet supported (use `{ path = \"...\" }` or `{ git = \"...\", ... }`)")
      case TomlValue.Obj(fields) =>
        decodeDepTable(name, fields, path)
      case other =>
        Left(s"$path: dependency `$name` must be an inline table, got ${TomlValue.typeLabel(other)}")

  private def decodeDepTable(name: String, fields: VectorMap[String, TomlValue], path: String): Either[String, SyslDep] =
    val pathField = fields.get("path").collect { case TomlValue.Str(s) => s }
    val gitField = fields.get("git").collect { case TomlValue.Str(s) => s }
    (pathField, gitField) match
      case (Some(_), Some(_)) =>
        Left(s"$path: dependency `$name` may have either `path` or `git`, not both")
      case (Some(p), None) =>
        // Path deps must not also carry rev/tag/branch — surface obvious mistakes.
        List("rev", "tag", "branch").find(fields.contains) match
          case Some(k) => Left(s"$path: dependency `$name` is a path dep; the `$k` field only applies to git deps")
          case None => Right(SyslDep.Path(p))
      case (None, Some(url)) =>
        val rev = fields.get("rev").collect { case TomlValue.Str(s) => s }
        val tag = fields.get("tag").collect { case TomlValue.Str(s) => s }
        val branch = fields.get("branch").collect { case TomlValue.Str(s) => s }
        val refs = List(rev.map(GitRef.Rev.apply), tag.map(GitRef.Tag.apply), branch.map(GitRef.Branch.apply)).flatten
        if refs.size != 1 then
          Left(s"$path: git dependency `$name` must specify exactly one of `rev`, `tag`, or `branch`")
        else
          Right(SyslDep.Git(url, refs.head))
      case (None, None) =>
        Left(s"$path: dependency `$name` must specify `path` or `git`")
