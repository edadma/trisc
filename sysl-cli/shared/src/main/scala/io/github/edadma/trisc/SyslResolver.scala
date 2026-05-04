package io.github.edadma.trisc

import scala.collection.mutable

/** Result of resolving the dependency graph rooted at the user's inputs.
 *
 *  `projectRoot` is the directory containing the `sysl.toml` discovered by
 *  walking up from the inputs (or `None` if the user is operating outside any
 *  sysl project — this is the legacy path, used by sub-directory tests in
 *  the trisc repo itself).
 *
 *  `searchRoots` is the list of directories that the source-set composer
 *  scans for files matching imported module paths. It always includes the
 *  project root (when present) and the resolved roots of every transitive
 *  path dependency. The order is "project first, then deps in resolution
 *  order"; the existing `resolveTransitiveSources` picks the first matching
 *  candidate, so a module in the project shadows a dep with the same path.
 *
 *  `manifests` carries the parsed `sysl.toml` of the project (and each
 *  resolved dep) so callers that need to inspect dep metadata don't have to
 *  re-parse. `Map[rootDir, SyslManifest]`.
 */
case class ResolvedDeps(
    projectRoot: Option[String],
    searchRoots: List[String],
    manifests: Map[String, SyslManifest],
    /** For every dep that came in via a git source (chunk 5+), the resolved
     *  `(url, refKind, refName, sha)`. Keyed by checkout directory — same key
     *  shape as `manifests` so the lock writer can join them in one pass.
     *  Empty for path-only resolves. */
    gitSources: Map[String, ResolvedGitSource] = Map.empty,
    /** `(consumerDir, depAlias) -> depDir` for every edge in the dep graph.
     *  Path deps point at the path-resolved directory; git deps point at the
     *  git checkout directory. Lets the lock writer turn "this consumer
     *  declared `foo = { ... }`" into the actual `<pkg.name> <pkg.version>`
     *  string regardless of source kind. */
    depResolutions: Map[(String, String), String] = Map.empty,
)

/** Outcome of a git-dep resolution: where the checkout lives + what the
 *  lock should record. The (url, refKind, refName) tuple matches the
 *  manifest's `[dependencies]` entry verbatim so manifest↔lock identity is
 *  byte-stable across runs; `sha` is the rev-parsed commit. */
case class ResolvedGitSource(
    url: String,
    refKind: GitRefKind,
    refName: String,
    sha: String,
)

object SyslResolver:

  private val ManifestFile = SyslDriver.ProjectMarker

  /** Resolve the dep graph for the given CLI inputs. Errors are returned as
   *  Left so callers can emit them with the rest of their diagnostics.
   *
   *  `gitFetcher` is the bridge to the git cache. Pass `None` for path-only
   *  builds (or for the JS/Native CLIs that have no git support); a manifest
   *  with a git dep then produces a clear "git dependencies are not
   *  supported" error rather than a silent fallback. `lockPins` carries
   *  resolved shas from `sysl.lock` so `sysl fetch` can short-circuit
   *  network ref-resolution when the manifest is unchanged.
   */
  def resolve(
      io: FileOps,
      inputs: Seq[String],
      gitFetcher: Option[GitFetcher] = None,
      lockPins: Map[(String, String, String), String] = Map.empty,
  ): Either[String, ResolvedDeps] =
    val rootOpt = findProjectRoot(io, inputs)
    rootOpt match
      case None =>
        // No sysl.toml in scope. Legacy mode — caller behaves exactly as
        // before (e.g. `.` is the search root). No deps to resolve.
        Right(ResolvedDeps(None, Nil, Map.empty))
      case Some(root) =>
        loadManifestAt(io, root).flatMap { manifest =>
          val ctx = new ResolveCtx(io, gitFetcher, lockPins)
          manifest match
            case PackageManifest(pkg) => resolvePackage(ctx, root, pkg)
            case WorkspaceManifest(ws) => resolveWorkspace(ctx, root, ws)
        }

  /** Mutable per-resolve scratch space. Holds the visit order, dep-resolution
   *  edges, and git source records so `walkPackageDeps` doesn't have to
   *  thread three half-built maps through every recursive call. */
  private class ResolveCtx(
      val io: FileOps,
      val gitFetcher: Option[GitFetcher],
      val lockPins: Map[(String, String, String), String],
  ):
    val visited: mutable.LinkedHashMap[String, SyslManifest] = mutable.LinkedHashMap.empty
    val gitSources: mutable.HashMap[String, ResolvedGitSource] = mutable.HashMap.empty
    val depResolutions: mutable.HashMap[(String, String), String] = mutable.HashMap.empty
    /** Per-URL conflict guard: same URL must resolve to the same sha across
     *  every consumer in the tree (v1 has no version-resolution policy). */
    val urlShas: mutable.HashMap[String, String] = mutable.HashMap.empty

  /** Find the project containing the user's inputs by walking up from each
   *  input. All inputs must share a single project root — mixing paths from
   *  different projects in one CLI invocation isn't supported.
   *
   *  Public so the CLI can locate the lock file before invoking `resolve`,
   *  which lets `sysl fetch` build a pin map from the on-disk lock without
   *  paying for a wasted no-pins resolve first. */
  def findProjectRoot(io: FileOps, inputs: Seq[String]): Option[String] =
    val roots = inputs.flatMap(p => findProjectRootForInput(io, p)).distinct
    roots match
      case Seq() => None
      case Seq(one) => Some(one)
      case many =>
        // Pick the deepest one — heuristic for the case where one input is
        // a sub-dir and another is the project root itself. Any cross-project
        // mismatch will surface as "module not found" later.
        Some(many.maxBy(_.length))

  /** Locate the project root for a single CLI input. SyslDriver.findProjectRoot
   *  walks up from the parent of the path, which is correct for file inputs
   *  (the file's directory is the first one to check) but wrong for directory
   *  inputs that *are themselves* a project root — it would skip the dir and
   *  find some unrelated higher-up `sysl.toml`. Check the directory itself
   *  first, then fall back to the walk-up. */
  private def findProjectRootForInput(io: FileOps, input: String): Option[String] =
    if io.exists(input) && io.isDirectory(input) then
      val markerHere = io.joinPath(input, ManifestFile)
      if io.exists(markerHere) then Some(input.stripSuffix("/"))
      else SyslDriver.findProjectRoot(io, input)
    else SyslDriver.findProjectRoot(io, input)

  /** Workspace member directories, resolved against the workspace root, in
   *  declaration order. Returns None if the resolved project is not a
   *  workspace (or no project was discovered). Used by the CLI to dispatch a
   *  per-member test/run pass when the user points at a workspace root. */
  def workspaceMemberDirs(io: FileOps, resolved: ResolvedDeps): Option[List[String]] =
    resolved.projectRoot.flatMap { root =>
      resolved.manifests.get(root) match
        case Some(WorkspaceManifest(ws)) =>
          Some(ws.members.map(m => normalizePath(io.joinPath(root, m))))
        case _ => None
    }

  /** Resolve a path-dep string against an owning manifest's directory. Mirrors
   *  the logic in `walkPackageDeps` so other modules (lock writer, future
   *  diagnostics) don't have to duplicate it. Absolute paths bypass joinPath;
   *  the result is normalized. */
  def resolveDepPath(io: FileOps, ownerDir: String, depPath: String): String =
    val raw = if depPath.startsWith("/") then depPath else io.joinPath(ownerDir, depPath)
    normalizePath(raw)

  private def loadManifestAt(io: FileOps, dir: String): Either[String, SyslManifest] =
    val path = io.joinPath(dir, ManifestFile)
    if !io.exists(path) then Left(s"$path: not found")
    else SyslManifest.parse(io.readFile(path), path)

  private def resolvePackage(ctx: ResolveCtx, root: String, pkg: SyslPackage): Either[String, ResolvedDeps] =
    ctx.visited(root) = PackageManifest(pkg)
    walkPackageDeps(ctx, root, pkg).map { _ =>
      ResolvedDeps(
        projectRoot = Some(root),
        searchRoots = ctx.visited.keys.toList,
        manifests = ctx.visited.toMap,
        gitSources = ctx.gitSources.toMap,
        depResolutions = ctx.depResolutions.toMap,
      )
    }

  private def resolveWorkspace(ctx: ResolveCtx, root: String, ws: SyslWorkspace): Either[String, ResolvedDeps] =
    ctx.visited(root) = WorkspaceManifest(ws)
    var err: Option[String] = None
    for member <- ws.members if err.isEmpty do
      val memberDir = normalizePath(ctx.io.joinPath(root, member))
      loadManifestAt(ctx.io, memberDir) match
        case Left(e) => err = Some(s"workspace member `$member` at $memberDir: $e")
        case Right(m) =>
          ctx.visited(memberDir) = m
          m match
            case PackageManifest(p) => walkPackageDeps(ctx, memberDir, p).left.foreach(e => err = Some(e))
            case WorkspaceManifest(_) => err = Some(s"$memberDir: nested workspaces are not supported")
    err match
      case Some(e) => Left(e)
      case None => Right(ResolvedDeps(
        projectRoot = Some(root),
        searchRoots = ctx.visited.keys.toList,
        manifests = ctx.visited.toMap,
        gitSources = ctx.gitSources.toMap,
        depResolutions = ctx.depResolutions.toMap,
      ))

  private def walkPackageDeps(
      ctx: ResolveCtx,
      ownerDir: String,
      pkg: SyslPackage,
  ): Either[String, Unit] =
    var err: Option[String] = None
    for (depName, dep) <- pkg.deps if err.isEmpty do
      dep match
        case SyslDep.Path(rel) =>
          // Absolute paths bypass joinPath (which would concatenate them onto
          // ownerDir on the JVM); relative paths resolve against ownerDir.
          val raw = if rel.startsWith("/") then rel else ctx.io.joinPath(ownerDir, rel)
          val depDir = normalizePath(raw)
          ctx.depResolutions((ownerDir, depName)) = depDir
          if ctx.visited.contains(depDir) then ()
          else
            loadManifestAt(ctx.io, depDir) match
              case Left(e) => err = Some(s"dependency `$depName` at $depDir: $e")
              case Right(m) =>
                m match
                  case PackageManifest(subPkg) =>
                    ctx.visited(depDir) = m
                    walkPackageDeps(ctx, depDir, subPkg).left.foreach(e => err = Some(e))
                  case WorkspaceManifest(_) =>
                    err = Some(s"dependency `$depName` at $depDir is a workspace; depend on a workspace member directly")
        case SyslDep.Git(url, ref) =>
          ctx.gitFetcher match
            case None =>
              err = Some(s"dependency `$depName`: git dependencies are not supported here (CLI built without git fetcher)")
            case Some(fetcher) =>
              val (refKind, refName) = GitRefKind.fromGitRef(ref)
              val knownSha = ctx.lockPins.get((url, GitRefKind.label(refKind), refName))
              fetcher.ensureCheckout(url, refKind, refName, knownSha) match
                case Left(msg) =>
                  err = Some(s"dependency `$depName` (git $url): $msg")
                case Right((sha, dir)) =>
                  // Conflict guard: same URL across two consumers must
                  // resolve to the same sha. If two manifests pin different
                  // refs of the same repo, this is the v1 hard error.
                  ctx.urlShas.get(url) match
                    case Some(prev) if prev != sha =>
                      err = Some(s"dependency `$depName`: conflicting git refs for $url ($prev vs $sha); v1 requires a single resolved sha per url")
                    case _ =>
                      ctx.urlShas(url) = sha
                      val depDir = normalizePath(dir)
                      ctx.depResolutions((ownerDir, depName)) = depDir
                      ctx.gitSources(depDir) = ResolvedGitSource(url, refKind, refName, sha)
                      if ctx.visited.contains(depDir) then ()
                      else
                        loadManifestAt(ctx.io, depDir) match
                          case Left(e) => err = Some(s"dependency `$depName` at $depDir: $e")
                          case Right(m) =>
                            m match
                              case PackageManifest(subPkg) =>
                                ctx.visited(depDir) = m
                                walkPackageDeps(ctx, depDir, subPkg).left.foreach(e => err = Some(e))
                              case WorkspaceManifest(_) =>
                                err = Some(s"dependency `$depName` at $depDir is a workspace; depend on a workspace member directly")
    err match
      case Some(e) => Left(e)
      case None => Right(())

  /** Collapse `a/b/../c` to `a/c`. Idempotent on already-normalized paths. */
  private def normalizePath(path: String): String =
    val (leading, segs) = if path.startsWith("/") then ("/", path.drop(1).split("/").toList)
                          else ("", path.split("/").toList)
    val out = mutable.ListBuffer[String]()
    for s <- segs do s match
      case "" | "." => ()
      case ".." if out.nonEmpty && out.last != ".." => out.remove(out.length - 1)
      case other => out += other
    val joined = out.mkString("/")
    leading + (if joined.isEmpty then "." else joined)
