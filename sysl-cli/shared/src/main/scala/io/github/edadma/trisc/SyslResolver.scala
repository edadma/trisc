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
)

object SyslResolver:

  private val ManifestFile = SyslDriver.ProjectMarker

  /** Resolve the dep graph for the given CLI inputs. Errors are returned as
   *  Left so callers can emit them with the rest of their diagnostics. */
  def resolve(io: FileOps, inputs: Seq[String]): Either[String, ResolvedDeps] =
    val rootOpt = findProjectRoot(io, inputs)
    rootOpt match
      case None =>
        // No sysl.toml in scope. Legacy mode — caller behaves exactly as
        // before (e.g. `.` is the search root). No deps to resolve.
        Right(ResolvedDeps(None, Nil, Map.empty))
      case Some(root) =>
        loadManifestAt(io, root).flatMap { manifest =>
          manifest match
            case PackageManifest(pkg) => resolvePackage(io, root, pkg)
            case WorkspaceManifest(ws) => resolveWorkspace(io, root, ws)
        }

  /** Find the project containing the user's inputs by walking up from each
   *  input. All inputs must share a single project root — mixing paths from
   *  different projects in one CLI invocation isn't supported. */
  private def findProjectRoot(io: FileOps, inputs: Seq[String]): Option[String] =
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

  private def loadManifestAt(io: FileOps, dir: String): Either[String, SyslManifest] =
    val path = io.joinPath(dir, ManifestFile)
    if !io.exists(path) then Left(s"$path: not found")
    else SyslManifest.parse(io.readFile(path), path)

  private def resolvePackage(io: FileOps, root: String, pkg: SyslPackage): Either[String, ResolvedDeps] =
    val visited = mutable.LinkedHashMap[String, SyslManifest]()
    visited(root) = PackageManifest(pkg)
    walkPackageDeps(io, root, pkg, visited).map { _ =>
      ResolvedDeps(
        projectRoot = Some(root),
        searchRoots = visited.keys.toList,
        manifests = visited.toMap,
      )
    }

  private def resolveWorkspace(io: FileOps, root: String, ws: SyslWorkspace): Either[String, ResolvedDeps] =
    val visited = mutable.LinkedHashMap[String, SyslManifest]()
    visited(root) = WorkspaceManifest(ws)
    var err: Option[String] = None
    for member <- ws.members if err.isEmpty do
      val memberDir = normalizePath(io.joinPath(root, member))
      loadManifestAt(io, memberDir) match
        case Left(e) => err = Some(s"workspace member `$member` at $memberDir: $e")
        case Right(m) =>
          visited(memberDir) = m
          m match
            case PackageManifest(p) => walkPackageDeps(io, memberDir, p, visited).left.foreach(e => err = Some(e))
            case WorkspaceManifest(_) => err = Some(s"$memberDir: nested workspaces are not supported")
    err match
      case Some(e) => Left(e)
      case None => Right(ResolvedDeps(Some(root), visited.keys.toList, visited.toMap))

  private def walkPackageDeps(
      io: FileOps,
      ownerDir: String,
      pkg: SyslPackage,
      visited: mutable.LinkedHashMap[String, SyslManifest],
  ): Either[String, Unit] =
    var err: Option[String] = None
    for (depName, dep) <- pkg.deps if err.isEmpty do
      dep match
        case SyslDep.Path(rel) =>
          // Absolute paths bypass joinPath (which would concatenate them onto
          // ownerDir on the JVM); relative paths resolve against ownerDir.
          val raw = if rel.startsWith("/") then rel else io.joinPath(ownerDir, rel)
          val depDir = normalizePath(raw)
          if visited.contains(depDir) then ()
          else
            loadManifestAt(io, depDir) match
              case Left(e) => err = Some(s"dependency `$depName` at $depDir: $e")
              case Right(m) =>
                m match
                  case PackageManifest(subPkg) =>
                    visited(depDir) = m
                    walkPackageDeps(io, depDir, subPkg, visited).left.foreach(e => err = Some(e))
                  case WorkspaceManifest(_) =>
                    err = Some(s"dependency `$depName` at $depDir is a workspace; depend on a workspace member directly")
        case SyslDep.Git(_, _) =>
          err = Some(s"dependency `$depName`: git dependencies are not yet supported in this build")
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
