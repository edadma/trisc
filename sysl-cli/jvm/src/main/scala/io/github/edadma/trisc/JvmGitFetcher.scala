package io.github.edadma.trisc

import java.io.File
import java.security.MessageDigest

/** JVM `GitFetcher` implementation: shells out to the `git` CLI and lays
 *  out checkouts under `cacheRoot`.
 *
 *  Layout (Cargo-inspired):
 *    <cacheRoot>/git/db/<name>-<urlHash>           bare clone, one per URL
 *    <cacheRoot>/git/checkouts/<name>-<urlHash>/<sha>/   working trees, one per sha
 *
 *  `<name>` is the URL's last path segment with `.git` stripped, sanitized
 *  to ASCII so it's filesystem-safe. `<urlHash>` is a 16-char prefix of
 *  sha256(url) so two repos that happen to share a basename don't collide.
 *
 *  Network calls happen in `ensureBare` (clone or `git fetch`) and in
 *  `resolveRef` (`git rev-parse` against the bare). The fetcher avoids
 *  network when `knownSha` is supplied and a checkout already exists — that
 *  is the lock-pinned `sysl fetch` happy path.
 *
 *  Errors return `Left(msg)` with both the failing command and stderr so
 *  callers can show a Cargo-style "failed to fetch" diagnostic without
 *  digging through process plumbing.
 */
class JvmGitFetcher(val cacheRoot: String) extends GitFetcher:

  import scala.sys.process.*

  private val gitDir = new File(cacheRoot, "git")
  private val dbDir = new File(gitDir, "db")
  private val checkoutsDir = new File(gitDir, "checkouts")

  def ensureCheckout(
      url: String,
      refKind: GitRefKind,
      refName: String,
      knownSha: Option[String],
  ): Either[String, (String, String)] =
    // Lock-pinned happy path: if we already have the requested sha checked
    // out, skip every network step. This is the whole point of `sysl fetch`
    // honoring `sysl.lock` — second run with no manifest changes does no IO.
    knownSha match
      case Some(sha) =>
        val co = checkoutPath(url, sha)
        if co.exists() then Right((sha, co.getAbsolutePath))
        else
          for
            _   <- ensureBare(url)
            _   <- ensureRefAvailable(url, refKind, refName, Some(sha))
            dir <- ensureWorktree(url, sha)
          yield (sha, dir)
      case None =>
        for
          _   <- ensureBare(url)
          _   <- ensureRefAvailable(url, refKind, refName, None)
          sha <- resolveRef(url, refKind, refName)
          dir <- ensureWorktree(url, sha)
        yield (sha, dir)

  private def repoName(url: String): String =
    val last = url.split("[/:]").lastOption.getOrElse("repo").stripSuffix(".git")
    val cleaned = last.map(c => if c.isLetterOrDigit || c == '-' || c == '_' || c == '.' then c else '_')
    if cleaned.nonEmpty then cleaned else "repo"

  private def urlHash(url: String): String =
    val md = MessageDigest.getInstance("SHA-256")
    val digest = md.digest(url.getBytes("UTF-8"))
    digest.take(8).map(b => f"${b & 0xff}%02x").mkString  // 16-hex-char prefix

  private def repoSlug(url: String): String = s"${repoName(url)}-${urlHash(url)}"

  private def barePath(url: String): File = new File(dbDir, repoSlug(url))

  private def checkoutPath(url: String, sha: String): File =
    new File(new File(checkoutsDir, repoSlug(url)), sha)

  private def ensureBare(url: String): Either[String, File] =
    val bare = barePath(url)
    if bare.exists() then
      // Try to refresh; if that fails (offline, deleted remote, etc.) fall
      // through with the existing bare — callers will hit a clearer error
      // when the requested ref/sha is missing.
      runGit(Seq("git", "-C", bare.getAbsolutePath, "fetch", "--quiet", "--tags", "--prune", "origin"))
      Right(bare)
    else
      bare.getParentFile.mkdirs()
      // `--mirror` implies `--bare` and additionally configures
      // `remote.origin.fetch = +refs/*:refs/*` so subsequent
      // `git fetch origin` calls actually update local refs/heads.
      // Plain `--bare` leaves the refspec empty, which silently breaks
      // `branch = "main"` after the upstream advances.
      runGit(Seq("git", "clone", "--quiet", "--mirror", url, bare.getAbsolutePath))
        .map(_ => bare)

  /** Make sure a tag/branch ref or specific sha is reachable in the bare.
   *
   *  Default `git clone --bare` already pulls all refs and (with --tags above
   *  on subsequent fetches) all tags. The only case that needs an extra
   *  fetch is a `rev = "<sha>"` that's only reachable via a non-default ref
   *  on the remote — for that we do an explicit `git fetch origin <sha>` if
   *  `cat-file` says the sha is missing. */
  private def ensureRefAvailable(
      url: String,
      refKind: GitRefKind,
      refName: String,
      maybeSha: Option[String],
  ): Either[String, Unit] =
    refKind match
      case GitRefKind.Rev =>
        val sha = maybeSha.getOrElse(refName)
        val bare = barePath(url)
        // cat-file -e exits 0 iff the object exists locally.
        val present = scala.sys.process.Process(
          Seq("git", "-C", bare.getAbsolutePath, "cat-file", "-e", sha)
        ).!(silentLogger) == 0
        if present then Right(())
        else runGit(Seq("git", "-C", bare.getAbsolutePath, "fetch", "--quiet", "origin", sha)).map(_ => ())
      case _ => Right(())

  private def resolveRef(url: String, refKind: GitRefKind, refName: String): Either[String, String] =
    val bare = barePath(url)
    val target = refKind match
      case GitRefKind.Rev    => refName
      case GitRefKind.Tag    => s"refs/tags/$refName"
      case GitRefKind.Branch => s"refs/heads/$refName"
    runGitCapture(Seq("git", "-C", bare.getAbsolutePath, "rev-parse", "--verify", s"$target^{commit}"))
      .map(_.trim)
      .filterOrElse(s => s.matches("[0-9a-f]{40}"), s"git rev-parse: unexpected output for $target")

  /** Materialize a working-tree-style checkout via `git --work-tree=DIR
   *  checkout-index -af`. We deliberately avoid `git worktree add` because
   *  worktrees keep a back-reference into the bare and pollute its admin
   *  state — checkout-index just dumps a snapshot and is the right tool for
   *  read-only consumers. */
  private def ensureWorktree(url: String, sha: String): Either[String, String] =
    val co = checkoutPath(url, sha)
    if co.exists() then Right(co.getAbsolutePath)
    else
      val bare = barePath(url)
      co.mkdirs()
      val gitDir = bare.getAbsolutePath
      for
        _ <- runGit(Seq("git", "--git-dir", gitDir, "--work-tree", co.getAbsolutePath,
                         "read-tree", sha))
        _ <- runGit(Seq("git", "--git-dir", gitDir, "--work-tree", co.getAbsolutePath,
                         "checkout-index", "-af"))
      yield co.getAbsolutePath

  private def runGit(cmd: Seq[String]): Either[String, Unit] =
    val errBuf = new StringBuilder
    val logger = ProcessLogger(_ => (), line => errBuf.append(line).append('\n'))
    val rc = try Process(cmd).!(logger)
             catch case e: java.io.IOException => return Left(s"git invocation failed: ${e.getMessage} (is `git` on PATH?)")
    if rc == 0 then Right(())
    else Left(s"${cmd.mkString(" ")} (exit $rc): ${errBuf.toString.trim}")

  private def runGitCapture(cmd: Seq[String]): Either[String, String] =
    val outBuf = new StringBuilder
    val errBuf = new StringBuilder
    val logger = ProcessLogger(line => outBuf.append(line).append('\n'),
                               line => errBuf.append(line).append('\n'))
    val rc = try Process(cmd).!(logger)
             catch case e: java.io.IOException => return Left(s"git invocation failed: ${e.getMessage}")
    if rc == 0 then Right(outBuf.toString)
    else Left(s"${cmd.mkString(" ")} (exit $rc): ${errBuf.toString.trim}")

  private val silentLogger = ProcessLogger(_ => (), _ => ())

object JvmGitFetcher:
  /** Default cache root: `$SYSL_CACHE_DIR` if set, else `~/.sysl/cache`. */
  def defaultCacheRoot: String =
    Option(System.getenv("SYSL_CACHE_DIR")).getOrElse {
      val home = Option(System.getProperty("user.home")).getOrElse(".")
      new File(new File(home, ".sysl"), "cache").getAbsolutePath
    }

  def default: JvmGitFetcher = new JvmGitFetcher(defaultCacheRoot)
