package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import scala.sys.process.*

/** Integration tests for git-source dep resolution + cache (chunk 5).
 *
 *  Hermetic by construction: every test stages a tiny on-disk git repo
 *  under `target/sysl_test_git/` and points consumers at it via a `file://`
 *  URL, and every test runs against an isolated `JvmGitFetcher` rooted at a
 *  fresh tempdir (so we never touch `~/.sysl/cache`).
 *
 *  Tests that don't want git involvement at all keep using the regular
 *  CLI entry point; this file owns the fetcher-installation/teardown so
 *  the suite stays restartable in any order.
 */
class SyslCliGitDepTests extends AnyFreeSpec with Matchers {

  FileOps.instance = JvmFileOps

  private def runCli(args: String*): (Int, String) = CliCapture.runCli(args)

  private val stagingCounter = new java.util.concurrent.atomic.AtomicInteger(0)

  private def stagingDir(label: String): (Path, String) =
    val baseRel = "target/sysl_test_git"
    val baseAbs = java.nio.file.Paths.get(baseRel).toAbsolutePath
    Files.createDirectories(baseAbs)
    val unique = stagingCounter.incrementAndGet()
    val abs = baseAbs.resolve(s"${label}_$unique")
    if Files.exists(abs) then deleteRecursive(abs)
    Files.createDirectories(abs)
    val rel = s"$baseRel/${label}_$unique"
    (abs, rel)

  private def deleteRecursive(p: Path): Unit =
    if Files.isDirectory(p) then
      val it = Files.list(p)
      try it.forEach(child => deleteRecursive(child))
      finally it.close()
    Files.deleteIfExists(p)

  private def writeFile(dir: Path, rel: String, contents: String): Path =
    val p = dir.resolve(rel)
    Files.createDirectories(p.getParent)
    Files.writeString(p, contents)
    p

  /** Run a git command in `cwd`. Throws on non-zero so test setup fails
   *  loudly rather than silently producing a malformed fixture. */
  private def git(cwd: Path, args: String*): String =
    val out = new StringBuilder
    val err = new StringBuilder
    val log = ProcessLogger(line => out.append(line).append('\n'),
                            line => err.append(line).append('\n'))
    val rc = Process(Seq("git") ++ args, cwd.toFile).!(log)
    if rc != 0 then
      throw new RuntimeException(
        s"git ${args.mkString(" ")} (in $cwd) failed: ${err.toString.trim}")
    out.toString.trim

  /** Initialize a working tree at `dir`, commit the supplied files (path →
   *  contents), and return both the resulting commit's sha and a `file://`
   *  URL that fetchers can clone from. The commit-time identity is set
   *  inline so this works on systems with no global git user.email. */
  private def makeFixtureRepo(dir: Path, files: Map[String, String]): (String, String) =
    Files.createDirectories(dir)
    git(dir, "init", "--quiet", "--initial-branch=main")
    git(dir, "config", "user.email", "test@example.invalid")
    git(dir, "config", "user.name", "Test")
    for (rel, contents) <- files do
      val f = dir.resolve(rel)
      Files.createDirectories(f.getParent)
      Files.writeString(f, contents)
      git(dir, "add", rel)
    git(dir, "commit", "--quiet", "-m", "fixture commit")
    val sha = git(dir, "rev-parse", "HEAD")
    val url = "file://" + dir.toAbsolutePath.toString
    (sha, url)

  /** Standard fixture: a tiny package at `repoDir` whose `lib/lib.lsysl`
   *  exports an `answer()` constant. Returned (sha, url) point at the
   *  initial commit. */
  private def libFixture(repoDir: Path, pkgName: String, version: String, answer: Int): (String, String) =
    makeFixtureRepo(repoDir, Map(
      "sysl.toml" ->
        s"""[package]
           |name = "$pkgName"
           |version = "$version"
           |""".stripMargin,
      "lib/lib.lsysl" ->
        s"""    module lib
           |
           |    answer() -> int = $answer
           |""".stripMargin,
    ))

  /** Per-test fetcher anchored at a fresh cache root. Restored on exit so
   *  one test's fetcher doesn't bleed into another. */
  private def withFetcher[A](label: String)(body: (Path, JvmGitFetcher) => A): A =
    // Hold CliCapture.ioLock for the whole body so other suites' tests can't
    // overwrite GitFetcherProvider.instance between two of our runCli calls.
    CliCapture.ioLock.synchronized {
      val cacheRoot = java.nio.file.Paths.get("target", "sysl_test_git_cache", label + "_" + stagingCounter.incrementAndGet()).toAbsolutePath
      if Files.exists(cacheRoot) then deleteRecursive(cacheRoot)
      Files.createDirectories(cacheRoot)
      val fetcher = new JvmGitFetcher(cacheRoot.toString)
      val saved = GitFetcherProvider.instance
      GitFetcherProvider.instance = Some(fetcher)
      try body(cacheRoot, fetcher)
      finally GitFetcherProvider.instance = saved
    }

  "a git dep with `branch = main` clones, runs the consumer's tests, and locks the resolved sha" in
    withFetcher("branch") { (cacheRoot, _) =>
      val (abs, rel) = stagingDir("branch")
      val (sha, url) = libFixture(abs.resolve("repo"), "branch_lib", "0.3.0", 7)
      writeFile(abs, "app/sysl.toml",
        s"""[package]
           |name = "branch_app"
           |version = "0.1.0"
           |
           |[dependencies]
           |branch_lib = { git = "$url", branch = "main" }
           |""".stripMargin)
      writeFile(abs, "app/app/main.lsysl",
        """    module app
          |
          |    import lib.{answer}
          |
          |    #test
          |    t() -> unit
          |        if answer() != 7 then panic("wrong answer")
          |""".stripMargin)

      val (code, out) = runCli("test", s"$rel/app/app/main.lsysl")
      withClue(out) { code shouldBe 0 }

      val lock = Files.readString(abs.resolve("app/sysl.lock"))
      lock should include("branch_lib")
      lock should include(s"git+$url?branch=main#$sha")

      // Cache layout produced by JvmGitFetcher.
      val gitDir = cacheRoot.resolve("git")
      Files.isDirectory(gitDir.resolve("db")) shouldBe true
      Files.isDirectory(gitDir.resolve("checkouts")) shouldBe true
    }

  "a git dep with `rev = <sha>` honors the explicit pin and records it in the lock" in
    withFetcher("rev") { (_, _) =>
      val (abs, rel) = stagingDir("rev")
      val (sha, url) = libFixture(abs.resolve("repo"), "rev_lib", "0.4.0", 11)
      writeFile(abs, "app/sysl.toml",
        s"""[package]
           |name = "rev_app"
           |version = "0.1.0"
           |
           |[dependencies]
           |rev_lib = { git = "$url", rev = "$sha" }
           |""".stripMargin)
      writeFile(abs, "app/app/main.lsysl",
        """    module app
          |
          |    import lib.{answer}
          |
          |    #test
          |    t() -> unit
          |        if answer() != 11 then panic("wrong rev answer")
          |""".stripMargin)

      val (code, out) = runCli("test", s"$rel/app/app/main.lsysl")
      withClue(out) { code shouldBe 0 }

      val lock = Files.readString(abs.resolve("app/sysl.lock"))
      lock should include(s"git+$url?rev=$sha#$sha")
    }

  "a git dep with `tag = v1.0` resolves the tag to a sha" in
    withFetcher("tag") { (_, _) =>
      val (abs, rel) = stagingDir("tag")
      val repoDir = abs.resolve("repo")
      val (sha, url) = libFixture(repoDir, "tag_lib", "1.0.0", 13)
      git(repoDir, "tag", "v1.0")

      writeFile(abs, "app/sysl.toml",
        s"""[package]
           |name = "tag_app"
           |version = "0.1.0"
           |
           |[dependencies]
           |tag_lib = { git = "$url", tag = "v1.0" }
           |""".stripMargin)
      writeFile(abs, "app/app/main.lsysl",
        """    module app
          |
          |    import lib.{answer}
          |
          |    #test
          |    t() -> unit
          |        if answer() != 13 then panic("wrong tag answer")
          |""".stripMargin)

      val (code, out) = runCli("test", s"$rel/app/app/main.lsysl")
      withClue(out) { code shouldBe 0 }

      val lock = Files.readString(abs.resolve("app/sysl.lock"))
      lock should include(s"git+$url?tag=v1.0#$sha")
    }

  "`sysl fetch` honors a lock-pinned sha even when the upstream branch advances" in
    withFetcher("pin") { (_, _) =>
      val (abs, rel) = stagingDir("pin")
      val repoDir = abs.resolve("repo")
      val (firstSha, url) = libFixture(repoDir, "pin_lib", "0.1.0", 1)

      writeFile(abs, "app/sysl.toml",
        s"""[package]
           |name = "pin_app"
           |version = "0.1.0"
           |
           |[dependencies]
           |pin_lib = { git = "$url", branch = "main" }
           |""".stripMargin)
      writeFile(abs, "app/app/main.lsysl",
        """    module app
          |    #test
          |    t() -> unit = ()
          |""".stripMargin)

      // First fetch resolves branch=main → firstSha and writes the lock.
      val (c1, out1) = runCli("fetch", s"$rel/app")
      withClue(out1) { c1 shouldBe 0 }
      val lock1 = Files.readString(abs.resolve("app/sysl.lock"))
      lock1 should include(s"#$firstSha")

      // Advance the upstream branch by adding a second commit.
      writeFile(repoDir, "lib/extra.lsysl",
        """    module lib
          |
          |    extra() -> int = 99
          |""".stripMargin)
      git(repoDir, "add", "lib/extra.lsysl")
      git(repoDir, "commit", "--quiet", "-m", "second")
      val secondSha = git(repoDir, "rev-parse", "HEAD")
      secondSha should not be firstSha

      // Second fetch should keep firstSha (pin honored) and skip rewriting.
      val (c2, out2) = runCli("fetch", s"$rel/app")
      withClue(out2) {
        c2 shouldBe 0
        out2 should include("up to date")
      }
      Files.readString(abs.resolve("app/sysl.lock")) shouldBe lock1

      // `sysl update` should re-resolve to the new sha.
      val (c3, out3) = runCli("update", s"$rel/app")
      withClue(out3) {
        c3 shouldBe 0
        out3 should include("rewrote sysl.lock")
      }
      val lock3 = Files.readString(abs.resolve("app/sysl.lock"))
      lock3 should include(s"#$secondSha")
      lock3 should not include s"#$firstSha"
    }

  "two consumers depending on different shas of the same url produce a clear conflict error" in
    withFetcher("conflict") { (_, _) =>
      val (abs, rel) = stagingDir("conflict")
      val repoDir = abs.resolve("repo")
      val (firstSha, url) = libFixture(repoDir, "shared_lib", "0.1.0", 1)
      writeFile(repoDir, "lib/extra.lsysl",
        """    module lib
          |    extra() -> int = 2
          |""".stripMargin)
      git(repoDir, "add", "lib/extra.lsysl")
      git(repoDir, "commit", "--quiet", "-m", "second")
      val secondSha = git(repoDir, "rev-parse", "HEAD")

      // Workspace whose two members pin the same url at different shas.
      writeFile(abs, "ws/sysl.toml",
        """[workspace]
          |members = ["a", "b"]
          |""".stripMargin)
      writeFile(abs, "ws/a/sysl.toml",
        s"""[package]
           |name = "a_app"
           |version = "0.1.0"
           |
           |[dependencies]
           |shared_lib = { git = "$url", rev = "$firstSha" }
           |""".stripMargin)
      writeFile(abs, "ws/a/a/main.lsysl",
        """    module a
          |    #test
          |    t() -> unit = ()
          |""".stripMargin)
      writeFile(abs, "ws/b/sysl.toml",
        s"""[package]
           |name = "b_app"
           |version = "0.1.0"
           |
           |[dependencies]
           |shared_lib = { git = "$url", rev = "$secondSha" }
           |""".stripMargin)
      writeFile(abs, "ws/b/b/main.lsysl",
        """    module b
          |    #test
          |    t() -> unit = ()
          |""".stripMargin)

      val (_, out) = runCli("fetch", s"$rel/ws")
      withClue(out) {
        out should include("conflicting git refs")
        out should include(url)
      }
    }

  "a git dep on a totally bogus url surfaces git's failure verbatim" in
    withFetcher("bad") { (_, _) =>
      val (abs, rel) = stagingDir("bad")
      writeFile(abs, "app/sysl.toml",
        """[package]
          |name = "bad_app"
          |version = "0.1.0"
          |
          |[dependencies]
          |bad = { git = "file:///definitely/not/a/repo/at/all", rev = "deadbeef" }
          |""".stripMargin)
      writeFile(abs, "app/app/main.lsysl",
        """    module app
          |    #test
          |    t() -> unit = ()
          |""".stripMargin)

      val (_, out) = runCli("fetch", s"$rel/app")
      withClue(out) {
        out should include("dependency `bad`")
        // The error should mention git or the bogus path so debugging
        // is straightforward.
        val mentionsGitOrPath = out.contains("git") || out.contains("/definitely/not/a/repo")
        mentionsGitOrPath shouldBe true
      }
    }
}
