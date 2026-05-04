package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

/** Integration tests for `[dependencies]` resolution in `sysl.toml`.
 *
 *  These tests stage two-project fixtures under `target/sysl_test_dep/` (a
 *  separate root from the test-runner suite to avoid label-id collisions) and
 *  run the real `sysl test` CLI against the consumer project. Each consumer's
 *  `sysl.toml` declares the producer project as a path dep; the resolver
 *  must (a) find the dep's project root, (b) add it to the import-search
 *  path, so the analyzer sees the dep's modules under their declared names.
 */
class SyslCliTomlDepTests extends AnyFreeSpec with Matchers {

  FileOps.instance = JvmFileOps

  private def runCli(args: String*): (Int, String) = CliCapture.runCli(args)

  private val stagingCounter = new java.util.concurrent.atomic.AtomicInteger(0)

  private def stagingDir(label: String): (Path, String) =
    val baseRel = "target/sysl_test_dep"
    val baseAbs = java.nio.file.Paths.get(baseRel).toAbsolutePath
    Files.createDirectories(baseAbs)
    val unique = stagingCounter.incrementAndGet()
    val abs = baseAbs.resolve(s"${label}_$unique")
    Files.createDirectories(abs)
    val rel = s"$baseRel/${label}_$unique"
    (abs, rel)

  private def writeFile(dir: Path, rel: String, contents: String): Path =
    val p = dir.resolve(rel)
    Files.createDirectories(p.getParent)
    Files.writeString(p, contents)
    p

  "test against an app that path-depends on a lib resolves the lib's modules" in {
    val (abs, rel) = stagingDir("pathdep")

    // Producer — a tiny `lib` project that exposes one function.
    writeFile(abs, "lib/sysl.toml",
      """[package]
        |name = "depfix_lib"
        |version = "0.1.0"
        |""".stripMargin)
    writeFile(abs, "lib/lib/lib.lsysl",
      """    module lib
        |
        |    answer() -> int = 42
        |""".stripMargin)

    // Consumer — declares the producer as a path dep and tests against it.
    writeFile(abs, "app/sysl.toml",
      """[package]
        |name = "depfix_app"
        |version = "0.1.0"
        |
        |[dependencies]
        |depfix_lib = { path = "../lib" }
        |""".stripMargin)
    writeFile(abs, "app/app/main.lsysl",
      """    module app
        |
        |    import lib.{answer}
        |
        |    #test
        |    test_uses_lib() -> unit
        |        if answer() != 42 then panic("dep call broken")
        |""".stripMargin)

    val (code, out) = runCli("test", s"$rel/app/app/main.lsysl")
    withClue(out) {
      code shouldBe 0
      out should include("running 1 tests")
      out should include("test_uses_lib")
      out should include("1 passed")
    }
  }

  // CliError is swallowed by execute(), so the exit code stays 0 on resolver
  // failures. The message-only assertions below verify the diagnostic surfaces
  // properly while leaving room for a future CLI change to start propagating
  // exit codes — at which point these tests can tighten without refactoring.

  "an unresolvable path dep produces a clear error" in {
    val (abs, rel) = stagingDir("missing")
    writeFile(abs, "app/sysl.toml",
      """[package]
        |name = "missing_consumer"
        |version = "0.1.0"
        |
        |[dependencies]
        |nope = { path = "../does-not-exist" }
        |""".stripMargin)
    writeFile(abs, "app/app/main.lsysl",
      """    module app
        |
        |    #test
        |    t() -> unit = ()
        |""".stripMargin)

    val (_, out) = runCli("test", s"$rel/app/app/main.lsysl")
    withClue(out) {
      out should include("dependency `nope`")
      out should include("does-not-exist")
      // Must not have run any tests — a failed dep resolution must abort.
      out should not include "1 passed"
    }
  }

  "a malformed sysl.toml produces a parse error pointing at the file" in {
    val (abs, rel) = stagingDir("badtoml")
    writeFile(abs, "app/sysl.toml",
      """[package
        |name = "broken"
        |""".stripMargin)
    writeFile(abs, "app/app/main.lsysl",
      """    module app
        |
        |    #test
        |    t() -> unit = ()
        |""".stripMargin)

    val (_, out) = runCli("test", s"$rel/app/app/main.lsysl")
    withClue(out) {
      out should include("sysl.toml")
      out should not include "1 passed"
    }
  }

  "an absolute path dep is honored as-is, not appended to the consumer dir" in {
    val (abs, rel) = stagingDir("absdep")
    // Producer in its own directory at an absolute path.
    val libAbs = abs.resolve("alib").toString
    writeFile(abs, "alib/sysl.toml",
      """[package]
        |name = "alib"
        |version = "0.1.0"
        |""".stripMargin)
    writeFile(abs, "alib/lib/lib.lsysl",
      """    module lib
        |
        |    answer() -> int = 7
        |""".stripMargin)
    writeFile(abs, "app/sysl.toml",
      s"""[package]
         |name = "consumer_abs"
         |version = "0.1.0"
         |
         |[dependencies]
         |alib = { path = "$libAbs" }
         |""".stripMargin)
    writeFile(abs, "app/app/main.lsysl",
      """    module app
        |
        |    import lib.{answer}
        |
        |    #test
        |    test_abs_dep() -> unit
        |        if answer() != 7 then panic("absolute-path dep broken")
        |""".stripMargin)

    val (code, out) = runCli("test", s"$rel/app/app/main.lsysl")
    withClue(out) {
      code shouldBe 0
      out should include("running 1 tests")
      out should include("1 passed")
    }
  }

  // Workspace tests (chunk 3): pointing the CLI at a workspace root iterates
  // over every member, running each member as its own compilation. Module
  // keys live in each member's own namespace, so two members can declare the
  // same internal module name without colliding.

  "a workspace tests every member from the workspace root" in {
    val (abs, rel) = stagingDir("workspace")

    // Workspace root: no [package], just the member list.
    writeFile(abs, "sysl.toml",
      """[workspace]
        |members = ["alpha", "beta"]
        |""".stripMargin)

    // Member alpha: one #test.
    writeFile(abs, "alpha/sysl.toml",
      """[package]
        |name = "alpha"
        |version = "0.1.0"
        |""".stripMargin)
    writeFile(abs, "alpha/alpha/foo.lsysl",
      """    module alpha
        |
        |    #test
        |    test_alpha_one() -> unit
        |        if false then panic("never")
        |""".stripMargin)

    // Member beta: a different #test, in its own namespace.
    writeFile(abs, "beta/sysl.toml",
      """[package]
        |name = "beta"
        |version = "0.1.0"
        |""".stripMargin)
    writeFile(abs, "beta/beta/bar.lsysl",
      """    module beta
        |
        |    #test
        |    test_beta_one() -> unit
        |        if false then panic("never")
        |""".stripMargin)

    val (code, out) = runCli("test", rel)
    withClue(out) {
      code shouldBe 0
      out should include("workspace at")
      out should include("test_alpha_one")
      out should include("test_beta_one")
      out should include("workspace total: 2 passed")
    }
  }

  "a workspace member that path-deps on a sibling resolves cross-member" in {
    val (abs, rel) = stagingDir("workspace_xdep")

    writeFile(abs, "sysl.toml",
      """[workspace]
        |members = ["lib", "app"]
        |""".stripMargin)

    writeFile(abs, "lib/sysl.toml",
      """[package]
        |name = "ws_lib"
        |version = "0.1.0"
        |""".stripMargin)
    writeFile(abs, "lib/lib/lib.lsysl",
      """    module lib
        |
        |    answer() -> int = 11
        |""".stripMargin)

    // Member app declares the sibling as a path dep — exactly the same form
    // a non-workspace consumer would use. The workspace toml gives no
    // implicit cross-member visibility; deps stay explicit.
    writeFile(abs, "app/sysl.toml",
      """[package]
        |name = "ws_app"
        |version = "0.1.0"
        |
        |[dependencies]
        |ws_lib = { path = "../lib" }
        |""".stripMargin)
    writeFile(abs, "app/app/main.lsysl",
      """    module app
        |
        |    import lib.{answer}
        |
        |    #test
        |    test_app_uses_lib() -> unit
        |        if answer() != 11 then panic("cross-member dep broken")
        |""".stripMargin)

    val (code, out) = runCli("test", rel)
    withClue(out) {
      code shouldBe 0
      out should include("test_app_uses_lib")
      out should include("workspace total: 1 passed")
    }
  }

  "a workspace declaring a missing member fails with a clear error" in {
    val (abs, rel) = stagingDir("workspace_missing")

    writeFile(abs, "sysl.toml",
      """[workspace]
        |members = ["present", "absent"]
        |""".stripMargin)

    writeFile(abs, "present/sysl.toml",
      """[package]
        |name = "present_pkg"
        |version = "0.1.0"
        |""".stripMargin)
    writeFile(abs, "present/present/p.lsysl",
      """    module present
        |
        |    #test
        |    t() -> unit = ()
        |""".stripMargin)
    // `absent/` doesn't exist — resolver must surface a clear member error.

    val (_, out) = runCli("test", rel)
    withClue(out) {
      out should include("workspace member `absent`")
      out should not include "workspace total"
    }
  }

  "a git dep on a CLI built without a git fetcher fails with a clear error" in {
    // GitFetcherProvider.instance is None when no JVM main has run, but the
    // suite setup may have left a fetcher installed by a sibling test
    // class. Save / restore it so tests stay independent regardless of
    // suite execution order.
    val saved = GitFetcherProvider.instance
    GitFetcherProvider.instance = None
    try
      val (abs, rel) = stagingDir("gitdep")
      writeFile(abs, "app/sysl.toml",
        """[package]
          |name = "gitdep_consumer"
          |version = "0.1.0"
          |
          |[dependencies]
          |something = { git = "https://example.invalid/repo", rev = "abc" }
          |""".stripMargin)
      writeFile(abs, "app/app/main.lsysl",
        """    module app
          |
          |    #test
          |    t() -> unit = ()
          |""".stripMargin)

      val (_, out) = runCli("test", s"$rel/app/app/main.lsysl")
      withClue(out) {
        out should include("git dependencies are not supported here")
        out should not include "1 passed"
      }
    finally GitFetcherProvider.instance = saved
  }
}
