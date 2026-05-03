package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

/** Integration tests for `sysl test <path>` test-discovery scope.
 *
 *  The test runner used to execute every `#test` function in every parsed
 *  unit, including modules pulled in transitively via `import`. Library
 *  authors paid a per-import test-count tax: `import std.option.{Some}`
 *  added 16 std.option tests to your run. The fix scopes discovery to the
 *  user-supplied source files (or directory walk roots).
 *
 *  These tests use temporary directories on the JVM filesystem and invoke
 *  `SyslCli.parse` + `SyslCli.execute` directly, capturing stdout to assert
 *  on the run summary. JVM-only because the integration touches `JvmFileOps`.
 */
class SyslCliTestRunnerTests extends AnyFreeSpec with Matchers {

  // Wire up the platform IO once for the whole suite. Main.scala does the
  // same on real CLI startup; without it `SyslCli` would NPE on first
  // `io.exists(...)` call inside `executeTest`.
  FileOps.instance = JvmFileOps

  /** Run `sysl <args...>` programmatically, returning the combined captured
   *  stdout + stderr, plus an exit-style code (0 = clean, 1 = SyslCli threw,
   *  2 = scopt parse failure). Redirects `System.out` (used by `println`),
   *  `System.err` (used by `fail`), and `Console.out` so all paths inside
   *  `SyslCli` are captured.
   *
   *  Args are passed verbatim — when the caller wants short relative paths,
   *  the test stages files under a directory inside the actual CWD (sbt's
   *  test runner pins `user.dir` per JVM and a mid-test mutation does not
   *  reach `java.io.File.getAbsolutePath`).
   */
  private def runCli(args: String*): (Int, String) = CliCapture.runCli(args)

  /** Stage test files under `target/sysl_test_runner/<label>_<n>/` inside
   *  the project root's actual CWD so the driver sees short relative paths
   *  matching the embedded `module …` declarations. The directory name uses
   *  only sysl-identifier-safe chars (letters, digits, underscore) so that
   *  the generated `module foo.bar.baz` declarations parse cleanly.
   */
  private def stagingDir(label: String): (Path, String) =
    val baseRel = "target/sysl_test_runner"
    val baseAbs = java.nio.file.Paths.get(baseRel).toAbsolutePath
    Files.createDirectories(baseAbs)
    val unique = SyslCliTestRunnerTests.nextStagingId()
    val abs = baseAbs.resolve(s"${label}_$unique")
    Files.createDirectories(abs)
    val rel = s"$baseRel/${label}_$unique"
    (abs, rel)

  private def writeFile(dir: Path, rel: String, contents: String): Path =
    val p = dir.resolve(rel)
    Files.createDirectories(p.getParent)
    Files.writeString(p, contents)
    p

  "test <file> runs only tests defined in <file>, skipping transitive imports" in {
    val (abs, rel) = stagingDir("scope")
    val modBase = rel.replace("/", ".")
    writeFile(abs, s"scope_a/scope_a.lsysl",
      s"""    module $modBase.scope_a
         |
         |    a_helper() -> int = 41
         |
         |    #test
         |    test_a() -> unit
         |        if a_helper() != 41 then panic("nope")
         |""".stripMargin)
    writeFile(abs, s"scope_b/scope_b.lsysl",
      s"""    module $modBase.scope_b
         |
         |    import $modBase.scope_a.{a_helper}
         |
         |    #test
         |    test_b_uses_a() -> unit
         |        if a_helper() != 41 then panic("nope")
         |""".stripMargin)
    val (code, out) = runCli("test", s"$rel/scope_b/scope_b.lsysl")
    withClue(out) {
      code shouldBe 0
      out should include("running 1 tests")
      out should include("test_b_uses_a")
      out should not include "test_a "
      out should include("1 passed")
    }
  }

  "test <file_a> <file_b> includes only tests defined in those files" in {
    val (abs, rel) = stagingDir("multi")
    val modBase = rel.replace("/", ".")
    writeFile(abs, "u_a/u_a.lsysl",
      s"""    module $modBase.u_a
         |
         |    a_val() -> int = 1
         |
         |    #test
         |    test_a_one() -> unit
         |        if a_val() != 1 then panic("a")
         |""".stripMargin)
    writeFile(abs, "u_b/u_b.lsysl",
      s"""    module $modBase.u_b
         |
         |    import $modBase.u_a.{a_val}
         |
         |    #test
         |    test_b_one() -> unit
         |        if a_val() != 1 then panic("b1")
         |
         |    #test
         |    test_b_two() -> unit
         |        if a_val() + 1 != 2 then panic("b2")
         |""".stripMargin)
    val (code, out) = runCli("test", s"$rel/u_a/u_a.lsysl", s"$rel/u_b/u_b.lsysl")
    withClue(out) {
      code shouldBe 0
      out should include("running 3 tests")
      out should include("3 passed")
    }
  }

  "test <dir> runs every test under that dir (recursive), but not external imports" in {
    val (abs, rel) = stagingDir("dir")
    val modBase = rel.replace("/", ".")
    writeFile(abs, "lib_x/lib_x.lsysl",
      s"""    module $modBase.lib_x
         |
         |    x_val() -> int = 7
         |
         |    #test
         |    test_x_external() -> unit
         |        if x_val() != 7 then panic("x")
         |""".stripMargin)
    writeFile(abs, "scoped/one/one.lsysl",
      s"""    module $modBase.scoped.one
         |
         |    import $modBase.lib_x.{x_val}
         |
         |    #test
         |    test_scoped_one() -> unit
         |        if x_val() != 7 then panic("o")
         |""".stripMargin)
    writeFile(abs, "scoped/two/two.lsysl",
      s"""    module $modBase.scoped.two
         |
         |    import $modBase.lib_x.{x_val}
         |
         |    #test
         |    test_scoped_two() -> unit
         |        if x_val() + 1 != 8 then panic("t")
         |""".stripMargin)
    val (code, out) = runCli("test", s"$rel/scoped")
    withClue(out) {
      code shouldBe 0
      out should include("running 2 tests")
      out should include("test_scoped_one")
      out should include("test_scoped_two")
      out should not include "test_x_external"
      out should include("2 passed")
    }
  }
}

private object SyslCliTestRunnerTests:
  private val counter = new java.util.concurrent.atomic.AtomicLong(0L)
  def nextStagingId(): Long = counter.incrementAndGet()
