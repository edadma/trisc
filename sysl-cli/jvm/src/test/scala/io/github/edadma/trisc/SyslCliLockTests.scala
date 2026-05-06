package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

/** Integration tests for `sysl.lock` writer + `sysl fetch` / `sysl update`
 *  subcommands (chunk 4 of the dep-resolution feature).
 *
 *  Mirrors the staging pattern used by `SyslCliTomlDepTests`: two-project
 *  fixtures live under `target/sysl_test_lock/` and run through the real
 *  CLI. Path-only deps + workspace members are the surface this chunk
 *  covers; git deps land in chunk 5 and will use the same lock schema.
 */
class SyslCliLockTests extends AnyFreeSpec with Matchers {

  FileOps.instance = JvmFileOps

  private def runCli(args: String*): (Int, String) = CliCapture.runCli(args)

  private val stagingCounter = new java.util.concurrent.atomic.AtomicInteger(0)

  private def stagingDir(label: String): (Path, String) =
    val baseRel = "target/sysl_test_lock"
    val baseAbs = java.nio.file.Paths.get(baseRel).toAbsolutePath
    Files.createDirectories(baseAbs)
    val unique = stagingCounter.incrementAndGet()
    val abs = baseAbs.resolve(s"${label}_$unique")
    // The staging counter resets each JVM, so a re-run of the suite would
    // collide with files left from the previous run. Wipe first to keep
    // each test's fixture pristine — important for tests that assert on
    // first-write vs. up-to-date paths (`sysl fetch` idempotency).
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

  private def readLock(dir: Path, sub: String = ""): String =
    val p = if sub.isEmpty then dir.resolve("sysl.lock") else dir.resolve(sub).resolve("sysl.lock")
    Files.readString(p)

  "running tests on a single-project fixture writes a sysl.lock alongside its sysl.toml" in {
    val (abs, rel) = stagingDir("single")
    writeFile(abs, "myc/sysl.toml",
      """[package]
        |name = "myc_lock"
        |version = "0.1.0"
        |""".stripMargin)
    writeFile(abs, "myc/myc/main.lsysl",
      """    module myc
        |
        |    #test
        |    t() -> unit = ()
        |""".stripMargin)

    val (code, out) = runCli("test", s"$rel/myc/myc/main.lsysl")
    withClue(out) { code shouldBe 0 }

    val lockPath = abs.resolve("myc/sysl.lock")
    Files.exists(lockPath) shouldBe true
    val lock = Files.readString(lockPath)
    lock should include("version = 1")
    lock should include("[[package]]")
    lock should include(""""myc_lock"""")
    lock should include(""""0.1.0"""")
    // Project root is local — no source field.
    lock should not include "source = "
  }

  "a path-dep is recorded with a source = path+ entry" in {
    val (abs, rel) = stagingDir("pathdep")
    writeFile(abs, "lib/sysl.toml",
      """[package]
        |name = "pathdep_lib"
        |version = "0.2.0"
        |""".stripMargin)
    writeFile(abs, "lib/lib/lib.lsysl",
      """    module lib
        |
        |    answer() -> int = 5
        |""".stripMargin)
    writeFile(abs, "app/sysl.toml",
      """[package]
        |name = "pathdep_app"
        |version = "0.1.0"
        |
        |[dependencies]
        |pathdep_lib = { path = "../lib" }
        |""".stripMargin)
    writeFile(abs, "app/app/main.lsysl",
      """    module app
        |
        |    import lib.{answer}
        |
        |    #test
        |    t() -> unit
        |        if answer() != 5 then panic("dep broken")
        |""".stripMargin)

    val (code, out) = runCli("test", s"$rel/app/app/main.lsysl")
    withClue(out) { code shouldBe 0 }

    val lock = readLock(abs, "app")
    val libAbs = abs.resolve("lib").toString
    lock should include("pathdep_app")
    lock should include("pathdep_lib")
    lock should include(s"""source = "path+$libAbs"""")
    lock should include(""""pathdep_lib 0.2.0"""")
  }

  "a workspace produces one sysl.lock at the workspace root, listing each member" in {
    val (abs, rel) = stagingDir("workspace")
    writeFile(abs, "sysl.toml",
      """[workspace]
        |members = ["beta", "alpha"]
        |""".stripMargin)
    writeFile(abs, "alpha/sysl.toml",
      """[package]
        |name = "alpha_pkg"
        |version = "0.1.0"
        |""".stripMargin)
    writeFile(abs, "alpha/alpha/foo.lsysl",
      """    module alpha
        |
        |    #test
        |    t() -> unit = ()
        |""".stripMargin)
    writeFile(abs, "beta/sysl.toml",
      """[package]
        |name = "beta_pkg"
        |version = "0.1.0"
        |""".stripMargin)
    writeFile(abs, "beta/beta/bar.lsysl",
      """    module beta
        |
        |    #test
        |    t() -> unit = ()
        |""".stripMargin)

    val (code, out) = runCli("test", rel)
    withClue(out) { code shouldBe 0 }

    // Lock lives at the workspace root, not per member.
    Files.exists(abs.resolve("alpha/sysl.lock")) shouldBe false
    Files.exists(abs.resolve("beta/sysl.lock")) shouldBe false

    val lock = readLock(abs)
    lock should include("alpha_pkg")
    lock should include("beta_pkg")
    // Members are sorted alphabetically by package name regardless of [workspace] order.
    lock.indexOf("alpha_pkg") should be < lock.indexOf("beta_pkg")
    // Workspace members are local — no source field.
    lock should not include "source = "
  }

  "`sysl fetch` writes the lock without running tests" in {
    val (abs, rel) = stagingDir("fetch")
    writeFile(abs, "sysl.toml",
      """[package]
        |name = "fetch_only"
        |version = "0.1.0"
        |""".stripMargin)
    writeFile(abs, "fetch_only/main.lsysl",
      """    module fetch_only
        |
        |    #test
        |    t() -> unit = panic("must not run")
        |""".stripMargin)

    val (code, out) = runCli("fetch", rel)
    withClue(out) {
      code shouldBe 0
      out should include("wrote sysl.lock")
      out should not include "running"
      out should not include "1 passed"
    }
    Files.exists(abs.resolve("sysl.lock")) shouldBe true
  }

  "`sysl fetch` is idempotent — second run reports up-to-date and does not rewrite" in {
    val (abs, rel) = stagingDir("fetch_idem")
    writeFile(abs, "sysl.toml",
      """[package]
        |name = "fetch_idem"
        |version = "0.1.0"
        |""".stripMargin)

    val (code1, out1) = runCli("fetch", rel)
    withClue(out1) { code1 shouldBe 0 }
    val firstMtime = Files.getLastModifiedTime(abs.resolve("sysl.lock"))

    // Sleep a bit so a fresh write would produce a different mtime.
    Thread.sleep(20)

    val (code2, out2) = runCli("fetch", rel)
    withClue(out2) {
      code2 shouldBe 0
      out2 should include("up to date")
    }
    val secondMtime = Files.getLastModifiedTime(abs.resolve("sysl.lock"))
    secondMtime shouldBe firstMtime
  }

  "`sysl update` rewrites the lock unconditionally" in {
    val (abs, rel) = stagingDir("update")
    writeFile(abs, "sysl.toml",
      """[package]
        |name = "upd"
        |version = "0.1.0"
        |""".stripMargin)
    val (codeF, _) = runCli("fetch", rel)
    codeF shouldBe 0

    // Corrupt the lock — `update` should overwrite without complaint
    // because it re-resolves from scratch and discards the prior file.
    Files.writeString(abs.resolve("sysl.lock"), "garbage = bytes\n")

    val (code, out) = runCli("update", rel)
    withClue(out) {
      code shouldBe 0
      out should include("rewrote sysl.lock")
    }
    val lock = readLock(abs)
    lock should include("version = 1")
    lock should include("upd")
    lock should not include "garbage"
  }

  "lock-file rendering is byte-stable: re-resolving the same fixture writes identical content" in {
    val (abs, rel) = stagingDir("stable")
    writeFile(abs, "sysl.toml",
      """[workspace]
        |members = ["zeta", "delta", "alpha"]
        |""".stripMargin)
    for m <- Seq("alpha", "delta", "zeta") do
      writeFile(abs, s"$m/sysl.toml",
        s"""[package]
           |name = "${m}_pkg"
           |version = "0.1.0"
           |""".stripMargin)

    val (code1, _) = runCli("fetch", rel)
    code1 shouldBe 0
    val first = Files.readString(abs.resolve("sysl.lock"))

    // Force a regen via `update` and confirm the bytes are identical.
    val (code2, _) = runCli("update", rel)
    code2 shouldBe 0
    val second = Files.readString(abs.resolve("sysl.lock"))
    second shouldBe first

    // And the package order is alphabetical, not the [workspace] order.
    first.indexOf("alpha_pkg") should be < first.indexOf("delta_pkg")
    first.indexOf("delta_pkg") should be < first.indexOf("zeta_pkg")
  }
}
