package io.github.edadma.trisc

import java.nio.file.{Files, Path, Paths}

class SyslModulePathTests extends SyslTestHelpers {

  // ===== Project-root marker (`sysl.toml`) =====
  //
  // Without a marker, the driver computes a file's expected module path from
  // its full filesystem directory (joined with dots). That's fine for the
  // trisc tree because tests pass in-tree relative paths, but it makes
  // sysl-native repos unusable: `parsyl/parsyl/parsyl.sysl` invoked by an
  // absolute path forces `module Users.ed.dev.parsyl.parsyl`, which is
  // obviously not a usable convention.
  //
  // The marker file (`sysl.toml`) at a directory designates that directory
  // as the project root. Files under it have their module paths computed
  // *relative to the marker dir*, so `<root>/parsyl/parsyl.sysl` declares
  // `module parsyl` regardless of where `<root>` sits on disk. Files outside
  // any marker'd tree fall back to today's path-based behaviour, so the
  // trisc/std/oskit suites keep working without needing a marker.

  private val testFileOps: FileOps = new FileOps:
    def readFile(path: String): String = scala.io.Source.fromFile(path).mkString
    def writeFile(path: String, content: String): Unit =
      Files.writeString(Paths.get(path), content)
    def exists(path: String): Boolean = java.io.File(path).exists()
    def isDirectory(path: String): Boolean = java.io.File(path).isDirectory
    def listFiles(path: String): Seq[String] = java.io.File(path).listFiles().map(_.getPath).toSeq
    def fileName(path: String): String = java.io.File(path).getName
    def mkdirs(path: String): Unit = java.io.File(path).mkdirs()
    def joinPath(dir: String, name: String): String = Paths.get(dir, name).toString

  /** Create a temp dir, write the given files into it (relative paths →
   *  contents), return the absolute root. Caller is responsible for cleanup
   *  via `cleanup`. */
  private def setupTempProject(files: Map[String, String]): Path =
    val tmp = Files.createTempDirectory("sysl-modpath-test-")
    for (relPath, content) <- files do
      val abs = tmp.resolve(relPath)
      Files.createDirectories(abs.getParent)
      Files.writeString(abs, content)
    tmp

  private def cleanup(root: Path): Unit =
    if Files.exists(root) then
      Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)

  /** Compile a single .sysl/.lsysl file by its filesystem path, applying
   *  project-marker discovery the same way the CLI does. Returns the unit
   *  count. Mirrors `SyslCli.resolveSource` minus the option-plumbing. */
  private def compileFile(path: String): Int =
    val key = SyslDriver.computeSourceKey(testFileOps, path)
    val raw = scala.io.Source.fromFile(path).mkString
    val source =
      if path.endsWith(".lsysl") then LiterateRenderer.tangle(new LiterateParser().parse(raw))
      else raw
    val driver = new SyslDriver(Some(testFileOps))
    val result = driver.compile(Map(key -> source))
    result.units.length

  // ===== findProjectRoot =====

  "findProjectRoot returns the dir containing the marker, walking up" in {
    val tmp = setupTempProject(Map(
      "sysl.toml" -> "",
      "foo/bar/baz.sysl" -> "module foo.bar\nx() -> int = 1\n",
    ))
    try
      val found = SyslDriver.findProjectRoot(testFileOps, tmp.resolve("foo/bar/baz.sysl").toString)
      found shouldBe Some(tmp.toString)
    finally cleanup(tmp)
  }

  "findProjectRoot returns None when no marker is present anywhere" in {
    val tmp = setupTempProject(Map(
      "foo/bar/baz.sysl" -> "module foo.bar\nx() -> int = 1\n",
    ))
    try
      val found = SyslDriver.findProjectRoot(testFileOps, tmp.resolve("foo/bar/baz.sysl").toString)
      // We may not find a marker walking up to /, but a stray marker on the
      // ambient filesystem shouldn't make this assertion flaky — just check
      // that nothing inside `tmp` was reported as the root.
      assert(found.forall(p => !p.startsWith(tmp.toString)),
        s"unexpected project root inside tmp: $found")
    finally cleanup(tmp)
  }

  "findProjectRoot picks the closest ancestor when markers nest" in {
    val tmp = setupTempProject(Map(
      "sysl.toml" -> "",
      "inner/sysl.toml" -> "",
      "inner/foo.sysl" -> "module foo\nx() -> int = 1\n",
    ))
    try
      val found = SyslDriver.findProjectRoot(testFileOps, tmp.resolve("inner/foo.sysl").toString)
      found shouldBe Some(tmp.resolve("inner").toString)
    finally cleanup(tmp)
  }

  // ===== End-to-end: marker enables short module names =====

  "module declaration relative to project-root marker is accepted" in {
    val tmp = setupTempProject(Map(
      "sysl.toml" -> "",
      "foo/foo.sysl" -> "module foo\nbar() -> int = 42\n",
    ))
    try compileFile(tmp.resolve("foo/foo.sysl").toString) shouldBe 1
    finally cleanup(tmp)
  }

  "marker found multiple levels up; module path uses relative dirs" in {
    val tmp = setupTempProject(Map(
      "sysl.toml" -> "",
      "lib/core/core.sysl" -> "module lib.core\nx() -> int = 1\n",
    ))
    try compileFile(tmp.resolve("lib/core/core.sysl").toString) shouldBe 1
    finally cleanup(tmp)
  }

  "top-of-project file declaring `module foo` for `foo.sysl` is accepted" in {
    // A single-segment module path on a top-level file (relative to root)
    // hits the existing `actualModPath == fullFilePath` branch in Step 3b.
    val tmp = setupTempProject(Map(
      "sysl.toml" -> "",
      "foo.sysl" -> "module foo\nx() -> int = 1\n",
    ))
    try compileFile(tmp.resolve("foo.sysl").toString) shouldBe 1
    finally cleanup(tmp)
  }

  "absolute-path invocation without marker still rejects mismatched modules" in {
    // Pin the no-regression contract: when there is no marker, the existing
    // path-based validation still fires for an obviously-wrong declaration.
    // Use `.sysl` here (not `.lsysl`) so the source isn't run through the
    // literate tangler — the test is about Step 3b validation, not literate
    // parsing.
    val tmp = setupTempProject(Map(
      "wrong/path/file.sysl" -> "module some.other.place\nf() -> int = 0\n",
    ))
    try
      val ex = intercept[Exception] {
        compileFile(tmp.resolve("wrong/path/file.sysl").toString)
      }
      assert(ex.getMessage.toLowerCase.contains("module declaration"),
        s"expected a module-declaration error, got: ${ex.getMessage}")
    finally cleanup(tmp)
  }

  "marker doesn't loosen validation: wrong module under a marker still rejects" in {
    val tmp = setupTempProject(Map(
      "sysl.toml" -> "",
      "foo/foo.sysl" -> "module nope\nx() -> int = 1\n",
    ))
    try
      val ex = intercept[Exception] {
        compileFile(tmp.resolve("foo/foo.sysl").toString)
      }
      assert(ex.getMessage.toLowerCase.contains("module declaration"),
        s"marker must not bypass validation entirely, got: ${ex.getMessage}")
    finally cleanup(tmp)
  }

  // ===== Backward compat: in-tree sources keep working =====

  "in-tree relative path with no marker uses today's path-based mapping" in {
    // Mirrors what `sbt "syslCliJVM/run test std/option/option.sysl"` does:
    // baseDir empty + no marker → key derived from full slash-stripped path,
    // module declaration must match dotted directory.
    val driver = new SyslDriver
    val sources = Map(
      "std/option/option" -> "module std.option\nf() -> int = 0\n",
    )
    val result = driver.compile(sources)
    result.units.length shouldBe 1
  }
}
