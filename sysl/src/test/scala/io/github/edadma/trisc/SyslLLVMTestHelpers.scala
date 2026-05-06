package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path}
import scala.sys.process.*

private object TestFileOps extends FileOps:
  def readFile(path: String): String =
    val source = scala.io.Source.fromFile(path)
    try source.mkString finally source.close()
  def writeFile(path: String, content: String): Unit =
    val writer = new PrintWriter(path)
    try writer.write(content) finally writer.close()
  def exists(path: String): Boolean = new File(path).exists()
  def isDirectory(path: String): Boolean = new File(path).isDirectory
  def listFiles(path: String): Seq[String] = new File(path).listFiles().toSeq.map(_.getPath)
  def fileName(path: String): String = new File(path).getName
  def mkdirs(path: String): Unit = new File(path).mkdirs()
  def joinPath(dir: String, name: String): String = new File(dir, name).getPath
  def absolutePath(path: String): String = new File(path).getAbsoluteFile.getPath

trait SyslLLVMTestHelpers extends AnyFreeSpec with Matchers {

  private def withTempDir[T](f: Path => T): T =
    val dir = Files.createTempDirectory("sysl-llvm-test")
    try f(dir)
    finally
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists)

  def compileLLVM(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslLLVMCodegen).generate(typed)

  /** Compile multiple source files via SyslDriver, merge into single LLVM IR. */
  def compileLLVMMulti(sources: Map[String, String]): String =
    val driver = new SyslDriver()
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    (new SyslLLVMCodegen).generate(merged)

  /** Compile a program with access to the std library.
    * Recursively resolves all transitive std imports. */
  def compileLLVMWithStd(source: String): String =
    val tangler = (raw: String) => LiterateRenderer.tangle(new LiterateParser().parse(raw))
    val collected = scala.collection.mutable.Map[String, String]()
    val visited = scala.collection.mutable.Set[String]()

    def collectStdModule(modPath: String): Unit =
      if visited.contains(modPath) then return
      visited += modPath
      val dir = new File(modPath)
      val sources: List[(String, String)] =
        if dir.isDirectory then
          dir.listFiles().toList.filter(_.getName.endsWith(".lsysl")).map { f =>
            val key = modPath + "/" + f.getName.stripSuffix(".lsysl")
            key -> tangler(TestFileOps.readFile(f.getPath))
          }
        else
          val lf = new File(modPath + ".lsysl")
          if lf.exists() then List(modPath -> tangler(TestFileOps.readFile(lf.getPath)))
          else Nil
      for (key, src) <- sources do
        collected(key) = src
        // Parse to find transitive imports
        (new SyslParser).parseProgram(src) match
          case Right(ast) =>
            for case ImportDeclAST(path, _) <- ast.decls if path.startsWith("std/") do
              collectStdModule(path)
          case _ =>

    // Parse user source for direct imports
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    for case ImportDeclAST(path, _) <- ast.decls if path.startsWith("std/") do
      collectStdModule(path)

    val allSources = collected.toMap + ("main" -> source)
    val driver = new SyslDriver(Some(TestFileOps), List("."), tangler = Some(tangler))
    val result = driver.compile(allSources)
    val merged = TProgram(result.units.flatMap(u => u.typed.decls.filter {
      case f: TFunDecl => !f.attributes.exists(_.name == "test")
      case _ => true
    }))
    (new SyslLLVMCodegen).generate(merged)

  private def runIR(ir: String, sanitize: Boolean = false): (Int, String) =
    withTempDir { dir =>
      val llFile = dir.resolve("test.ll")
      val exeFile = dir.resolve("test")
      Files.writeString(llFile, ir)
      // Files.writeString(java.nio.file.Paths.get(s"/tmp/test-${ir.length}.ll"), ir)
      val sanFlags = if sanitize then Seq("-fsanitize=address") else Seq.empty
      val compileResult = Process(Seq("clang", "-w") ++ sanFlags ++ Seq(llFile.toString, "-o", exeFile.toString)).!
      if compileResult != 0 then
        fail(s"clang failed with exit code $compileResult\n\nLLVM IR:\n$ir")
      val outBuf = new StringBuilder
      val errBuf = new StringBuilder
      val exitCode = Process(exeFile.toString).!(ProcessLogger(s => { outBuf ++= s; outBuf += '\n' }, s => { errBuf ++= s; errBuf += '\n' }))
      // Surface ASan diagnostics (which go to stderr) so a regressing test fails loudly
      // with the actual ASan report instead of just a non-zero exit.
      if sanitize && errBuf.toString.contains("AddressSanitizer") then
        fail(s"AddressSanitizer reported an error (exit $exitCode):\n${errBuf.toString}")
      (exitCode, outBuf.toString.stripSuffix("\n"))
    }

  def runLLVM(source: String): (Int, String) = runIR(compileLLVM(source))

  def runLLVMMulti(sources: Map[String, String]): (Int, String) = runIR(compileLLVMMulti(sources))

  def runLLVMWithStd(source: String): (Int, String) = runIR(compileLLVMWithStd(source))

  /** Compile + run with AddressSanitizer enabled. ASan catches use-after-free and
    * double-free; macOS does not support LeakSanitizer, but the rc work primarily
    * cares about UAF/double-free regressions (a missing incr / extra decr / wrong
    * deinit traversal all manifest as UAF). */
  def runLLVMASan(source: String): (Int, String) = runIR(compileLLVM(source), sanitize = true)

  def llvmOutput(source: String): String = runLLVM(source)._2

  def llvmExit(source: String): Int = runLLVM(source)._1

  def llvmOutputMulti(sources: Map[String, String]): String = runLLVMMulti(sources)._2

  def llvmExitMulti(sources: Map[String, String]): Int = runLLVMMulti(sources)._1

  def llvmOutputWithStd(source: String): String = runLLVMWithStd(source)._2

  def llvmExitWithStd(source: String): Int = runLLVMWithStd(source)._1

  def llvmExitASan(source: String): Int = runLLVMASan(source)._1
}
