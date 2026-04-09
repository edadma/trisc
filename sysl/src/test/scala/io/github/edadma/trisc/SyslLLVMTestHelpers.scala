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
    * Resolves imports by including std source files in the compilation. */
  def compileLLVMWithStd(source: String): String =
    val tangler = (raw: String) => LiterateRenderer.tangle(new LiterateParser().parse(raw))
    // First pass: parse to find imports
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val stdImports = ast.decls.collect { case ImportDeclAST(path, _) if path.startsWith("std/") => path }
    // Collect std source files for each import
    val stdSources = stdImports.flatMap { modPath =>
      val dir = new File(modPath)
      if dir.isDirectory then
        dir.listFiles().filter(_.getName.endsWith(".lsysl")).map { f =>
          val key = modPath + "/" + f.getName.stripSuffix(".lsysl")
          key -> tangler(TestFileOps.readFile(f.getPath))
        }.toList
      else
        val singleFile = modPath + ".lsysl"
        if new File(singleFile).exists() then
          List(modPath -> tangler(TestFileOps.readFile(singleFile)))
        else Nil
    }.toMap
    val allSources = stdSources + ("main" -> source)
    val driver = new SyslDriver(Some(TestFileOps), List("."), tangler = Some(tangler))
    val result = driver.compile(allSources)
    val merged = TProgram(result.units.flatMap(u => u.typed.decls.filter {
      case f: TFunDecl => !f.attributes.exists(_.name == "test")
      case _ => true
    }))
    (new SyslLLVMCodegen).generate(merged)

  private def runIR(ir: String): (Int, String) =
    withTempDir { dir =>
      val llFile = dir.resolve("test.ll")
      val exeFile = dir.resolve("test")
      Files.writeString(llFile, ir)
      val compileResult = Process(Seq("clang", "-w", llFile.toString, "-o", exeFile.toString)).!
      if compileResult != 0 then
        fail(s"clang failed with exit code $compileResult\n\nLLVM IR:\n$ir")
      val outBuf = new StringBuilder
      val errBuf = new StringBuilder
      val exitCode = Process(exeFile.toString).!(ProcessLogger(s => { outBuf ++= s; outBuf += '\n' }, s => { errBuf ++= s; errBuf += '\n' }))
      (exitCode, outBuf.toString.stripSuffix("\n"))
    }

  def runLLVM(source: String): (Int, String) = runIR(compileLLVM(source))

  def runLLVMMulti(sources: Map[String, String]): (Int, String) = runIR(compileLLVMMulti(sources))

  def runLLVMWithStd(source: String): (Int, String) = runIR(compileLLVMWithStd(source))

  def llvmOutput(source: String): String = runLLVM(source)._2

  def llvmExit(source: String): Int = runLLVM(source)._1

  def llvmOutputMulti(sources: Map[String, String]): String = runLLVMMulti(sources)._2

  def llvmExitMulti(sources: Map[String, String]): Int = runLLVMMulti(sources)._1

  def llvmOutputWithStd(source: String): String = runLLVMWithStd(source)._2

  def llvmExitWithStd(source: String): Int = runLLVMWithStd(source)._1
}
