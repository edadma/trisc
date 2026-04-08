package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.sys.process.*

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

  def llvmOutput(source: String): String = runLLVM(source)._2

  def llvmExit(source: String): Int = runLLVM(source)._1

  def llvmOutputMulti(sources: Map[String, String]): String = runLLVMMulti(sources)._2

  def llvmExitMulti(sources: Map[String, String]): Int = runLLVMMulti(sources)._1
}
