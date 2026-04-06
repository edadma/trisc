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

  def runLLVM(source: String): (Int, String) =
    val ir = compileLLVM(source)
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

  def llvmOutput(source: String): String = runLLVM(source)._2

  def llvmExit(source: String): Int = runLLVM(source)._1
}
