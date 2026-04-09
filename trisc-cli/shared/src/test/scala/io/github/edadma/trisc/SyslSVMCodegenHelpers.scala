package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

trait SyslSVMCodegenHelpers extends AnyFreeSpec with Matchers {

  def compile(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslSVMCodegen).generate(typed)

  def compileAndRun(source: String, maxCycles: Int = 100000): Long =
    val asm = compile(source)
    val tof = svmAssemble(asm, relocatable = true)
    val linked = Linker.link(Seq(SVMRuntime.bootTof, tof, SVMRuntime.ioTof))
    val stdout = new Stdout(SVMRuntime.stdoutAddress)
    val ram = new RAM(0, SVMRuntime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val svm = new SVM(mem) { limit = maxCycles }
    svm.reset()
    svm.run()
    svm.result

  def compileMultiAndRun(sources: Map[String, String], maxCycles: Int = 100000): Long =
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val codegen = new SyslSVMCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      svmAssemble(asm, relocatable = true)
    val linked = Linker.link(Seq(SVMRuntime.bootTof) ++ tofs ++ Seq(SVMRuntime.ioTof))
    val stdout = new Stdout(SVMRuntime.stdoutAddress)
    val ram = new RAM(0, SVMRuntime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val svm = new SVM(mem) { limit = maxCycles }
    svm.reset()
    svm.run()
    svm.result
}
