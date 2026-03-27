package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

class SyslLLVMTests extends AnyFreeSpec with Matchers {

  def compileLLVM(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val codegen = new SyslLLVMCodegen
    val ir = codegen.generate(typed)
    // Add format strings for print/println if needed
    val withFormats =
      if ir.contains("@.fmt_d") || ir.contains("@.fmt_dn") then
        ir + """@.fmt_d = private constant [4 x i8] c"%ld\00"
               |@.fmt_dn = private constant [5 x i8] c"%ld\0A\00"
               |""".stripMargin
      else ir
    withFormats

  def compileAndRun(source: String): (Int, String) =
    val ir = compileLLVM(source)
    val tmpDir = Files.createTempDirectory("sysl")
    val llFile = tmpDir.resolve("program.ll")
    val exeFile = tmpDir.resolve("program")
    try
      Files.writeString(llFile, ir)
      val compile = new ProcessBuilder("clang", "-O0", "-o", exeFile.toString, llFile.toString)
        .redirectErrorStream(true).start()
      val compileOutput = new String(compile.getInputStream.readAllBytes())
      val compileExit = compile.waitFor()
      if compileExit != 0 then
        fail(s"clang failed ($compileExit):\n$compileOutput\n\nIR:\n$ir")

      val run = new ProcessBuilder(exeFile.toString)
        .redirectErrorStream(true).start()
      val runOutput = new String(run.getInputStream.readAllBytes())
      val exitCode = run.waitFor()
      (exitCode, runOutput)
    finally
      Files.deleteIfExists(exeFile)
      Files.deleteIfExists(llFile)
      Files.deleteIfExists(tmpDir)

  def runExitCode(source: String): Int = compileAndRun(source)._1

  def runOutput(source: String): String = compileAndRun(source)._2

  // ===== Constants =====

  "return 0" in {
    runExitCode("main() -> int = 0\n") shouldBe 0
  }

  "return 42" in {
    runExitCode("main() -> int = 42\n") shouldBe 42
  }

  // ===== Arithmetic =====

  "addition" in {
    runExitCode("main() -> int = 3 + 4\n") shouldBe 7
  }

  "subtraction" in {
    runExitCode("main() -> int = 10 - 3\n") shouldBe 7
  }

  "multiplication" in {
    runExitCode("main() -> int = 6 * 7\n") shouldBe 42
  }

  "complex expression" in {
    runExitCode("main() -> int = (2 + 3) * 4\n") shouldBe 20
  }

  "unary minus" in {
    // Exit codes are 0-255, so test via putchar or comparison
    runExitCode("main() -> int = if -1 < 0 then 1 else 0\n") shouldBe 1
  }

  // ===== Variables =====

  "local variable" in {
    runExitCode(
      """main() -> int
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "multiple variables" in {
    runExitCode(
      """main() -> int
        |    a = 10
        |    b = 20
        |    a + b
        |""".stripMargin) shouldBe 30
  }

  // ===== Comparison =====

  "equal" in {
    runExitCode("main() -> int = 5 == 5\n") shouldBe 1
  }

  "less than" in {
    runExitCode("main() -> int = 3 < 5\n") shouldBe 1
  }

  // ===== Output =====

  "putchar" in {
    runOutput(
      """main() -> int
        |    putchar(72)
        |    putchar(105)
        |    0
        |""".stripMargin) shouldBe "Hi"
  }

  // ===== Function calls =====

  "function call" in {
    runExitCode(
      """dbl(x: int) -> int = x * 2
        |
        |main() -> int = dbl(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== While =====

  "while loop" in {
    runExitCode(
      """main() -> int
        |    i = 0
        |    sum = 0
        |    while i < 10
        |        sum = sum + i
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 45
  }

  // ===== If/else =====

  "if true" in {
    runExitCode(
      """main() -> int
        |    if true
        |        return 42
        |    0
        |""".stripMargin) shouldBe 42
  }

  "if false" in {
    runExitCode(
      """main() -> int
        |    if false
        |        return 42
        |    7
        |""".stripMargin) shouldBe 7
  }
}
