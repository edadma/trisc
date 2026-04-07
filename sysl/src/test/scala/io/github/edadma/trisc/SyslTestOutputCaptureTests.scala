package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Tests that the test runner captures print output from test functions. */
class SyslTestOutputCaptureTests extends AnyFreeSpec with Matchers {

  private def runTestProgram(source: String): (Long, String) =
    val buf = new StringBuilder
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val interp = new SyslInterpreter(s => buf ++= s)
    val result = interp.run(typed)
    (result, buf.toString)

  "print output is captured from interpreter" in {
    val (_, output) = runTestProgram(
      """main() -> int
        |    puti(42)
        |    0
        |""".stripMargin)
    output shouldBe "42"
  }

  "expect passes silently" in {
    val (result, _) = runTestProgram(
      """main() -> int
        |    expect(10, 10, "ok")
        |    0
        |""".stripMargin)
    result shouldBe 0
  }

  "expect failure includes both values" in {
    val e = intercept[RuntimeException] {
      runTestProgram(
        """main() -> int
          |    expect(5, 10, "mycheck")
          |    0
          |""".stripMargin)
    }
    e.getMessage shouldBe "mycheck: expected 10, got 5"
  }

  "print output captured before expect failure" in {
    val buf = new StringBuilder
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    puti(999)
        |    expect(1, 2, "fail")
        |    0
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val interp = new SyslInterpreter(s => buf ++= s)
    val e = intercept[RuntimeException] { interp.run(typed) }
    e.getMessage should include("expected 2, got 1")
    buf.toString shouldBe "999"
  }
}
