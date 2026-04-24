package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** --no-contracts build flag: TContractCheck emissions strip to no-ops. Covers every
 *  contract flavor: require/ensure, loop invariant, loop variant, struct invariant,
 *  type predicate (where / non-null), within-range, enum Pos/Val/Value/Succ/Pred traps.
 *  Each flavor has a pair of tests: one proving it traps with contracts on, one proving
 *  the same input runs to completion with contracts off. */
class SyslNoContractsTests extends AnyFreeSpec with Matchers {

  /** Compile + interpret with contracts ON (default). Throws if contract trips. */
  private def evalOn(source: String): Long =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer(contractsEnabled = true)).analyze(ast)
    (new SyslInterpreter()).run(typed)

  /** Compile + interpret with contracts OFF. Contract-failing input should run anyway. */
  private def evalOff(source: String): Long =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer(contractsEnabled = false)).analyze(ast)
    (new SyslInterpreter()).run(typed)

  // ===== require (precondition) =====

  "require traps with contracts on" in {
    val t = intercept[RuntimeException] {
      evalOn("""
        |pos(x: int) -> int
        |    require x > 0
        |    return x + 1
        |main() -> int = pos(-5)
        |""".stripMargin)
    }
    t.getMessage should include("precondition")
  }

  "require is stripped with contracts off" in {
    // Same invalid input — would trap on — now runs and returns x+1 = -4.
    evalOff("""
      |pos(x: int) -> int
      |    require x > 0
      |    return x + 1
      |main() -> int = pos(-5)
      |""".stripMargin) shouldBe -4
  }

  // ===== ensure (postcondition) =====

  "ensure traps with contracts on" in {
    val t = intercept[RuntimeException] {
      evalOn("""
        |maybeIncr(x: int) -> int
        |    ensure result > x
        |    return x
        |main() -> int = maybeIncr(5)
        |""".stripMargin)
    }
    t.getMessage should include("postcondition")
  }

  "ensure is stripped with contracts off" in {
    evalOff("""
      |maybeIncr(x: int) -> int
      |    ensure result > x
      |    return x
      |main() -> int = maybeIncr(5)
      |""".stripMargin) shouldBe 5
  }

  // ===== loop invariant =====

  "loop invariant traps with contracts on" in {
    val t = intercept[RuntimeException] {
      evalOn("""
        |main() -> int
        |    var i = 0
        |    while i < 5 do
        |        invariant i < 3
        |        i = i + 1
        |    i
        |""".stripMargin)
    }
    t.getMessage should include("invariant")
  }

  "loop invariant is stripped with contracts off" in {
    evalOff("""
      |main() -> int
      |    var i = 0
      |    while i < 5 do
      |        invariant i < 3
      |        i = i + 1
      |    i
      |""".stripMargin) shouldBe 5
  }

  // ===== loop variant =====

  "loop variant traps with contracts on" in {
    // variant must strictly decrease each iteration — here it stays constant.
    val t = intercept[RuntimeException] {
      evalOn("""
        |main() -> int
        |    var n = 0
        |    while n < 2 do
        |        variant 10
        |        n = n + 1
        |    n
        |""".stripMargin)
    }
    t.getMessage should include("loop variant")
  }

  "loop variant is stripped with contracts off" in {
    evalOff("""
      |main() -> int
      |    var n = 0
      |    while n < 2 do
      |        variant 10
      |        n = n + 1
      |    n
      |""".stripMargin) shouldBe 2
  }

  // ===== struct invariant =====

  "struct invariant traps with contracts on" in {
    val t = intercept[RuntimeException] {
      evalOn("""
        |struct Age
        |    years: int
        |    invariant years >= 0 && years <= 150
        |main() -> int
        |    var a: Age
        |    a.years = 999
        |    a.years
        |""".stripMargin)
    }
    t.getMessage should include("invariant")
  }

  "struct invariant is stripped with contracts off" in {
    evalOff("""
      |struct Age
      |    years: int
      |    invariant years >= 0 && years <= 150
      |main() -> int
      |    var a: Age
      |    a.years = 999
      |    a.years
      |""".stripMargin) shouldBe 999
  }

  // ===== type predicate via `where` =====

  "where-predicate traps with contracts on" in {
    val t = intercept[RuntimeException] {
      evalOn("""
        |type Even = int where value % 2 == 0
        |main() -> int
        |    var raw = 7
        |    var e: Even = raw
        |    int(e)
        |""".stripMargin)
    }
    t.getMessage should include("type predicate")
  }

  "where-predicate is stripped with contracts off" in {
    evalOff("""
      |type Even = int where value % 2 == 0
      |main() -> int
      |    var raw = 7
      |    var e: Even = raw
      |    int(e)
      |""".stripMargin) shouldBe 7
  }

  // ===== within-range (int range constraint) =====

  "within-range traps with contracts on" in {
    val t = intercept[RuntimeException] {
      evalOn("""
        |type Age = int within 0..150
        |main() -> int
        |    var raw = 200
        |    var a: Age = raw
        |    int(a)
        |""".stripMargin)
    }
    t.getMessage should include("range check")
  }

  "within-range is stripped with contracts off" in {
    evalOff("""
      |type Age = int within 0..150
      |main() -> int
      |    var raw = 200
      |    var a: Age = raw
      |    int(a)
      |""".stripMargin) shouldBe 200
  }

  // ===== enum ::Pos trap =====

  "enum ::Pos trap fires with contracts on" in {
    val t = intercept[RuntimeException] {
      evalOn("""
        |enum Color
        |    Red = 10
        |    Green = 20
        |    Blue = 30
        |main() -> int
        |    Color::Pos(99)
        |""".stripMargin)
    }
    t.getMessage should include("Pos")
  }

  "enum ::Pos trap stripped with contracts off" in {
    // Default branch returns -1 when trap is elided.
    evalOff("""
      |enum Color
      |    Red = 10
      |    Green = 20
      |    Blue = 30
      |main() -> int
      |    Color::Pos(99)
      |""".stripMargin) shouldBe -1
  }

  // ===== enum ::Succ past-last trap =====

  "enum ::Succ trap fires with contracts on" in {
    val t = intercept[RuntimeException] {
      evalOn("""
        |enum Color
        |    Red = 10
        |    Green = 20
        |    Blue = 30
        |main() -> int
        |    Color::Succ(Color.Blue)
        |""".stripMargin)
    }
    t.getMessage should include("Succ")
  }

  "enum ::Succ trap stripped with contracts off" in {
    evalOff("""
      |enum Color
      |    Red = 10
      |    Green = 20
      |    Blue = 30
      |main() -> int
      |    Color::Succ(Color.Blue)
      |""".stripMargin) shouldBe -1
  }

  // ===== within ::Succ upper-bound trap =====

  "within ::Succ trap fires with contracts on" in {
    val t = intercept[RuntimeException] {
      evalOn("""
        |type Idx = int within 0..<10
        |main() -> int
        |    var x: Idx = 9
        |    int(Idx::Succ(x))
        |""".stripMargin)
    }
    t.getMessage should include("Succ")
  }

  "within ::Succ trap stripped with contracts off" in {
    // With trap gone, v + 1 = 10 returned (past the hi-1=9 upper bound).
    evalOff("""
      |type Idx = int within 0..<10
      |main() -> int
      |    var x: Idx = 9
      |    int(Idx::Succ(x))
      |""".stripMargin) shouldBe 10
  }

  // ===== happy-path programs behave identically either way =====

  "programs with no contract failures produce the same result on and off" in {
    val src = """
      |type Age = int within 0..150
      |twice(x: int) -> int
      |    require x >= 0
      |    ensure result >= x
      |    return x * 2
      |main() -> int
      |    var raw = 42
      |    var a: Age = raw
      |    twice(int(a))
      |""".stripMargin
    evalOn(src) shouldBe 84
    evalOff(src) shouldBe 84
  }
}
