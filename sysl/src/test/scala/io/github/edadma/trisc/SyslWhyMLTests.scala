package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Phase 1 sysl → WhyML translator. Snapshot-style tests: each case feeds a sysl source
  * string through the parser and SyslWhyMLBackend, then compares the emitted WhyML against
  * a hand-written expected string. WhyML's syntax is whitespace-tolerant but humans aren't,
  * so the expected strings double as a readable specification of the output format. */
class SyslWhyMLTests extends AnyFreeSpec with Matchers {

  private def translate(source: String, moduleName: String = "M"): String =
    (new SyslParser).parseProgram(source) match
      case Right(ast) => new SyslWhyMLBackend(moduleName).generate(ast)
      case Left(err)  => fail(s"parse error: $err")

  "trivial expression-function emits a let function" in {
    val mlw = translate(
      """def twice(x: int) -> int
        |    x * 2
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  let function twice (x: int) : int
        |    = (x * 2)
        |end
        |""".stripMargin
  }

  "bool-returning function" in {
    val mlw = translate(
      """def is_pos(x: int) -> bool
        |    x > 0
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  let function is_pos (x: int) : bool
        |    = (x > 0)
        |end
        |""".stripMargin
  }

  "require clause translates to requires" in {
    val mlw = translate(
      """def incr(x: int) -> int
        |    require x > 0
        |    x + 1
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  let function incr (x: int) : int
        |    requires { x > 0 }
        |    = (x + 1)
        |end
        |""".stripMargin
  }

  "ensure with old(x) becomes WhyML's prefix `old`" in {
    val mlw = translate(
      """def incr(x: int) -> int
        |    require x > 0
        |    ensure result == old(x) + 1
        |    x + 1
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  let function incr (x: int) : int
        |    requires { x > 0 }
        |    ensures  { result = ((old x) + 1) }
        |    = (x + 1)
        |end
        |""".stripMargin
  }

  "self-recursive function gets `let rec function` and a variant clause" in {
    val mlw = translate(
      """def fact(n: int) -> int
        |    require n >= 0
        |    ensure result >= 1
        |    variant n
        |    if n == 0 then 1 else n * fact(n - 1)
        |""".stripMargin,
      moduleName = "Factorial")
    mlw shouldBe
      """module Factorial
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  let rec function fact (n: int) : int
        |    requires { n >= 0 }
        |    ensures  { result >= 1 }
        |    variant  { n }
        |    = (if (n = 0) then 1 else (n * (fact (n - 1))))
        |end
        |""".stripMargin
  }

  "multi-arg function emits space-separated parameters and call applications" in {
    val mlw = translate(
      """def gcd(a: int, b: int) -> int
        |    require a >= 0
        |    require b >= 0
        |    variant a + b
        |    if b == 0 then a else gcd(b, a - (a / b) * b)
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  let rec function gcd (a: int) (b: int) : int
        |    requires { a >= 0 }
        |    requires { b >= 0 }
        |    variant  { a + b }
        |    = (if (b = 0) then a else (gcd b (a - ((div a b) * b))))
        |end
        |""".stripMargin
  }

  "multiple functions render in declaration order separated by blank lines" in {
    val mlw = translate(
      """def twice(x: int) -> int
        |    x * 2
        |
        |def is_pos(x: int) -> bool
        |    x > 0
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  let function twice (x: int) : int
        |    = (x * 2)
        |
        |  let function is_pos (x: int) : bool
        |    = (x > 0)
        |end
        |""".stripMargin
  }

  "multiple ensure clauses each emit their own ensures block" in {
    val mlw = translate(
      """def abs(x: int) -> int
        |    ensure result >= 0
        |    ensure result == x || result == -x
        |    if x >= 0 then x else -x
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  let function abs (x: int) : int
        |    ensures  { result >= 0 }
        |    ensures  { (result = x) \/ (result = (- x)) }
        |    = (if (x >= 0) then x else (- x))
        |end
        |""".stripMargin
  }

  "inequality ==/!= map to WhyML's =/<>" in {
    val mlw = translate(
      """def neq(x: int, y: int) -> bool
        |    x != y
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  let function neq (x: int) (y: int) : bool
        |    = (x <> y)
        |end
        |""".stripMargin
  }

  "every signed/unsigned int width collapses to mathematical int" in {
    val mlw = translate(
      """def sum(a: i32, b: u64) -> i64
        |    a + b
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  let function sum (a: int) (b: int) : int
        |    = (a + b)
        |end
        |""".stripMargin
  }

  "negative integer literal renders with the unary-minus form Why3 expects" in {
    val mlw = translate(
      """def neg() -> int
        |    -5
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  let function neg () : int
        |    = (- 5)
        |end
        |""".stripMargin
  }

  "test-attributed functions are skipped (would clutter the proof obligations)" in {
    val mlw = translate(
      """def twice(x: int) -> int
        |    x * 2
        |
        |#test
        |smoke() -> bool
        |    twice(2) == 4
        |""".stripMargin)
    // Only `twice` should be present; the #test fn is filtered out.
    mlw should include("twice")
    mlw should not include "smoke"
  }

  "for all in a contract-less def-bool body lifts to a logic-level `predicate`" in {
    // WhyML's forall / exists are formula-level (return prop), not value-level (bool).
    // A `def f -> bool` whose body IS a quantifier maps cleanly to WhyML's `predicate`,
    // which is exactly the right construct for a pure prop-valued query function.
    val mlw = translate(
      """def all_nonneg(n: int) -> bool
        |    for all i in 0..n => i >= 0
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  predicate all_nonneg (n: int) = forall i: int. 0 <= i <= n -> (i >= 0)
        |end
        |""".stripMargin
  }

  "for all over exclusive range uses strict less-than at the upper bound" in {
    val mlw = translate(
      """def under_n(n: int) -> bool
        |    for all i in 0..<n => i < n
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  predicate under_n (n: int) = forall i: int. 0 <= i < n -> (i < n)
        |end
        |""".stripMargin
  }

  "for some translates to bounded exists with conjunction" in {
    val mlw = translate(
      """def has_zero(n: int) -> bool
        |    for some i in 0..n => i == 0
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  predicate has_zero (n: int) = exists i: int. 0 <= i <= n /\ (i = 0)
        |end
        |""".stripMargin
  }

  "quantifier in an ensure clause threads through (contract is formula position)" in {
    // The body here is a function call (not a quantifier), so the ensure clause carries
    // the formula and the function stays as a `let function`.
    val mlw = translate(
      """def verify(n: int) -> bool
        |    require n >= 0
        |    ensure result == (for all i in 0..<n => i >= 0)
        |    n >= 0
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |
        |  let function verify (n: int) : bool
        |    requires { n >= 0 }
        |    ensures  { result = (forall i: int. 0 <= i < n -> (i >= 0)) }
        |    = (n >= 0)
        |end
        |""".stripMargin
  }

  "unsupported expression form yields a clear error naming the gap" in {
    val ex = intercept[RuntimeException](translate(
      """def s() -> int
        |    "hello".length
        |""".stripMargin))
    ex.getMessage should include("WhyML translator: unsupported")
  }
}
