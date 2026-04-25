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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
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
        |  use ref.Ref
        |
        |  let function verify (n: int) : bool
        |    requires { n >= 0 }
        |    ensures  { result = (forall i: int. 0 <= i < n -> (i >= 0)) }
        |    = (n >= 0)
        |end
        |""".stripMargin
  }

  "#ghost on a let-function emits the WhyML ghost qualifier" in {
    val mlw = translate(
      """#ghost
        |helper(x: int) -> int
        |    require x >= 0
        |    ensure result == x + 1
        |    x + 1
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let ghost function helper (x: int) : int
        |    requires { x >= 0 }
        |    ensures  { result = (x + 1) }
        |    = (x + 1)
        |end
        |""".stripMargin
  }

  "#ghost combines with `rec` for recursive ghost helpers" in {
    val mlw = translate(
      """#ghost
        |def gsum(n: int) -> int
        |    require n >= 0
        |    variant n
        |    if n == 0 then 0 else n + gsum(n - 1)
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let rec ghost function gsum (n: int) : int
        |    requires { n >= 0 }
        |    variant  { n }
        |    = (if (n = 0) then 0 else (n + (gsum (n - 1))))
        |end
        |""".stripMargin
  }

  "#ghost on a predicate-shape def is dropped (predicates are inherently logic-level)" in {
    val mlw = translate(
      """#ghost
        |def all_pos(n: int) -> bool
        |    for all i in 1..n => i >= 1
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  predicate all_pos (n: int) = forall i: int. 1 <= i <= n -> (i >= 1)
        |end
        |""".stripMargin
  }

  "simple enum becomes a WhyML algebraic type with bare-constructor variants" in {
    val mlw = translate(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type color = Red | Green | Blue
        |end
        |""".stripMargin
  }

  "EnumName.Variant in contract position strips the type prefix" in {
    // Inside contracts WhyML's `=` is structural and works on any type, so enum equality
    // composes there directly. Body-position use needs a `match` expression — deferred to a
    // later piece — so this test exercises only the contract path.
    val mlw = translate(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |
        |def first_color(c: Color) -> int
        |    require c == Color.Red
        |    ensure result == 0
        |    0
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type color = Red | Green | Blue
        |
        |  let function first_color (c: color) : int
        |    requires { c = Red }
        |    ensures  { result = 0 }
        |    = 0
        |end
        |""".stripMargin
  }

  "match on enum dispatches with bare-constructor patterns" in {
    val mlw = translate(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |
        |def color_code(c: Color) -> int
        |    ensure result >= 0
        |    c match
        |        Color.Red   -> 1
        |        Color.Green -> 2
        |        Color.Blue  -> 3
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type color = Red | Green | Blue
        |
        |  let function color_code (c: color) : int
        |    ensures  { result >= 0 }
        |    = (match c with | Red -> 1 | Green -> 2 | Blue -> 3 end)
        |end
        |""".stripMargin
  }

  "match with else-default emits a wildcard arm" in {
    val mlw = translate(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |
        |def is_warm(c: Color) -> int
        |    c match
        |        Color.Red -> 1
        |        else      -> 0
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type color = Red | Green | Blue
        |
        |  let function is_warm (c: color) : int
        |    = (match c with | Red -> 1 | _ -> 0 end)
        |end
        |""".stripMargin
  }

  "match on integer literals lowers to an if-chain (WhyML rejects literal patterns)" in {
    val mlw = translate(
      """def categorize(n: int) -> int
        |    n match
        |        0    -> 100
        |        1    -> 200
        |        else -> 0
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let function categorize (n: int) : int
        |    = (if n = 0 then 100 else if n = 1 then 200 else 0)
        |end
        |""".stripMargin
  }

  "single val binding lowers to `let x = e in body`" in {
    val mlw = translate(
      """def plus_one(x: int) -> int
        |    val y = x + 1
        |    y
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let function plus_one (x: int) : int
        |    = let y = (x + 1) in y
        |end
        |""".stripMargin
  }

  "multiple val bindings chain into nested lets" in {
    val mlw = translate(
      """def compute(x: int) -> int
        |    require x >= 0
        |    ensure result >= 0
        |    val doubled = x * 2
        |    val plus_one = doubled + 1
        |    plus_one
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let function compute (x: int) : int
        |    requires { x >= 0 }
        |    ensures  { result >= 0 }
        |    = let doubled = (x * 2) in let plus_one = (doubled + 1) in plus_one
        |end
        |""".stripMargin
  }

  "ghost-marked local becomes a ghost let-binding" in {
    val mlw = translate(
      """def identity(x: int) -> int
        |    ensure result == x
        |    #ghost val snap = x
        |    x
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let function identity (x: int) : int
        |    ensures  { result = x }
        |    = let ghost snap = x in x
        |end
        |""".stripMargin
  }

  // ====================================================================================
  // Phase 4a — mutable refs and assignment
  // ====================================================================================

  "var that is later reassigned becomes a WhyML ref" in {
    // Two signals trigger ref-form: declared `var` AND reassigned somewhere later in scope.
    // Reads of the name pick up `!`, the assignment becomes `:=`, and the surrounding
    // function loses its `function` keyword (a `let function` body must be pure expr).
    val mlw = translate(
      """def assign_test(x: int) -> int
        |    var y = 0
        |    y = x
        |    y
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let assign_test (x: int) : int
        |    = let y = ref 0 in y := x; !y
        |end
        |""".stripMargin
  }

  "compound assignment `s += x` desugars to `s := !s + x`" in {
    val mlw = translate(
      """def add_them(a: int, b: int) -> int
        |    var s = 0
        |    s += a
        |    s += b
        |    s
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let add_them (a: int) (b: int) : int
        |    = let s = ref 0 in s := (!s + a); s := (!s + b); !s
        |end
        |""".stripMargin
  }

  "var that is never reassigned stays as an immutable let (function keyword preserved)" in {
    // `var` alone doesn't force ref form — only actual reassignment does. Avoids Why3
    // imposing the impure-call restriction on something effectively pure.
    val mlw = translate(
      """def echo(x: int) -> int
        |    var y = x
        |    y
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let function echo (x: int) : int
        |    = let y = x in y
        |end
        |""".stripMargin
  }

  "assignment to a non-mutable binding is rejected with a clear message" in {
    val ex = intercept[RuntimeException](translate(
      """def f(x: int) -> int
        |    val y = 0
        |    y = x
        |    y
        |""".stripMargin))
    ex.getMessage should include("assignment to non-mutable binding")
  }

  // ====================================================================================
  // Phase 4b — while loops with invariant / variant
  // ====================================================================================

  "while loop with invariants and variant translates to WhyML loop annotations" in {
    // The invariant / variant statements inside the loop body get hoisted into WhyML
    // annotations between `do` and the body. The loop counter `i` is mutated, so it's a ref;
    // the read in the loop guard becomes `!i`.
    val mlw = translate(
      """def count_up(n: int) -> int
        |    require n >= 0
        |    ensure result == n
        |    var i = 0
        |    while i < n
        |        invariant 0 <= i
        |        invariant i <= n
        |        variant n - i
        |        i += 1
        |    i
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let count_up (n: int) : int
        |    requires { n >= 0 }
        |    ensures  { result = n }
        |    = let i = ref 0 in (while (!i < n) do invariant { 0 <= !i } invariant { !i <= n } variant { n - !i } i := (!i + 1) done); !i
        |end
        |""".stripMargin
  }

  "while loop with no annotations still parses (annotations are optional in WhyML)" in {
    val mlw = translate(
      """def drain(n: int) -> int
        |    require n >= 0
        |    var i = 0
        |    while i < n
        |        i += 1
        |    i
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let drain (n: int) : int
        |    requires { n >= 0 }
        |    = let i = ref 0 in (while (!i < n) do i := (!i + 1) done); !i
        |end
        |""".stripMargin
  }

  "loop body with two assignments sequences with `;`" in {
    val mlw = translate(
      """def gauss(n: int) -> int
        |    require n >= 0
        |    var s = 0
        |    var i = 0
        |    while i <= n
        |        invariant 2 * s == i * (i - 1)
        |        variant n - i + 1
        |        s = s + i
        |        i = i + 1
        |    s
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let gauss (n: int) : int
        |    requires { n >= 0 }
        |    = let s = ref 0 in let i = ref 0 in (while (!i <= n) do invariant { (2 * !s) = (!i * (!i - 1)) } variant { (n - !i) + 1 } s := (!s + !i); i := (!i + 1) done); !s
        |end
        |""".stripMargin
  }

  // ====================================================================================
  // Phase 4b+ — for-loops (canonical range emits native WhyML `for i = lo to hi`)
  // ====================================================================================

  "canonical `for i in 0..<n` emits native WhyML for-loop with hi = n - 1" in {
    // The counter `i` is implicitly immutable in a WhyML for-loop, so reads are bare `i`
    // (no `!`). Only the mutable accumulator `s` is a ref. WhyML's for-loop has implicit
    // termination — no `variant` clause, even if the user wrote one.
    val mlw = translate(
      """def loop_count(n: int) -> int
        |    require n >= 0
        |    var s = 0
        |    for i in 0..<n
        |        s = s + i
        |    s
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let loop_count (n: int) : int
        |    requires { n >= 0 }
        |    = let s = ref 0 in (for i = 0 to (n - 1) do s := (!s + i) done); !s
        |end
        |""".stripMargin
  }

  "inclusive `for i in 0..n` keeps the upper bound as-is (WhyML's `to` is inclusive)" in {
    val mlw = translate(
      """def gauss_for(n: int) -> int
        |    require n >= 0
        |    var s = 0
        |    for i in 0..n
        |        invariant 2 * s == i * (i - 1)
        |        s = s + i
        |    s
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let gauss_for (n: int) : int
        |    requires { n >= 0 }
        |    = let s = ref 0 in (for i = 0 to n do invariant { (2 * !s) = (i * (i - 1)) } s := (!s + i) done); !s
        |end
        |""".stripMargin
  }

  "non-canonical for (downTo) falls back to a while-equivalent" in {
    // Sysl `downTo` has `>=` cond + `-` update, neither of which matches WhyML's natural
    // `for i = lo to hi`. Lower to while; the user must supply a `variant` for termination.
    val mlw = translate(
      """def countdown(n: int) -> int
        |    require n >= 0
        |    var s = 0
        |    for i in n downTo 0
        |        invariant s >= 0
        |        variant i + 1
        |        s += 1
        |    s
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let countdown (n: int) : int
        |    requires { n >= 0 }
        |    = let s = ref 0 in let i = ref n in (while (!i >= 0) do invariant { !s >= 0 } variant { !i + 1 } s := (!s + 1); i := (!i - 1) done); !s
        |end
        |""".stripMargin
  }

  "single early-exit lowers to if-else terminating in the rest of the body" in {
    val mlw = translate(
      """def clamp_low(x: int) -> int
        |    ensure result >= 0
        |    if x < 0 then return 0
        |    x
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let function clamp_low (x: int) : int
        |    ensures  { result >= 0 }
        |    = (if (x < 0) then 0 else x)
        |end
        |""".stripMargin
  }

  "stacked early-exit guards chain into nested if-else" in {
    val mlw = translate(
      """def clamp(x: int) -> int
        |    ensure result >= 0
        |    ensure result <= 100
        |    if x < 0   then return 0
        |    if x > 100 then return 100
        |    x
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let function clamp (x: int) : int
        |    ensures  { result >= 0 }
        |    ensures  { result <= 100 }
        |    = (if (x < 0) then 0 else (if (x > 100) then 100 else x))
        |end
        |""".stripMargin
  }

  "early exit composes with val let-bindings before and after" in {
    val mlw = translate(
      """def safe_div(a: int, b: int) -> int
        |    if b == 0 then return 0
        |    val q = a / b
        |    q
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let function safe_div (a: int) (b: int) : int
        |    = (if (b = 0) then 0 else let q = (div a b) in q)
        |end
        |""".stripMargin
  }

  "module-level `val` becomes a WhyML `constant`" in {
    val mlw = translate(
      """val max_age: int = 150
        |
        |def is_alive(age: int) -> bool
        |    require age >= 0
        |    require age <= max_age
        |    age < max_age
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let constant max_age : int = 150
        |
        |  let function is_alive (age: int) : bool
        |    requires { age >= 0 }
        |    requires { age <= max_age }
        |    = (age < max_age)
        |end
        |""".stripMargin
  }

  "all-uppercase identifier is fully lowercased (WhyML rejects uppercase-first values)" in {
    // Sysl convention is `MAX_AGE` for constants, but WhyML treats uppercase-first
    // identifiers as constructor names. We lowercase the whole word for clarity rather than
    // emitting a half-cased `mAX_AGE`. References use the same sanitizer so they line up.
    val mlw = translate(
      """val MAX_AGE: int = 150
        |
        |def is_alive(age: int) -> bool
        |    require age <= MAX_AGE
        |    age < MAX_AGE
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  let constant max_age : int = 150
        |
        |  let function is_alive (age: int) : bool
        |    requires { age <= max_age }
        |    = (age < max_age)
        |end
        |""".stripMargin
  }

  "module-level `var` is rejected with a clear message" in {
    val ex = intercept[RuntimeException](translate(
      """var counter: int = 0
        |""".stripMargin))
    ex.getMessage should include("module-level `var`")
  }

  "unsupported expression form yields a clear error naming the gap" in {
    val ex = intercept[RuntimeException](translate(
      """def s() -> int
        |    "hello".length
        |""".stripMargin))
    ex.getMessage should include("WhyML translator: unsupported")
  }
}
