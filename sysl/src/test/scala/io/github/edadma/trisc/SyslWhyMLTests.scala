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
        |    ensures  { (result = x) || (result = (- x)) }
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

  // ====================================================================================
  // Phase 4-structs — value structs ↔ WhyML records
  // ====================================================================================

  "struct decl emits a WhyML record type with lowercased name" in {
    val mlw = translate(
      """struct Point
        |    x: int
        |    y: int
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type point = { x: int; y: int }
        |end
        |""".stripMargin
  }

  "struct field access uses dot notation (same as WhyML)" in {
    val mlw = translate(
      """struct Point
        |    x: int
        |    y: int
        |
        |def magnitude_sq(p: Point) -> int
        |    require p.x >= 0
        |    require p.y >= 0
        |    ensure result >= 0
        |    p.x * p.x + p.y * p.y
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type point = { x: int; y: int }
        |
        |  let function magnitude_sq (p: point) : int
        |    requires { p.x >= 0 }
        |    requires { p.y >= 0 }
        |    ensures  { result >= 0 }
        |    = ((p.x * p.x) + (p.y * p.y))
        |end
        |""".stripMargin
  }

  "struct construction call lowers to a record literal in field-declaration order" in {
    // Sysl `Point(0, 0)` is positional construction. Sysl also supports named-arg form
    // `Point(x=0, y=0)` which the parser binds positionally too. Either way we emit
    // `{ x = ...; y = ... }` — record literals are name-keyed in WhyML so we look up the
    // field names by struct name, then zip with the actual arg expressions.
    val mlw = translate(
      """struct Point
        |    x: int
        |    y: int
        |
        |def origin() -> Point
        |    Point(0, 0)
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type point = { x: int; y: int }
        |
        |  let function origin () : point
        |    = { x = 0; y = 0 }
        |end
        |""".stripMargin
  }

  "struct returned from a function with `result.field` ensures clauses" in {
    val mlw = translate(
      """struct Range
        |    lo: int
        |    hi: int
        |
        |def make_range(lo: int, hi: int) -> Range
        |    require lo <= hi
        |    ensure result.lo == lo
        |    ensure result.hi == hi
        |    Range(lo, hi)
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type range = { lo: int; hi: int }
        |
        |  let function make_range (lo: int) (hi: int) : range
        |    requires { lo <= hi }
        |    ensures  { result.lo = lo }
        |    ensures  { result.hi = hi }
        |    = { lo = lo; hi = hi }
        |end
        |""".stripMargin
  }

  "struct invariants emit a WhyML record invariant + synthesized `by` witness" in {
    // Multiple `invariant` clauses join with `&&`. The `by` witness uses 0 for int fields
    // and false for bool — sufficient for typical numeric invariants like `balance >= -limit`
    // (0 >= -0 holds). Why3 generates a witness goal which Alt-Ergo discharges trivially
    // when the witness satisfies the invariant.
    val mlw = translate(
      """struct Account
        |    balance: int
        |    limit: int
        |    invariant balance >= -limit
        |    invariant limit >= 0
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type account = { balance: int; limit: int }
        |    invariant { balance >= (- limit) && limit >= 0 }
        |    by { balance = 0; limit = 0 }
        |end
        |""".stripMargin
  }

  "single invariant on a struct is emitted without join operator" in {
    val mlw = translate(
      """struct Range
        |    lo: int
        |    hi: int
        |    invariant lo <= hi
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type range = { lo: int; hi: int }
        |    invariant { lo <= hi }
        |    by { lo = 0; hi = 0 }
        |end
        |""".stripMargin
  }

  "boolean conjunction `&&` in body position uses WhyML's bool && operator" in {
    // WhyML `/\` is formula-only — using it in a body that returns bool is a syntax error.
    // The translator emits `&&` / `||` everywhere; Why3 implicitly coerces bool to prop in
    // formula contexts, so contracts still parse correctly.
    val mlw = translate(
      """struct Range
        |    lo: int
        |    hi: int
        |
        |def contains(r: Range, x: int) -> bool
        |    ensure result == (x >= r.lo && x <= r.hi)
        |    x >= r.lo && x <= r.hi
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type range = { lo: int; hi: int }
        |
        |  let function contains (r: range) (x: int) : bool
        |    ensures  { result = ((x >= r.lo) && (x <= r.hi)) }
        |    = ((x >= r.lo) && (x <= r.hi))
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

  // ====================================================================================
  // Phase 4-data — generic data enums (Option, Result)
  // ====================================================================================

  "generic data enum with one type param emits a parametric WhyML ADT" in {
    val mlw = translate(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type option 't = Some 't | None
        |end
        |""".stripMargin
  }

  "generic data enum with two type params emits both as positional WhyML type vars" in {
    val mlw = translate(
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type result 't 'e = Ok 't | Err 'e
        |end
        |""".stripMargin
  }

  "match on data enum with destructure binds the payload" in {
    val mlw = translate(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |def unwrap_or[T](o: Option[T], default: T) -> T
        |    o match
        |        Some(v) -> v
        |        None -> default
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type option 't = Some 't | None
        |
        |  let function unwrap_or (o: option 't) (default: 't) : 't
        |    = (match o with | Some v -> v | None -> default end)
        |end
        |""".stripMargin
  }

  "wildcard inside destructure pattern emits as `Some _`" in {
    val mlw = translate(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |def is_some[T](o: Option[T]) -> bool
        |    o match
        |        Some(_) -> true
        |        None -> false
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type option 't = Some 't | None
        |
        |  let function is_some (o: option 't) : bool
        |    = (match o with | Some _ -> true | None -> false end)
        |end
        |""".stripMargin
  }

  "constructor call `Some(42)` lowers to `(Some 42)` and bare `None` emits as-is" in {
    val mlw = translate(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |def of_int(x: int) -> Option[int]
        |    Some(x)
        |
        |def empty() -> Option[int]
        |    None
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |
        |  type option 't = Some 't | None
        |
        |  let function of_int (x: int) : option int
        |    = (Some x)
        |
        |  let function empty () : option int
        |    = None
        |end
        |""".stripMargin
  }

  // ====================================================================================
  // Phase 4-data+ — panic / assert / `?` propagation
  // ====================================================================================

  "panic call lowers to WhyML `absurd`" in {
    // `absurd` claims unreachability; the verifier requires a proof that this branch
    // never executes. Without a precondition, this proof fails (correctly): it tells
    // the user that calling unwrap on None genuinely panics. Adding `requires { is_some o }`
    // discharges the obligation cleanly.
    val mlw = translate(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |def is_some[T](o: Option[T]) -> bool
        |    o match
        |        Some(_) -> true
        |        None -> false
        |
        |def unwrap[T](o: Option[T]) -> T
        |    require is_some(o)
        |    o match
        |        Some(v) -> v
        |        None -> panic("unwrap on None")
        |""".stripMargin)
    mlw should include("None -> absurd")
    mlw should include("requires { is_some o }")
  }

  "assert call in body lowers to WhyML `assert { ... }`" in {
    val mlw = translate(
      """def double_pos(x: int) -> int
        |    require x >= 0
        |    val r = x * 2
        |    assert(r >= x, "doubling preserves order")
        |    r
        |""".stripMargin)
    mlw shouldBe
      """module M
        |  use int.Int
        |  use int.ComputerDivision
        |  use ref.Ref
        |  use string.String
        |
        |  let function double_pos (x: int) : int
        |    requires { x >= 0 }
        |    = let r = (x * 2) in assert { r >= x }; r
        |end
        |""".stripMargin
  }

  "`?` on Option binds success and propagates None" in {
    val mlw = translate(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |def safe_div(a: int, b: int) -> Option[int]
        |    if b == 0 then
        |        return None
        |    Some(a / b)
        |
        |def half_of_quotient(a: int, b: int) -> Option[int]
        |    val q = safe_div(a, b)?
        |    safe_div(q, 2)
        |""".stripMargin)
    mlw should include("(match (safe_div a b) with | Some _try_v_q -> let q = _try_v_q in (safe_div q 2) | None -> None end)")
  }

  "`?` on Result reconstructs the failure variant with bound payload" in {
    val mlw = translate(
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |def attempt(x: int) -> Result[int, int]
        |    if x < 0 then
        |        return Err(x)
        |    Ok(x + 1)
        |
        |def chain(a: int) -> Result[int, int]
        |    val y = attempt(a)?
        |    Ok(y + 1)
        |""".stripMargin)
    mlw should include("Err _try_e0 -> (Err _try_e0)")
  }

  "`?` outside top-level binding is rejected with a clear gap message" in {
    val ex = intercept[RuntimeException](translate(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |def f(o: Option[int]) -> Option[int]
        |    Some(o? + 1)
        |""".stripMargin))
    ex.getMessage should include("`?` operator outside top-level")
  }

  "module-level `var` emits a WhyML ref" in {
    // γ.2: module-level mutable vars become `val name : ref t = ref init`. Reads
    // through `!name`, writes through `name := value`. Drivers and kernel state
    // live here; this gates a large slice of OS verification.
    val mlw = translate(
      """var counter: int = 0
        |""".stripMargin)
    mlw should include("val counter : ref int = ref 0")
  }

  "module-level `var` reads emit deref `!name`" in {
    val mlw = translate(
      """var counter: int = 0
        |
        |def get_counter() -> int
        |    counter
        |""".stripMargin)
    mlw should include("val counter : ref int = ref 0")
    mlw should include("= !counter")
  }

  // ====================================================================================
  // Phase γ.1 — generic structs as parametric WhyML records
  // ====================================================================================

  "generic struct with one type param emits a parametric record" in {
    val mlw = translate(
      """struct Box[T]
        |    value: T
        |""".stripMargin)
    mlw should include("type box 't = { value: 't }")
  }

  "generic struct with two type params" in {
    val mlw = translate(
      """struct Pair[T, U]
        |    fst: T
        |    snd: U
        |""".stripMargin)
    mlw should include("type pair 't 'u = { fst: 't; snd: 'u }")
  }

  "generic struct used as field type uses parametric application" in {
    val mlw = translate(
      """struct Box[T]
        |    value: T
        |
        |def unbox(b: Box[int]) -> int
        |    b.value
        |""".stripMargin)
    mlw should include("(b: box int)")
  }

  // ====================================================================================
  // Phase γ.3 — function without explicit return type lets WhyML infer
  // ====================================================================================

  "function without return type drops the `: ret` clause" in {
    // sysl `def` style often omits the return type. Why3 can infer from the body
    // expression; emit without `: ret` and let WhyML unify.
    val mlw = translate(
      """def twice(x: int)
        |    x * 2
        |""".stripMargin)
    // The signature line is `let function twice (x: int)` (no `: ret`).
    mlw should include("let function twice (x: int)")
    mlw should not include "twice (x: int) :"
  }

  // ====================================================================================
  // Phase γ.4 — `if without else` lowers to `else ()`
  // ====================================================================================

  "if without else lowers to `else ()`" in {
    val mlw = translate(
      """def f(b: bool)
        |    if b then 0
        |""".stripMargin)
    // The else branch is `()` (unit literal). Emitted regardless of body's type;
    // value-typed bodies hit a Why3 type-mismatch at verification time.
    mlw should include("else ()")
  }

  // ====================================================================================
  // Phase γ.5 — module-level val/var without type annotation lets WhyML infer
  // ====================================================================================

  "module-level const without type annotation lets WhyML infer" in {
    val mlw = translate(
      """const FORTY_TWO = 42
        |""".stripMargin)
    // sanitizeName lowercases all-uppercase names to avoid mAX_AGE-style ugliness.
    mlw should include("let constant forty_two = 42")
  }

  "module-level var without type annotation emits `val name = ref init`" in {
    val mlw = translate(
      """var counter = 0
        |""".stripMargin)
    mlw should include("val counter = ref 0")
  }

  // ====================================================================================
  // Phase δ.1 — frame conditions: emit #writes / #reads as WhyML writes/reads clauses
  // ====================================================================================

  "#writes attribute emits a WhyML writes clause" in {
    val mlw = translate(
      """var counter: int = 0
        |
        |#writes(counter)
        |bump() -> int
        |    counter = counter + 1
        |    counter
        |""".stripMargin)
    mlw should include("writes { counter }")
  }

  "#reads attribute emits a WhyML reads clause" in {
    val mlw = translate(
      """var counter: int = 0
        |
        |#reads(counter)
        |get_counter() -> int
        |    counter
        |""".stripMargin)
    // Annotation forces the impure-emit path (`let f`); reads clause emitted.
    mlw should include("reads { counter }")
  }

  "#writes with multiple vars emits semicolon-separated list" in {
    val mlw = translate(
      """var a: int = 0
        |var b: int = 0
        |
        |#writes(a)
        |#writes(b)
        |bump_both() -> int
        |    a = a + 1
        |    b = b + 1
        |    a + b
        |""".stripMargin)
    // Two #writes attrs combined; semicolon-joined inside the braces.
    mlw should (include("writes { a }") or include("writes { a; b }"))
    mlw should include("writes { b }")
  }

  // ====================================================================================
  // Phase δ.3 — module invariants: predicate over module state preserved by every
  // public function. The translator emits `predicate module_inv ()` at module scope
  // and adds implicit requires/ensures clauses to each non-private fn.
  // ====================================================================================

  "module_invariant emits a top-level predicate" in {
    val mlw = translate(
      """var counter: int = 0
        |
        |module_invariant counter >= 0
        |""".stripMargin)
    mlw should include("predicate module_inv ()")
    mlw should include("!counter >= 0")
  }

  "multiple module_invariant decls are conjoined with /\\" in {
    val mlw = translate(
      """var lo: int = 0
        |var hi: int = 100
        |
        |module_invariant lo <= hi
        |module_invariant lo >= 0
        |""".stripMargin)
    mlw should include("predicate module_inv ()")
    // Both clauses joined with formula-AND; deref `!` on the mutable refs.
    mlw should include("/\\")
    mlw should include("!lo <= !hi")
    mlw should include("!lo >= 0")
  }

  "public fn implicitly carries requires/ensures of module_invariant" in {
    val mlw = translate(
      """var counter: int = 0
        |
        |module_invariant counter >= 0
        |
        |#writes(counter)
        |bump() -> int
        |    counter = counter + 1
        |    counter
        |""".stripMargin)
    mlw should include("requires { module_inv () }")
    mlw should include("ensures  { module_inv () }")
  }

  "private fn does NOT carry the implicit module_inv clauses" in {
    val mlw = translate(
      """var counter: int = 0
        |
        |module_invariant counter >= 0
        |
        |private def helper(n: int) -> int
        |    n + 1
        |""".stripMargin)
    // Helper is private; the implicit clauses are skipped.
    mlw should not include "requires { module_inv () }"
  }

  // ====================================================================================
  // Phase δ.2 — lexicographic termination measures: `variant { e1, e2 }`
  // ====================================================================================

  "single-expr `variant e` keeps the existing single-measure form" in {
    val mlw = translate(
      """def fact(n: int) -> int
        |    variant n
        |    if n <= 0 then 1
        |    else n * fact(n - 1)
        |""".stripMargin)
    // Single-expression form: `variant  { n }` (existing keyword spacing).
    mlw should include("variant  { n }")
    mlw should not include "variant  { n;"
  }

  "multi-arg `variant { a, b }` emits a lex tuple" in {
    val mlw = translate(
      """def ack(m: int, n: int) -> int
        |    variant { m, n }
        |    if m <= 0 then n + 1
        |    else if n <= 0 then ack(m - 1, 1)
        |    else ack(m - 1, ack(m, n - 1))
        |""".stripMargin)
    // Lex tuple — semicolon-joined.
    mlw should include("variant  { m; n }")
  }

  "module_invariant must be bool — non-bool expression is rejected by analyzer" in {
    // Module invariants live at the spec layer; analyzer validates the expression
    // is bool-typed before the WhyML backend ever sees it.
    val ex = intercept[RuntimeException] {
      val src =
        """var counter: int = 0
          |
          |module_invariant counter
          |""".stripMargin
      val Right(ast) = (new SyslParser).parseProgram(src): @unchecked
      (new SyslAnalyzer).analyze(ast)
    }
    ex.getMessage should include("module_invariant")
    ex.getMessage should include("bool")
  }

  "unsupported expression form yields a clear error naming the gap" in {
    // Slices aren't part of any verification phase yet — translator should reject
    // them up front rather than silently produce ill-formed WhyML.
    val ex = intercept[RuntimeException](translate(
      """def s(xs: []int) -> int
        |    xs[0]
        |""".stripMargin))
    ex.getMessage should include("WhyML translator: unsupported")
  }

  // ====================================================================================
  // Phase β — match/pattern support: multi-pattern arms, ranges, struct destructure,
  // wildcard before else in literal path. Guards still rejected (Why3 logic-mode `match`
  // has no `when` clauses).
  // ====================================================================================

  "match arm with multiple ADT patterns OR-joins them" in {
    val mlw = translate(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |
        |def is_warm(c: Color) -> bool
        |    c match
        |        Color.Red, Color.Green -> true
        |        Color.Blue             -> false
        |""".stripMargin)
    mlw should include("Red | Green -> true")
  }

  "match arm with multiple literal patterns OR-joins conditions in if-chain" in {
    val mlw = translate(
      """def is_one_or_two(n: int) -> bool
        |    n match
        |        1, 2 -> true
        |        else -> false
        |""".stripMargin)
    // Multi-pattern in the literal-path produces a `\/` join between conditions.
    mlw should include("n = 1 \\/ n = 2")
  }

  "range pattern in literal-path lowers to inclusive bounds check" in {
    val mlw = translate(
      """def in_range(n: int) -> bool
        |    n match
        |        1..10 -> true
        |        else  -> false
        |""".stripMargin)
    mlw should include("n >= 1 /\\ n <= 10")
  }

  "wildcard before else in literal path matches anything" in {
    val mlw = translate(
      """def label(n: int) -> int
        |    n match
        |        0    -> 100
        |        _    -> 999
        |        else -> -1
        |""".stripMargin)
    // The `_` becomes `if true then ... else ...`. (Reaching the wildcard arm
    // means the 0 arm didn't match.)
    mlw should include("if true then")
  }

  "struct destructure pattern emits WhyML record pattern" in {
    val mlw = translate(
      """struct Point
        |    x: int
        |    y: int
        |
        |def get_x(p: Point) -> int
        |    p match
        |        Point(a, b) -> a
        |""".stripMargin)
    mlw should include("{ x = a; y = b }")
  }

  "match arm with guard is rejected with the new clearer message" in {
    // Guards aren't surface syntax in sysl yet; this test pins the rejection
    // path. When sysl gains guard syntax, the lowering strategy will need to
    // change (Why3 logic-mode `match` has no `when` clauses).
    // For now, the rejection fires when an arm carries a guard internally.
    // No surface-test driver — checked indirectly via the `when` keyword in
    // the rejection message text. Skip this case.
    pending
  }
}
