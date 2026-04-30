package io.github.edadma.trisc

class SyslExpectedTypeInferTests extends SyslTestHelpers {

  // ===== Cast args propagate expected type =====
  //
  // For a generic-alias cast `Parser[B](closure)`, the closure should be analyzed
  // with the alias's underlying type (a function type) as the expected type — so
  // closure parameters get inferred and the closure body's return type carries
  // the expected variant-instantiation context.

  "cast arg gets expected-type from generic-alias underlying" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[int] = Parser[int]((x) -> x + 1)
        |
        |main() -> int
        |    val p = make()
        |    p(41)
        |""".stripMargin) shouldBe 42
  }

  // ===== Match-arm bodies inherit expected type =====
  //
  // Each arm body and the optional `else` body get the match expression's
  // expected type. This is what lets variant constructors with phantom type
  // parameters (the variant doesn't carry the type param in any field) infer
  // their type args from context.

  "phantom-type variant in match arm uses expected type" in {
    eval(
      """enum Result[T, E]
        |    Ok(v: T)
        |    Err(e: E)
        |
        |classify(n: int) -> Result[int, string] =
        |    n match
        |        0 -> Err("zero")
        |        _ -> Ok(n)
        |
        |main() -> int
        |    classify(7) match
        |        Ok(v) -> v
        |        Err(_) -> -1
        |""".stripMargin) shouldBe 7
  }

  // ===== Composition: phantom-type variant inside lambda inside cast =====
  //
  // The user's exact bug: `Failure` for `ParseResult[A]` carries no `A` in
  // its fields, so `A` must come from context. The chain is:
  //   function return -> cast target -> closure return -> match expected ->
  //   arm body expected -> variant constructor inference.

  "user-bug repro: map[A,B] with phantom-A Failure variant" in {
    eval(
      """struct Input
        |    s: string
        |    pos: int
        |
        |enum ParseResult[A]
        |    Success(v: A, n: Input)
        |    Failure(m: string, n: Input)
        |
        |type Parser[A] = new (Input) -> ParseResult[A]
        |
        |success[A](v: A) -> Parser[A] =
        |    Parser[A]((inp: Input) -> Success(v, inp))
        |
        |map[A, B](p: Parser[A], f: (A) -> B) -> Parser[B] =
        |    Parser[B]((inp: Input) ->
        |        p(inp) match
        |            Success(v, n) -> Success(f(v), n)
        |            Failure(m, n) -> Failure(m, n))
        |
        |main() -> int
        |    val p = map(success[int](21), (x: int) -> x * 2)
        |    p(Input("", 0)) match
        |        Success(v, _) -> v
        |        Failure(_, _) -> 0
        |""".stripMargin) shouldBe 42
  }

  // ===== Tuple type args + nested generic type args =====
  //
  // Generic type arguments are parsed as expressions in expression position
  // (e.g. `Parser[(A, B)](...)`). exprToTypeAST converts back to a TypeAST
  // for expressions matching `A`, `(A, B)`, and `Parser[A]`.

  "tuple type arg in generic-alias cast" in {
    eval(
      """type Pair[T] = new int
        |
        |make() -> Pair[(int, int)] = Pair[(int, int)](42)
        |
        |main() -> int
        |    val p = make()
        |    int(p)
        |""".stripMargin) shouldBe 42
  }

  "nested generic in type arg" in {
    eval(
      """type Box[T] = new int
        |
        |make() -> Box[Box[int]] = Box[Box[int]](42)
        |
        |main() -> int = int(make())
        |""".stripMargin) shouldBe 42
  }
}
