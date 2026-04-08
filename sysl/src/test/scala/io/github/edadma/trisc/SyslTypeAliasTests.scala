package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslTypeAliasTests extends SyslTestHelpers {

  // ===== Basic type aliases =====

  "type alias for primitive" in {
    eval(
      """type Size = u32
        |
        |main() -> int
        |    var s: Size = 42
        |    int(s)
        |""".stripMargin) shouldBe 42
  }

  "type alias for signed int" in {
    eval(
      """type Score = i32
        |
        |main() -> int
        |    var s: Score = -10
        |    s
        |""".stripMargin) shouldBe -10
  }

  "type alias for pointer" in {
    eval(
      """type IntPtr = *int
        |
        |main() -> int
        |    x = 42
        |    var p: IntPtr = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "type alias in function signature" in {
    eval(
      """type Num = i32
        |
        |dbl_it(x: Num) -> Num = x * 2
        |
        |main() -> int = dbl_it(21)
        |""".stripMargin) shouldBe 42
  }

  "multiple type aliases" in {
    eval(
      """type Width = u32
        |type Height = u32
        |
        |area(w: Width, h: Height) -> u32 = w * h
        |
        |main() -> int = int(area(6, 7))
        |""".stripMargin) shouldBe 42
  }

  "chained type aliases" in {
    eval(
      """type A = i32
        |type B = A
        |
        |main() -> int
        |    var x: B = 99
        |    x
        |""".stripMargin) shouldBe 99
  }

  // ===== Uninitialized declarations via type alias =====

  "type alias for array — uninitialized" in {
    eval(
      """type Vec3 = [3]int
        |
        |main() -> int
        |    v: Vec3
        |    v[0] = 10
        |    v[1] = 20
        |    v[2] = 30
        |    v[0] + v[1] + v[2]
        |""".stripMargin) shouldBe 60
  }

  "type alias for struct — uninitialized" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |type Pos = Point
        |
        |main() -> int
        |    p: Pos
        |    p.x = 10
        |    p.y = 20
        |    p.x + p.y
        |""".stripMargin) shouldBe 30
  }

  "type alias for scalar — uninitialized" in {
    eval(
      """type Count = i32
        |
        |main() -> int
        |    c: Count
        |    c
        |""".stripMargin) shouldBe 0
  }

  // ===== Generic type aliases =====

  "generic type alias for function type" in {
    eval(
      """type Transform[T] = (T) -> T
        |
        |apply(f: Transform[int], x: int) -> int = f(x)
        |
        |dbl(n: int) -> int = n * 2
        |
        |main() -> int = apply(dbl, 21)
        |""".stripMargin) shouldBe 42
  }

  "generic type alias with data enum" in {
    eval(
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |type ParseResult[T] = Result[T, string]
        |
        |parse_int(s: string) -> ParseResult[int]
        |    if s == "42" then Ok(42)
        |    else Err("bad")
        |
        |main() -> int
        |    parse_int("42") match
        |        Ok(v) -> v
        |        Err(_) -> 0
        |""".stripMargin) shouldBe 42
  }

  "generic type alias with multiple params" in {
    eval(
      """type Pair[A, B] = (A) -> B
        |
        |run(f: Pair[int, int], x: int) -> int = f(x)
        |
        |inc(n: int) -> int = n + 1
        |
        |main() -> int = run(inc, 41)
        |""".stripMargin) shouldBe 42
  }

  "generic type alias nested in function type" in {
    eval(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |type Predicate[T] = (T) -> bool
        |
        |find(pred: Predicate[int], vals: *int, n: int) -> Option[int]
        |    var i = 0
        |    while i < n
        |        if pred(vals[i]) then return Some(vals[i])
        |        i++
        |    None
        |
        |is_big(x: int) -> bool = x > 10
        |
        |main() -> int
        |    var arr: [3]int
        |    arr[0] = 1
        |    arr[1] = 42
        |    arr[2] = 3
        |    find(is_big, &arr[0], 3) match
        |        Some(v) -> v
        |        None -> 0
        |""".stripMargin) shouldBe 42
  }

  // ===== Generic combinators (generic functions + generic type aliases) =====

  "generic function with generic type alias parameter" in {
    eval(
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |unwrap_or[T, E](r: Result[T, E], default: T) -> T
        |    r match
        |        Ok(v) -> v
        |        Err(_) -> default
        |
        |type Fallible[T] = Result[T, string]
        |
        |try_parse(s: string) -> Fallible[int]
        |    if s == "42" then Ok(42)
        |    else Err("bad")
        |
        |main() -> int
        |    val r = try_parse("42")
        |    unwrap_or(r, 0)
        |""".stripMargin) shouldBe 42
  }

  "generic combinator: map over Result" in {
    eval(
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |map_ok[A, B, E](r: Result[A, E], f: (A) -> B) -> Result[B, E]
        |    r match
        |        Ok(v) -> Ok(f(v))
        |        Err(e) -> Err(e)
        |
        |dbl(x: int) -> int = x * 2
        |
        |main() -> int
        |    val r: Result[int, string] = Ok(21)
        |    val mapped = map_ok(r, dbl)
        |    mapped match
        |        Ok(v) -> v
        |        Err(_) -> 0
        |""".stripMargin) shouldBe 42
  }

  "generic combinator: map with type alias" in {
    eval(
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |type ParseResult[T] = Result[T, string]
        |
        |map_ok[A, B](r: ParseResult[A], f: (A) -> B) -> ParseResult[B]
        |    r match
        |        Ok(v) -> Ok(f(v))
        |        Err(e) -> Err(e)
        |
        |dbl(x: int) -> int = x * 2
        |
        |main() -> int
        |    val r: ParseResult[int] = Ok(21)
        |    val mapped = map_ok(r, dbl)
        |    mapped match
        |        Ok(v) -> v
        |        Err(_) -> 0
        |""".stripMargin) shouldBe 42
  }

  // ===== Error cases =====

  "duplicate type alias is error" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """type Foo = i32
        |type Foo = u32
        |
        |main() -> int = 0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "wrong arity on generic type alias is error" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """type Pair[A, B] = (A) -> B
        |
        |main() -> int
        |    val f: Pair[int] = 0
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }
}
