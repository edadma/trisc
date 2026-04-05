package io.github.edadma.trisc

class SyslGenericStructTests extends SyslTestHelpers {

  // ===== Basic generic struct =====

  "generic struct with one field, construct and read" in {
    eval(
      """struct Box[T]
        |    value: T
        |
        |main() -> int
        |    b = Box(42)
        |    b.value
        |""".stripMargin) shouldBe 42
  }

  "generic struct with two fields" in {
    eval(
      """struct Pair[T]
        |    first: T
        |    second: T
        |
        |main() -> int
        |    p = Pair(10, 20)
        |    p.first + p.second
        |""".stripMargin) shouldBe 30
  }

  "generic struct with multiple type params" in {
    eval(
      """struct Tuple[K, V]
        |    key: K
        |    value: V
        |
        |main() -> int
        |    t = Tuple(5, 100)
        |    t.key * t.value
        |""".stripMargin) shouldBe 500
  }

  "different instantiations are independent types" in {
    eval(
      """struct Box[T]
        |    value: T
        |
        |main() -> int
        |    b: Box[int] = Box(7)
        |    c: Box[i64] = Box(9i64)
        |    b.value + i32(c.value)
        |""".stripMargin) shouldBe 16
  }

  // ===== Use as parameter type =====

  "generic struct as function parameter (concrete instantiation)" in {
    eval(
      """struct Box[T]
        |    value: T
        |
        |getVal(b: Box[int]) -> int = b.value
        |
        |main() -> int
        |    b = Box(100)
        |    getVal(b)
        |""".stripMargin) shouldBe 100
  }

  "generic struct via pointer" in {
    eval(
      """struct Pair[T]
        |    a: T
        |    b: T
        |
        |sumPair(p: *Pair[int]) -> int = p.a + p.b
        |
        |main() -> int
        |    var p = Pair(3, 4)
        |    sumPair(&p)
        |""".stripMargin) shouldBe 7
  }

  // ===== Generic struct used with generic function =====

  "generic function over generic struct" in {
    eval(
      """struct Box[T]
        |    value: T
        |
        |unbox[T](b: *Box[T]) -> T = b.value
        |
        |main() -> int
        |    var b = Box(42)
        |    unbox(&b)
        |""".stripMargin) shouldBe 42
  }

  "generic function that swaps fields of generic struct" in {
    eval(
      """struct Pair[T]
        |    a: T
        |    b: T
        |
        |swapPair[T](p: *Pair[T])
        |    var tmp: T = p.a
        |    p.a = p.b
        |    p.b = tmp
        |
        |main() -> int
        |    var p = Pair(1, 99)
        |    swapPair(&p)
        |    p.a * 100 + p.b
        |""".stripMargin) shouldBe 9901
  }

  // ===== Explicit type annotation =====

  "explicit Pair[int] annotation" in {
    eval(
      """struct Pair[T]
        |    a: T
        |    b: T
        |
        |main() -> int
        |    p: Pair[int] = Pair(2, 3)
        |    p.a * p.b
        |""".stripMargin) shouldBe 6
  }

  // ===== Errors =====

  "wrong arity on generic struct construct is error" in {
    an[Exception] should be thrownBy eval(
      """struct Pair[T]
        |    a: T
        |    b: T
        |
        |main() -> int
        |    p = Pair(1)
        |    0
        |""".stripMargin)
  }

  "inconsistent type inference is error" in {
    an[Exception] should be thrownBy eval(
      """struct Pair[T]
        |    a: T
        |    b: T
        |
        |main() -> int
        |    p = Pair(1, 2.5)
        |    0
        |""".stripMargin)
  }
}
