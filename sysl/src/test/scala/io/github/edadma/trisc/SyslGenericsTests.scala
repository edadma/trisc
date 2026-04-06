package io.github.edadma.trisc

class SyslGenericsTests extends SyslTestHelpers {

  // ===== Basic generic functions =====

  "generic identity function — int" in {
    eval(
      """id[T](x: T) -> T = x
        |main() -> int = id(42)
        |""".stripMargin) shouldBe 42
  }

  "generic identity function — different types" in {
    eval(
      """id[T](x: T) -> T = x
        |main() -> int
        |    a = id(10)
        |    b = id(20)
        |    a + b
        |""".stripMargin) shouldBe 30
  }

  "generic swap via pointers" in {
    eval(
      """swap[T](a: *T, b: *T)
        |    var tmp: T = *a
        |    *a = *b
        |    *b = tmp
        |
        |main() -> int
        |    var x = 10
        |    var y = 20
        |    swap(&x, &y)
        |    x * 100 + y
        |""".stripMargin) shouldBe 2010
  }

  "generic swap — different types" in {
    eval(
      """swap[T](a: *T, b: *T)
        |    var tmp: T = *a
        |    *a = *b
        |    *b = tmp
        |
        |main() -> int
        |    var x = 1
        |    var y = 2
        |    swap(&x, &y)
        |    var a: f64 = 1.5
        |    var b: f64 = 2.5
        |    swap(&a, &b)
        |    if a > 2.0 && x == 2 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "generic max — int" in {
    eval(
      """max[T](a: T, b: T) -> T
        |    if a > b then a else b
        |
        |main() -> int = max(3, 7)
        |""".stripMargin) shouldBe 7
  }

  "generic max — float" in {
    eval(
      """max[T](a: T, b: T) -> T
        |    if a > b then a else b
        |
        |main() -> int
        |    var m: f64 = max(1.5, 3.25)
        |    int(m * 100.0)
        |""".stripMargin) shouldBe 325
  }

  "generic with two type params" in {
    eval(
      """pair_first[K, V](k: K, v: V) -> K = k
        |pair_second[K, V](k: K, v: V) -> V = v
        |main() -> int = pair_first(42, 'x') + pair_second(10, 20)
        |""".stripMargin) shouldBe 62
  }

  // ===== Instantiation cache — same name used for different types =====

  "calling generic with same type twice reuses instantiation" in {
    eval(
      """id[T](x: T) -> T = x
        |main() -> int
        |    a = id(10)
        |    b = id(20)
        |    a + b
        |""".stripMargin) shouldBe 30
  }

  // ===== Recursion within generics =====

  "generic function recursive" in {
    eval(
      """sumn[T](n: T, acc: T) -> T
        |    if n == 0 then acc else sumn(n - 1, acc + n)
        |main() -> int = sumn(5, 0)
        |""".stripMargin) shouldBe 15
  }

  // ===== Instantiation-time error =====

  "generic function with operation invalid for type fails" in {
    an[Exception] should be thrownBy eval(
      """add[T](a: T, b: T) -> T = a + b
        |main() -> int = if add(true, false) then 1 else 0
        |""".stripMargin)
  }

  // ===== Generics with local type variable =====

  "generic with T in local variable type" in {
    eval(
      """twice[T](x: T) -> T
        |    var result: T = x + x
        |    result
        |main() -> int = twice(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Generics using return type only =====

  "generic return type can be inferred from args" in {
    eval(
      """first[T](x: T, y: T) -> T = x
        |main() -> int = first(100, 200)
        |""".stripMargin) shouldBe 100
  }
}
