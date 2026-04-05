package io.github.edadma.trisc

class SyslCodegenGenericsTests extends SyslCodegenHelpers {

  "generic identity — int" in {
    compileAndRun(
      """id[T](x: T) -> T = x
        |main() -> int = id(42)
        |""".stripMargin) shouldBe 42
  }

  "generic swap via pointers" in {
    compileAndRun(
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

  "generic max" in {
    compileAndRun(
      """max[T](a: T, b: T) -> T
        |    if a > b then a else b
        |
        |main() -> int = max(3, 7)
        |""".stripMargin) shouldBe 7
  }

  "two independent instantiations" in {
    compileAndRun(
      """id[T](x: T) -> T = x
        |main() -> int
        |    a = id(10)
        |    b = id(20)
        |    a + b
        |""".stripMargin) shouldBe 30
  }

  "generic with two type params" in {
    compileAndRun(
      """pair_first[K, V](k: K, v: V) -> K = k
        |pair_second[K, V](k: K, v: V) -> V = v
        |main() -> int = pair_first(42, 'x') + pair_second(10, 20)
        |""".stripMargin) shouldBe 62
  }

  "generic with T in local variable type" in {
    compileAndRun(
      """twice[T](x: T) -> T
        |    var result: T = x + x
        |    result
        |main() -> int = twice(21)
        |""".stripMargin) shouldBe 42
  }

  "recursive generic function" in {
    compileAndRun(
      """sumn[T](n: T, acc: T) -> T
        |    if n == 0 then acc else sumn(n - 1, acc + n)
        |main() -> int = sumn(5, 0)
        |""".stripMargin) shouldBe 15
  }
}
