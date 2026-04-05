package io.github.edadma.trisc

class SyslCodegenTraitsTests extends SyslCodegenHelpers {

  "trait with required method" in {
    compileAndRun(
      """trait Double[T]
        |    twice(x: T) -> T
        |
        |impl Double[int]
        |    twice(x: int) -> int = x + x
        |
        |main() -> int = Double.twice(21)
        |""".stripMargin) shouldBe 42
  }

  "trait with multiple impls" in {
    compileAndRun(
      """trait Scale[T]
        |    scale(x: T) -> T
        |
        |impl Scale[int]
        |    scale(x: int) -> int = x * 10
        |
        |impl Scale[i64]
        |    scale(x: i64) -> i64 = x * 100i64
        |
        |main() -> int
        |    a = Scale.scale(5)
        |    b: i64 = Scale.scale(5i64)
        |    a + i32(b)
        |""".stripMargin) shouldBe (50 + 500)
  }

  "default method from trait" in {
    compileAndRun(
      """trait Ord[T]
        |    cmp(a: T, b: T) -> int
        |    lt(a: T, b: T) -> bool = cmp(a, b) < 0
        |
        |impl Ord[int]
        |    cmp(a: int, b: int) -> int = a - b
        |
        |main() -> int = if Ord.lt(3, 7) then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "overridden default method" in {
    compileAndRun(
      """trait X[T]
        |    base(x: T) -> T
        |    derived(x: T) -> T = base(x) * 2
        |
        |impl X[int]
        |    base(x: int) -> int = x + 1
        |    derived(x: int) -> int = 999
        |
        |main() -> int = X.derived(5)
        |""".stripMargin) shouldBe 999
  }

  "chained default methods" in {
    compileAndRun(
      """trait Succ[T]
        |    succ(x: T) -> T
        |    plus2(x: T) -> T = succ(succ(x))
        |    plus3(x: T) -> T = succ(plus2(x))
        |
        |impl Succ[int]
        |    succ(x: int) -> int = x + 1
        |
        |main() -> int = Succ.plus3(10)
        |""".stripMargin) shouldBe 13
  }
}
