package io.github.edadma.trisc

class SyslSVMFuncPtrTests extends SyslSVMCodegenHelpers {

  "call through function variable" in {
    compileAndRun(
      """add(a: i64, b: i64) -> i64 = a + b
        |
        |main() -> i64
        |    var f = add
        |    f(3, 4)
        |""".stripMargin) shouldBe 7
  }

  "higher-order apply" in {
    compileAndRun(
      """apply(f: (i64) -> i64, x: i64) -> i64 = f(x)
        |
        |dbl(n: i64) -> i64 = n * 2
        |
        |main() -> i64 = apply(dbl, 21)
        |""".stripMargin) shouldBe 42
  }

  "closure with no captures" in {
    compileAndRun(
      """apply(f: (i64) -> i64, x: i64) -> i64 = f(x)
        |
        |main() -> i64 = apply((n: i64) -> n + 1, 41)
        |""".stripMargin) shouldBe 42
  }

  "closure capturing scalar" in {
    compileAndRun(
      """apply(f: (i64) -> i64, x: i64) -> i64 = f(x)
        |
        |main() -> i64
        |    val factor = 5i64
        |    apply((n: i64) -> n * factor, 8)
        |""".stripMargin) shouldBe 40
  }

  "closure capturing pointer (test_each_sum shape)" in {
    compileAndRun(
      """struct Acc
        |    n: i64
        |
        |add_to(p: *Acc, v: i64) -> unit
        |    (*p).n = (*p).n + v
        |
        |run3(f: (i64) -> unit)
        |    f(1)
        |    f(2)
        |    f(3)
        |
        |main() -> i64
        |    var acc: Acc
        |    val pa = &acc
        |    run3((v: i64) -> add_to(pa, v))
        |    acc.n
        |""".stripMargin) shouldBe 6
  }

  // ===== Inner def declarations (recursive named local closures) =====

  "inner def with self-recursion (factorial)" in {
    compileAndRun(
      """outer() -> i64
        |    def fact(n: i64) -> i64
        |        if n == 0 then return 1i64
        |        n * fact(n - 1)
        |    fact(5i64)
        |
        |main() -> i64 = outer()
        |""".stripMargin) shouldBe 120
  }

  "inner def captures outer parameter" in {
    compileAndRun(
      """outer(base: i64) -> i64
        |    def add_base(n: i64) -> i64
        |        n + base
        |    add_base(7i64)
        |
        |main() -> i64 = outer(35i64)
        |""".stripMargin) shouldBe 42
  }

  "inner def with self-recursion uses captured outer local" in {
    compileAndRun(
      """outer(bonus: i64) -> i64
        |    def sum_with_bonus(n: i64) -> i64
        |        if n == 0 then return 0i64
        |        n + bonus + sum_with_bonus(n - 1)
        |    sum_with_bonus(3i64)
        |
        |main() -> i64 = outer(10i64)
        |""".stripMargin) shouldBe 36
  }

  "inner def with zero parameters" in {
    compileAndRun(
      """outer() -> i64
        |    def constant() -> i64 = 42i64
        |    constant()
        |
        |main() -> i64 = outer()
        |""".stripMargin) shouldBe 42
  }
}
