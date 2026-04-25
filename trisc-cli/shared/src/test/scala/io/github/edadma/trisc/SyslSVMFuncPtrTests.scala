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
}
