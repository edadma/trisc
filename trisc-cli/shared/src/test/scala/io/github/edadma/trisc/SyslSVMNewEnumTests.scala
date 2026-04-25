package io.github.edadma.trisc

class SyslSVMNewEnumTests extends SyslSVMCodegenHelpers {

  // Heap-allocated enum variants: `new VariantName(args)` returns
  // `&EnumType` (a pointer to the variant data, with rc header reserved on
  // backends that have one). On SVM, allocation is on the memory stack and
  // leaks per-test.

  "new data-enum variant tag is readable" in {
    compileAndRun(
      """enum Result
        |    Ok(x: i64)
        |    Err
        |
        |main() -> i64
        |    val r = new Ok(42i64)
        |    *r match
        |        Ok(_) -> 1i64
        |        Err -> 0i64
        |""".stripMargin) shouldBe 1
  }

  "new data-enum binds payload" in {
    compileAndRun(
      """enum Result
        |    Ok(x: i64)
        |    Err
        |
        |main() -> i64
        |    val r = new Ok(123i64)
        |    *r match
        |        Ok(v) -> v
        |        Err -> -1i64
        |""".stripMargin) shouldBe 123
  }

  "new data-enum nullary variant" in {
    compileAndRun(
      """enum Result
        |    Ok(x: i64)
        |    Err
        |
        |main() -> i64
        |    val r = new Err()
        |    *r match
        |        Ok(v) -> v
        |        Err -> 99i64
        |""".stripMargin) shouldBe 99
  }

  "new data-enum with multiple fields" in {
    compileAndRun(
      """enum Pair
        |    Both(a: i64, b: i64)
        |    Empty
        |
        |main() -> i64
        |    val p = new Both(7i64, 11i64)
        |    *p match
        |        Both(x, y) -> x + y
        |        Empty -> 0i64
        |""".stripMargin) shouldBe 18
  }
}
