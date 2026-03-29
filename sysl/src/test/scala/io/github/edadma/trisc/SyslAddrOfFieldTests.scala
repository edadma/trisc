package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslAddrOfFieldTests extends SyslTestHelpers {

  // ===== Address-of variable (already works) =====

  "address-of variable" in {
    eval(
      """main() -> int
        |    x = 42
        |    p = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "address-of array element (already works)" in {
    eval(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 42
        |    p = &arr[1]
        |    *p
        |""".stripMargin) shouldBe 42
  }

  // ===== Address-of struct field =====

  "address-of struct field" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    p.y = 42
        |    ptr = &p.y
        |    *ptr
        |""".stripMargin) shouldBe 42
  }

  "address-of struct field write through pointer" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    ptr = &p.x
        |    *ptr = 99
        |    p.x
        |""".stripMargin) shouldBe 99
  }

  "address-of nested struct field" in {
    eval(
      """struct Inner
        |    value: int
        |
        |struct Outer
        |    inner: Inner
        |
        |main() -> int
        |    o: Outer
        |    o.inner.value = 42
        |    ptr = &o.inner.value
        |    *ptr
        |""".stripMargin) shouldBe 42
  }

  "pass address-of field to function" in {
    eval(
      """struct Pair
        |    a: int
        |    b: int
        |
        |set_val(p: *int, v: int)
        |    *p = v
        |
        |main() -> int
        |    pair: Pair
        |    set_val(&pair.b, 42)
        |    pair.b
        |""".stripMargin) shouldBe 42
  }
}
