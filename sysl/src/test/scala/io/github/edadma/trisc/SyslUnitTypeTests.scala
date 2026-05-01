package io.github.edadma.trisc

class SyslUnitTypeTests extends SyslTestHelpers {

  // ===== Basic value-position usage =====

  "val of unit type with () literal" in {
    eval(
      """main() -> int
        |    val x: unit = ()
        |    7
        |""".stripMargin) shouldBe 7
  }

  "var of unit type, reassigned" in {
    eval(
      """main() -> int
        |    var x: unit = ()
        |    x = ()
        |    8
        |""".stripMargin) shouldBe 8
  }

  "function returns unit via () literal" in {
    eval(
      """f() -> unit = ()
        |
        |main() -> int
        |    f()
        |    9
        |""".stripMargin) shouldBe 9
  }

  "function takes unit param" in {
    eval(
      """take(x: unit) -> int = 11
        |
        |main() -> int = take(())
        |""".stripMargin) shouldBe 11
  }

  // ===== unit as a generic type argument =====

  "unit as type arg in generic alias" in {
    eval(
      """type Box[T] = new int
        |
        |main() -> int
        |    var b: Box[unit] = Box[unit](42)
        |    int(b)
        |""".stripMargin) shouldBe 42
  }

  "Option[unit] round-trips: Some(()) and pattern match" in {
    eval(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |main() -> int
        |    var x: Option[unit] = Some(())
        |    x match
        |        Some(_) -> 13
        |        None -> 0
        |""".stripMargin) shouldBe 13
  }

  "generic enum variant carrying unit payload" in {
    eval(
      """enum E[T]
        |    Done(x: T)
        |    Pending
        |
        |main() -> int
        |    var e: E[unit] = Done(())
        |    e match
        |        Done(_) -> 17
        |        Pending -> 0
        |""".stripMargin) shouldBe 17
  }

  // ===== unit in struct fields =====

  "struct field of type unit is allowed" in {
    eval(
      """struct WithUnit
        |    flag: unit
        |    n: int
        |
        |main() -> int
        |    var w = WithUnit((), 19)
        |    w.n
        |""".stripMargin) shouldBe 19
  }

  // ===== Negative cases =====

  "() does not convert to int" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    var x: int = ()
          |    x
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.nonEmpty, s"should reject implicit unit→int, got: $msg")
  }

  "() does not convert to bool" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    var x: bool = ()
          |    if x then 1 else 0
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.nonEmpty, s"should reject implicit unit→bool, got: $msg")
  }

  "int does not convert to unit" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    var x: unit = 42
          |    7
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.nonEmpty, s"should reject implicit int→unit, got: $msg")
  }
}
