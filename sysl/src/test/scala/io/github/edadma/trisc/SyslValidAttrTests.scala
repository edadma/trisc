package io.github.edadma.trisc

/** T::Valid(x) — non-throwing introspection: does `x` satisfy the type's constraints? */
class SyslValidAttrTests extends SyslTestHelpers {

  // ===== ::Valid on within-constrained int types =====

  "::Valid is true inside inclusive range" in {
    eval("""
      |type Age = int within 0..150
      |main() -> int
      |    if Age::Valid(42) then 1 else 0
      |""".stripMargin) shouldBe 1
  }

  "::Valid is true at inclusive lower bound" in {
    eval("""
      |type Age = int within 0..150
      |main() -> int
      |    if Age::Valid(0) then 1 else 0
      |""".stripMargin) shouldBe 1
  }

  "::Valid is true at inclusive upper bound" in {
    eval("""
      |type Age = int within 0..150
      |main() -> int
      |    if Age::Valid(150) then 1 else 0
      |""".stripMargin) shouldBe 1
  }

  "::Valid is false above upper bound" in {
    eval("""
      |type Age = int within 0..150
      |main() -> int
      |    if Age::Valid(200) then 1 else 0
      |""".stripMargin) shouldBe 0
  }

  "::Valid is false below lower bound" in {
    eval("""
      |type Age = int within 0..150
      |main() -> int
      |    if Age::Valid(-5) then 1 else 0
      |""".stripMargin) shouldBe 0
  }

  "::Valid with exclusive-upper range excludes the upper endpoint" in {
    eval("""
      |type Idx = int within 0..<10
      |main() -> int
      |    if Idx::Valid(10) then 1 else 0
      |""".stripMargin) shouldBe 0
  }

  "::Valid with exclusive-upper range includes upper - 1" in {
    eval("""
      |type Idx = int within 0..<10
      |main() -> int
      |    if Idx::Valid(9) then 1 else 0
      |""".stripMargin) shouldBe 1
  }

  "::Valid on derived type with range" in {
    eval("""
      |type Meters = new int within 0..1000
      |main() -> int
      |    if Meters::Valid(500) then 1 else 0
      |""".stripMargin) shouldBe 1
  }

  // ===== Common guard-style use =====

  "::Valid guards a safe assignment" in {
    eval("""
      |type Age = int within 0..150
      |main() -> int
      |    var raw = 42
      |    if Age::Valid(raw) then
      |        var a: Age = raw
      |        int(a)
      |    else
      |        -1
      |""".stripMargin) shouldBe 42
  }

  "::Valid does not trap on invalid input" in {
    // This specifically verifies the non-throwing semantic.
    eval("""
      |type Age = int within 0..150
      |main() -> int
      |    var raw = 9999
      |    if Age::Valid(raw) then raw else 7
      |""".stripMargin) shouldBe 7
  }

  // ===== ::Valid on simple enums =====

  "::Valid is true for a known enum variant value" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int
      |    if Color::Valid(1) then 1 else 0
      |""".stripMargin) shouldBe 1
  }

  "::Valid is false for an unknown integer (no matching variant)" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int
      |    if Color::Valid(99) then 1 else 0
      |""".stripMargin) shouldBe 0
  }

  "::Valid respects enum with explicit values — in range" in {
    eval("""
      |enum Status
      |    Ok = 200
      |    NotFound = 404
      |    Error = 500
      |
      |main() -> int
      |    if Status::Valid(404) then 1 else 0
      |""".stripMargin) shouldBe 1
  }

  "::Valid respects enum with explicit values — gap value" in {
    // 300 is between Ok (200) and NotFound (404) but not a variant value.
    eval("""
      |enum Status
      |    Ok = 200
      |    NotFound = 404
      |    Error = 500
      |
      |main() -> int
      |    if Status::Valid(300) then 1 else 0
      |""".stripMargin) shouldBe 0
  }

  "::Valid round-trips with ::Val" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int
      |    if Color::Valid(Color::Val(1)) then 1 else 0
      |""".stripMargin) shouldBe 1
  }

  // ===== Error cases =====

  "::Valid on plain alias fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type MyInt = int
        |main() -> int
        |    if MyInt::Valid(42) then 1 else 0
        |""".stripMargin)
    }
    thrown.getMessage should include("range-constrained")
  }

  "::Valid rejects non-integer argument" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |main() -> int
        |    if Age::Valid(true) then 1 else 0
        |""".stripMargin)
    }
    thrown.getMessage should include("expects integer")
  }
}
