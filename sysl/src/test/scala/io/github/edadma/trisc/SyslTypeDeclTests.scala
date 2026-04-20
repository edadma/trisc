package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslTypeDeclTests extends SyslTestHelpers {

  // ===== Plain aliases (pre-existing behavior) =====

  "plain alias is compatible with base" in {
    eval("""
      |type MyInt = int
      |main() -> int =
      |    var a: MyInt = 42
      |    var b: int = a
      |    b
      |""".stripMargin) shouldBe 42
  }

  // ===== Subtypes: `within` range, base-compatible =====

  "subtype within accepts in-range literal" in {
    eval("""
      |type Age = int within 0..150
      |main() -> int =
      |    var a: Age = 42
      |    a
      |""".stripMargin) shouldBe 42
  }

  "subtype within is base-compatible without cast" in {
    eval("""
      |type Age = int within 0..150
      |main() -> int =
      |    var a: Age = 42
      |    var b: int = a
      |    b + 1
      |""".stripMargin) shouldBe 43
  }

  "subtype within rejects compile-time out-of-range literal" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |main() -> int =
        |    var a: Age = 200
        |    a
        |""".stripMargin)
    }
    thrown.getMessage should include("out of range")
  }

  "subtype within traps on runtime out-of-range value" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |main() -> int =
        |    var x = 200
        |    var a: Age = x
        |    a
        |""".stripMargin)
    }
    thrown.getMessage should include("range check failed")
    thrown.getMessage should include("Age")
  }

  "subtype within checks at assignment" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |main() -> int =
        |    var a: Age = 10
        |    var x = 999
        |    a = x
        |    a
        |""".stripMargin)
    }
    thrown.getMessage should include("range check failed")
  }

  "subtype within exclusive upper bound" in {
    eval("""
      |type Prob = int within 0..<100
      |main() -> int =
      |    var a: Prob = 99
      |    a
      |""".stripMargin) shouldBe 99
  }

  "subtype within exclusive upper bound rejects equal to hi" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Prob = int within 0..<100
        |main() -> int =
        |    var a: Prob = 100
        |    a
        |""".stripMargin)
    }
    thrown.getMessage should include("out of range")
  }

  // ===== Derived types (`new`): nominal, base-incompatible =====

  "derived type rejects assignment from base without cast" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Meters = new int
        |main() -> int =
        |    var m: Meters = 5
        |    int(m)
        |""".stripMargin)
    }
    thrown.getMessage should (include("cannot") and include("Meters"))
  }

  "derived type accepts explicit cast from base" in {
    eval("""
      |type Meters = new int
      |main() -> int =
      |    var m: Meters = Meters(5)
      |    int(m)
      |""".stripMargin) shouldBe 5
  }

  "derived type arithmetic between values keeps nominal type" in {
    eval("""
      |type Meters = new int
      |main() -> int =
      |    var a: Meters = Meters(3)
      |    var b: Meters = Meters(4)
      |    var c: Meters = a + b
      |    int(c)
      |""".stripMargin) shouldBe 7
  }

  "derived type rejects mixing with base in arithmetic" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Meters = new int
        |main() -> int =
        |    var m: Meters = Meters(3)
        |    var x: int = 2
        |    var r: Meters = m + x
        |    int(r)
        |""".stripMargin)
    }
    thrown.getMessage should include("nominal")
  }

  "derived type preserves unrelated nominal distinctness" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Meters = new int
        |type Seconds = new int
        |main() -> int =
        |    var m: Meters = Meters(3)
        |    var s: Seconds = Meters(5)
        |    int(m) + int(s)
        |""".stripMargin)
    }
    thrown.getMessage should include("Seconds")
  }

  // ===== Derived + range =====

  "derived + within traps on out-of-range runtime cast" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type SafeAge = new int within 0..150
        |main() -> int =
        |    var x = 999
        |    var a: SafeAge = SafeAge(x)
        |    int(a)
        |""".stripMargin)
    }
    thrown.getMessage should include("range check failed")
    thrown.getMessage should include("SafeAge")
  }

  "derived + within passes in-range runtime cast" in {
    eval("""
      |type SafeAge = new int within 0..150
      |main() -> int =
      |    var x = 42
      |    var a: SafeAge = SafeAge(x)
      |    int(a)
      |""".stripMargin) shouldBe 42
  }

  "derived + within rejects compile-time out-of-range literal" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type SafeAge = new int within 0..150
        |main() -> int =
        |    var a: SafeAge = SafeAge(200)
        |    int(a)
        |""".stripMargin)
    }
    thrown.getMessage should include("out of range")
  }

  // ===== Float ranges =====

  "float subtype accepts in-range literal" in {
    val (code, _) = run("""
      |type Prob = f64 within 0.0..<1.0
      |main() -> int =
      |    var p: Prob = 0.5
      |    if p < 1.0 then 1 else 0
      |""".stripMargin)
    code shouldBe 1
  }

  "float subtype rejects compile-time out-of-range literal" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Prob = f64 within 0.0..<1.0
        |main() -> int =
        |    var p: Prob = 1.5
        |    0
        |""".stripMargin)
    }
    thrown.getMessage should include("out of range")
  }

  // ===== Range checks at produce-sites =====

  "range check fires on function parameter" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |takesAge(a: Age) -> int = int(a)
        |main() -> int =
        |    var x = 500
        |    takesAge(x)
        |""".stripMargin)
    }
    thrown.getMessage should include("range check failed")
  }

  "range check fires on return" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |produce(x: int) -> Age = x
        |main() -> int =
        |    int(produce(999))
        |""".stripMargin)
    }
    thrown.getMessage should include("range check failed")
  }

  // ===== Empty range =====

  "empty range is a compile error" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Bad = int within 10..5
        |main() -> int = 0
        |""".stripMargin)
    }
    thrown.getMessage should include("empty range")
  }

  "within on non-numeric base is a compile error" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Bad = bool within 0..1
        |main() -> int = 0
        |""".stripMargin)
    }
    thrown.getMessage should include("numeric base")
  }
}
