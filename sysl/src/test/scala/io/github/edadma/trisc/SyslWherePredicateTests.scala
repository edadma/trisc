package io.github.edadma.trisc

class SyslWherePredicateTests extends SyslTestHelpers {

  // ===== Basic predicates =====

  "where predicate passes" in {
    eval("""
      |type Even = int where value % 2 == 0
      |main() -> int =
      |    var x = 4
      |    var e: Even = x
      |    int(e)
      |""".stripMargin) shouldBe 4
  }

  "where predicate traps when false" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Even = int where value % 2 == 0
        |main() -> int =
        |    var x = 5
        |    var e: Even = x
        |    int(e)
        |""".stripMargin)
    }
    thrown.getMessage should include("type predicate")
  }

  "where predicate with compound condition" in {
    eval("""
      |type Score = int where value >= 0 && value <= 100
      |main() -> int =
      |    var x = 42
      |    var s: Score = x
      |    int(s)
      |""".stripMargin) shouldBe 42
  }

  "where predicate rejects out-of-range value" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Score = int where value >= 0 && value <= 100
        |main() -> int =
        |    var x = 200
        |    var s: Score = x
        |    int(s)
        |""".stripMargin)
    }
    thrown.getMessage should include("type predicate")
  }

  // ===== Produce-site coverage =====

  "where predicate fires on function parameter" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Positive = int where value > 0
        |accept(p: Positive) -> int = int(p)
        |main() -> int =
        |    var x = -5
        |    accept(x)
        |""".stripMargin)
    }
    thrown.getMessage should include("type predicate")
  }

  "where predicate fires on return" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Positive = int where value > 0
        |produce(x: int) -> Positive = x
        |main() -> int = int(produce(-1))
        |""".stripMargin)
    }
    thrown.getMessage should include("type predicate")
  }

  "where predicate fires on explicit cast" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Even = int where value % 2 == 0
        |main() -> int =
        |    var x = 7
        |    var e = Even(x)
        |    int(e)
        |""".stripMargin)
    }
    thrown.getMessage should include("type predicate")
  }

  // ===== Combined with `new` and `within` =====

  "where predicate on derived type" in {
    eval("""
      |type EvenMeters = new int where value % 2 == 0
      |main() -> int =
      |    var m = EvenMeters(6)
      |    int(m)
      |""".stripMargin) shouldBe 6
  }

  "where predicate on derived type traps" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type EvenMeters = new int where value % 2 == 0
        |main() -> int =
        |    var m = EvenMeters(5)
        |    int(m)
        |""".stripMargin)
    }
    thrown.getMessage should include("type predicate")
  }

  "within and where combined" in {
    eval("""
      |type PosEven = int within 0..100 where value % 2 == 0
      |main() -> int =
      |    var x = 42
      |    var p: PosEven = x
      |    int(p)
      |""".stripMargin) shouldBe 42
  }

  "within fires before where" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type PosEven = int within 0..100 where value % 2 == 0
        |main() -> int =
        |    var x = 200
        |    var p: PosEven = x
        |    int(p)
        |""".stripMargin)
    }
    thrown.getMessage should include("range check")
  }

  "where fires when within passes" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type PosEven = int within 0..100 where value % 2 == 0
        |main() -> int =
        |    var x = 51
        |    var p: PosEven = x
        |    int(p)
        |""".stripMargin)
    }
    thrown.getMessage should include("type predicate")
  }

  // ===== where with const =====

  "where predicate can reference module const" in {
    eval("""
      |const MIN = 10
      |type BigEnough = int where value >= MIN
      |main() -> int =
      |    var x = 42
      |    var b: BigEnough = x
      |    int(b)
      |""".stripMargin) shouldBe 42
  }

  "where predicate with const fails correctly" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |const MIN = 10
        |type BigEnough = int where value >= MIN
        |main() -> int =
        |    var x = 5
        |    var b: BigEnough = x
        |    int(b)
        |""".stripMargin)
    }
    thrown.getMessage should include("type predicate")
  }
}
