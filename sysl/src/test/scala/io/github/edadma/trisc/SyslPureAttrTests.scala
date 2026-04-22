package io.github.edadma.trisc

class SyslPureAttrTests extends SyslTestHelpers {

  // ===== Positive cases: legal pure functions =====

  "pure function with arithmetic and local vars compiles" in {
    eval("""
      |#pure
      |square(x: int) -> int
      |    var y = x * x
      |    return y
      |main() -> int = square(7)
      |""".stripMargin) shouldBe 49
  }

  "pure function may call other pure functions" in {
    eval("""
      |#pure
      |twice(x: int) -> int = x + x
      |#pure
      |quad(x: int) -> int = twice(twice(x))
      |main() -> int = quad(5)
      |""".stripMargin) shouldBe 20
  }

  "pure function may recurse" in {
    eval("""
      |#pure
      |fact(n: int) -> int
      |    if n <= 1 then return 1
      |    return n * fact(n - 1)
      |main() -> int = fact(5)
      |""".stripMargin) shouldBe 120
  }

  "pure function may loop and mutate locals" in {
    eval("""
      |#pure
      |sumTo(n: int) -> int
      |    var acc = 0
      |    var i = 1
      |    while i <= n do
      |        acc = acc + i
      |        i = i + 1
      |    return acc
      |main() -> int = sumTo(10)
      |""".stripMargin) shouldBe 55
  }

  "pure function may use if/else / match" in {
    eval("""
      |#pure
      |sign(x: int) -> int
      |    if x > 0 then return 1
      |    elif x < 0 then return -1
      |    else return 0
      |main() -> int = sign(-7) + sign(0) + sign(42)
      |""".stripMargin) shouldBe 0
  }

  "pure function may read module-level consts" in {
    eval("""
      |const K = 10
      |#pure
      |addK(x: int) -> int = x + K
      |main() -> int = addK(5)
      |""".stripMargin) shouldBe 15
  }

  "pure function may call assert (termination-only side effect)" in {
    eval("""
      |#pure
      |safeDiv(a: int, b: int) -> int
      |    assert(b != 0, "divide by zero")
      |    return a / b
      |main() -> int = safeDiv(20, 4)
      |""".stripMargin) shouldBe 5
  }

  // ===== Negative cases: illegal side effects =====

  "pure function rejects calling an impure function" in {
    val t = intercept[Exception] {
      eval("""
        |impure(x: int) -> int
        |    puts("hello")
        |    return x
        |#pure
        |tryPure(x: int) -> int = impure(x)
        |main() -> int = tryPure(1)
        |""".stripMargin)
    }
    t.getMessage should include("#pure")
    t.getMessage should include("impure")
  }

  "pure function rejects calling an IO builtin" in {
    val t = intercept[Exception] {
      eval("""
        |#pure
        |bad(x: int) -> int
        |    puts("side effect")
        |    return x
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("puts")
  }

  "pure function rejects calling malloc" in {
    val t = intercept[Exception] {
      eval("""
        |#pure
        |bad(n: int) -> int
        |    var p = malloc(100)
        |    return n
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("malloc")
  }

  "pure function rejects asm blocks" in {
    val t = intercept[Exception] {
      eval("""
        |#pure
        |bad(x: int) -> int
        |    asm "nop"
        |    return x
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("asm")
  }

  "pure function rejects writing to a module-level var" in {
    val t = intercept[Exception] {
      eval("""
        |var counter = 0
        |#pure
        |bad() -> int
        |    counter = counter + 1
        |    return counter
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("non-local")
  }

  "pure function rejects new allocation" in {
    val t = intercept[Exception] {
      eval("""
        |struct Point
        |    x: int
        |    y: int
        |#pure
        |bad() -> int
        |    var p = new Point(1, 2)
        |    return p.x
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("allocate")
  }

  "pure function rejects field assignment through a pointer parameter" in {
    val t = intercept[Exception] {
      eval("""
        |struct Point
        |    x: int
        |    y: int
        |#pure
        |bad(p: *Point) -> int
        |    p.x = 99
        |    return p.x
        |main() -> int
        |    var pt: Point
        |    pt.x = 0
        |    pt.y = 0
        |    return bad(&pt)
        |""".stripMargin)
    }
    t.getMessage should include("field")
  }

  "pure function rejects indirect (function-pointer) calls" in {
    val t = intercept[Exception] {
      eval("""
        |add(a: int, b: int) -> int = a + b
        |#pure
        |bad(x: int) -> int
        |    var f = &add
        |    return f(x, x)
        |main() -> int = bad(3)
        |""".stripMargin)
    }
    t.getMessage should include("indirect")
  }

  "pure function rejects calling an unannotated function in the same file" in {
    // No #pure on `twice` → it's impure by default, even though trivially safe.
    val t = intercept[Exception] {
      eval("""
        |twice(x: int) -> int = x + x
        |#pure
        |bad(x: int) -> int = twice(x)
        |main() -> int = bad(5)
        |""".stripMargin)
    }
    t.getMessage should include("#pure")
  }

  "pure function rejects compound assignment to a struct field" in {
    val t = intercept[Exception] {
      eval("""
        |struct Box
        |    n: int
        |#pure
        |bad(b: *Box, d: int) -> int
        |    b.n += d
        |    return b.n
        |main() -> int
        |    var x: Box
        |    x.n = 10
        |    return bad(&x, 5)
        |""".stripMargin)
    }
    t.getMessage should include("field")
  }

  // ===== Cross-module purity propagation (SMETA) =====

  "pure function may call pure function from imported module" in {
    evalWithLibs(
      Map(
        "mymod/nums" -> """
          |module mymod
          |#pure
          |sq(x: int) -> int = x * x
          |""".stripMargin,
      ),
      """
        |import mymod.*
        |#pure
        |quad(x: int) -> int = sq(x) + sq(x)
        |main() -> int = quad(3)
        |""".stripMargin,
    ) shouldBe 18
  }

  "pure function rejects calling unannotated function from imported module" in {
    // `twice` has no #pure in mymod — imports with Func(isPure=false) — caller must fail.
    val t = intercept[Exception] {
      evalWithLibs(
        Map(
          "mymod/nums" -> """
            |module mymod
            |twice(x: int) -> int = x + x
            |""".stripMargin,
        ),
        """
          |import mymod.*
          |#pure
          |bad(x: int) -> int = twice(x)
          |main() -> int = bad(5)
          |""".stripMargin,
      )
    }
    t.getMessage should include("#pure")
    t.getMessage should include("twice")
  }

  "SMETA round-trips #pure flag via toSmeta / fromSmeta" in {
    // Guards the wire format: compile a #pure decl, serialize to SMETA text,
    // parse it back, assert the flag survived.
    val Right(ast) = (new SyslParser).parseProgram(
      """#pure
        |sq(x: int) -> int = x * x
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val metaOut = ModuleMeta.fromProgram(typed)
    metaOut.symbols.find(_.name == "sq").get.typ match
      case SymbolMeta.Kind.Func(_, _, _, isPure) => isPure shouldBe true
      case other                                  => fail(s"expected Func, got $other")

    val text = metaOut.toSmeta
    text should include("FUNCP sq")
    val Some(metaIn) = ModuleMeta.fromSmeta(text): @unchecked
    metaIn.symbols.find(_.name == "sq").get.typ match
      case SymbolMeta.Kind.Func(_, _, _, isPure) => isPure shouldBe true
      case other                                  => fail(s"expected Func, got $other")
  }
}
