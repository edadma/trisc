package io.github.edadma.trisc

class SyslConstFnTests extends SyslTestHelpers {

  // ===== Positive cases =====
  // `#const` is structurally validated at analysis time; the body is also legal at
  // runtime, so these programs evaluate normally through the interpreter and return
  // the expected value. Compile-time evaluation (replacing a `const NAME = expr`
  // initializer with a folded literal via `SyslInterpreter`) is a separate, future
  // pass; these tests pin only the parser + validator surface.

  "const function with arithmetic runs identically" in {
    eval("""
      |#const
      |square(x: int) -> int = x * x
      |main() -> int = square(7)
      |""".stripMargin) shouldBe 49
  }

  "const function may call other const functions" in {
    eval("""
      |#const
      |twice(x: int) -> int = x + x
      |#const
      |quad(x: int) -> int = twice(twice(x))
      |main() -> int = quad(5)
      |""".stripMargin) shouldBe 20
  }

  "const function may recurse" in {
    eval("""
      |#const
      |sumTo(n: int) -> int
      |    if n <= 0 then return 0
      |    return n + sumTo(n - 1)
      |main() -> int = sumTo(10)
      |""".stripMargin) shouldBe 55
  }

  "const function may loop and mutate locals" in {
    eval("""
      |#const
      |sumLoop(n: int) -> int
      |    var acc = 0
      |    var i = 1
      |    while i <= n do
      |        acc = acc + i
      |        i = i + 1
      |    return acc
      |main() -> int = sumLoop(10)
      |""".stripMargin) shouldBe 55
  }

  "const function may combine #const with #pure (redundant pure permitted)" in {
    eval("""
      |#pure
      |#const
      |cube(x: int) -> int = x * x * x
      |main() -> int = cube(3)
      |""".stripMargin) shouldBe 27
  }

  "const function may write through a pointer parameter" in {
    eval("""
      |#const
      |writeOne(p: *int) -> unit
      |    *p = 99
      |main() -> int
      |    var x = 0
      |    writeOne(&x)
      |    return x
      |""".stripMargin) shouldBe 99
  }

  "const function may construct fixed-size structs by value" in {
    eval("""
      |struct Point
      |    x: int
      |    y: int
      |#const
      |makePoint(x: int, y: int) -> Point = Point(x, y)
      |main() -> int
      |    var p = makePoint(20, 22)
      |    return p.x + p.y
      |""".stripMargin) shouldBe 42
  }

  "const function may build a stack array" in {
    eval("""
      |#const
      |sumFirstThree() -> int
      |    var a: [3]int
      |    a[0] = 10
      |    a[1] = 20
      |    a[2] = 12
      |    return a[0] + a[1] + a[2]
      |main() -> int = sumFirstThree()
      |""".stripMargin) shouldBe 42
  }

  "const function may call assert" in {
    eval("""
      |#const
      |safeDiv(a: int, b: int) -> int
      |    assert(b != 0, "divide by zero")
      |    return a / b
      |main() -> int = safeDiv(84, 2)
      |""".stripMargin) shouldBe 42
  }

  "const function may be passed via an indirect call typed #const" in {
    eval("""
      |#const
      |add(a: int, b: int) -> int = a + b
      |#const
      |apply(f: (int, int)->int #const, x: int) -> int = f(x, x)
      |main() -> int = apply(add, 21)
      |""".stripMargin) shouldBe 42
  }

  // ===== Negative cases =====

  "const function rejects calling a non-const function" in {
    val t = intercept[Exception] {
      eval("""
        |slow(x: int) -> int = x + 1
        |#const
        |bad(x: int) -> int = slow(x)
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("#const")
    t.getMessage should include("slow")
  }

  "const function rejects calling an IO builtin" in {
    val t = intercept[Exception] {
      eval("""
        |#const
        |bad(x: int) -> int
        |    puts("side effect")
        |    return x
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("puts")
  }

  "const function rejects calling malloc" in {
    val t = intercept[Exception] {
      eval("""
        |#const
        |bad(n: int) -> int
        |    var p = malloc(100)
        |    return n
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("malloc")
  }

  "const function rejects new heap allocation" in {
    val t = intercept[Exception] {
      eval("""
        |struct Point
        |    x: int
        |    y: int
        |#const
        |bad() -> int
        |    var p = new Point(1, 2)
        |    return p.x
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("allocate")
  }

  "const function rejects new array allocation" in {
    val t = intercept[Exception] {
      eval("""
        |#const
        |bad() -> int
        |    var a = new [10]int
        |    return a[0]
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("allocate")
  }

  "const function rejects formatted string interpolation" in {
    val t = intercept[Exception] {
      eval("""
        |#const
        |bad(x: int) -> int
        |    var s = s"got ${x}"
        |    return x
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("string")
  }

  "const function rejects str(...) conversion" in {
    val t = intercept[Exception] {
      eval("""
        |#const
        |bad(x: int) -> int
        |    var s = str(x)
        |    return x
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("str")
  }

  "const function rejects closures" in {
    val t = intercept[Exception] {
      eval("""
        |#const
        |bad(x: int) -> int
        |    var f = (y: int) -> y + x
        |    return f(1)
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("closure")
  }

  "const function rejects asm expression" in {
    val t = intercept[Exception] {
      eval("""
        |#const
        |bad(x: int) -> int
        |    asm("nop")
        |    return x + 1
        |main() -> int = bad(41)
        |""".stripMargin)
    }
    t.getMessage.toLowerCase should include("asm")
  }

  "const function rejects indirect call to a non-const function type" in {
    val t = intercept[Exception] {
      eval("""
        |add(a: int, b: int) -> int = a + b
        |#const
        |bad(x: int) -> int
        |    var f = &add
        |    return f(x, x)
        |main() -> int = bad(3)
        |""".stripMargin)
    }
    t.getMessage should include("indirect")
  }

  "const function rejects an unannotated callee in the same file" in {
    val t = intercept[Exception] {
      eval("""
        |plain(x: int) -> int = x + 1
        |#const
        |bad(x: int) -> int = plain(x)
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("#const")
    t.getMessage should include("plain")
  }

  "non-const function rejected when passed to a #const function-pointer slot" in {
    val t = intercept[Exception] {
      eval("""
        |add(a: int, b: int) -> int = a + b
        |apply(f: (int, int)->int #const, x: int) -> int = f(x, x)
        |main() -> int = apply(add, 21)
        |""".stripMargin)
    }
    t.getMessage.toLowerCase should (include("effect") or include("const"))
  }

  "non-const interface impl rejected when satisfying a #const method slot" in {
    val t = intercept[Exception] {
      eval("""
        |interface Op
        |    run(x: int) -> int #const
        |
        |struct Plain
        |    delta: int
        |
        |Plain_run(__self__: Plain, x: int) -> int = x + __self__.delta
        |
        |drive(op: Op, v: int) -> int = op.run(v)
        |
        |main() -> int
        |    var p = Plain(1)
        |    return drive(p, 41)
        |""".stripMargin)
    }
    t.getMessage.toLowerCase should (include("const") or include("interface") or include("effect") or include("expects op"))
  }

  "const combined with #reads is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |var counter = 0
        |#const
        |#reads(counter)
        |bad(x: int) -> int = counter + x
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage.toLowerCase should (include("#const") or include("const"))
  }

  "const combined with #writes is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |var counter = 0
        |#const
        |#writes(counter)
        |bad(x: int) -> int
        |    counter = counter + x
        |    return counter
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage.toLowerCase should (include("#const") or include("const"))
  }

  "ghost combined with #const is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |#ghost
        |#const
        |bad(x: int) -> int = x + 1
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage.toLowerCase should (include("#ghost") or include("#const"))
  }
}
