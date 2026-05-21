package io.github.edadma.trisc

class SyslRealtimeAttrTests extends SyslTestHelpers {

  // ===== Positive cases =====

  "realtime function with arithmetic runs identically" in {
    eval("""
      |#realtime
      |square(x: int) -> int = x * x
      |main() -> int = square(7)
      |""".stripMargin) shouldBe 49
  }

  "realtime function may call other realtime functions" in {
    eval("""
      |#realtime
      |twice(x: int) -> int = x + x
      |#realtime
      |quad(x: int) -> int = twice(twice(x))
      |main() -> int = quad(5)
      |""".stripMargin) shouldBe 20
  }

  "realtime function may recurse" in {
    eval("""
      |#realtime
      |sumTo(n: int) -> int
      |    if n <= 0 then return 0
      |    return n + sumTo(n - 1)
      |main() -> int = sumTo(10)
      |""".stripMargin) shouldBe 55
  }

  "realtime function may loop and mutate locals" in {
    eval("""
      |#realtime
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

  "realtime function may combine #realtime with #pure" in {
    eval("""
      |#pure
      |#realtime
      |cube(x: int) -> int = x * x * x
      |main() -> int = cube(3)
      |""".stripMargin) shouldBe 27
  }

  "realtime function may write through a pointer parameter" in {
    eval("""
      |#realtime
      |writeOne(p: *int) -> unit
      |    *p = 99
      |main() -> int
      |    var x = 0
      |    writeOne(&x)
      |    return x
      |""".stripMargin) shouldBe 99
  }

  "realtime function may write through a struct field" in {
    eval("""
      |struct Point
      |    x: int
      |    y: int
      |#realtime
      |bumpX(p: *Point) -> unit
      |    p.x = p.x + 1
      |main() -> int
      |    var pt: Point
      |    pt.x = 41
      |    pt.y = 0
      |    bumpX(&pt)
      |    return pt.x
      |""".stripMargin) shouldBe 42
  }

  "realtime function may write to module-level state" in {
    eval("""
      |var counter = 0
      |#realtime
      |tick() -> unit
      |    counter = counter + 1
      |main() -> int
      |    tick()
      |    tick()
      |    tick()
      |    return counter
      |""".stripMargin) shouldBe 3
  }

  "realtime function may contain asm" in {
    eval("""
      |#realtime
      |withAsm(x: int) -> int
      |    asm("nop")
      |    return x + 1
      |main() -> int = withAsm(41)
      |""".stripMargin) shouldBe 42
  }

  "realtime function may call assert" in {
    eval("""
      |#realtime
      |safeDiv(a: int, b: int) -> int
      |    assert(b != 0, "divide by zero")
      |    return a / b
      |main() -> int = safeDiv(84, 2)
      |""".stripMargin) shouldBe 42
  }

  // ===== Negative cases =====

  "realtime function rejects calling a non-realtime function" in {
    val t = intercept[Exception] {
      eval("""
        |slow(x: int) -> int = x + 1
        |#realtime
        |bad(x: int) -> int = slow(x)
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("#realtime")
    t.getMessage should include("slow")
  }

  "realtime function rejects calling an IO builtin" in {
    val t = intercept[Exception] {
      eval("""
        |#realtime
        |bad(x: int) -> int
        |    puts("side effect")
        |    return x
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("puts")
  }

  "realtime function rejects calling malloc" in {
    val t = intercept[Exception] {
      eval("""
        |#realtime
        |bad(n: int) -> int
        |    var p = malloc(100)
        |    return n
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("malloc")
  }

  "realtime function rejects new heap allocation" in {
    val t = intercept[Exception] {
      eval("""
        |struct Point
        |    x: int
        |    y: int
        |#realtime
        |bad() -> int
        |    var p = new Point(1, 2)
        |    return p.x
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("allocate")
  }

  "realtime function rejects new array allocation" in {
    val t = intercept[Exception] {
      eval("""
        |#realtime
        |bad() -> int
        |    var a = new [10]int
        |    return a[0]
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("allocate")
  }

  "realtime function rejects formatted string interpolation" in {
    val t = intercept[Exception] {
      eval("""
        |#realtime
        |bad(x: int) -> int
        |    var s = s"got ${x}"
        |    return x
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("string")
  }

  "realtime function rejects str(...) conversion" in {
    val t = intercept[Exception] {
      eval("""
        |#realtime
        |bad(x: int) -> int
        |    var s = str(x)
        |    return x
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("str")
  }

  "realtime function rejects closures" in {
    val t = intercept[Exception] {
      eval("""
        |#realtime
        |bad(x: int) -> int
        |    var f = (y: int) -> y + x
        |    return f(1)
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("closure")
  }

  "realtime function rejects indirect call to a non-realtime function type" in {
    val t = intercept[Exception] {
      eval("""
        |add(a: int, b: int) -> int = a + b
        |#realtime
        |bad(x: int) -> int
        |    var f = &add
        |    return f(x, x)
        |main() -> int = bad(3)
        |""".stripMargin)
    }
    t.getMessage should include("indirect")
  }

  "realtime function accepts indirect call to a #realtime function type" in {
    eval("""
      |#realtime
      |add(a: int, b: int) -> int = a + b
      |#realtime
      |apply(f: (int, int)->int #realtime, x: int) -> int = f(x, x)
      |main() -> int = apply(add, 21)
      |""".stripMargin) shouldBe 42
  }

  "realtime function rejects unannotated function in same file" in {
    val t = intercept[Exception] {
      eval("""
        |plain(x: int) -> int = x + 1
        |#realtime
        |bad(x: int) -> int = plain(x)
        |main() -> int = bad(1)
        |""".stripMargin)
    }
    t.getMessage should include("#realtime")
    t.getMessage should include("plain")
  }

  "non-realtime function rejected when passed to a #realtime function-pointer slot" in {
    val t = intercept[Exception] {
      eval("""
        |add(a: int, b: int) -> int = a + b
        |apply(f: (int, int)->int #realtime, x: int) -> int = f(x, x)
        |main() -> int = apply(add, 21)
        |""".stripMargin)
    }
    t.getMessage.toLowerCase should (include("effect") or include("realtime"))
  }

  "non-realtime interface impl rejected when satisfying a #realtime method slot" in {
    val t = intercept[Exception] {
      eval("""
        |interface Op
        |    run(x: int) -> int #realtime
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
    t.getMessage.toLowerCase should (include("realtime") or include("interface") or include("effect") or include("expects op"))
  }
}
