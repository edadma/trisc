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

  // ===== Compile-time evaluation =====
  // The const-evaluation driver runs `#const fn` calls at compile time when they
  // sit in `const NAME = ...` or `val NAME = ...` initializer position with
  // scalar-foldable arguments. The result is folded into `compileTimeConstants`
  // and propagated to use sites as a literal — same machinery as scalar `const`
  // foldings of `1 + 2`. These tests pin the scalar surface; float / aggregate
  // results are deferred to later stages.

  "const binding evaluates a const-fn call at compile time" in {
    eval("""
      |#const
      |sq(x: int) -> int = x * x
      |const SQ49: int = sq(7)
      |main() -> int = SQ49
      |""".stripMargin) shouldBe 49
  }

  "const binding evaluates a multi-arg const-fn call" in {
    eval("""
      |#const
      |add3(a: int, b: int, c: int) -> int = a + b + c
      |const SUM: int = add3(10, 20, 30)
      |main() -> int = SUM
      |""".stripMargin) shouldBe 60
  }

  "const binding evaluates nested const-fn calls" in {
    eval("""
      |#const
      |twice(x: int) -> int = x + x
      |#const
      |quad(x: int) -> int = twice(twice(x))
      |const Q5: int = quad(5)
      |main() -> int = Q5
      |""".stripMargin) shouldBe 20
  }

  "const binding evaluates a recursive const fn" in {
    eval("""
      |#const
      |fact(n: int) -> int
      |    if n <= 1 then return 1
      |    return n * fact(n - 1)
      |const FACT5: int = fact(5)
      |main() -> int = FACT5
      |""".stripMargin) shouldBe 120
  }

  "const binding evaluates a const fn that loops and mutates" in {
    eval("""
      |#const
      |sumTo(n: int) -> int
      |    var acc = 0
      |    var i = 1
      |    while i <= n do
      |        acc = acc + i
      |        i = i + 1
      |    return acc
      |const S10: int = sumTo(10)
      |main() -> int = S10
      |""".stripMargin) shouldBe 55
  }

  "val binding also evaluates a const-fn call (constant propagation)" in {
    eval("""
      |#const
      |sq(x: int) -> int = x * x
      |main() -> int
      |    val x = sq(9)
      |    return x
      |""".stripMargin) shouldBe 81
  }

  "const-fn result is referencable from other const-folded sites" in {
    // The biggest user-facing benefit of routing const-fn evaluation through
    // `compileTimeConstants` is that downstream `val`-folding picks up the
    // result transparently — same name-resolution path as a literal `const`.
    eval("""
      |#const
      |dim() -> int = 4 + 4
      |const N: int = dim()
      |main() -> int
      |    val twice = N + N
      |    return twice
      |""".stripMargin) shouldBe 16
  }

  "const binding rejects calling a non-const function" in {
    val t = intercept[Exception] {
      eval("""
        |plain(x: int) -> int = x * x
        |const SQ49: int = plain(7)
        |main() -> int = SQ49
        |""".stripMargin)
    }
    t.getMessage.toLowerCase should include("compile-time evaluable")
  }

  "const binding rejects an unannotated callee declared later" in {
    // Even if a `#const fn` is declared *below* the `const` binding, the
    // driver can only run already-analyzed bodies. Source order matters.
    val t = intercept[Exception] {
      eval("""
        |const SQ49: int = sq(7)
        |#const
        |sq(x: int) -> int = x * x
        |main() -> int = SQ49
        |""".stripMargin)
    }
    t.getMessage.toLowerCase should include("compile-time evaluable")
  }

  "const-fn infinite loop is caught by the step limit" in {
    // Synthesize a guaranteed runaway: a recursion the body cannot escape
    // within the step budget. The driver returns None, surfacing as the
    // standard "not compile-time evaluable" diagnostic.
    val t = intercept[Exception] {
      eval("""
        |#const
        |spin(n: int) -> int
        |    if n == 0 then return 0
        |    return spin(n)
        |const X: int = spin(1)
        |main() -> int = X
        |""".stripMargin)
    }
    t.getMessage.toLowerCase should include("compile-time evaluable")
  }

  // ===== Float compile-time evaluation =====
  // Float-typed `const NAME: f64 = …` bindings route through `tryConstEvalFloat`,
  // which mirrors the integer folder via a parallel `compileTimeFloats` map and
  // a shared call-driver (`evaluateConstCallValue`). The result is folded into
  // an inline `TFloatLit` at every use site.

  "const binding folds a float literal" in {
    output("""
      |const HALF: f64 = 0.5
      |main() -> int
      |    println(HALF)
      |    return 0
      |""".stripMargin) shouldBe "0.5\n"
  }

  "const binding folds float arithmetic" in {
    output("""
      |const QUARTER: f64 = 1.0 / 4.0
      |main() -> int
      |    println(QUARTER)
      |    return 0
      |""".stripMargin) shouldBe "0.25\n"
  }

  "const binding folds a float-returning const-fn call" in {
    output("""
      |#const
      |scale(x: f64) -> f64 = x * 2.0
      |const SIX: f64 = scale(3.0)
      |main() -> int
      |    println(SIX)
      |    return 0
      |""".stripMargin) shouldBe "6\n"
  }

  "const binding folds a const-fn that mixes int and float args" in {
    output("""
      |#const
      |stride(n: int) -> f64 = 1.0 / f64(n)
      |const STEP4: f64 = stride(4)
      |main() -> int
      |    println(STEP4)
      |    return 0
      |""".stripMargin) shouldBe "0.25\n"
  }

  "const binding folds a recursive float const fn" in {
    // Powers of two computed by recursion — keeps result exact in f64.
    output("""
      |#const
      |pow2(n: int) -> f64
      |    if n <= 0 then return 1.0
      |    return 2.0 * pow2(n - 1)
      |const P10: f64 = pow2(10)
      |main() -> int
      |    println(P10)
      |    return 0
      |""".stripMargin) shouldBe "1024\n"
  }

  "float const propagates into downstream const-folded sites" in {
    // Same chained-const shape as the integer test — the second `const`
    // binding's RHS references the first by name; the float folder must
    // resolve `BASE` from `compileTimeFloats`.
    output("""
      |#const
      |dim() -> f64 = 2.5
      |const BASE: f64 = dim()
      |const QUAD: f64 = BASE + BASE + BASE + BASE
      |main() -> int
      |    println(QUAD)
      |    return 0
      |""".stripMargin) shouldBe "10\n"
  }

  "float const folds and casts to int at use site" in {
    // Bridges the float folder back to the int test harness — verifies that
    // a float const can be read inside an integer expression via an explicit
    // cast, the same path application code will take when sizing a runtime
    // sine table from a folded `const TABLE_SIZE: f64`.
    eval("""
      |#const
      |area(r: f64) -> f64 = r * r * 3.0
      |const AREA: f64 = area(2.0)
      |main() -> int = int(AREA)
      |""".stripMargin) shouldBe 12
  }

  "const binding of f32 narrows the folded result to single precision" in {
    // 0.1 is not representable exactly; the f32 narrowing must match the
    // bit pattern a runtime `let x: f32 = 0.1` would observe. We compare
    // against `0.1f.toDouble` to assert the narrowing actually happened.
    val expected = 0.1f.toDouble.toString // e.g. "0.10000000149011612"
    output(s"""
      |const TENTH: f32 = 0.1
      |main() -> int
      |    println(TENTH)
      |    return 0
      |""".stripMargin) shouldBe s"$expected\n"
  }

  // ===== Aggregate compile-time evaluation =====
  // Array and struct consts don't inline at use sites — they materialize as a
  // module-level immutable storage slot whose initializer is composed entirely
  // of literal nodes (`TIntLit`, `TFloatLit`, `TArrayLit`, `TStructConstruct`).
  // Every backend already lowers a literal-of-literals initializer to a static
  // data block, so no per-backend codegen change is needed for the array case;
  // the struct case adds a `TStructConstruct` initializer handler to llvm-host,
  // svm-host, and trisc (each previously zero-filled struct globals at module
  // scope).

  "const binding folds a stack-array literal" in {
    eval("""
      |const SMALL: [3]int = [10, 20, 12]
      |main() -> int = SMALL[0] + SMALL[1] + SMALL[2]
      |""".stripMargin) shouldBe 42
  }

  "const binding folds an aggregate-returning const-fn call" in {
    eval("""
      |#const
      |make3() -> [3]int
      |    var t: [3]int
      |    t[0] = 10
      |    t[1] = 20
      |    t[2] = 12
      |    return t
      |const TABLE: [3]int = make3()
      |main() -> int = TABLE[0] + TABLE[1] + TABLE[2]
      |""".stripMargin) shouldBe 42
  }

  "const binding folds a sized-loop const-fn returning an array" in {
    // The shape `const SINE_TABLE: [N]i32 = build_sine_table(N)` will take —
    // a fixed-size loop fills a stack array, the function returns it, and the
    // binding materializes the folded array as static data.
    eval("""
      |#const
      |squares() -> [8]int
      |    var t: [8]int
      |    var i = 0
      |    while i < 8 do
      |        t[i] = i * i
      |        i = i + 1
      |    return t
      |const SQS: [8]int = squares()
      |main() -> int = SQS[0] + SQS[1] + SQS[2] + SQS[3] + SQS[4] + SQS[5] + SQS[6] + SQS[7]
      |""".stripMargin) shouldBe 140
  }

  "const binding folds a struct-returning const-fn" in {
    eval("""
      |struct Point
      |    x: int
      |    y: int
      |#const
      |make_point(x: int, y: int) -> Point = Point(x, y)
      |const ORIGIN: Point = make_point(3, 4)
      |main() -> int = ORIGIN.x + ORIGIN.y
      |""".stripMargin) shouldBe 7
  }

  "const binding folds a float-array const-fn" in {
    output("""
      |#const
      |make_floats() -> [4]f64
      |    var t: [4]f64
      |    t[0] = 0.5
      |    t[1] = 1.0
      |    t[2] = 1.5
      |    t[3] = 2.0
      |    return t
      |const TABLE: [4]f64 = make_floats()
      |main() -> int
      |    println(TABLE[0])
      |    println(TABLE[3])
      |    return 0
      |""".stripMargin) shouldBe "0.5\n2\n"
  }
}
