package io.github.edadma.trisc

/** Effect-bearing function and interface types — `#pure`, `#reads(...)`, `#writes(...)`
  * suffixes on `fn(...) -> T` and on interface methods. Lifts the v1 restriction that
  * `#pure`/`#reads`/`#writes` functions could not make indirect calls or interface
  * dispatches: a callee whose effect signature is satisfied by the caller's now compiles.
  *
  * Phases:
  *   P1 — `#pure` on FuncType
  *   P2 — `#reads`/`#writes` on FuncType (subset check)
  *   P3 — interface method effect signatures (boxing + dispatch)
  *   P4 — SMETA round-trip (covered indirectly by the cross-module-friendly tests below) */
class SyslFuncEffectsTests extends SyslTestHelpers {

  // ===== P1: #pure on FuncType =====

  "pure callback can be passed and indirectly invoked from #pure context" in {
    eval("""
      |#pure
      |add(a: int, b: int) -> int = a + b
      |
      |#pure
      |apply(f: (int, int) -> int #pure, x: int, y: int) -> int = f(x, y)
      |
      |main() -> int = apply(&add, 20, 22)
      |""".stripMargin) shouldBe 42
  }

  "pure callback satisfies #reads/#writes caller (more restrictive)" in {
    eval("""
      |#pure
      |add(a: int, b: int) -> int = a + b
      |
      |#reads()
      |#writes()
      |use_pure(f: (int, int) -> int #pure, x: int, y: int) -> int = f(x, y)
      |
      |main() -> int = use_pure(&add, 1, 41)
      |""".stripMargin) shouldBe 42
  }

  "non-pure callee cannot be assigned to a #pure callback slot" in {
    val t = intercept[Exception] {
      eval("""
        |adder(a: int, b: int) -> int = a + b
        |
        |#pure
        |use_pure(f: (int, int) -> int #pure, x: int, y: int) -> int = f(x, y)
        |
        |main() -> int = use_pure(&adder, 1, 2)
        |""".stripMargin)
    }
    t.getMessage should not be ""
  }

  "indirect call to non-pure callee is rejected from #pure context" in {
    val t = intercept[Exception] {
      eval("""
        |adder(a: int, b: int) -> int = a + b
        |
        |#pure
        |bad() -> int
        |    var f: (int, int) -> int = &adder
        |    return f(1, 2)
        |
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("indirect")
  }

  // ===== P2: #reads/#writes on FuncType =====

  "indirect call to #reads/#writes callback satisfies subset check" in {
    eval("""
      |var counter = 0
      |
      |#writes(counter)
      |bump()
      |    counter = counter + 1
      |
      |#writes(counter)
      |run_n(f: () -> unit #writes(counter), n: int)
      |    for i in 0..<n do f()
      |
      |main() -> int
      |    run_n(&bump, 5)
      |    return counter
      |""".stripMargin) shouldBe 5
  }

  "callee with effects exceeding caller's set is rejected at call site" in {
    val t = intercept[Exception] {
      eval("""
        |var x = 0
        |var y = 0
        |
        |#writes(x, y)
        |bump_both() =
        |    x = x + 1
        |    y = y + 1
        |
        |#writes(x)
        |bad(f: () -> unit #writes(x, y))
        |    f()
        |
        |main() -> int
        |    bad(&bump_both)
        |    return x + y
        |""".stripMargin)
    }
    t.getMessage should not be ""
  }

  // ===== P3: Interface method effect signatures =====

  "interface with pure method accepts impl whose method is pure" in {
    eval("""
      |interface Compare
      |    less(a: int, b: int) -> bool #pure
      |
      |struct Asc
      |    dummy: int
      |
      |#pure
      |Asc.less(a: int, b: int) -> bool = a < b
      |
      |#pure
      |use_iface(c: Compare, a: int, b: int) -> int
      |    if c.less(a, b) then return a
      |    return b
      |
      |main() -> int
      |    var asc = Asc(0)
      |    return use_iface(asc, 7, 11)
      |""".stripMargin) shouldBe 7
  }

  "interface boxing rejects impl whose method has wider effects than declared" in {
    val t = intercept[Exception] {
      eval("""
        |var hits = 0
        |
        |interface Look
        |    peek(x: int) -> int #pure
        |
        |struct Counter
        |    dummy: int
        |
        |#writes(hits)
        |Counter.peek(x: int) -> int
        |    hits = hits + 1
        |    return x
        |
        |main() -> int
        |    var c = Counter(0)
        |    var l: Look = c
        |    return l.peek(42)
        |""".stripMargin)
    }
    t.getMessage should not be ""
  }

  "interface dispatch in annotated context honors method's declared effects" in {
    eval("""
      |var seen = 0
      |
      |interface Sink
      |    push(x: int) #writes(seen)
      |
      |struct Adder
      |    dummy: int
      |
      |#writes(seen)
      |Adder.push(x: int)
      |    seen = seen + x
      |
      |#writes(seen)
      |feed(s: Sink, vals: []int)
      |    for i in 0..<len(vals) do s.push(vals[i])
      |
      |main() -> int
      |    var a = Adder(0)
      |    var data: [3]int
      |    data[0] = 10
      |    data[1] = 11
      |    data[2] = 12
      |    feed(a, data[:])
      |    return seen
      |""".stripMargin) shouldBe 33
  }

  // ===== P1/P2 misc validation =====

  "function-type effects: #pure cannot combine with #reads/#writes" in {
    val t = intercept[Exception] {
      eval("""
        |main() -> int
        |    var f: (int) -> int #pure #reads(x) = (x: int) => x
        |    return 0
        |""".stripMargin)
    }
    t.getMessage should not be ""
  }

  // ===== P4: SMETA round-trip via cross-unit imports =====

  // ===== Closure effect inference =====

  "pure lambda passed to a #pure callback slot is accepted" in {
    // The closure `(a, b) -> a + b` has no side effects, no allocation, no impure calls —
    // the analyzer probes it with `validatePureFn` and marks the synthesized FuncType as
    // `#pure`, so it satisfies the slot.
    eval("""
      |#pure
      |apply(f: (int, int) -> int #pure, x: int, y: int) -> int = f(x, y)
      |
      |main() -> int = apply((a: int, b: int) -> a + b, 20, 22)
      |""".stripMargin) shouldBe 42
  }

  "closure reading a captured local is still pure" in {
    // Reading captures is permitted in pure probes — closure only reads, doesn't mutate.
    eval("""
      |#pure
      |apply(f: (int) -> int #pure, x: int) -> int = f(x)
      |
      |main() -> int
      |    val offset = 10
      |    return apply((v: int) -> v + offset, 32)
      |""".stripMargin) shouldBe 42
  }

  "closure calling an impure function is not pure" in {
    val t = intercept[Exception] {
      eval("""
        |var counter = 0
        |
        |bump() -> int
        |    counter = counter + 1
        |    return counter
        |
        |#pure
        |apply(f: (int) -> int #pure, x: int) -> int = f(x)
        |
        |main() -> int = apply((v: int) -> v + bump(), 10)
        |""".stripMargin)
    }
    t.getMessage should not be ""
  }

  // ===== Closure #reads / #writes inference =====

  "closure reading a module-level var infers #reads" in {
    // Closure reads `threshold` (a module-level var) — inferred as #reads(threshold),
    // which satisfies the slot's declared #reads(threshold).
    eval("""
      |var threshold = 5
      |
      |#reads(threshold)
      |find(f: (int) -> bool #reads(threshold), arr: []int) -> int
      |    for i in 0..<len(arr) do
      |        if f(arr[i]) then return arr[i]
      |    return -1
      |
      |main() -> int
      |    var data: [3]int
      |    data[0] = 3
      |    data[1] = 7
      |    data[2] = 1
      |    return find((v: int) -> v > threshold, data[:])
      |""".stripMargin) shouldBe 7
  }

  "closure writing a module-level var infers #writes" in {
    // Single-expression closure body `() -> counter = counter + 1` isn't valid syntax
    // because `=` isn't an expression — so the writing happens via a compound-assign
    // expression `counter += 1` which IS expression-position as a post/pre-inc. But
    // cleanest: wrap in a helper with block body passed by &ref.
    eval("""
      |var counter = 0
      |
      |#writes(counter)
      |tick()
      |    counter = counter + 1
      |
      |#writes(counter)
      |run_through_closure(f: () -> unit #writes(counter), n: int)
      |    for i in 0..<n do f()
      |
      |main() -> int
      |    run_through_closure(() -> tick(), 4)
      |    return counter
      |""".stripMargin) shouldBe 4
  }

  "closure whose effects exceed the slot is rejected" in {
    // Closure calls `touch_both` which writes `{counter, extra}`, exceeding the slot's
    // declared #writes(counter). Effect inference absorbs the callee's sets into the
    // closure, then `effectsSatisfy` flags the excess.
    val t = intercept[Exception] {
      eval("""
        |var counter = 0
        |var extra = 0
        |
        |#writes(counter, extra)
        |touch_both()
        |    counter = counter + 1
        |    extra = extra + 1
        |
        |#writes(counter)
        |run_one(f: () -> unit #writes(counter))
        |    f()
        |
        |main() -> int
        |    run_one(() -> touch_both())
        |    return counter
        |""".stripMargin)
    }
    t.getMessage should not be ""
  }

  "imported #pure function reference satisfies pure callback slot in another unit" in {
    val libs = Map(
      "lib" -> """
        |module lib
        |
        |#pure
        |adder(a: int, b: int) -> int = a + b
        |""".stripMargin
    )
    val main = """
      |module test
      |import lib.adder
      |
      |#pure
      |apply2(f: (int, int) -> int #pure, x: int, y: int) -> int = f(x, y)
      |
      |main() -> int = apply2(&adder, 100, 23)
      |""".stripMargin
    evalWithLibs(libs, main) shouldBe 123
  }
}
