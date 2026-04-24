package io.github.edadma.trisc

/** Ada-style `in` / `out` / `inout` parameter modes. The body sees every Out/Inout param
 *  as a plain T (no explicit `*`), while the compiler passes a hidden pointer and stores
 *  the final value back through it on exit. Call sites require an lvalue for Out/Inout and
 *  auto-wrap it (no explicit `&`). `in` is the default and preserves current pass-by-value. */
class SyslParamModeTests extends SyslTestHelpers {

  // ===== Out =====

  "out: caller sees new value after call" in {
    eval("""
      |set_to(out x: int)
      |    x = 42
      |main() -> int
      |    var v: int = 0
      |    set_to(v)
      |    return v
      |""".stripMargin) shouldBe 42
  }

  "out: body may write multiple times; last write wins" in {
    eval("""
      |pick(out x: int, flag: bool)
      |    x = 1
      |    if flag then
      |        x = 2
      |    else
      |        x = 3
      |main() -> int
      |    var a: int = 0
      |    var b: int = 0
      |    pick(a, true)
      |    pick(b, false)
      |    return a * 10 + b
      |""".stripMargin) shouldBe 23
  }

  "out: multiple out params" in {
    eval("""
      |split(x: int, out q: int, out r: int)
      |    q = x / 10
      |    r = x % 10
      |main() -> int
      |    var q: int = 0
      |    var r: int = 0
      |    split(73, q, r)
      |    return q * 100 + r
      |""".stripMargin) shouldBe 703
  }

  // ===== Inout =====

  "inout: body reads initial value and writes back" in {
    eval("""
      |inc_by(inout x: int, by: int)
      |    x = x + by
      |main() -> int
      |    var v: int = 10
      |    inc_by(v, 5)
      |    inc_by(v, 7)
      |    return v
      |""".stripMargin) shouldBe 22
  }

  "inout: compound assignment works" in {
    eval("""
      |twice(inout x: int)
      |    x *= 2
      |main() -> int
      |    var v: int = 21
      |    twice(v)
      |    return v
      |""".stripMargin) shouldBe 42
  }

  "inout: swap via two inout params" in {
    eval("""
      |swap(inout a: int, inout b: int)
      |    val t: int = a
      |    a = b
      |    b = t
      |main() -> int
      |    var x: int = 7
      |    var y: int = 3
      |    swap(x, y)
      |    return x * 10 + y
      |""".stripMargin) shouldBe 37
  }

  // ===== in (explicit, default) =====

  "in: explicit 'in' prefix parses and behaves like default" in {
    eval("""
      |add(in a: int, in b: int) -> int = a + b
      |main() -> int
      |    return add(2, 3)
      |""".stripMargin) shouldBe 5
  }

  // ===== Struct field / index as out/inout arg =====

  "inout: struct field as argument" in {
    eval("""
      |struct Point
      |    x: int
      |    y: int
      |
      |inc(inout n: int)
      |    n = n + 1
      |main() -> int
      |    var p: Point = Point(10, 20)
      |    inc(p.x)
      |    inc(p.y)
      |    return p.x + p.y
      |""".stripMargin) shouldBe 32
  }

  "inout: array element as argument" in {
    eval("""
      |bump(inout n: int)
      |    n += 100
      |main() -> int
      |    var arr: [3]int = [1, 2, 3]
      |    bump(arr[1])
      |    return arr[0] + arr[1] + arr[2]
      |""".stripMargin) shouldBe 106
  }

  // ===== Errors =====

  "error: out with literal argument" in {
    val t = intercept[Exception] {
      eval("""
        |set(out x: int)
        |    x = 1
        |main() -> int
        |    set(42)
        |    return 0
        |""".stripMargin)
    }
    t.getMessage should include("lvalue")
  }

  "error: out with arithmetic expression argument" in {
    val t = intercept[Exception] {
      eval("""
        |set(out x: int)
        |    x = 1
        |main() -> int
        |    var v: int = 5
        |    set(v + 1)
        |    return 0
        |""".stripMargin)
    }
    t.getMessage should include("lvalue")
  }

  "error: out with default value" in {
    val t = intercept[Exception] {
      eval("""
        |f(out x: int = 0)
        |    x = 1
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("default")
  }

  "error: inout with default value" in {
    val t = intercept[Exception] {
      eval("""
        |f(inout x: int = 0)
        |    x = 1
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("default")
  }

  "contextual: 'out' as identifier still works outside param position" in {
    eval("""
      |main() -> int
      |    var out: int = 41
      |    out = out + 1
      |    return out
      |""".stripMargin) shouldBe 42
  }

  "contextual: 'inout' as identifier still works outside param position" in {
    eval("""
      |main() -> int
      |    var inout: int = 99
      |    return inout
      |""".stripMargin) shouldBe 99
  }

  // ===== SMETA round-trip =====

  "SMETA: param modes survive serialize/deserialize" in {
    val Right(ast) = (new SyslParser).parseProgram("""
      |module m
      |set_to(out x: int, v: int)
      |    x = v
      |inc_by(inout x: int, by: int)
      |    x = x + by
      |add(a: int, b: int) -> int = a + b
      |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val metaOut = ModuleMeta.fromProgram(typed)
    val smetaText = metaOut.toSmeta
    smetaText should include("MODES OI")   // set_to — out, in
    smetaText should include("MODES UI")   // inc_by — inout, in
    smetaText should not include "MODES II" // add — all-In never emits modes

    val Some(metaIn) = ModuleMeta.fromSmeta(smetaText): @unchecked
    metaIn.symbols.find(_.name.endsWith("set_to")).get.typ match
      case SymbolMeta.Kind.Func(_, _, _, _, modes, _) =>
        modes shouldBe List(ParamMode.Out, ParamMode.In)
      case other => fail(s"expected Func, got $other")
    metaIn.symbols.find(_.name.endsWith("inc_by")).get.typ match
      case SymbolMeta.Kind.Func(_, _, _, _, modes, _) =>
        modes shouldBe List(ParamMode.Inout, ParamMode.In)
      case other => fail(s"expected Func, got $other")
    metaIn.symbols.find(_.name.endsWith("add")).get.typ match
      case SymbolMeta.Kind.Func(_, _, _, _, modes, _) =>
        modes shouldBe Nil // all-In omitted
      case other => fail(s"expected Func, got $other")
  }

  // ===== Cross-module usage =====

  "cross-module: imported function with out param works" in {
    val lib = """module util
                |set_to(out x: int, v: int)
                |    x = v
                |""".stripMargin
    val main = """import util.*
                 |main() -> int
                 |    var v: int = 0
                 |    set_to(v, 42)
                 |    return v
                 |""".stripMargin
    runWithLibs(Map("util" -> lib), main)._1 shouldBe 42
  }
}
