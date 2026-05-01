package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Pin for tuple-destructure with composite-typed fields on TRISC.
  *
  * Bug: TDestructureStmt and TDestructureAssignStmt unconditionally
  * called emitLoad(1, 1, fieldType) for each tuple field, which threw
  * `emitLoad: unexpected type [composite]` for slices, strings, structs,
  * enums, etc. Aggregates use address-as-value (matching TVarRef line
  * 2657-2659), so the field address must be left in r1 for the subsequent
  * emitStore → emitAggregateCopy.
  *
  * This was the largest single bug surfaced by audit item #34: ~190 of the
  * 651 std/-on-TRISC failures (regex × 148, encoding × 45, csv × 8).
  */
class SyslTriscDestructureCompositeTests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  "destructure tuple with slice field codegens (no emitLoad on []T)" in {
    val out = asm(
      """build() -> ([]int, int)
        |    val xs = new [4]int
        |    xs[0] = 7
        |    return (xs[:1], 9)
        |
        |use() -> int
        |    n, x = build()
        |    return x
        |""".stripMargin)
    out should include("# function: use")
    // Must reach the return — i.e., codegen ran past destructuring.
    out should include("# function: build")
  }

  "destructure declaration with string field codegens" in {
    val out = asm(
      """build() -> (string, int)
        |    return ("hello", 1)
        |
        |use() -> int
        |    val (s, n) = build()
        |    return n
        |""".stripMargin)
    out should include("# function: use")
  }

  "destructure with struct field codegens" in {
    val out = asm(
      """struct Point
        |    x: int
        |    y: int
        |
        |build() -> (Point, int)
        |    return (Point(3, 4), 9)
        |
        |use() -> int
        |    p, n = build()
        |    return n
        |""".stripMargin)
    out should include("# function: use")
  }

  "destructure assignment with slice field codegens" in {
    val out = asm(
      """build() -> ([]int, int)
        |    val xs = new [4]int
        |    return (xs[:0], 5)
        |
        |use() -> int
        |    var n: []int = (new [1]int)[:0]
        |    var x: int = 0
        |    n, x = build()
        |    return x
        |""".stripMargin)
    out should include("# function: use")
  }

  "scalar-only destructure still goes through emitLoad (regression guard)" in {
    val out = asm(
      """build() -> (int, int)
        |    return (3, 4)
        |
        |use() -> int
        |    a, b = build()
        |    return a + b
        |""".stripMargin)
    out should include("# function: use")
    // For an int field at offset 0, an ldw load is the natural emission.
    // (We don't pin the exact mnemonic — the point is the function compiled.)
  }

  // Same composite-binding bug in match-arm patterns (TVariantPattern,
  // TDestructurePattern). Bug surfaced from std/encoding/csv when unwrapping
  // Result[[]string, Error] — the variant binding was a slice.
  "match arm binding a slice variant field codegens" in {
    val out = asm(
      """enum Result_strs
        |    Ok(value: []string)
        |    Err
        |
        |make() -> Result_strs
        |    val a = new [2]string
        |    a[0] = "x"; a[1] = "y"
        |    Ok(a[:])
        |
        |use() -> int
        |    val r = make()
        |    r match
        |        Ok(v) -> return len(v)
        |        Err -> return -1
        |""".stripMargin)
    out should include("# function: use")
  }

  "match arm binding a struct variant field codegens" in {
    val out = asm(
      """struct Pair
        |    a: int
        |    b: int
        |
        |enum Box
        |    Some(p: Pair)
        |    None
        |
        |make() -> Box = Some(Pair(1, 2))
        |
        |use() -> int
        |    val b = make()
        |    b match
        |        Some(p) -> return p.a + p.b
        |        None -> return 0
        |""".stripMargin)
    out should include("# function: use")
  }

}
