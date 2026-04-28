package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for TRISC aggregate-copy alignment.
  *
  * Bug surfaced by the audit item #19 TRISC test runner: copying a struct
  * or enum whose natural alignment is < 8 (e.g. an enum carrying only `int`
  * variants embedded in another enum) was using `ldd`/`std` 8-byte memory
  * ops, which fault on 4-aligned addresses with `MisalignedAccess`.
  *
  * Fix: `emitAggregateCopy` chooses 4-byte vs 8-byte ops based on the
  * type's natural alignment. This pin checks the asm shape; the runtime
  * verification lives in `sysl/tests/enum_misalign/` and the now-passing
  * `std/result/` and `std/option/` corpora on `--backend trisc`.
  */
class SyslTriscEnumCopyAlignTests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  "embedded enum with int-only payload uses 4-byte stores (not std)" in {
    val out = asm(
      """enum Inner
        |    A
        |    B(value: int)
        |
        |enum Outer
        |    First(inner: Inner)
        |    Second
        |
        |build() -> Outer = First(B(42))
        |""".stripMargin)
    // The Inner-into-Outer copy must use stw, not std. Look at the build
    // function specifically; helper functions emitted later may legitimately
    // use std for 8-aligned data.
    val build = out.linesIterator.dropWhile(!_.contains("# function: build"))
      .takeWhile(line => !line.startsWith("# function:") || line.contains("# function: build"))
      .mkString("\n")
    build should include("stw")
    // The copy of Inner should not use std (the parent enum's payload is at
    // offset 4 = 4-aligned).
    build should not include "std r4"
  }

  "embedded enum with i64 payload still uses 8-byte stores (regression guard)" in {
    val out = asm(
      """enum Inner
        |    A
        |    B(value: i64)
        |
        |enum Outer
        |    First(inner: Inner)
        |    Second
        |
        |build() -> Outer = First(B(42))
        |""".stripMargin)
    val build = out.linesIterator.dropWhile(!_.contains("# function: build"))
      .takeWhile(line => !line.startsWith("# function:") || line.contains("# function: build"))
      .mkString("\n")
    // i64 payload bumps Inner's data alignment to 8, which bumps Outer's
    // payload offset to 8, so the copy can safely use std.
    build should include("std")
  }

  "struct with int-only fields uses 4-byte stores when copied" in {
    val out = asm(
      """struct Pair
        |    a: int
        |    b: int
        |
        |enum E
        |    V(p: Pair)
        |    W
        |
        |build() -> E = V(Pair(1, 2))
        |""".stripMargin)
    val build = out.linesIterator.dropWhile(!_.contains("# function: build"))
      .takeWhile(line => !line.startsWith("# function:") || line.contains("# function: build"))
      .mkString("\n")
    // Pair has align 4; embedded in E.V's payload (offset 4 within E),
    // copy must use stw not std.
    build should include("stw")
    build should not include "std r4"
  }

  "struct with pointer field uses 8-byte stores (regression guard)" in {
    val out = asm(
      """struct WithPtr
        |    p: *byte
        |    n: int
        |
        |build() -> WithPtr
        |    var x: byte = 0
        |    return WithPtr(&x, 0)
        |""".stripMargin)
    // align(WithPtr) = 8 (from *byte), so std is correct.
    out should include("std")
  }
}
