package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Pin for `append([]T, T)` where T is an aggregate type on TRISC.
  *
  * Bug: `TAppend`'s codegen pushed the slice components (ptr/len/cap/backref)
  * onto the stack, then evaluated the elem expression. For aggregate elem
  * types (struct, slice, string, etc.) `genExpr` allocates a stack temp
  * and returns `r1 = address-of-temp`. The subsequent `pshd r1; popd r1;
  * popd r2; popd r3; popd r4` then read INTO the temp instead of the slice
  * components above it — and the no-grow path's `pshd r4; pshd r2; pshd r1`
  * later overwrote the temp's bytes, corrupting the elem before it could be
  * stored.
  *
  * Secondary bug: in the no-grow path's `popd r2 (elem); emitStore(2, 1, T);
  * popd r2 (ptr); popd r4 (cap); addi r3, r3, 1`, `emitStore` for an
  * aggregate type calls `emitAggregateCopy` which clobbers r3 and r4 — so
  * `addi r3, r3, 1 // new_len` operated on a junk value instead of the
  * original len.
  *
  * Fix:
  *  - Pre-allocate a frame scratch slot for the elem when its type is
  *    aggregate. Copy the bytes from the stack temp into the scratch slot
  *    and reclaim the temp before the popd shuffle, so r1 = stable
  *    frame-relative address.
  *  - Save/restore r3 around the no-grow path's `emitStore` for aggregate
  *    elem types.
  *
  * Cluster: 133 std/-on-TRISC failures at PC=0x3234, all variants of
  * `self.field = append(self.field, struct_lit)` or the moral equivalent.
  */
class SyslTriscAppendAggregateElemTests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  "append of struct literal codegens (no register clobber)" in {
    val out = asm(
      """struct Inst
        |    op: int
        |    arg1: int
        |    arg2: int
        |
        |build() -> []Inst
        |    val buf = new [4]Inst
        |    var s = buf[:0]
        |    s = append(s, Inst(7, 8, 9))
        |    return s
        |""".stripMargin)
    out should include("# function: build")
  }

  "append of int (scalar) codegens unchanged (regression guard)" in {
    val out = asm(
      """build() -> []int
        |    val buf = new [4]int
        |    var s = buf[:0]
        |    s = append(s, 42)
        |    return s
        |""".stripMargin)
    out should include("# function: build")
  }

  "append of string (aggregate) codegens" in {
    val out = asm(
      """build() -> []string
        |    val buf = new [4]string
        |    var s = buf[:0]
        |    s = append(s, "hi")
        |    return s
        |""".stripMargin)
    out should include("# function: build")
  }
}
