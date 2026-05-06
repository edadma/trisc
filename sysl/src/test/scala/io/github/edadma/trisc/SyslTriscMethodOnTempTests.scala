package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Regression for method-on-temporary receivers where the inner expression
  * returns a struct/enum by value.
  *
  * Bug: `of_seconds(0i64).is_zero()` builds a `TTempAddr(of_seconds(...))`
  * selfArg around a struct-returning call. The OUTER call's regArg path
  * `genExpr`'d the inner call, which left r1 pointing into a stack-resident
  * ret slot at retSlotOffset (with stackOffset descended below preOffset),
  * then either reclaimed `extra = preOffset - stackOffset` (dangling r1
  * into discarded memory — `is_zero`'s `pshd r5` overwrote the receiver's
  * tail) or skipped the reclaim (which broke the ABI: the callee's first
  * stack arg is at `[r5_callee+24]` = caller's r7 at jalr, so leaving r7
  * inside the ret-slot region makes the callee misread its stack args
  * out of the receiver's bytes).
  *
  * Fix: extend the existing fat-regArg pre-eval (`StructType`/`EnumType`)
  * to also fire for `TTempAddr(call)` where the call returns a struct or
  * enum. Copy the inner call's bytes into a fresh slot below it, then
  * use that copy's address as r1. The OUTER's final cleanup at
  * `savedOffset` reclaims both the copy and the now-dead inner ret slot
  * in one go.
  *
  * `isStructLikeTempAddr` filters by inner shape (Call/IndirectCall/
  * InterfaceDispatch only) so `TTempAddr(TDeref(...))` from `inout` param
  * auto-deref still uses the existing pointer path — it has no
  * stack-resident temp to copy.
  *
  * Runtime verification lives in `sysl/tests/dur_min/repro.lsysl`; this
  * suite covers the codegen smoke shape.
  */
class SyslTriscMethodOnTempTests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  "method on struct-returning temporary codegens (no fault, has the call)" in {
    val out = asm(
      """struct Duration
        |    seconds: i64
        |    nano: i32
        |
        |of_seconds(s: i64) -> Duration
        |    var d: Duration
        |    d.seconds = s
        |    d.nano = 0
        |    return d
        |
        |Duration.is_zero() -> bool
        |    return self.seconds == 0i64 && self.nano == 0
        |
        |use() -> bool = of_seconds(0i64).is_zero()
        |""".stripMargin)
    out should include("# function: use")
    out should include("Duration_is_zero")
    out should include("of_seconds")
  }

  "method on inout param codegens (no fault, has the call)" in {
    val out = asm(
      """struct Builder
        |    n: int
        |
        |Builder.bump()
        |    self.n = self.n + 1
        |
        |use(inout b: Builder)
        |    b.bump()
        |""".stripMargin)
    out should include("# function: use")
    out should include("Builder_bump")
  }

  "method on local struct codegens (no fault, has the call)" in {
    val out = asm(
      """struct Duration
        |    seconds: i64
        |    nano: i32
        |
        |of_seconds(s: i64) -> Duration
        |    var d: Duration
        |    d.seconds = s
        |    d.nano = 0
        |    return d
        |
        |Duration.is_zero() -> bool
        |    return self.seconds == 0i64 && self.nano == 0
        |
        |use() -> bool
        |    var d = of_seconds(0i64)
        |    return d.is_zero()
        |""".stripMargin)
    out should include("# function: use")
    out should include("Duration_is_zero")
  }

  "indexing a slice returned from a method codegens (no fault)" in {
    // Bug: TIndex(slice_returning_call, idx) pshd'd idx, then genExpr(array)
    // left a 24-byte slice ret slot between the index slot and r7. popd r2
    // read from inside the ret slot — never the pushed index. Fix: reclaim
    // any extra stack genExpr left, then popd. Plus: track stackOffset
    // around pshd/popd so the inner call's retSlotOffset is correct.
    val out = asm(
      """struct Holder
        |    items: []string
        |    count: int
        |
        |Holder.args() -> []string = self.items
        |
        |use(h: Holder) -> string = h.args()[1]
        |""".stripMargin)
    out should include("# function: use")
    out should include("Holder_args")
  }
}
