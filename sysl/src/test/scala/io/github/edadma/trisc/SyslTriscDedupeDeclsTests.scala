package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Pin for cross-unit decl dedupe in TRISC codegen.
  *
  * Bug: the test runner builds `runOneTRISC`'s TProgram by flattening all
  * reachable units' typed decls (`result.units.flatMap(_.typed.decls)`).
  * When two units in the same module independently instantiated the same
  * generic — e.g. `is_err[i64, Error]` from both std/flag/flag.lsysl and
  * std/flag/flag_test.lsysl — both `TFunDecl` entries reached codegen and
  * we emitted two `global is_err_i64_Error, ...` lines and two
  * `is_err_i64_Error:` labels, which the asm assembler rejects.
  *
  * Fix: `generate()` filters program.decls by name (per the TFunDecl /
  * TVarDecl / TExternFuncDecl / TExternVarDecl axes), keeping the first
  * occurrence. Any duplicate is the same template re-instantiated and
  * therefore byte-identical.
  *
  * Cluster: ~88 std/flag failures with `duplicate symbol: 'is_err_i64_Error'`.
  */
class SyslTriscDedupeDeclsTests extends AnyFreeSpec with Matchers {

  private def asm(decls: List[io.github.edadma.trisc.TDecl]): String =
    (new SyslTriscCodegen).generate(TProgram(decls))

  "duplicate TFunDecl entries are deduped (one label)" in {
    val body = TExprBody(TIntLit(0, SyslType.I64))
    val fun = TFunDecl("ham", Nil, SyslType.I64, body)
    val out = asm(List(fun, fun))
    val labelCount = "(?m)^ham:".r.findAllIn(out).size
    labelCount shouldBe 1
  }

  "duplicate TExternFuncDecl entries are deduped" in {
    val ext = TExternFuncDecl("c_ham", List(SyslType.I64), SyslType.I64)
    val out = asm(List(ext, ext))
    val externCount = "(?m)^extern c_ham\\b".r.findAllIn(out).size
    externCount shouldBe 1
  }
}
