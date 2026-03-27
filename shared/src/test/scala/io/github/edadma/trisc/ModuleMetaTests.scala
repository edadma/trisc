package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class ModuleMetaTests extends AnyFreeSpec with Matchers {

  private def analyze(source: String): TProgram =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    (new SyslAnalyzer).analyze(ast)

  // ===== fromProgram =====

  "extracts function symbols" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """add(a: int, b: int) -> int = a + b
        |main() -> int = 0
        |""".stripMargin))
    meta.symbols.length shouldBe 2
    val add = meta.symbols(0)
    add.name shouldBe "add"
    add.typ shouldBe SymbolMeta.Kind.Func(List(I64, I64), I64)
    add.isPrivate shouldBe false
  }

  "extracts variable symbols" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """x = 42
        |main() -> int = x
        |""".stripMargin))
    val x = meta.symbols.find(_.name == "x").get
    x.typ shouldBe SymbolMeta.Kind.Data(I64)
  }

  "marks private symbols" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """private helper(x: int) -> int = x
        |main() -> int = helper(1)
        |""".stripMargin))
    meta.symbols.find(_.name == "helper").get.isPrivate shouldBe true
    meta.symbols.find(_.name == "main").get.isPrivate shouldBe false
  }

  "publicSymbols filters private" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """private secret = 42
        |main() -> int = secret
        |""".stripMargin))
    meta.publicSymbols.map(_.name) should not contain "secret"
    meta.publicSymbols.map(_.name) should contain("main")
  }

  // ===== toSmeta / fromSmeta round-trip =====

  "round-trips simple function" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """add(a: int, b: int) -> int = a + b
        |main() -> int = 0
        |""".stripMargin))
    val text = meta.toSmeta
    text should include("SMETA v1")
    text should include("FUNC add 2 i64 i64 i64")
    text should include("FUNC main 0 i64")
    val meta2 = ModuleMeta.fromSmeta(text)
    meta2.symbols.length shouldBe 2
    meta2.symbols(0).name shouldBe "add"
    meta2.symbols(0).typ shouldBe SymbolMeta.Kind.Func(List(I64, I64), I64)
  }

  "round-trips private symbols" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """private helper(x: int) -> int = x
        |main() -> int = helper(1)
        |""".stripMargin))
    val text = meta.toSmeta
    text should include("PRIVATE FUNC helper")
    val meta2 = ModuleMeta.fromSmeta(text)
    meta2.symbols.find(_.name == "helper").get.isPrivate shouldBe true
    meta2.symbols.find(_.name == "main").get.isPrivate shouldBe false
  }

  "round-trips data symbols" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """x = 42
        |main() -> int = x
        |""".stripMargin))
    val text = meta.toSmeta
    text should include("DATA x i64")
    val meta2 = ModuleMeta.fromSmeta(text)
    meta2.symbols.find(_.name == "x").get.typ shouldBe SymbolMeta.Kind.Data(I64)
  }

  "round-trips pointer types" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """swap(a: *int, b: *int)
        |    tmp = *a
        |    *a = *b
        |    *b = tmp
        |main() -> int = 0
        |""".stripMargin))
    val text = meta.toSmeta
    text should include("FUNC swap 2 ptr i64 ptr i64 void")
    val meta2 = ModuleMeta.fromSmeta(text)
    val swap = meta2.symbols.find(_.name == "swap").get
    swap.typ shouldBe SymbolMeta.Kind.Func(List(PtrType(I64), PtrType(I64)), VoidType)
  }

  "round-trips array types" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """buf: [10]int
        |main() -> int = 0
        |""".stripMargin))
    val text = meta.toSmeta
    text should include("DATA buf arr 10 i64")
    val meta2 = ModuleMeta.fromSmeta(text)
    meta2.symbols.find(_.name == "buf").get.typ shouldBe SymbolMeta.Kind.Data(ArrayType(I64, 10))
  }

  "round-trips no-param void function" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """doNothing()
        |    x = 1
        |main() -> int = 0
        |""".stripMargin))
    val text = meta.toSmeta
    text should include("FUNC doNothing 0 void")
    val meta2 = ModuleMeta.fromSmeta(text)
    meta2.symbols.find(_.name == "doNothing").get.typ shouldBe SymbolMeta.Kind.Func(Nil, VoidType)
  }

  // ===== toAsmGlobals =====

  "toAsmGlobals emits only public symbols" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """private helper(x: int) -> int = x
        |add(a: int, b: int) -> int = a + b
        |main() -> int = 0
        |""".stripMargin))
    val asm = meta.toAsmGlobals
    asm should include("global add, func, 2 i64 i64 i64")
    asm should include("global main, func, 0 i64")
    asm should not include "helper"
  }

  "toAsmGlobals emits data types" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """x = 42
        |main() -> int = x
        |""".stripMargin))
    val asm = meta.toAsmGlobals
    asm should include("global x, data, i64")
  }

  // ===== Error handling =====

  "fromSmeta rejects missing header" in {
    an[IllegalArgumentException] should be thrownBy ModuleMeta.fromSmeta("FUNC add 0 i64\n")
  }

  "fromSmeta rejects empty input" in {
    an[IllegalArgumentException] should be thrownBy ModuleMeta.fromSmeta("")
  }

  "fromSmeta rejects unknown kind" in {
    an[IllegalArgumentException] should be thrownBy ModuleMeta.fromSmeta("SMETA v1\nSTRUCT foo\n")
  }
}
