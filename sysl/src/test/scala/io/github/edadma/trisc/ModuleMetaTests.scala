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
    add.typ shouldBe SymbolMeta.Kind.Func(List(I32, I32), I32)
    add.isPrivate shouldBe false
  }

  "extracts variable symbols" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """x = 42
        |main() -> int = x
        |""".stripMargin))
    val x = meta.symbols.find(_.name == "x").get
    // `x = 42` at module level is shorthand for `var x = 42` — mutable.
    x.typ shouldBe SymbolMeta.Kind.Data(I32, isMutable = true)
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
    text should include(s"SMETA v${ModuleMeta.SMETA_VERSION}")
    text should include("FUNC add 2 i32 i32 i32")
    text should include("FUNC main 0 i32")
    val meta2 = ModuleMeta.fromSmeta(text).get
    meta2.symbols.length shouldBe 2
    meta2.symbols(0).name shouldBe "add"
    meta2.symbols(0).typ shouldBe SymbolMeta.Kind.Func(List(I32, I32), I32)
  }

  "round-trips private symbols" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """private helper(x: int) -> int = x
        |main() -> int = helper(1)
        |""".stripMargin))
    val text = meta.toSmeta
    text should include("PRIVATE FUNC helper")
    val meta2 = ModuleMeta.fromSmeta(text).get
    meta2.symbols.find(_.name == "helper").get.isPrivate shouldBe true
    meta2.symbols.find(_.name == "main").get.isPrivate shouldBe false
  }

  "round-trips data symbols" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """x = 42
        |main() -> int = x
        |""".stripMargin))
    val text = meta.toSmeta
    text should include("DATA x i32 MUT")
    val meta2 = ModuleMeta.fromSmeta(text).get
    meta2.symbols.find(_.name == "x").get.typ shouldBe SymbolMeta.Kind.Data(I32, isMutable = true)
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
    text should include("FUNC swap 2 ptr i32 ptr i32 unit")
    val meta2 = ModuleMeta.fromSmeta(text).get
    val swap = meta2.symbols.find(_.name == "swap").get
    swap.typ shouldBe SymbolMeta.Kind.Func(List(PtrType(I32), PtrType(I32)), UnitType)
  }

  "round-trips array types" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """buf: [10]int
        |main() -> int = 0
        |""".stripMargin))
    val text = meta.toSmeta
    text should include("DATA buf arr 10 i32 MUT")
    val meta2 = ModuleMeta.fromSmeta(text).get
    meta2.symbols.find(_.name == "buf").get.typ shouldBe SymbolMeta.Kind.Data(ArrayType(I32, 10), isMutable = true)
  }

  "round-trips no-param void function" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """doNothing()
        |    x = 1
        |main() -> int = 0
        |""".stripMargin))
    val text = meta.toSmeta
    text should include("FUNC doNothing 0 unit")
    val meta2 = ModuleMeta.fromSmeta(text).get
    meta2.symbols.find(_.name == "doNothing").get.typ shouldBe SymbolMeta.Kind.Func(Nil, UnitType)
  }

  // ===== toAsmGlobals =====

  "toAsmGlobals emits all symbols including private" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """private helper(x: int) -> int = x
        |add(a: int, b: int) -> int = a + b
        |main() -> int = 0
        |""".stripMargin))
    val asm = meta.toAsmGlobals
    asm should include("global add, func, 2 i32 i32 i32")
    asm should include("global main, func, 0 i32")
    asm should include("global helper, func, 1 i32 i32")
  }

  "toAsmGlobals emits data types" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """x = 42
        |main() -> int = x
        |""".stripMargin))
    val asm = meta.toAsmGlobals
    asm should include("global x, data, i32")
  }

  // ===== Error handling =====

  "fromSmeta returns None for missing header" in {
    ModuleMeta.fromSmeta("FUNC add 0 i32\n") shouldBe None
  }

  "fromSmeta returns None for empty input" in {
    ModuleMeta.fromSmeta("") shouldBe None
  }

  "fromSmeta rejects unknown kind" in {
    an[IllegalArgumentException] should be thrownBy ModuleMeta.fromSmeta(s"SMETA v${ModuleMeta.SMETA_VERSION}\nBLOB foo\n")
  }

  "fromSmeta returns None for stale version" in {
    ModuleMeta.fromSmeta("SMETA v1\nFUNC add 0 i32\n") shouldBe None
  }

  // ===== Generic template round-trip through smeta =====

  "round-trips generic data enum templates" in {
    val source =
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |main() -> int = 0
        |""".stripMargin
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val templates = ast.decls.filter {
      case DataEnumDeclAST(_, _, tps, _, _, _) => tps.nonEmpty
      case _ => false
    }
    templates.length shouldBe 1
    val meta = new ModuleMeta(Nil, templates)
    val text = meta.toSmeta
    text should include("TEMPLATES")
    text should include("TEMPLATES_END")
    text should include("enum Result[T, E]")
    text should include("Ok(value: T)")
    text should include("Err(error: E)")
    val meta2 = ModuleMeta.fromSmeta(text).get
    meta2.genericTemplates.length shouldBe 1
    meta2.genericTemplates.head shouldBe a[DataEnumDeclAST]
    val de = meta2.genericTemplates.head.asInstanceOf[DataEnumDeclAST]
    de.name shouldBe "Result"
    de.typeParams shouldBe List("T", "E")
    de.variants.length shouldBe 2
    de.variants(0).name shouldBe "Ok"
    de.variants(1).name shouldBe "Err"
  }

  "round-trips generic struct templates" in {
    val source =
      """struct Pair[A, B]
        |    first: A
        |    second: B
        |
        |main() -> int = 0
        |""".stripMargin
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val templates = ast.decls.filter {
      case StructDeclAST(_, _, tps, _, _, _, _) => tps.nonEmpty
      case _ => false
    }
    templates.length shouldBe 1
    val meta = new ModuleMeta(Nil, templates)
    val text = meta.toSmeta
    text should include("struct Pair[A, B]")
    val meta2 = ModuleMeta.fromSmeta(text).get
    meta2.genericTemplates.length shouldBe 1
    val st = meta2.genericTemplates.head.asInstanceOf[StructDeclAST]
    st.name shouldBe "Pair"
    st.typeParams shouldBe List("A", "B")
    st.fields.length shouldBe 2
  }

  "round-trips generic function templates" in {
    val source =
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |unwrap[T](o: Option[T]) -> T
        |    o match
        |        Some(v) -> v
        |        None -> panic("unwrap on None")
        |
        |main() -> int = 0
        |""".stripMargin
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val templates = ast.decls.filter {
      case DataEnumDeclAST(_, _, tps, _, _, _) => tps.nonEmpty
      case FunDeclAST(_, _, _, _, _, tps, _, _, _, _, _) => tps.nonEmpty
      case _ => false
    }
    templates.length shouldBe 2
    val meta = new ModuleMeta(Nil, templates)
    val text = meta.toSmeta
    text should include("unwrap[T]")
    val meta2 = ModuleMeta.fromSmeta(text).get
    meta2.genericTemplates.length shouldBe 2
    val funTemplate = meta2.genericTemplates.find(_.isInstanceOf[FunDeclAST]).get.asInstanceOf[FunDeclAST]
    funTemplate.name shouldBe "unwrap"
    funTemplate.typeParams shouldBe List("T")
    funTemplate.params.length shouldBe 1
  }

  "smeta without templates has no TEMPLATES section" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """main() -> int = 0
        |""".stripMargin))
    val text = meta.toSmeta
    text should not include "TEMPLATES"
  }

  "generic templates usable after smeta round-trip" in {
    // Build smeta with Result[T, E] template
    val source =
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |is_ok[T, E](r: Result[T, E]) -> bool
        |    r match
        |        Ok(_) -> true
        |        Err(_) -> false
        |
        |main() -> int = 0
        |""".stripMargin
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val analyzer = new SyslAnalyzer
    val typed = analyzer.analyze(ast)
    val baseMeta = ModuleMeta.fromProgram(typed)
    val templates = ast.decls.filter {
      case DataEnumDeclAST(_, _, tps, _, _, _) => tps.nonEmpty
      case FunDeclAST(_, _, _, _, _, tps, _, _, _, _, _) => tps.nonEmpty
      case _ => false
    }
    val meta = new ModuleMeta(baseMeta.symbols, templates)
    val smetaText = meta.toSmeta
    // Round-trip through text
    val meta2 = ModuleMeta.fromSmeta(smetaText).get
    meta2.genericTemplates.length shouldBe 2
    // Now use meta2 to compile code that uses Result[int, string]
    val userSource =
      """import result.*
        |
        |test() -> int
        |    val r: Result[int, string] = Ok(42)
        |    r match
        |        Ok(v) -> v
        |        Err(_) -> 0
        |""".stripMargin
    val Right(userAst) = (new SyslParser).parseProgram(userSource): @unchecked
    val userAnalyzer = new SyslAnalyzer
    userAnalyzer.registerImport(meta2)
    noException should be thrownBy userAnalyzer.analyze(userAst)
  }

  // ===== Extension method round-trip (Phase 2b) =====

  "round-trips extension entries through smeta" in {
    val src =
      """module mylib
        |
        |extension (x: i32)
        |    def doubled -> i32 = x * 2
        |    def tripled -> i32 = x * 3
        |""".stripMargin
    val Right(ast) = (new SyslParser).parseProgram(src): @unchecked
    val analyzer = new SyslAnalyzer
    val typed = analyzer.analyze(ast)
    val baseMeta = ModuleMeta.fromProgram(typed, Some("mylib.sysl"))
    val meta = new ModuleMeta(
      baseMeta.symbols,
      Nil,
      analyzer.getTraitImplMetas,
      analyzer.getGenericEnumInstances,
      analyzer.getExtensionMetas,
    )
    meta.extensions.length shouldBe 2
    meta.extensions.map(_.methodName).toSet shouldBe Set("doubled", "tripled")
    meta.extensions.foreach(_.definingModule shouldBe "mylib")
    meta.extensions.foreach(_.receiverType shouldBe SyslType.IntType(32))

    val text = meta.toSmeta
    text should include("EXT doubled mylib")
    text should include("EXT tripled mylib")
    text should include("i32")

    val meta2 = ModuleMeta.fromSmeta(text).get
    meta2.extensions.length shouldBe 2
    meta2.extensions.map(_.methodName).toSet shouldBe Set("doubled", "tripled")
    meta2.extensions.foreach(_.definingModule shouldBe "mylib")
    meta2.extensions.foreach(_.receiverType shouldBe SyslType.IntType(32))
    meta2.extensions.find(_.methodName == "doubled").get.mangledFnName shouldBe "__ext_i32__doubled"
  }

  // ===== `#const fn` body round-trip =====
  // Non-generic `#const fn` bodies travel through a dedicated SMETA block so
  // importing modules can fold cross-module `const X = lib::fn(7)` at compile
  // time. These tests pin the serialization / re-parse boundary directly,
  // ahead of the integration coverage in SyslConstFnTests.

  "emits CONST_FN_BODIES block for a non-generic #const fn" in {
    val src =
      """#const
        |scale(x: int) -> int = x * 3
        |main() -> int = 0
        |""".stripMargin
    val Right(ast) = (new SyslParser).parseProgram(src): @unchecked
    val analyzer = new SyslAnalyzer
    val typed = analyzer.analyze(ast)
    val baseMeta = ModuleMeta.fromProgram(typed)
    val meta = new ModuleMeta(
      baseMeta.symbols, Nil, Nil, Nil, Nil, analyzer.getConstFunBodies,
    )
    val text = meta.toSmeta
    text should include("CONST_FN_BODIES")
    text should include("CONST_FN_BODIES_END")
    text should include("scale")
    val meta2 = ModuleMeta.fromSmeta(text).get
    meta2.constFunBodies.length shouldBe 1
    meta2.constFunBodies.head.name shouldBe "scale"
  }

  "omits CONST_FN_BODIES block when no non-generic #const fn is declared" in {
    val meta = ModuleMeta.fromProgram(analyze(
      """add(a: int, b: int) -> int = a + b
        |main() -> int = 0
        |""".stripMargin))
    val text = meta.toSmeta
    text should not include "CONST_FN_BODIES"
  }

  "round-trips a #const fn body through SMETA" in {
    val src =
      """#const
        |inc(x: int) -> int = x + 1
        |#const
        |squared(x: int) -> int
        |    val y = x * x
        |    return y
        |""".stripMargin
    val Right(ast) = (new SyslParser).parseProgram(src): @unchecked
    val analyzer = new SyslAnalyzer
    val typed = analyzer.analyze(ast)
    val baseMeta = ModuleMeta.fromProgram(typed)
    val meta = new ModuleMeta(
      baseMeta.symbols, Nil, Nil, Nil, Nil, analyzer.getConstFunBodies,
    )
    val text = meta.toSmeta
    val meta2 = ModuleMeta.fromSmeta(text).get
    meta2.constFunBodies.map(_.name).toSet shouldBe Set("inc", "squared")
    // Re-parsed FunDeclAST should retain the #const attribute.
    meta2.constFunBodies.foreach { fd =>
      fd.attributes.map(_.name) should contain("const")
    }
  }
}
