package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslModuleSystemTests extends AnyFreeSpec with Matchers {

  // ===== .smeta SOURCE directive: serialization =====

  "smeta with source files serializes SOURCE lines" in {
    val meta = new ModuleMeta(List(
      SymbolMeta("strlen", SymbolMeta.Kind.Func(List(SyslType.PtrType(SyslType.I8)), SyslType.I64), isPrivate = false, Some("strlen.sysl")),
      SymbolMeta("strcpy", SymbolMeta.Kind.Func(List(SyslType.PtrType(SyslType.I8), SyslType.PtrType(SyslType.I8)), SyslType.PtrType(SyslType.I8)), isPrivate = false, Some("strcpy.sysl")),
    ))
    val smeta = meta.toSmeta
    smeta should include("SOURCE strlen.sysl")
    smeta should include("SOURCE strcpy.sysl")
    smeta should include("FUNC strlen")
    smeta should include("FUNC strcpy")
  }

  "smeta without source files has no SOURCE lines" in {
    val meta = new ModuleMeta(List(
      SymbolMeta("add", SymbolMeta.Kind.Func(List(SyslType.I32, SyslType.I32), SyslType.I32), isPrivate = false),
    ))
    val smeta = meta.toSmeta
    smeta should not include "SOURCE"
    smeta should include("FUNC add")
  }

  "smeta groups symbols under same SOURCE" in {
    val meta = new ModuleMeta(List(
      SymbolMeta("f1", SymbolMeta.Kind.Func(Nil, SyslType.VoidType), isPrivate = false, Some("a.sysl")),
      SymbolMeta("f2", SymbolMeta.Kind.Func(Nil, SyslType.VoidType), isPrivate = false, Some("a.sysl")),
      SymbolMeta("f3", SymbolMeta.Kind.Func(Nil, SyslType.VoidType), isPrivate = false, Some("b.sysl")),
    ))
    val smeta = meta.toSmeta
    // Should only emit SOURCE a.sysl once, not twice
    smeta.split("SOURCE a.sysl").length shouldBe 2 // 1 occurrence = split into 2 parts
    smeta should include("SOURCE b.sysl")
  }

  // ===== .smeta SOURCE directive: deserialization =====

  "smeta round-trip preserves source files" in {
    val original = new ModuleMeta(List(
      SymbolMeta("strlen", SymbolMeta.Kind.Func(List(SyslType.PtrType(SyslType.I8)), SyslType.I64), isPrivate = false, Some("strlen.sysl")),
      SymbolMeta("helper", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = true, Some("strlen.sysl")),
      SymbolMeta("strcpy", SymbolMeta.Kind.Func(List(SyslType.PtrType(SyslType.I8), SyslType.PtrType(SyslType.I8)), SyslType.PtrType(SyslType.I8)), isPrivate = false, Some("strcpy.sysl")),
    ))
    val parsed = ModuleMeta.fromSmeta(original.toSmeta)
    parsed.symbols.length shouldBe 3
    parsed.symbols(0).name shouldBe "strlen"
    parsed.symbols(0).sourceFile shouldBe Some("strlen.sysl")
    parsed.symbols(1).name shouldBe "helper"
    parsed.symbols(1).isPrivate shouldBe true
    parsed.symbols(1).sourceFile shouldBe Some("strlen.sysl")
    parsed.symbols(2).name shouldBe "strcpy"
    parsed.symbols(2).sourceFile shouldBe Some("strcpy.sysl")
  }

  "smeta round-trip without source files" in {
    val original = new ModuleMeta(List(
      SymbolMeta("add", SymbolMeta.Kind.Func(List(SyslType.I32, SyslType.I32), SyslType.I32), isPrivate = false),
    ))
    val parsed = ModuleMeta.fromSmeta(original.toSmeta)
    parsed.symbols.length shouldBe 1
    parsed.symbols.head.sourceFile shouldBe None
  }

  "smeta with DATA and STRUCT and SOURCE" in {
    val original = new ModuleMeta(List(
      SymbolMeta("count", SymbolMeta.Kind.Data(SyslType.I32), isPrivate = false, Some("globals.sysl")),
      SymbolMeta("Point", SymbolMeta.Kind.Struct(SyslType.StructType("Point", List(("x", SyslType.I32), ("y", SyslType.I32)))), isPrivate = false, Some("types.sysl")),
    ))
    val parsed = ModuleMeta.fromSmeta(original.toSmeta)
    parsed.symbols.length shouldBe 2
    parsed.symbols(0).sourceFile shouldBe Some("globals.sysl")
    parsed.symbols(1).sourceFile shouldBe Some("types.sysl")
  }

  // ===== ModuleMeta.merge =====

  "merge replaces symbols from same source file" in {
    val existing = new ModuleMeta(List(
      SymbolMeta("strlen", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, Some("strlen.sysl")),
      SymbolMeta("strcpy", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, Some("strcpy.sysl")),
    ))
    val updated = new ModuleMeta(List(
      SymbolMeta("strlen", SymbolMeta.Kind.Func(List(SyslType.PtrType(SyslType.I8)), SyslType.I64), isPrivate = false, Some("strlen.sysl")),
    ))
    val merged = existing.merge(updated)
    merged.symbols.length shouldBe 2
    // strlen should be the updated version
    val strlen = merged.symbols.find(_.name == "strlen").get
    strlen.typ match
      case SymbolMeta.Kind.Func(params, _) => params.length shouldBe 1
      case _ => fail("expected Func")
    // strcpy should be preserved
    merged.symbols.exists(_.name == "strcpy") shouldBe true
  }

  "merge adds new source files" in {
    val existing = new ModuleMeta(List(
      SymbolMeta("f1", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, Some("a.sysl")),
    ))
    val newFile = new ModuleMeta(List(
      SymbolMeta("f2", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, Some("b.sysl")),
    ))
    val merged = existing.merge(newFile)
    merged.symbols.length shouldBe 2
    merged.symbols.map(_.name).toSet shouldBe Set("f1", "f2")
  }

  "merge with no source files appends" in {
    val existing = new ModuleMeta(List(
      SymbolMeta("f1", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false),
    ))
    val other = new ModuleMeta(List(
      SymbolMeta("f2", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false),
    ))
    val merged = existing.merge(other)
    merged.symbols.length shouldBe 2
  }

  // ===== ModuleMeta helpers =====

  "sourceFilesFor returns correct files" in {
    val meta = new ModuleMeta(List(
      SymbolMeta("strlen", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, Some("strlen.sysl")),
      SymbolMeta("strcpy", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, Some("strcpy.sysl")),
      SymbolMeta("memcpy", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, Some("memcpy.sysl")),
    ))
    meta.sourceFilesFor(Set("strlen", "memcpy")) shouldBe Set("strlen.sysl", "memcpy.sysl")
  }

  "allSourceFiles returns all files" in {
    val meta = new ModuleMeta(List(
      SymbolMeta("f1", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, Some("a.sysl")),
      SymbolMeta("f2", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, Some("a.sysl")),
      SymbolMeta("f3", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, Some("b.sysl")),
    ))
    meta.allSourceFiles shouldBe Set("a.sysl", "b.sysl")
  }

  // ===== Driver: module extraction =====

  "driver extracts module declarations" in {
    val sources = Map(
      "strlen" -> "module posix.lib.string\nstrlen(s: *i8) -> int = 0",
      "main" -> "main() -> int = 0",
    )
    val driver = new SyslDriver
    val asts = driver.parseSources(sources)
    val modules = driver.extractModules(asts)
    modules shouldBe Map("strlen" -> "posix/lib/string")
  }

  // ===== Driver: compile with module declarations =====

  "driver compiles files with module declarations" in {
    val sources = Map(
      "math_add" ->
        """module mymath
          |add(a: int, b: int) -> int = a + b
          |""".stripMargin,
      "math_mul" ->
        """module mymath
          |mul(a: int, b: int) -> int = a * b
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    result.units.length shouldBe 2

    // Both files should be assigned to the same module
    result.units.foreach(_.modulePath shouldBe Some("mymath"))

    // Package meta should contain symbols from both files
    result.packageMetas.contains("mymath") shouldBe true
    val pkgMeta = result.packageMetas("mymath")
    val names = pkgMeta.publicSymbols.map(_.name).toSet
    names should contain("add")
    names should contain("mul")
  }

  "driver compiles module with importer" in {
    val sources = Map(
      "mathlib" ->
        """add(a: int, b: int) -> int = a + b
          |""".stripMargin,
      "app" ->
        """import mathlib.*
          |main() -> int = add(3, 4)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    result.units.length shouldBe 2
  }

  "driver package meta has source file attribution" in {
    val sources = Map(
      "strlen" ->
        """module stringlib
          |strlen(s: *i8) -> int = 0
          |""".stripMargin,
      "strcpy" ->
        """module stringlib
          |strcpy(dst: *i8, src: *i8) -> *i8
          |    dst
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val pkgMeta = result.packageMetas("stringlib")
    val strlenSym = pkgMeta.symbols.find(_.name == "strlen").get
    strlenSym.sourceFile shouldBe Some("strlen.sysl")
    val strcpySym = pkgMeta.symbols.find(_.name == "strcpy").get
    strcpySym.sourceFile shouldBe Some("strcpy.sysl")
  }

  "standalone file has no module path" in {
    val sources = Map(
      "app" ->
        """main() -> int = 42
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    result.units.head.modulePath shouldBe None
    result.packageMetas shouldBe empty
  }

  // ===== Driver: cross-module imports =====

  "module files can import from standalone modules" in {
    val sources = Map(
      "utils" ->
        """twice(x: int) -> int = x * 2
          |""".stripMargin,
      "mymod" ->
        """module mymod
          |import utils.*
          |quad(x: int) -> int = twice(twice(x))
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    result.units.length shouldBe 2
  }

  // ===== Interpreter: module declaration has no runtime effect =====

  "interpreter ignores module declaration" in {
    val buf = new StringBuilder
    val Right(ast) = (new SyslParser).parseProgram(
      """module test.mod
        |main() -> int = 42
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val interp = new SyslInterpreter(s => buf ++= s)
    interp.run(typed) shouldBe 42
  }

  "interpreter runs multi-file module via driver" in {
    val sources = Map(
      "mathlib" ->
        """module mathlib
          |add(a: int, b: int) -> int = a + b
          |""".stripMargin,
      "app" ->
        """import mathlib.*
          |main() -> int = add(19, 23)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged) shouldBe 42
  }

  "interpreter runs module with private functions" in {
    val sources = Map(
      "mathlib" ->
        """module mathlib
          |private helper(x: int) -> int = x + 1
          |inc(x: int) -> int = helper(x)
          |""".stripMargin,
      "app" ->
        """import mathlib.*
          |main() -> int = inc(41)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged) shouldBe 42
  }

  // ===== Analyzer: module declaration accepted =====

  "analyzer produces TModuleDecl" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """module foo.bar
        |main() -> int = 0
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    typed.decls.head shouldBe a[TModuleDecl]
    typed.decls.head.asInstanceOf[TModuleDecl].path shouldBe List("foo", "bar")
  }

  // ===== ModuleMeta.fromProgram with sourceFile =====

  "fromProgram attaches source file" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """add(a: int, b: int) -> int = a + b
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val meta = ModuleMeta.fromProgram(typed, Some("add.sysl"))
    meta.symbols.head.sourceFile shouldBe Some("add.sysl")
  }

  "fromProgram without source file" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """add(a: int, b: int) -> int = a + b
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val meta = ModuleMeta.fromProgram(typed)
    meta.symbols.head.sourceFile shouldBe None
  }
}
