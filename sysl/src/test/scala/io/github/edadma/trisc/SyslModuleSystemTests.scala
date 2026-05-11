package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslModuleSystemTests extends AnyFreeSpec with Matchers {

  // ===== .smeta SOURCE directive: serialization =====

  "smeta with source files serializes SOURCE lines" in {
    val meta = new ModuleMeta(List(
      SymbolMeta("strlen", SymbolMeta.Kind.Func(List(SyslType.PtrType(SyslType.I8)), SyslType.I64), isPrivate = false, sourceFile = Some("strlen.sysl")),
      SymbolMeta("strcpy", SymbolMeta.Kind.Func(List(SyslType.PtrType(SyslType.I8), SyslType.PtrType(SyslType.I8)), SyslType.PtrType(SyslType.I8)), isPrivate = false, sourceFile = Some("strcpy.sysl")),
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
      SymbolMeta("f1", SymbolMeta.Kind.Func(Nil, SyslType.UnitType), isPrivate = false, sourceFile = Some("a.sysl")),
      SymbolMeta("f2", SymbolMeta.Kind.Func(Nil, SyslType.UnitType), isPrivate = false, sourceFile = Some("a.sysl")),
      SymbolMeta("f3", SymbolMeta.Kind.Func(Nil, SyslType.UnitType), isPrivate = false, sourceFile = Some("b.sysl")),
    ))
    val smeta = meta.toSmeta
    smeta.split("SOURCE a.sysl").length shouldBe 2
    smeta should include("SOURCE b.sysl")
  }

  // ===== .smeta SOURCE directive: deserialization =====

  "smeta round-trip preserves source files" in {
    val original = new ModuleMeta(List(
      SymbolMeta("strlen", SymbolMeta.Kind.Func(List(SyslType.PtrType(SyslType.I8)), SyslType.I64), isPrivate = false, sourceFile = Some("strlen.sysl")),
      SymbolMeta("helper", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = true, sourceFile = Some("strlen.sysl")),
      SymbolMeta("strcpy", SymbolMeta.Kind.Func(List(SyslType.PtrType(SyslType.I8), SyslType.PtrType(SyslType.I8)), SyslType.PtrType(SyslType.I8)), isPrivate = false, sourceFile = Some("strcpy.sysl")),
    ))
    val parsed = ModuleMeta.fromSmeta(original.toSmeta).get
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
    val parsed = ModuleMeta.fromSmeta(original.toSmeta).get
    parsed.symbols.length shouldBe 1
    parsed.symbols.head.sourceFile shouldBe None
  }

  "smeta with DATA and STRUCT and SOURCE" in {
    val original = new ModuleMeta(List(
      SymbolMeta("count", SymbolMeta.Kind.Data(SyslType.I32), isPrivate = false, sourceFile = Some("globals.sysl")),
      SymbolMeta("Point", SymbolMeta.Kind.Struct(SyslType.StructType("Point", List(("x", SyslType.I32), ("y", SyslType.I32)))), isPrivate = false, sourceFile = Some("types.sysl")),
    ))
    val parsed = ModuleMeta.fromSmeta(original.toSmeta).get
    parsed.symbols.length shouldBe 2
    parsed.symbols(0).sourceFile shouldBe Some("globals.sysl")
    parsed.symbols(1).sourceFile shouldBe Some("types.sysl")
  }

  // ===== ModuleMeta.merge =====

  "merge replaces symbols from same source file" in {
    val existing = new ModuleMeta(List(
      SymbolMeta("strlen", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, sourceFile = Some("strlen.sysl")),
      SymbolMeta("strcpy", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, sourceFile = Some("strcpy.sysl")),
    ))
    val updated = new ModuleMeta(List(
      SymbolMeta("strlen", SymbolMeta.Kind.Func(List(SyslType.PtrType(SyslType.I8)), SyslType.I64), isPrivate = false, sourceFile = Some("strlen.sysl")),
    ))
    val merged = existing.merge(updated)
    merged.symbols.length shouldBe 2
    val strlen = merged.symbols.find(_.name == "strlen").get
    strlen.typ match
      case SymbolMeta.Kind.Func(params, _, _, _, _, _, _) => params.length shouldBe 1
      case _ => fail("expected Func")
    merged.symbols.exists(_.name == "strcpy") shouldBe true
  }

  "merge adds new source files" in {
    val existing = new ModuleMeta(List(
      SymbolMeta("f1", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, sourceFile = Some("a.sysl")),
    ))
    val newFile = new ModuleMeta(List(
      SymbolMeta("f2", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, sourceFile = Some("b.sysl")),
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
      SymbolMeta("strlen", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, sourceFile = Some("strlen.sysl")),
      SymbolMeta("strcpy", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, sourceFile = Some("strcpy.sysl")),
      SymbolMeta("memcpy", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, sourceFile = Some("memcpy.sysl")),
    ))
    meta.sourceFilesFor(Set("strlen", "memcpy")) shouldBe Set("strlen.sysl", "memcpy.sysl")
  }

  "allSourceFiles returns all files" in {
    val meta = new ModuleMeta(List(
      SymbolMeta("f1", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, sourceFile = Some("a.sysl")),
      SymbolMeta("f2", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, sourceFile = Some("a.sysl")),
      SymbolMeta("f3", SymbolMeta.Kind.Func(Nil, SyslType.I32), isPrivate = false, sourceFile = Some("b.sysl")),
    ))
    meta.allSourceFiles shouldBe Set("a.sysl", "b.sysl")
  }

  // ===== Driver: module extraction =====

  "driver extracts module declarations" in {
    val sources = Map(
      "posix/lib/string/strlen" -> "module posix.lib.string\nstrlen(s: *i8) -> int = 0",
      "main" -> "main() -> int = 0",
    )
    val driver = new SyslDriver
    val asts = driver.parseSources(sources)
    val modules = driver.extractModules(asts)
    modules shouldBe Map("posix/lib/string/strlen" -> "posix/lib/string")
  }

  // ===== Driver: compile with module declarations =====

  "driver compiles files with module declarations" in {
    val sources = Map(
      "mymath/add" ->
        """module mymath
          |add(a: int, b: int) -> int = a + b
          |""".stripMargin,
      "mymath/mul" ->
        """module mymath
          |mul(a: int, b: int) -> int = a * b
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    result.units.length shouldBe 2

    result.units.foreach(_.modulePath shouldBe Some("mymath"))

    result.packageMetas.contains("mymath") shouldBe true
    val pkgMeta = result.packageMetas("mymath")
    val names = pkgMeta.publicSymbols.map(_.name).toSet
    names should contain("mymath__add")
    names should contain("mymath__mul")
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
      "stringlib/strlen" ->
        """module stringlib
          |strlen(s: *i8) -> int = 0
          |""".stripMargin,
      "stringlib/strcpy" ->
        """module stringlib
          |strcpy(dst: *i8, src: *i8) -> *i8
          |    dst
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val pkgMeta = result.packageMetas("stringlib")
    val strlenSym = pkgMeta.symbols.find(_.name == "stringlib__strlen").get
    strlenSym.sourceFile shouldBe Some("stringlib/strlen.sysl")
    val strcpySym = pkgMeta.symbols.find(_.name == "stringlib__strcpy").get
    strcpySym.sourceFile shouldBe Some("stringlib/strcpy.sysl")
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
      "mymod/quad" ->
        """module mymod
          |import utils.*
          |quad(x: int) -> int = twice(twice(x))
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    result.units.length shouldBe 2
  }

  // ===== Driver: validation =====

  "driver rejects module declaration that doesn't match path" in {
    val sources = Map(
      "wrong/path/file" ->
        """module some.other.place
          |f() -> int = 0
          |""".stripMargin,
    )
    val driver = new SyslDriver
    an[driver.DriverError] should be thrownBy driver.compile(sources)
  }

  "driver rejects module declaration on top-level file" in {
    val sources = Map(
      "myfile" ->
        """module some.package
          |f() -> int = 0
          |""".stripMargin,
    )
    val driver = new SyslDriver
    an[driver.DriverError] should be thrownBy driver.compile(sources)
  }

  "driver accepts file without module in subdirectory" in {
    // A file in a subdirectory without a module statement is a standalone module
    val sources = Map(
      "utils/helper" ->
        """twice(x: int) -> int = x * 2
          |""".stripMargin,
      "app" ->
        """import utils.helper.*
          |main() -> int = twice(21)
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
      "mathlib/add" ->
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
      "mathlib/inc" ->
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

  // ===== Generic type aliases across files of the same module =====
  // A generic type alias defined in one file of a multi-file module must be
  // visible (with full generic syntax) from any other file in the same module.
  // Mirrors how generic structs and generic functions already cross file
  // boundaries within a module.

  "generic newtype alias visible across files in same module" in {
    val sources = Map(
      "boxlib/lib" ->
        """module boxlib
          |type Box[A] = new A
          |mk_box[A](v: A) -> Box[A] = Box[A](v)
          |""".stripMargin,
      "boxlib/use" ->
        """module boxlib
          |unbox(b: Box[int]) -> int = int(b)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    noException should be thrownBy driver.compile(sources)
  }

  "generic alias-of-function-type visible across files in same module" in {
    val sources = Map(
      "parselib/types" ->
        """module parselib
          |type Parser[A] = new (int) -> A
          |""".stripMargin,
      "parselib/runner" ->
        """module parselib
          |run(p: Parser[int], x: int) -> int = p(x)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    noException should be thrownBy driver.compile(sources)
  }

  "generic alias usable as return type in another file" in {
    val sources = Map(
      "boxlib/lib" ->
        """module boxlib
          |type Box[A] = new A
          |""".stripMargin,
      "boxlib/use" ->
        """module boxlib
          |make_int_box(v: int) -> Box[int] = Box[int](v)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    noException should be thrownBy driver.compile(sources)
  }

  "generic alias passed through another generic across files" in {
    val sources = Map(
      "boxlib/lib" ->
        """module boxlib
          |type Box[A] = new A
          |mk_box[A](v: A) -> Box[A] = Box[A](v)
          |""".stripMargin,
      "boxlib/use" ->
        """module boxlib
          |take_any[A](b: Box[A]) -> Box[A] = b
          |make_string_box(s: string) -> Box[string] = take_any[string](mk_box(s))
          |""".stripMargin,
    )
    val driver = new SyslDriver
    noException should be thrownBy driver.compile(sources)
  }

  "generic alias visible across module boundary (cross-module import)" in {
    val sources = Map(
      "boxlib/lib" ->
        """module boxlib
          |type Box[A] = new A
          |mk_box[A](v: A) -> Box[A] = Box[A](v)
          |""".stripMargin,
      "app" ->
        """import boxlib.*
          |unbox(b: Box[int]) -> int = int(b)
          |main() -> int = unbox(mk_box(42))
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged) shouldBe 42
  }

  "generic alias round-trips through SMETA" in {
    val sources = Map(
      "boxlib/lib" ->
        """module boxlib
          |type Box[A] = new A
          |mk_box[A](v: A) -> Box[A] = Box[A](v)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val unit = result.units.head
    // Round-trip: serialize, parse back, verify the generic alias survives
    val parsed = ModuleMeta.fromSmeta(unit.smeta).get
    val aliasTemplate = parsed.genericTemplates.collectFirst {
      case ta @ TypeAliasDeclAST("Box", _, tps, _, isNew, _, _, _, _) if tps.nonEmpty => (ta, tps, isNew)
    }
    aliasTemplate.isDefined shouldBe true
    aliasTemplate.get._2 shouldBe List("A")
    aliasTemplate.get._3 shouldBe true
  }

  "executable test runs across files via interpreter" in {
    val sources = Map(
      "boxlib/lib" ->
        """module boxlib
          |type Box[A] = new A
          |mk_box[A](v: A) -> Box[A] = Box[A](v)
          |""".stripMargin,
      "boxlib/use" ->
        """module boxlib
          |unbox(b: Box[int]) -> int = int(b)
          |main() -> int
          |    val b: Box[int] = mk_box(42)
          |    unbox(b)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged) shouldBe 42
  }

  // ===== Pre-collection carries generic templates in cached meta =====
  // Regression for the bug where pre-collection's cached meta dropped
  // genericTemplates (passed `Nil` instead of extracting them like Step 5
  // did). That left generic types invisible to dependent modules during
  // pre-collection, cascading failures that left empty package metas and
  // surfacing as misleading "unknown type" errors in sibling files.

  "cross-module generic enum visible during pre-collection" in {
    // Module A defines a generic enum. Module B imports it and uses it in a
    // function signature. With the bug, B fails pre-collect ("'Result' is not
    // a generic type"), leaving B's package meta empty, which would cascade
    // to any sibling file in B's module.
    val sources = Map(
      "alib/types" ->
        """module alib
          |enum Result[A, E]
          |    Ok(value: A)
          |    Err(error: E)
          |""".stripMargin,
      "blib/use" ->
        """module blib
          |import alib.*
          |wrap(x: int) -> Result[int, string] = Ok(x)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    noException should be thrownBy driver.compile(sources)
  }

  "cross-module generic struct visible during pre-collection (carries through templates)" in {
    // Module A defines a generic struct in one file. Module B (separate
    // module) imports A and uses A's generic in a function. With the
    // pre-collection bug, A's pre-collect produced a cached meta with empty
    // genericTemplates, so B's pre-collect failed with "'Box' is not a
    // generic type".
    val sources = Map(
      "alib/box" ->
        """module alib
          |struct Box[A]
          |    value: A
          |""".stripMargin,
      "blib/use" ->
        """module blib
          |import alib.*
          |make_int_box(v: int) -> Box[int] = Box[int](v)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    try driver.compile(sources) catch
      case e: Throwable => fail(s"compile failed: ${e.getMessage}", e)
  }

  // ===== Pre-collection cascade-error masking =====
  // Regression for the bug where a real user error in one sibling file
  // (say file A's `cannot access field 'len' on string`) was silently
  // swallowed during pre-collection, then the package meta for A's module
  // was published without A's contributions, and a different sibling B
  // that referenced one of A's vals failed with a misleading
  // `undefined variable: '<A's val>'` error during final-compile.
  //
  // The fix: after pre-collection iteration converges, if any file
  // persistently fails with a non-`undefined variable` error (i.e. an
  // actual user error rather than a cascade symptom), surface that error
  // so the user sees the root cause.

  "real user error in one sibling surfaces instead of cascade error in another" in {
    // File A has a real user error — a field access against a name that
    // is not a struct field, not a method, and not a registered extension.
    // File B is a sibling that uses a forward-referenced val from A.
    // Without the fix, the user-visible error is "undefined variable:
    // 'OWNERS_MAX'" in file B — completely misleading because OWNERS_MAX
    // is fine; A's pre-collection just failed silently and so its meta
    // was empty.
    val sources = Map(
      "mymod/proto" ->
        """module mymod
          |probe(s: string) -> int = s.fnordfield
          |val OWNERS_MAX = 4
          |use_max() -> int = OWNERS_MAX
          |""".stripMargin,
      "mymod/main" ->
        """module mymod
          |consume() -> int = OWNERS_MAX
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val ex = the[driver.DriverError] thrownBy driver.compile(sources)
    // The error should be from `mymod/proto` (the real bug) — not from
    // `mymod/main` (which references a perfectly-valid val).
    ex.getMessage should include("mymod/proto")
    ex.getMessage should include("cannot access field 'fnordfield' on string")
  }
}
