package io.github.edadma.trisc

class SyslCrossUnitTypeTests extends SyslTestHelpers {

  // ===== Structs across units =====

  "struct type visible across units" in {
    evalWithLibs(
      Map(
        "mymod/types" ->
          """module mymod
            |struct Point
            |    x: int
            |    y: int
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int
        |    var p: Point
        |    p.x = 10
        |    p.y = 32
        |    p.x + p.y
        |""".stripMargin
    ) shouldBe 42
  }

  "struct constructor across units" in {
    evalWithLibs(
      Map(
        "mymod/types" ->
          """module mymod
            |struct Point
            |    x: int
            |    y: int
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int
        |    val p = Point(10, 32)
        |    p.x + p.y
        |""".stripMargin
    ) shouldBe 42
  }

  "struct used in function across units" in {
    evalWithLibs(
      Map(
        "mymod/types" ->
          """module mymod
            |struct Point
            |    x: int
            |    y: int
            |sum(p: Point) -> int = p.x + p.y
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int = sum(Point(10, 32))
        |""".stripMargin
    ) shouldBe 42
  }

  // ===== Simple enums across units =====

  "simple enum visible across units" in {
    evalWithLibs(
      Map(
        "mymod/types" ->
          """module mymod
            |enum Color
            |    Red
            |    Green
            |    Blue
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int = int(Color.Blue)
        |""".stripMargin
    ) shouldBe 2
  }

  "simple enum member accessible across units" in {
    evalWithLibs(
      Map(
        "mymod/types" ->
          """module mymod
            |enum Dir
            |    North
            |    South
            |    East
            |    West
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int = int(Dir.East) + 40
        |""".stripMargin
    ) shouldBe 42
  }

  // ===== Data enums across units =====

  "data enum constructor and match across units" in {
    evalWithLibs(
      Map(
        "mymod/types" ->
          """module mymod
            |enum Result
            |    Ok(value: int)
            |    Err(code: int)
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int
        |    val r: Result = Ok(42)
        |    r match
        |        Ok(v) -> v
        |        Err(_) -> -1
        |""".stripMargin
    ) shouldBe 42
  }

  // ===== smeta round-trip for enums =====

  private def analyzeSource(source: String): TProgram =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    (new SyslAnalyzer).analyze(ast)

  "enum survives smeta round-trip" in {
    val meta = ModuleMeta.fromProgram(analyzeSource(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |main() -> int = 0
        |""".stripMargin))
    val text = meta.toSmeta
    text should include("ENUM Color")
    val meta2 = ModuleMeta.fromSmeta(text).get
    meta2.symbols.exists(s => s.name == "Color" && s.typ.isInstanceOf[SymbolMeta.Kind.Enum]) shouldBe true
  }

  "data enum survives smeta round-trip" in {
    val meta = ModuleMeta.fromProgram(analyzeSource(
      """enum Option
        |    Some(value: int)
        |    None
        |main() -> int = 0
        |""".stripMargin))
    val text = meta.toSmeta
    text should include("ENUM Option")
    val meta2 = ModuleMeta.fromSmeta(text).get
    val enumSym = meta2.symbols.find(_.name == "Option").get
    enumSym.typ match
      case SymbolMeta.Kind.Enum(et) =>
        et.variants.length shouldBe 2
        et.variants(0)._1 shouldBe "Some"
        et.variants(1)._1 shouldBe "None"
      case _ => fail("expected Enum kind")
  }
}
