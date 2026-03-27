package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslDriverTests extends AnyFreeSpec with Matchers {

  val driver = new SyslDriver

  // ===== Single file =====

  "compiles single file" in {
    val result = driver.compile(Map(
      "main" ->
        """main() -> int = 42
          |""".stripMargin
    ))
    result.units.length shouldBe 1
    result.units.head.name shouldBe "main"
  }

  "single file produces smeta" in {
    val result = driver.compile(Map(
      "main" ->
        """main() -> int = 42
          |""".stripMargin
    ))
    result.units.head.smeta should include("FUNC main 0 i64")
  }

  "single file has no externals" in {
    val result = driver.compile(Map(
      "main" ->
        """main() -> int = 42
          |""".stripMargin
    ))
    result.units.head.externals shouldBe empty
  }

  // ===== Two files with import =====

  "compiles two files with import" in {
    val result = driver.compile(Map(
      "math" ->
        """add(a: int, b: int) -> int = a + b
          |""".stripMargin,
      "main" ->
        """import "math"
          |main() -> int = add(1, 2)
          |""".stripMargin
    ))
    result.units.length shouldBe 2
  }

  "dependency compiled before dependent" in {
    val result = driver.compile(Map(
      "main" ->
        """import "math"
          |main() -> int = add(1, 2)
          |""".stripMargin,
      "math" ->
        """add(a: int, b: int) -> int = a + b
          |""".stripMargin
    ))
    result.order.indexOf("math") should be < result.order.indexOf("main")
  }

  "dependent file has externals from import" in {
    val result = driver.compile(Map(
      "math" ->
        """add(a: int, b: int) -> int = a + b
          |""".stripMargin,
      "main" ->
        """import "math"
          |main() -> int = add(1, 2)
          |""".stripMargin
    ))
    val mainUnit = result.units.find(_.name == "main").get
    mainUnit.externals should contain("add")
  }

  "imported function type-checks correctly" in {
    val result = driver.compile(Map(
      "math" ->
        """square(x: int) -> int = x * x
          |""".stripMargin,
      "main" ->
        """import "math"
          |main() -> int = square(5)
          |""".stripMargin
    ))
    val mainUnit = result.units.find(_.name == "main").get
    mainUnit.typed.decls.length shouldBe 2 // import + main
  }

  // ===== Three files, chain dependency =====

  "compiles chain: a -> b -> c" in {
    val result = driver.compile(Map(
      "c" ->
        """base() -> int = 1
          |""".stripMargin,
      "b" ->
        """import "c"
          |middle() -> int = base() + 1
          |""".stripMargin,
      "a" ->
        """import "b"
          |main() -> int = middle() + 1
          |""".stripMargin
    ))
    result.order.indexOf("c") should be < result.order.indexOf("b")
    result.order.indexOf("b") should be < result.order.indexOf("a")
  }

  // ===== Diamond dependency =====

  "compiles diamond: main -> (math, io), math and io independent" in {
    val result = driver.compile(Map(
      "math" ->
        """add(a: int, b: int) -> int = a + b
          |""".stripMargin,
      "io" ->
        """write(x: int)
          |    putchar(x)
          |""".stripMargin,
      "main" ->
        """import "math"
          |import "io"
          |main() -> int
          |    write(add(48, 1))
          |    0
          |""".stripMargin
    ))
    result.order.indexOf("math") should be < result.order.indexOf("main")
    result.order.indexOf("io") should be < result.order.indexOf("main")
  }

  // ===== Private symbols =====

  "private symbols not exported to importers" in {
    val result = driver.compile(Map(
      "lib" ->
        """private helper(x: int) -> int = x * 2
          |public_fn(x: int) -> int = helper(x)
          |""".stripMargin,
      "main" ->
        """import "lib"
          |main() -> int = public_fn(5)
          |""".stripMargin
    ))
    val mainUnit = result.units.find(_.name == "main").get
    mainUnit.externals should contain("public_fn")
    mainUnit.externals should not contain "helper"
  }

  "calling private imported function fails" in {
    an[Exception] should be thrownBy driver.compile(Map(
      "lib" ->
        """private secret() -> int = 42
          |""".stripMargin,
      "main" ->
        """import "lib"
          |main() -> int = secret()
          |""".stripMargin
    ))
  }

  // ===== No imports =====

  "files without imports compile in any order" in {
    val result = driver.compile(Map(
      "a" ->
        """fa() -> int = 1
          |""".stripMargin,
      "b" ->
        """fb() -> int = 2
          |""".stripMargin,
      "c" ->
        """fc() -> int = 3
          |""".stripMargin
    ))
    result.units.length shouldBe 3
    result.order.length shouldBe 3
  }

  // ===== Error cases =====

  "circular dependency detected" in {
    an[Exception] should be thrownBy driver.compile(Map(
      "a" ->
        """import "b"
          |fa() -> int = 1
          |""".stripMargin,
      "b" ->
        """import "a"
          |fb() -> int = 2
          |""".stripMargin
    ))
  }

  "missing import detected" in {
    an[Exception] should be thrownBy driver.compile(Map(
      "main" ->
        """import "nonexistent"
          |main() -> int = 0
          |""".stripMargin
    ))
  }

  "parse error in one file reported" in {
    an[Exception] should be thrownBy driver.compile(Map(
      "main" ->
        """main( -> int = 0
          |""".stripMargin
    ))
  }

  // ===== smeta content =====

  "each unit produces correct smeta" in {
    val result = driver.compile(Map(
      "math" ->
        """add(a: int, b: int) -> int = a + b
          |mul(a: int, b: int) -> int = a * b
          |""".stripMargin,
      "main" ->
        """import "math"
          |main() -> int = add(1, mul(2, 3))
          |""".stripMargin
    ))
    val mathMeta = result.units.find(_.name == "math").get.smeta
    mathMeta should include("FUNC add 2 i64 i64 i64")
    mathMeta should include("FUNC mul 2 i64 i64 i64")
    val mainMeta = result.units.find(_.name == "main").get.smeta
    mainMeta should include("FUNC main 0 i64")
  }

  // ===== Round-trip: smeta from one unit usable by another =====

  "smeta round-trips through ModuleMeta" in {
    val result = driver.compile(Map(
      "math" ->
        """add(a: int, b: int) -> int = a + b
          |""".stripMargin,
      "main" ->
        """import "math"
          |main() -> int = add(1, 2)
          |""".stripMargin
    ))
    val mathSmeta = result.units.find(_.name == "math").get.smeta
    val meta = ModuleMeta.fromSmeta(mathSmeta)
    meta.publicSymbols.length shouldBe 1
    meta.publicSymbols.head.name shouldBe "add"
    meta.publicSymbols.head.typ shouldBe SymbolMeta.Kind.Func(List(I64, I64), I64)
  }
}
