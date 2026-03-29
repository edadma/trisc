package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslLiteralSuffixTests extends SyslTestHelpers {

  // ===== Unsigned suffixes =====

  "u8 suffix" in {
    eval("main() -> int = int(200u8)\n") shouldBe 200
  }

  "u16 suffix" in {
    eval("main() -> int = int(50000u16)\n") shouldBe 50000
  }

  "u32 suffix" in {
    eval("main() -> int = int(42u32)\n") shouldBe 42
  }

  "u64 suffix" in {
    eval("main() -> int = i64(100u64)\n") shouldBe 100
  }

  // ===== Signed suffixes =====

  "i8 suffix" in {
    eval("main() -> int = i32(100i8)\n") shouldBe 100
  }

  "i16 suffix" in {
    eval("main() -> int = i32(1000i16)\n") shouldBe 1000
  }

  "i32 suffix" in {
    eval("main() -> int = 42i32\n") shouldBe 42
  }

  "i64 suffix" in {
    eval("main() -> int = i32(100i64)\n") shouldBe 100
  }

  // ===== Suffixed literals avoid mixed-sign errors =====

  "u32 suffixed literal in unsigned arithmetic" in {
    eval(
      """main() -> int
        |    var x: u32 = 10
        |    int(x + 5u32)
        |""".stripMargin) shouldBe 15
  }

  "u32 suffixed literal in comparison" in {
    eval(
      """main() -> int
        |    var x: u32 = 10
        |    if x > 5u32 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Hex literals with suffix =====

  "hex u8 suffix" in {
    eval("main() -> int = int(0xFFu8)\n") shouldBe 255
  }

  "hex u32 suffix" in {
    eval("main() -> int = int(0xCAFEu32)\n") shouldBe 0xCAFE
  }

  // ===== Suffixed literals in variable declaration =====

  "u32 suffixed in var decl" in {
    eval(
      """main() -> int
        |    x = 42u32
        |    int(x)
        |""".stripMargin) shouldBe 42
  }

  "u8 suffixed infers u8 type" in {
    eval(
      """main() -> int
        |    x = 200u8
        |    int(x)
        |""".stripMargin) shouldBe 200
  }

  // ===== Parser correctly handles suffix =====

  "suffixed literal parses correctly" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = 42u32\n"): @unchecked
    // Should not throw
    (new SyslAnalyzer).analyze(ast)
  }

  "hex suffixed literal parses correctly" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = 0xFFu8\n"): @unchecked
    (new SyslAnalyzer).analyze(ast)
  }
}
