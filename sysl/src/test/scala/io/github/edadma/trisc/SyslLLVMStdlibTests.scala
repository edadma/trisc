package io.github.edadma.trisc

class SyslLLVMStdlibTests extends SyslLLVMTestHelpers {

  // ===== std.math =====

  "std.math abs" in {
    llvmExitWithStd(
      """import std.math.*
        |
        |main() -> int = abs(-42)
        |""".stripMargin) shouldBe 42
  }

  "std.math min/max" in {
    llvmExitWithStd(
      """import std.math.*
        |
        |main() -> int = min(42, 100) + max(0, 0)
        |""".stripMargin) shouldBe 42
  }

  "std.math clamp" in {
    llvmExitWithStd(
      """import std.math.*
        |
        |main() -> int = clamp(100, 0, 42)
        |""".stripMargin) shouldBe 42
  }

  "std.math pow" in {
    llvmExitWithStd(
      """import std.math.*
        |
        |main() -> int = pow(2, 5) + pow(3, 2) + 1
        |""".stripMargin) shouldBe 42 // 32 + 9 + 1
  }

  // ===== std.cmp =====
  // TODO: traits from imported modules need cross-unit trait registration

  // ===== std.strings (pure functions, no builder dep) =====

  "std.strings has_prefix" in {
    llvmExitWithStd(
      """import std.strings.*
        |
        |main() -> int
        |    if has_prefix("hello world", "hello") then 42 else 0
        |""".stripMargin) shouldBe 42
  }

  "std.strings has_suffix" in {
    llvmExitWithStd(
      """import std.strings.*
        |
        |main() -> int
        |    if has_suffix("hello world", "world") then 42 else 0
        |""".stripMargin) shouldBe 42
  }

  "std.strings index" in {
    llvmExitWithStd(
      """import std.strings.*
        |
        |main() -> int
        |    index("hello world", "world")
        |""".stripMargin) shouldBe 6
  }

  "std.strings contains" in {
    llvmExitWithStd(
      """import std.strings.*
        |
        |main() -> int
        |    if contains("hello world", "lo wo") then 42 else 0
        |""".stripMargin) shouldBe 42
  }

  "std.strings count" in {
    llvmExitWithStd(
      """import std.strings.*
        |
        |main() -> int
        |    count("abcabc", "abc")
        |""".stripMargin) shouldBe 2
  }
}
