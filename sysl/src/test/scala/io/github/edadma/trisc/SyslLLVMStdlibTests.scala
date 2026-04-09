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

  "std.strings last_index" in {
    llvmExitWithStd(
      """import std.strings.*
        |
        |main() -> int
        |    last_index("abcabc", "abc")
        |""".stripMargin) shouldBe 3
  }

  "std.strings index_byte" in {
    llvmExitWithStd(
      """import std.strings.*
        |
        |main() -> int
        |    index_byte("hello", byte('l'))
        |""".stripMargin) shouldBe 2
  }

  "std.strings last_index_byte" in {
    llvmExitWithStd(
      """import std.strings.*
        |
        |main() -> int
        |    last_index_byte("hello", byte('l'))
        |""".stripMargin) shouldBe 3
  }

  "std.strings substring" in {
    llvmOutputWithStd(
      """import std.strings.*
        |
        |main()
        |    puts(substring("hello world", 6, 11))
        |""".stripMargin) shouldBe "world"
  }

  "std.strings trim_space" in {
    llvmOutputWithStd(
      """import std.strings.*
        |
        |main()
        |    puts(trim_space("  hello  "))
        |""".stripMargin) shouldBe "hello"
  }

  "std.strings trim" in {
    llvmOutputWithStd(
      """import std.strings.*
        |
        |main()
        |    puts(trim("xxhelloxx", "x"))
        |""".stripMargin) shouldBe "hello"
  }

  "std.strings to_upper" in {
    llvmOutputWithStd(
      """import std.strings.*
        |
        |main()
        |    puts(to_upper("hello"))
        |""".stripMargin) shouldBe "HELLO"
  }

  "std.strings to_lower" in {
    llvmOutputWithStd(
      """import std.strings.*
        |
        |main()
        |    puts(to_lower("HELLO"))
        |""".stripMargin) shouldBe "hello"
  }

  "std.strings repeat" in {
    llvmOutputWithStd(
      """import std.strings.*
        |
        |main()
        |    puts(repeat("ab", 3))
        |""".stripMargin) shouldBe "ababab"
  }

  "std.strings replace_all" in {
    llvmOutputWithStd(
      """import std.strings.*
        |
        |main()
        |    puts(replace_all("aabaa", "a", "x"))
        |""".stripMargin) shouldBe "xxbxx"
  }

  "new string array and slice" in {
    llvmExit(
      """main() -> int
        |    val a = new [3]string
        |    a[0] = "hello"
        |    a[1] = "world"
        |    a[2] = "!"
        |    val s = a[:]
        |    len(s)
        |""".stripMargin) shouldBe 3
  }

  "read string from slice" in {
    llvmOutput(
      """main()
        |    val a = new [2]string
        |    a[0] = "hello"
        |    a[1] = "world"
        |    val s = a[:]
        |    puts(s[0])
        |""".stripMargin) shouldBe "hello"
  }

  "std.strings split" in {
    llvmExitWithStd(
      """import std.strings.*
        |
        |main() -> int
        |    val parts = split("a,b,c", ",")
        |    len(parts)
        |""".stripMargin) shouldBe 3
  }

  // TODO: split returns []string but ref array is freed on return — string data lost
  // Need to either: bump refcount for returned slices, or use immortal refs for slices
  // "std.strings split output" — puts(parts[0]) returns empty
  // "std.strings join" — join(parts, "-") returns just the separator

  "std.strings fields" in {
    llvmExitWithStd(
      """import std.strings.*
        |
        |main() -> int
        |    val parts = fields("  hello  world  ")
        |    len(parts)
        |""".stripMargin) shouldBe 2
  }
}
