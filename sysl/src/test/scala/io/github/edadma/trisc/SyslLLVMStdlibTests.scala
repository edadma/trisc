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

  "std.strings split output" in {
    llvmOutputWithStd(
      """import std.strings.*
        |
        |main()
        |    val parts = split("hello world", " ")
        |    puts(parts[0])
        |""".stripMargin) shouldBe "hello"
  }

  "std.strings join" in {
    llvmOutputWithStd(
      """import std.strings.*
        |
        |main()
        |    val parts = split("hello world", " ")
        |    puts(join(parts, "-"))
        |""".stripMargin) shouldBe "hello-world"
  }

  "std.strings fields" in {
    llvmExitWithStd(
      """import std.strings.*
        |
        |main() -> int
        |    val parts = fields("  hello  world  ")
        |    len(parts)
        |""".stripMargin) shouldBe 2
  }

  // ===== std.strconv =====

  "std.strconv format_bool true" in {
    llvmOutputWithStd(
      """import std.strconv.*
        |
        |main()
        |    puts(format_bool(true))
        |""".stripMargin) shouldBe "true"
  }

  "std.strconv format_bool false" in {
    llvmOutputWithStd(
      """import std.strconv.*
        |
        |main()
        |    puts(format_bool(false))
        |""".stripMargin) shouldBe "false"
  }

  "std.strconv format_int" in {
    llvmOutputWithStd(
      """import std.strconv.*
        |
        |main()
        |    puts(format_int(42))
        |""".stripMargin) shouldBe "42"
  }

  "std.strconv format_int negative" in {
    llvmOutputWithStd(
      """import std.strconv.*
        |
        |main()
        |    puts(format_int(-123))
        |""".stripMargin) shouldBe "-123"
  }

  "std.strconv format_int_base hex" in {
    llvmOutputWithStd(
      """import std.strconv.*
        |
        |main()
        |    puts(format_int_base(255, 16))
        |""".stripMargin) shouldBe "ff"
  }

  "std.strconv parse_int" in {
    llvmExitWithStd(
      """import std.strconv.*
        |import std.result.*
        |
        |main() -> int
        |    val r = parse_int("42")
        |    r match
        |        Ok(v) -> v
        |        Err(_) -> -1
        |""".stripMargin) shouldBe 42
  }

  "std.strconv parse_bool" in {
    llvmExitWithStd(
      """import std.strconv.*
        |import std.result.*
        |
        |main() -> int
        |    val r = parse_bool("true")
        |    r match
        |        Ok(v) -> if v then 42 else 0
        |        Err(_) -> -1
        |""".stripMargin) shouldBe 42
  }

  // ===== std.encoding.hex =====

  "std.encoding.hex encoded_len" in {
    llvmExitWithStd(
      """import std.encoding.hex.*
        |
        |main() -> int
        |    encoded_len(3)
        |""".stripMargin) shouldBe 6
  }

  "std.encoding.hex encode_to_string" in {
    llvmOutputWithStd(
      """import std.encoding.hex.*
        |
        |main()
        |    val src = new [3]byte
        |    src[0] = byte(0xDE)
        |    src[1] = byte(0xAD)
        |    src[2] = byte(0x42)
        |    puts(encode_to_string(src[:]))
        |""".stripMargin) shouldBe "dead42"
  }

  // ===== std.builder =====

  "std.builder new_builder and write" in {
    llvmOutputWithStd(
      """import std.builder.*
        |
        |main()
        |    var b = new_builder()
        |    b.write("hello")
        |    b.write(" ")
        |    b.write("world")
        |    puts(b.to_str())
        |""".stripMargin) shouldBe "hello world"
  }

  "std.builder write_byte" in {
    llvmOutputWithStd(
      """import std.builder.*
        |
        |main()
        |    var b = new_builder()
        |    b.write_byte(byte('A'))
        |    b.write_byte(byte('B'))
        |    b.write_byte(byte('C'))
        |    puts(b.to_str())
        |""".stripMargin) shouldBe "ABC"
  }

  "std.builder write_int" in {
    llvmOutputWithStd(
      """import std.builder.*
        |
        |main()
        |    var b = new_builder()
        |    b.write("n=")
        |    b.write_int(42)
        |    puts(b.to_str())
        |""".stripMargin) shouldBe "n=42"
  }

  "std.builder len" in {
    llvmExitWithStd(
      """import std.builder.*
        |
        |main() -> int
        |    var b = new_builder()
        |    b.write("hello")
        |    b.len()
        |""".stripMargin) shouldBe 5
  }

  "std.builder reset" in {
    llvmOutputWithStd(
      """import std.builder.*
        |
        |main()
        |    var b = new_builder()
        |    b.write("old")
        |    b.reset()
        |    b.write("new")
        |    puts(b.to_str())
        |""".stripMargin) shouldBe "new"
  }

  "std.builder write_bool" in {
    llvmOutputWithStd(
      """import std.builder.*
        |
        |main()
        |    var b = new_builder()
        |    b.write_bool(true)
        |    b.write(" ")
        |    b.write_bool(false)
        |    puts(b.to_str())
        |""".stripMargin) shouldBe "true false"
  }

  // ===== std.encoding.hex (more) =====

  "std.encoding.hex encoded_len/decoded_len" in {
    llvmExitWithStd(
      """import std.encoding.hex.*
        |
        |main() -> int
        |    encoded_len(3) + decoded_len(6)
        |""".stripMargin) shouldBe 9 // 6 + 3
  }

  "std.encoding.hex decode_string" ignore {
    llvmExitWithStd(
      """import std.encoding.hex.*
        |
        |main() -> int
        |    val out, ok = decode_string("48454c4c4f")
        |    if ok then len(out) else -1
        |""".stripMargin) shouldBe 5
  }

  "std.encoding.hex roundtrip" ignore {
    llvmOutputWithStd(
      """import std.encoding.hex.*
        |
        |main()
        |    val src = new [5]byte
        |    src[0] = byte(72)
        |    src[1] = byte(69)
        |    src[2] = byte(76)
        |    src[3] = byte(76)
        |    src[4] = byte(79)
        |    val hex = encode_to_string(src[:])
        |    val decoded, ok = decode_string(hex)
        |    if ok
        |        puts(string(decoded))
        |""".stripMargin) shouldBe "HELLO"
  }

  // ===== std.utf8 =====

  "std.utf8 rune_len" in {
    llvmExitWithStd(
      """import std.utf8.*
        |
        |main() -> int
        |    rune_len(char(65)) + rune_len(char(0x80)) + rune_len(char(0x800)) + rune_len(char(0x10000))
        |""".stripMargin) shouldBe 10 // 1 + 2 + 3 + 4
  }

  "std.utf8 valid_rune" in {
    llvmExitWithStd(
      """import std.utf8.*
        |
        |main() -> int
        |    if valid_rune(char(65)) && !valid_rune(char(0x110000)) then 42 else 0
        |""".stripMargin) shouldBe 42
  }

  "std.utf8 rune_start" in {
    llvmExitWithStd(
      """import std.utf8.*
        |
        |main() -> int
        |    if rune_start(byte(0xC0)) && !rune_start(byte(0x80)) then 42 else 0
        |""".stripMargin) shouldBe 42
  }

  "std.utf8 rune_count ascii" in {
    llvmExitWithStd(
      """import std.utf8.*
        |
        |main() -> int
        |    val buf = new [5]byte
        |    buf[0] = byte('h')
        |    buf[1] = byte('e')
        |    buf[2] = byte('l')
        |    buf[3] = byte('l')
        |    buf[4] = byte('o')
        |    rune_count(buf[:])
        |""".stripMargin) shouldBe 5
  }

  "std.utf8 encode_rune" in {
    llvmExitWithStd(
      """import std.utf8.*
        |
        |main() -> int
        |    val buf = new [4]byte
        |    val n = encode_rune(buf[:], char(0xE4))
        |    n
        |""".stripMargin) shouldBe 2 // 0xE4 = ä, 2-byte UTF-8
  }

  "std.utf8 decode_rune" in {
    llvmExitWithStd(
      """import std.utf8.*
        |
        |main() -> int
        |    val buf = new [4]byte
        |    val n = encode_rune(buf[:], char(65))
        |    val r, sz = decode_rune(buf[:])
        |    int(r) + sz
        |""".stripMargin) shouldBe 66 // 65 + 1
  }
}
