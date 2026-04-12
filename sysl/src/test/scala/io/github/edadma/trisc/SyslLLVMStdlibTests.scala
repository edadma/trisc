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

  "std.cmp Eq trait" in {
    llvmExitWithStd(
      """import std.cmp.*
        |
        |struct Vec2
        |    x: int
        |    y: int
        |
        |impl Eq[Vec2]
        |    eq(a: Vec2, b: Vec2) -> bool = a.x == b.x && a.y == b.y
        |
        |main() -> int
        |    val a = Vec2(1, 2)
        |    val b = Vec2(1, 2)
        |    val c = Vec2(3, 4)
        |    if Eq.eq(a, b) && Eq.ne(a, c) then 42 else 0
        |""".stripMargin) shouldBe 42
  }

  "std.cmp Ord trait" in {
    llvmExitWithStd(
      """import std.cmp.*
        |
        |struct Score
        |    value: int
        |
        |impl Ord[Score]
        |    cmp(a: Score, b: Score) -> int = a.value - b.value
        |
        |main() -> int
        |    val a = Score(10)
        |    val b = Score(20)
        |    if Ord.lt(a, b) && Ord.ge(b, a) then 42 else 0
        |""".stripMargin) shouldBe 42
  }

  "std.cmp Eq default ne" in {
    llvmExitWithStd(
      """import std.cmp.*
        |
        |struct Pair
        |    a: int
        |    b: int
        |
        |impl Eq[Pair]
        |    eq(a: Pair, b: Pair) -> bool = a.a == b.a && a.b == b.b
        |
        |main() -> int
        |    val x = Pair(1, 2)
        |    val y = Pair(1, 3)
        |    if Eq.ne(x, y) then 42 else 0
        |""".stripMargin) shouldBe 42
  }

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

  "std.encoding.hex decode_string" in {
    llvmExitWithStd(
      """import std.encoding.hex.*
        |
        |main() -> int
        |    val out, ok = decode_string("48454c4c4f")
        |    if ok then len(out) else -1
        |""".stripMargin) shouldBe 5
  }

  "std.encoding.hex roundtrip" in {
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

  // ===== std.bytes =====

  "std.bytes as_bytes roundtrip" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    val b = as_bytes("hello")
        |    if string(b) == "hello" then len(b) else 0
        |""".stripMargin) shouldBe 5
  }

  "std.bytes has_prefix" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    var n = 0
        |    if has_prefix(as_bytes("hello world"), as_bytes("hello")) then n = n + 1
        |    if !has_prefix(as_bytes("hello"), as_bytes("world")) then n = n + 1
        |    if has_prefix(as_bytes("hi"), as_bytes("")) then n = n + 1
        |    if !has_prefix(as_bytes("hi"), as_bytes("hello")) then n = n + 1
        |    n
        |""".stripMargin) shouldBe 4
  }

  "std.bytes has_suffix" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    var n = 0
        |    if has_suffix(as_bytes("hello world"), as_bytes("world")) then n = n + 1
        |    if !has_suffix(as_bytes("hello"), as_bytes("world")) then n = n + 1
        |    n
        |""".stripMargin) shouldBe 2
  }

  "std.bytes index" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    var n = 0
        |    if index(as_bytes("hello world"), as_bytes("world")) == 6 then n = n + 1
        |    if index(as_bytes("hello"), as_bytes("xyz")) == -1 then n = n + 1
        |    n
        |""".stripMargin) shouldBe 2
  }

  "std.bytes last_index" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    if last_index(as_bytes("banana"), as_bytes("an")) == 3 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "std.bytes contains" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    var n = 0
        |    if contains(as_bytes("hello world"), as_bytes("world")) then n = n + 1
        |    if !contains(as_bytes("hello"), as_bytes("xyz")) then n = n + 1
        |    n
        |""".stripMargin) shouldBe 2
  }

  "std.bytes count" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    count(as_bytes("banana"), as_bytes("an"))
        |""".stripMargin) shouldBe 2
  }

  "std.bytes index_byte / last_index_byte" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    var n = 0
        |    if index_byte(as_bytes("hello"), byte(108)) == 2 then n = n + 1
        |    if last_index_byte(as_bytes("hello"), byte(108)) == 3 then n = n + 1
        |    n
        |""".stripMargin) shouldBe 2
  }

  "std.bytes subslice" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    val b = subslice(as_bytes("hello world"), 6, 11)
        |    if string(b) == "world" then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "std.bytes trim_space" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    val b = trim_space(as_bytes("  hello  "))
        |    if string(b) == "hello" then len(b) else 0
        |""".stripMargin) shouldBe 5
  }

  "std.bytes to_upper / to_lower" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    var n = 0
        |    if string(to_upper(as_bytes("hello"))) == "HELLO" then n = n + 1
        |    if string(to_lower(as_bytes("HELLO"))) == "hello" then n = n + 1
        |    n
        |""".stripMargin) shouldBe 2
  }

  "std.bytes repeat" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    val b = repeat(as_bytes("ab"), 3)
        |    if len(b) == 6 && b[0] == int('a') && b[5] == int('b') then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "std.bytes replace_all" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    val b = replace_all(as_bytes("aXbXc"), as_bytes("X"), as_bytes("--"))
        |    if string(b) == "a--b--c" then len(b) else 0
        |""".stripMargin) shouldBe 7
  }

  "std.bytes split" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    val parts = split(as_bytes("a,b,c"), as_bytes(","))
        |    len(parts)
        |""".stripMargin) shouldBe 3
  }

  "std.bytes join" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |main() -> int
        |    val parts = split(as_bytes("a,b,c"), as_bytes(","))
        |    val joined = join(parts, as_bytes("-"))
        |    len(joined)
        |""".stripMargin) shouldBe 5
  }

  // ===== std.heap =====

  "std.heap push and pop" in {
    llvmExitWithStd(
      """import std.heap.*
        |
        |main() -> int
        |    val less: (int, int) -> bool = (a, b) -> a < b
        |    var h = new_min_heap[int](less)
        |    h.push(30)
        |    h.push(10)
        |    h.push(20)
        |    val a = h.pop()
        |    val b = h.pop()
        |    val c = h.pop()
        |    a + b * 10 + c * 100
        |""".stripMargin) shouldBe (3210 % 256) // 10 + 200 + 3000
  }

  "std.heap len and empty" in {
    llvmExitWithStd(
      """import std.heap.*
        |
        |main() -> int
        |    val less: (int, int) -> bool = (a, b) -> a < b
        |    var h = new_min_heap[int](less)
        |    var n = 0
        |    if h.empty() then n = n + 1
        |    h.push(42)
        |    if h.len() == 1 then n = n + 1
        |    if !h.empty() then n = n + 1
        |    n
        |""".stripMargin) shouldBe 3
  }

  "std.heap peek" in {
    llvmExitWithStd(
      """import std.heap.*
        |
        |main() -> int
        |    val less: (int, int) -> bool = (a, b) -> a < b
        |    var h = new_min_heap[int](less)
        |    h.push(30)
        |    h.push(10)
        |    h.push(20)
        |    h.peek()
        |""".stripMargin) shouldBe 10
  }

  "std.heap ordering (many elements)" in {
    llvmExitWithStd(
      """import std.heap.*
        |
        |main() -> int
        |    val less: (int, int) -> bool = (a, b) -> a < b
        |    var h = new_min_heap[int](less)
        |    h.push(5)
        |    h.push(3)
        |    h.push(7)
        |    h.push(1)
        |    h.push(4)
        |    var sum = 0
        |    var prev = -1
        |    var ordered = true
        |    while !h.empty()
        |        val v = h.pop()
        |        if v < prev then ordered = false
        |        prev = v
        |        sum = sum + v
        |    if ordered then sum else -1
        |""".stripMargin) shouldBe 20
  }

  // ===== std.slices =====

  "std.slices equal" in { // TODO: generic instantiation collision with test section
    llvmExitWithStd(
      """import std.slices.*
        |
        |main() -> int
        |    val a = new [3]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    val b = new [3]int
        |    b[0] = 1; b[1] = 2; b[2] = 3
        |    val c = new [3]int
        |    c[0] = 1; c[1] = 2; c[2] = 4
        |    var n = 0
        |    if equal(a[:], b[:]) then n = n + 1
        |    if !equal(a[:], c[:]) then n = n + 1
        |    n
        |""".stripMargin) shouldBe 2
  }

  "std.slices contains" in {
    llvmExitWithStd(
      """import std.slices.*
        |
        |main() -> int
        |    val a = new [4]int
        |    a[0] = 10; a[1] = 20; a[2] = 30; a[3] = 40
        |    var n = 0
        |    if contains(a[:], 20) then n = n + 1
        |    if !contains(a[:], 99) then n = n + 1
        |    n
        |""".stripMargin) shouldBe 2
  }

  "std.slices index" in {
    llvmExitWithStd(
      """import std.slices.*
        |
        |main() -> int
        |    val a = new [4]int
        |    a[0] = 10; a[1] = 20; a[2] = 30; a[3] = 40
        |    var n = 0
        |    if index(a[:], 30) == 2 then n = n + 1
        |    if index(a[:], 99) == -1 then n = n + 1
        |    n
        |""".stripMargin) shouldBe 2
  }

  "std.slices clone" in {
    llvmExitWithStd(
      """import std.slices.*
        |
        |main() -> int
        |    val a = new [3]int
        |    a[0] = 10; a[1] = 20; a[2] = 30
        |    val b = clone(a[:])
        |    b[0] + b[1] + b[2]
        |""".stripMargin) shouldBe 60
  }

  "std.slices reverse" in {
    llvmExitWithStd(
      """import std.slices.*
        |
        |main() -> int
        |    val a = new [3]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    val b = reverse(a[:])
        |    b[0] * 100 + b[1] * 10 + b[2]
        |""".stripMargin) shouldBe (321 % 256)
  }

  "std.slices fill" in {
    llvmExitWithStd(
      """import std.slices.*
        |
        |main() -> int
        |    val a = new [3]int
        |    a[0] = 0; a[1] = 0; a[2] = 0
        |    fill(a[:], 7)
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 21
  }

  "std.slices concat" in {
    llvmExitWithStd(
      """import std.slices.*
        |
        |main() -> int
        |    val a = new [2]int
        |    a[0] = 1; a[1] = 2
        |    val b = new [3]int
        |    b[0] = 3; b[1] = 4; b[2] = 5
        |    val c = concat(a[:], b[:])
        |    len(c) * 100 + c[0] + c[4]
        |""".stripMargin) shouldBe (506 % 256)
  }

  "std.slices min/max" in {
    llvmExitWithStd(
      """import std.slices.*
        |
        |main() -> int
        |    val a = new [5]int
        |    a[0] = 30; a[1] = 10; a[2] = 50; a[3] = 20; a[4] = 40
        |    min(a[:]) + max(a[:])
        |""".stripMargin) shouldBe 60
  }

  // ===== std.sort =====

  "std.sort sort_int basic" in {
    llvmExitWithStd(
      """import std.sort.*
        |
        |main() -> int
        |    val a = new [5]int
        |    a[0] = 3; a[1] = 1; a[2] = 5; a[3] = 2; a[4] = 4
        |    sort_int(a[:])
        |    // Check sorted order: verify each position
        |    var ok = 1
        |    if a[0] != 1 then ok = 0
        |    if a[1] != 2 then ok = 0
        |    if a[2] != 3 then ok = 0
        |    if a[3] != 4 then ok = 0
        |    if a[4] != 5 then ok = 0
        |    ok
        |""".stripMargin) shouldBe 1
  }

  "std.sort sort_int already sorted" in {
    llvmExitWithStd(
      """import std.sort.*
        |
        |main() -> int
        |    val a = new [4]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3; a[3] = 4
        |    sort_int(a[:])
        |    if is_sorted(a[:], cmp_int) then a[0] + a[3] else -1
        |""".stripMargin) shouldBe 5
  }

  "std.sort sort_int descending" in {
    llvmExitWithStd(
      """import std.sort.*
        |
        |main() -> int
        |    val a = new [4]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3; a[3] = 4
        |    sort_int_desc(a[:])
        |    a[0] * 1000 + a[1] * 100 + a[2] * 10 + a[3]
        |""".stripMargin) shouldBe (4321 % 256)
  }

  "std.sort is_sorted" in {
    llvmExitWithStd(
      """import std.sort.*
        |
        |main() -> int
        |    val a = new [3]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    val b = new [3]int
        |    b[0] = 3; b[1] = 1; b[2] = 2
        |    var n = 0
        |    if is_sorted(a[:], cmp_int) then n = n + 1
        |    if !is_sorted(b[:], cmp_int) then n = n + 1
        |    n
        |""".stripMargin) shouldBe 2
  }

  "std.sort sort_by custom comparator" in {
    llvmExitWithStd(
      """import std.sort.*
        |
        |main() -> int
        |    val a = new [4]int
        |    a[0] = 10; a[1] = 30; a[2] = 20; a[3] = 40
        |    val desc: (int, int) -> int = (a, b) -> b - a
        |    sort_by(a[:], desc)
        |    a[0] * 1000 + a[1] * 100 + a[2] * 10 + a[3]
        |""".stripMargin) shouldBe (43210 % 256)
  }

  "std.sort sort_int duplicates" in {
    llvmExitWithStd(
      """import std.sort.*
        |
        |main() -> int
        |    val a = new [5]int
        |    a[0] = 3; a[1] = 1; a[2] = 3; a[3] = 2; a[4] = 1
        |    sort_int(a[:])
        |    a[0] * 10000 + a[1] * 1000 + a[2] * 100 + a[3] * 10 + a[4]
        |""".stripMargin) shouldBe (11233 % 256)
  }

  // ===== std.unicode =====

  "std.unicode is_letter" in {
    llvmExitWithStd(
      """import std.unicode.*
        |
        |main() -> int
        |    var n = 0
        |    if is_letter(65) then n = n + 1
        |    if is_letter(122) then n = n + 1
        |    if !is_letter(48) then n = n + 1
        |    if !is_letter(32) then n = n + 1
        |    n
        |""".stripMargin) shouldBe 4
  }

  "std.unicode is_digit" in {
    llvmExitWithStd(
      """import std.unicode.*
        |
        |main() -> int
        |    var n = 0
        |    if is_digit(48) then n = n + 1
        |    if is_digit(57) then n = n + 1
        |    if !is_digit(65) then n = n + 1
        |    n
        |""".stripMargin) shouldBe 3
  }

  "std.unicode is_space" in {
    llvmExitWithStd(
      """import std.unicode.*
        |
        |main() -> int
        |    var n = 0
        |    if is_space(32) then n = n + 1
        |    if is_space(9) then n = n + 1
        |    if is_space(10) then n = n + 1
        |    if !is_space(65) then n = n + 1
        |    n
        |""".stripMargin) shouldBe 4
  }

  "std.unicode classification" in {
    llvmExitWithStd(
      """import std.unicode.*
        |
        |main() -> int
        |    var n = 0
        |    if is_upper(65) then n = n + 1
        |    if is_lower(97) then n = n + 1
        |    if is_alnum(48) then n = n + 1
        |    if is_punct(33) then n = n + 1
        |    if is_print(32) then n = n + 1
        |    if is_control(0) then n = n + 1
        |    if is_hex_digit(102) then n = n + 1
        |    n
        |""".stripMargin) shouldBe 7
  }

  "std.unicode case conversion" in {
    llvmExitWithStd(
      """import std.unicode.*
        |
        |main() -> int
        |    var n = 0
        |    if to_upper(97) == 65 then n = n + 1
        |    if to_lower(65) == 97 then n = n + 1
        |    if to_upper(48) == 48 then n = n + 1
        |    n
        |""".stripMargin) shouldBe 3
  }

  // ===== std.mem =====

  "std.mem copy" in {
    llvmExitWithStd(
      """import std.mem.*
        |
        |main() -> int
        |    val src = new [4]byte
        |    val dst = new [4]byte
        |    src[0] = 10; src[1] = 20; src[2] = 30; src[3] = 40
        |    val n = copy(dst[:], src[:])
        |    if n != 4 then return 0
        |    if dst[0] != 10 then return 0
        |    if dst[3] != 40 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.mem set and zero" in {
    llvmExitWithStd(
      """import std.mem.*
        |
        |main() -> int
        |    val buf = new [4]byte
        |    set(buf[:], 0x7F)
        |    if buf[0] != 0x7F then return 0
        |    if buf[3] != 0x7F then return 0
        |    zero(buf[:])
        |    if buf[0] != 0 then return 0
        |    if buf[3] != 0 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.mem eq and cmp" in {
    llvmExitWithStd(
      """import std.mem.*
        |
        |main() -> int
        |    val a = new [3]byte
        |    val b = new [3]byte
        |    a[0] = 1; a[1] = 2; a[2] = 3
        |    b[0] = 1; b[1] = 2; b[2] = 3
        |    var n = 0
        |    if eq(a[:], b[:]) then n = n + 1
        |    if cmp(a[:], b[:]) == 0 then n = n + 1
        |    b[2] = 4
        |    if !eq(a[:], b[:]) then n = n + 1
        |    if cmp(a[:], b[:]) < 0 then n = n + 1
        |    n
        |""".stripMargin) shouldBe 4
  }

  "std.mem index_byte" in {
    llvmExitWithStd(
      """import std.mem.*
        |
        |main() -> int
        |    val s = new [4]byte
        |    s[0] = 1; s[1] = 2; s[2] = 1; s[3] = 2
        |    var n = 0
        |    if index_byte(s[:], 2) == 1 then n = n + 1
        |    if index_byte(s[:], 99) == -1 then n = n + 1
        |    n
        |""".stripMargin) shouldBe 2
  }

  // ===== std.errors =====

  "std.errors new and msg" in {
    llvmOutputWithStd(
      """import std.errors.*
        |
        |main()
        |    val e = new_error("something failed")
        |    puts(error_msg(e))
        |""".stripMargin) shouldBe "something failed"
  }

  "std.errors equal and sentinels" in {
    llvmExitWithStd(
      """import std.errors.*
        |
        |main() -> int
        |    var n = 0
        |    val a = new_error("timeout")
        |    val b = new_error("timeout")
        |    val c = new_error("not found")
        |    if errors_equal(a, b) then n = n + 1
        |    if !errors_equal(a, c) then n = n + 1
        |    if errors_equal(err_eof(), err_eof()) then n = n + 1
        |    if !errors_equal(err_eof(), err_not_found()) then n = n + 1
        |    n
        |""".stripMargin) shouldBe 4
  }

  "std.errors sentinel messages" in {
    llvmOutputWithStd(
      """import std.errors.*
        |
        |main()
        |    puts(error_msg(err_eof()))
        |    puts(error_msg(err_not_found()))
        |    puts(error_msg(err_permission()))
        |""".stripMargin) shouldBe "EOF\nnot found\npermission denied"
  }

  // ===== std.path =====

  "std.path base" in {
    llvmOutputWithStd(
      """import std.path.*
        |
        |main()
        |    puts(base("/a/b/c"))
        |    puts(base("/"))
        |    puts(base(""))
        |    puts(base("a"))
        |""".stripMargin) shouldBe "c\n/\n.\na"
  }

  "std.path dir" in {
    llvmOutputWithStd(
      """import std.path.*
        |
        |main()
        |    puts(dir("/a/b/c"))
        |    puts(dir("/"))
        |    puts(dir(""))
        |    puts(dir("a"))
        |""".stripMargin) shouldBe "/a/b\n/\n.\n."
  }

  "std.path ext" in {
    llvmOutputWithStd(
      """import std.path.*
        |
        |main()
        |    puts(ext("foo.txt"))
        |    puts(ext("foo"))
        |    puts(ext("a/b.tar.gz"))
        |""".stripMargin) shouldBe ".txt\n\n.gz"
  }

  "std.path is_abs" in {
    llvmExitWithStd(
      """import std.path.*
        |
        |main() -> int
        |    var n = 0
        |    if is_abs("/foo") then n = n + 1
        |    if !is_abs("foo") then n = n + 1
        |    if !is_abs("") then n = n + 1
        |    n
        |""".stripMargin) shouldBe 3
  }

  "std.path join" in {
    llvmOutputWithStd(
      """import std.path.*
        |
        |main()
        |    puts(join("a", "b"))
        |    puts(join("/a/", "/b"))
        |    puts(join("", "b"))
        |""".stripMargin) shouldBe "a/b\n/a/b\nb"
  }

  "std.path clean" in {
    llvmOutputWithStd(
      """import std.path.*
        |
        |main()
        |    puts(clean("a//b"))
        |    puts(clean("/a/"))
        |    puts(clean("/"))
        |    puts(clean(""))
        |""".stripMargin) shouldBe "a/b\n/a\n/\n."
  }

  // ===== std.rand =====

  "std.rand deterministic" in {
    llvmExitWithStd(
      """import std.rand.*
        |
        |main() -> int
        |    seed(42i64)
        |    val a = next()
        |    val b = next()
        |    seed(42i64)
        |    val c = next()
        |    val d = next()
        |    var n = 0
        |    if a == c then n = n + 1
        |    if b == d then n = n + 1
        |    n
        |""".stripMargin) shouldBe 2
  }

  "std.rand next_range bounds" in {
    llvmExitWithStd(
      """import std.rand.*
        |
        |main() -> int
        |    seed(456i64)
        |    var ok = true
        |    for i in 0..<100
        |        val r = next_range(10, 20)
        |        if r < 10 then ok = false
        |        if r >= 20 then ok = false
        |    if ok then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "std.rand shuffle preserves sum" in {
    llvmExitWithStd(
      """import std.rand.*
        |
        |main() -> int
        |    seed(42i64)
        |    val s = new [5]int
        |    s[0] = 10; s[1] = 20; s[2] = 30; s[3] = 40; s[4] = 50
        |    shuffle(s[:])
        |    var sum = 0
        |    for i in 0..<5
        |        sum = sum + s[i]
        |    sum
        |""".stripMargin) shouldBe (150 % 256)
  }

  // ===== std.debug =====

  "std.debug assert true" in {
    llvmExitWithStd(
      """import std.debug.*
        |
        |main() -> int
        |    assert(true, "should not fire")
        |    assert(1 + 1 == 2, "math works")
        |    1
        |""".stripMargin) shouldBe 1
  }

  // ===== std.option =====

  "std.option is_some and is_none" in {
    llvmExitWithStd(
      """import std.option.*
        |
        |main() -> int
        |    val s: Option[int] = Some(42)
        |    val n: Option[int] = None
        |    var count = 0
        |    if is_some(s) then count = count + 1
        |    if !is_none(s) then count = count + 1
        |    if is_none(n) then count = count + 1
        |    if !is_some(n) then count = count + 1
        |    count
        |""".stripMargin) shouldBe 4
  }

  "std.option unwrap and unwrap_or" in {
    llvmExitWithStd(
      """import std.option.*
        |
        |main() -> int
        |    val s: Option[int] = Some(42)
        |    val n: Option[int] = None
        |    var count = 0
        |    if unwrap(s) == 42 then count = count + 1
        |    if unwrap_or(s, 0) == 42 then count = count + 1
        |    if unwrap_or(n, 99) == 99 then count = count + 1
        |    count
        |""".stripMargin) shouldBe 3
  }

  "std.option match" in {
    llvmExitWithStd(
      """import std.option.*
        |
        |main() -> int
        |    val o: Option[int] = Some(7)
        |    val doubled = o match
        |        Some(v) -> v * 2
        |        None -> 0
        |    val n: Option[int] = None
        |    val def_val = n match
        |        Some(v) -> v
        |        None -> -1
        |    var count = 0
        |    if doubled == 14 then count = count + 1
        |    if def_val == -1 then count = count + 1
        |    count
        |""".stripMargin) shouldBe 2
  }

  // ===== std.result =====

  "std.result is_ok and is_err" in {
    llvmExitWithStd(
      """import std.result.*
        |
        |main() -> int
        |    val ok: Result[int, string] = Ok(42)
        |    val er: Result[int, string] = Err("fail")
        |    var count = 0
        |    if is_ok(ok) then count = count + 1
        |    if !is_err(ok) then count = count + 1
        |    if is_err(er) then count = count + 1
        |    if !is_ok(er) then count = count + 1
        |    count
        |""".stripMargin) shouldBe 4
  }

  "std.result unwrap and unwrap_or" in {
    llvmExitWithStd(
      """import std.result.*
        |
        |main() -> int
        |    val ok: Result[int, string] = Ok(42)
        |    val er: Result[int, string] = Err("fail")
        |    var count = 0
        |    if unwrap(ok) == 42 then count = count + 1
        |    if unwrap_or(ok, 0) == 42 then count = count + 1
        |    if unwrap_or(er, 99) == 99 then count = count + 1
        |    count
        |""".stripMargin) shouldBe 3
  }

  // ===== std.testing =====

  "std.testing assert_eq" in {
    llvmExitWithStd(
      """import std.testing.*
        |
        |main() -> int
        |    assert_eq(42, 42, "int eq")
        |    assert_ne(1, 2, "int ne")
        |    assert_in_range(5, 1, 10, "in range")
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.testing assert_eq_str" in {
    llvmExitWithStd(
      """import std.testing.*
        |
        |main() -> int
        |    assert_eq_str("hello", "hello", "str eq")
        |    assert_ne_str("a", "b", "str ne")
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.testing assert_eq_bool_byte" in {
    llvmExitWithStd(
      """import std.testing.*
        |
        |main() -> int
        |    assert_eq_bool(true, true, "bool eq")
        |    assert_eq_byte(byte(65), byte(65), "byte eq")
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.testing assert_slice_eq" in {
    llvmExitWithStd(
      """import std.testing.*
        |
        |main() -> int
        |    val a = new [3]byte
        |    val b = new [3]byte
        |    a[0] = 1; a[1] = 2; a[2] = 3
        |    b[0] = 1; b[1] = 2; b[2] = 3
        |    assert_slice_eq(a[:], b[:], "slices eq")
        |    1
        |""".stripMargin) shouldBe 1
  }

  // ===== std.io =====

  "std.io ByteReader basic" in {
    llvmExitWithStd(
      """import std.io.*
        |
        |main() -> int
        |    val data = new [5]byte
        |    data[0] = 72; data[1] = 101; data[2] = 108; data[3] = 108; data[4] = 111
        |    var r = ByteReader(data[:], 0)
        |    val buf = new [3]byte
        |    val n = r.read(buf[:])
        |    if n != 3 then return 0
        |    if buf[0] != 72 then return 0
        |    if buf[1] != 101 then return 0
        |    if buf[2] != 108 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.io ByteReader eof" in {
    llvmExitWithStd(
      """import std.io.*
        |
        |main() -> int
        |    val data = new [2]byte
        |    data[0] = 65; data[1] = 66
        |    var r = ByteReader(data[:], 0)
        |    val buf = new [10]byte
        |    val n1 = r.read(buf[:])
        |    val n2 = r.read(buf[:])
        |    if n1 != 2 then return 0
        |    if n2 != 0 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.io ByteWriter basic" in {
    llvmExitWithStd(
      """import std.io.*
        |
        |main() -> int
        |    var w = new_writer()
        |    val data = new [3]byte
        |    data[0] = 65; data[1] = 66; data[2] = 67
        |    val n = w.write(data[:])
        |    if n != 3 then return 0
        |    val out = w.bytes()
        |    if len(out) != 3 then return 0
        |    if out[0] != 65 then return 0
        |    if out[2] != 67 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.io ByteWriter multiple writes" in {
    llvmExitWithStd(
      """import std.io.*
        |
        |main() -> int
        |    var w = new_writer()
        |    val d1 = new [2]byte
        |    d1[0] = 65; d1[1] = 66
        |    w.write(d1[:])
        |    val d2 = new [2]byte
        |    d2[0] = 67; d2[1] = 68
        |    w.write(d2[:])
        |    val out = w.bytes()
        |    if len(out) != 4 then return 0
        |    if out[0] != 65 then return 0
        |    if out[3] != 68 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  // ===== std.crypto.sha256 =====

  "std.crypto.sha256 empty" in {
    llvmOutputWithStd(
      """import std.crypto.sha256.*
        |import std.encoding.hex.*
        |
        |main()
        |    val data = new [0]byte
        |    val out = new [32]byte
        |    sha256(data[:], out[:])
        |    puts(encode_to_string(out[:]))
        |""".stripMargin) shouldBe "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  }

  "std.crypto.sha256 abc" in {
    llvmOutputWithStd(
      """import std.crypto.sha256.*
        |import std.encoding.hex.*
        |
        |main()
        |    val msg = new [3]byte
        |    msg[0] = 97; msg[1] = 98; msg[2] = 99
        |    val out = new [32]byte
        |    sha256(msg[:], out[:])
        |    puts(encode_to_string(out[:]))
        |""".stripMargin) shouldBe "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  }

  "std.crypto.sha256 deterministic" in {
    llvmExitWithStd(
      """import std.crypto.sha256.*
        |
        |main() -> int
        |    val msg = new [3]byte
        |    msg[0] = 97; msg[1] = 98; msg[2] = 99
        |    val out1 = new [32]byte
        |    val out2 = new [32]byte
        |    sha256(msg[:], out1[:])
        |    sha256(msg[:], out2[:])
        |    var same = true
        |    for i in 0..<32
        |        if out1[i] != out2[i] then same = false
        |    if same then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== std.encoding.binary =====

  "std.encoding.binary u32_be" in {
    llvmExitWithStd(
      """import std.encoding.binary.*
        |
        |main() -> int
        |    val buf = new [4]byte
        |    put_u32_be_at(buf[:], 0, 0x41424344u32)
        |    var n = 0
        |    if buf[0] == 0x41 then n = n + 1
        |    if buf[1] == 0x42 then n = n + 1
        |    if buf[2] == 0x43 then n = n + 1
        |    if buf[3] == 0x44 then n = n + 1
        |    val v = u32_be_at(buf[:], 0)
        |    if v == 0x41424344u32 then n = n + 1
        |    n
        |""".stripMargin) shouldBe 5
  }

  // ===== std.text.tabwriter =====

  "std.text.tabwriter basic" in {
    llvmOutputWithStd(
      """import std.text.tabwriter.*
        |
        |main()
        |    var tw = new_tab_writer(1, 4, 1, byte(32), 0)
        |    tw.write("a\tb\n")
        |    tw.write("xxx\ty\n")
        |    tw.flush()
        |    puts(tw.to_str())
        |""".stripMargin) should include ("a")
  }

  // ===== std.bufio =====

  "std.bufio read_byte" in {
    llvmExitWithStd(
      """import std.bufio.*
        |
        |main() -> int
        |    val data = new [3]byte
        |    data[0] = 97; data[1] = 98; data[2] = 99
        |    var r = new_buf_reader(data[:])
        |    val b1, ok1 = r.read_byte()
        |    if !ok1 then return 0
        |    if b1 != 97 then return 0
        |    val b2, ok2 = r.read_byte()
        |    if !ok2 then return 0
        |    if b2 != 98 then return 0
        |    val b3, ok3 = r.read_byte()
        |    if !ok3 then return 0
        |    if b3 != 99 then return 0
        |    val _, ok4 = r.read_byte()
        |    if ok4 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.bufio read_line" in {
    llvmOutputWithStd(
      """import std.bufio.*
        |
        |main()
        |    val data = new [12]byte
        |    val src = "foo\nbar\nbaz\n"
        |    for i in 0..<12
        |        data[i] = byte(src[i])
        |    var r = new_buf_reader(data[:])
        |    val l1, _ = r.read_line()
        |    puts(l1)
        |    val l2, _ = r.read_line()
        |    puts(l2)
        |    val l3, _ = r.read_line()
        |    puts(l3)
        |""".stripMargin) shouldBe "foo\nbar\nbaz"
  }

  "std.bufio peek" in {
    llvmExitWithStd(
      """import std.bufio.*
        |
        |main() -> int
        |    val data = new [3]byte
        |    data[0] = 97; data[1] = 98; data[2] = 99
        |    var r = new_buf_reader(data[:])
        |    val b1, _ = r.peek()
        |    if b1 != 97 then return 0
        |    val b2, _ = r.peek()
        |    if b2 != 97 then return 0
        |    val b3, _ = r.read_byte()
        |    if b3 != 97 then return 0
        |    val b4, _ = r.peek()
        |    if b4 != 98 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.bufio at_eof" in {
    llvmExitWithStd(
      """import std.bufio.*
        |
        |main() -> int
        |    val data = new [2]byte
        |    data[0] = 65; data[1] = 66
        |    var r = new_buf_reader(data[:])
        |    if r.at_eof() then return 0
        |    val _, _ = r.read_byte()
        |    val _, _ = r.read_byte()
        |    if !r.at_eof() then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  // ===== std.crypto.chacha20 =====

  "std.crypto.chacha20 quarter_round" in {
    llvmExitWithStd(
      """import std.crypto.chacha20.*
        |
        |main() -> int
        |    var a: u32 = 0x11111111u32
        |    var b: u32 = 0x01020304u32
        |    var c: u32 = 0x9b8d6f43u32
        |    var d: u32 = 0x01234567u32
        |    quarter_round(&a, &b, &c, &d)
        |    if a != 0xea2a92f4u32 then return 0
        |    if b != 0xcb1cf8ceu32 then return 0
        |    if c != 0x4581472eu32 then return 0
        |    if d != 0x5881c4bbu32 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.crypto.chacha20 block rfc7539" in {
    llvmExitWithStd(
      """import std.crypto.chacha20.*
        |
        |main() -> int
        |    var key: [8]u32
        |    for i in 0..<8
        |        key[i] = 0u32
        |    var nonce: [3]u32
        |    nonce[0] = 0u32; nonce[1] = 0u32; nonce[2] = 0u32
        |    var out: [64]byte
        |    chacha20_block(&key[0], 0u32, &nonce[0], &out[0])
        |    // Expected first 4 bytes: 76 b8 e0 ad
        |    if out[0] != 0x76u8 then return 0
        |    if out[1] != 0xb8u8 then return 0
        |    if out[2] != 0xe0u8 then return 0
        |    if out[3] != 0xadu8 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  // ===== std.crypto.hmac =====

  "std.crypto.hmac rfc4231 tc1" in {
    llvmExitWithStd(
      """import std.crypto.hmac.*
        |
        |main() -> int
        |    var key: [20]byte
        |    for i in 0..<20
        |        key[i] = 0x0bu8
        |    val msg: [8]byte = "Hi There"
        |    var out: [32]byte
        |    hmac_sha256(key[:], msg[:], out[:])
        |    // b0344c61d8db...
        |    if out[0] != 0xb0u8 then return 0
        |    if out[1] != 0x34u8 then return 0
        |    if out[2] != 0x4cu8 then return 0
        |    if out[3] != 0x61u8 then return 0
        |    if out[31] != 0xf7u8 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  // ===== std.crypto.pbkdf2 =====

  "std.crypto.pbkdf2 c1" in {
    llvmExitWithStd(
      """import std.crypto.pbkdf2.*
        |
        |main() -> int
        |    val password: [8]byte = "password"
        |    val salt: [4]byte = "salt"
        |    var out: [32]byte
        |    pbkdf2_hmac_sha256(password[:], salt[:], 1, out[:])
        |    // 120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b
        |    if out[0] != 0x12u8 then return 0
        |    if out[1] != 0x0fu8 then return 0
        |    if out[2] != 0xb6u8 then return 0
        |    if out[3] != 0xcfu8 then return 0
        |    if out[31] != 0x7bu8 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  // ===== std.crypto.aead (ChaCha20-Poly1305) =====

  "std.crypto.aead poly1305" in {
    llvmExitWithStd(
      """import std.crypto.aead.*
        |
        |main() -> int
        |    val key: [32]byte = [0x85, 0xd6, 0xbe, 0x78, 0x57, 0x55, 0x6d, 0x33, 0x7f, 0x44, 0x52, 0xfe, 0x42, 0xd5, 0x06, 0xa8, 0x01, 0x03, 0x80, 0x8a, 0xfb, 0x0d, 0xb2, 0xfd, 0x4a, 0xbf, 0xf6, 0xaf, 0x41, 0x49, 0xf5, 0x1b]
        |    val msg: [34]byte = "Cryptographic Forum Research Group"
        |    var tag: [16]byte
        |    poly1305(tag[:], msg[:], key[:])
        |    // a8061dc1305136c6c22b8baf0c0127a9
        |    if tag[0] != 0xa8u8 then return 0
        |    if tag[1] != 0x06u8 then return 0
        |    if tag[2] != 0x1du8 then return 0
        |    if tag[15] != 0xa9u8 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.crypto.aead roundtrip" in {
    llvmExitWithStd(
      """import std.crypto.aead.*
        |
        |main() -> int
        |    var key: [32]byte
        |    for i in 0..<32
        |        key[i] = byte(i)
        |    var nonce: [12]byte
        |    for i in 0..<12
        |        nonce[i] = byte(i)
        |    val aad: [4]byte = "meta"
        |    val pt: [5]byte = "hello"
        |    var ct: [5]byte
        |    var tag: [16]byte
        |    seal(key[:], nonce[:], aad[:], pt[:], ct[:], tag[:])
        |    var dec: [5]byte
        |    val ok = open(key[:], nonce[:], aad[:], ct[:], tag[:], dec[:])
        |    if !ok then return 0
        |    for i in 0..<5
        |        if dec[i] != pt[i] then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  // ===== std.flag =====

  "std.flag bool short" in {
    llvmExitWithStd(
      """import std.flag.*
        |import std.result.{is_ok, unwrap}
        |
        |main() -> int
        |    var p = new_parser("test", "")
        |    val v = p.bool_flag("v", "verbose", "verbose")
        |    val argv: [1]string = ["-v"]
        |    val r = p.parse(argv[:])
        |    if !is_ok(r) then return 0
        |    if !unwrap(r).get_bool(v) then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.flag bool long" in {
    llvmExitWithStd(
      """import std.flag.*
        |import std.result.{is_ok, unwrap}
        |
        |main() -> int
        |    var p = new_parser("test", "")
        |    val v = p.bool_flag("v", "verbose", "verbose")
        |    val argv: [1]string = ["--verbose"]
        |    val r = p.parse(argv[:])
        |    if !is_ok(r) then return 0
        |    if !unwrap(r).get_bool(v) then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.flag int short with space" in {
    llvmExitWithStd(
      """import std.flag.*
        |import std.result.{is_ok, unwrap}
        |
        |main() -> int
        |    var p = new_parser("test", "")
        |    val port = p.int_flag("p", "port", 8080, "port")
        |    val argv: [2]string = ["-p", "3000"]
        |    val r = p.parse(argv[:])
        |    if !is_ok(r) then return 0
        |    if unwrap(r).get_int(port) != 3000 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.flag int long equals" in {
    llvmExitWithStd(
      """import std.flag.*
        |import std.result.{is_ok, unwrap}
        |
        |main() -> int
        |    var p = new_parser("test", "")
        |    val port = p.int_flag("p", "port", 8080, "port")
        |    val argv: [1]string = ["--port=3000"]
        |    val r = p.parse(argv[:])
        |    if !is_ok(r) then return 0
        |    if unwrap(r).get_int(port) != 3000 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.flag int default" in {
    llvmExitWithStd(
      """import std.flag.*
        |import std.result.{is_ok, unwrap}
        |
        |main() -> int
        |    var p = new_parser("test", "")
        |    val port = p.int_flag("p", "port", 8080, "port")
        |    val argv: [0]string = []
        |    val r = p.parse(argv[:])
        |    if !is_ok(r) then return 0
        |    if unwrap(r).get_int(port) != 8080 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.flag str flag" in {
    llvmExitWithStd(
      """import std.flag.*
        |import std.result.{is_ok, unwrap}
        |
        |main() -> int
        |    var p = new_parser("test", "")
        |    val out = p.str_flag("o", "output", "", "output")
        |    val argv: [2]string = ["-o", "result.txt"]
        |    val r = p.parse(argv[:])
        |    if !is_ok(r) then return 0
        |    if unwrap(r).get_str(out) != "result.txt" then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.flag combined short bools" in {
    llvmExitWithStd(
      """import std.flag.*
        |import std.result.{is_ok, unwrap}
        |
        |main() -> int
        |    var p = new_parser("test", "")
        |    val a = p.bool_flag("a", "all", "all")
        |    val l = p.bool_flag("l", "long_flag", "long")
        |    val h = p.bool_flag("h", "human", "human")
        |    val argv: [1]string = ["-alh"]
        |    val r = p.parse(argv[:])
        |    if !is_ok(r) then return 0
        |    val parsed = unwrap(r)
        |    if !parsed.get_bool(a) then return 0
        |    if !parsed.get_bool(l) then return 0
        |    if !parsed.get_bool(h) then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.flag positional args" in {
    llvmExitWithStd(
      """import std.flag.*
        |import std.result.{is_ok, unwrap}
        |
        |main() -> int
        |    var p = new_parser("test", "")
        |    val v = p.bool_flag("v", "verbose", "verbose")
        |    val argv: [3]string = ["-v", "file1.txt", "file2.txt"]
        |    val r = p.parse(argv[:])
        |    if !is_ok(r) then return 0
        |    val parsed = unwrap(r)
        |    if !parsed.get_bool(v) then return 0
        |    if parsed.arg_count() != 2 then return 0
        |    val files = parsed.args()
        |    if files[0] != "file1.txt" then return 0
        |    if files[1] != "file2.txt" then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  // ===== std.container.list =====
  //
  // The list module uses the `val slot = (new [1]T)[:]; &slot[0]` idiom to
  // return heap-allocated pointers from functions. In the LLVM backend's
  // current refcounting model, the local slice's scope-exit cleanup frees the
  // backing storage before the caller can use the returned pointer — a
  // use-after-free. Fixing this requires proper escape analysis or an
  // alternate ownership model. For now, only the trivial "new and empty"
  // case works (which doesn't trigger the bug).
  //
  // TODO: fix ownership model to support `&slice[i]` escapes, then un-ignore
  // the remaining tests.

  // ===== std.regex =====

  "std.regex literal match" in {
    llvmExitWithStd(
      """import std.regex.*
        |
        |main() -> int
        |    val m = match_regex("hello", "say hello world")
        |    if !m.matched() then return 0
        |    val s, e = m.group(0)
        |    if s != 4 then return 0
        |    if e != 9 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.regex alternation" in {
    llvmExitWithStd(
      """import std.regex.*
        |
        |main() -> int
        |    val m = match_regex("cat|dog", "the cat sat")
        |    if !m.matched() then return 0
        |    val s, e = m.group(0)
        |    if s != 4 then return 0
        |    if e != 7 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.regex no match" in {
    llvmExitWithStd(
      """import std.regex.*
        |
        |main() -> int
        |    val m = match_regex("cat|dog", "the bird sat")
        |    if m.matched() then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.regex anchored" ignore { // TODO: SIGBUS — likely alignment or memory issue in complex patterns
    llvmExitWithStd(
      """import std.regex.*
        |
        |main() -> int
        |    val m1 = match_regex("^hello$", "hello")
        |    if !m1.matched() then return 0
        |    val m2 = match_regex("^hello$", "say hello")
        |    if m2.matched() then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.regex capture groups" in {
    llvmExitWithStd(
      """import std.regex.*
        |
        |main() -> int
        |    val m = match_regex("(hello) (world)", "say hello world")
        |    if !m.matched() then return 0
        |    val s1, e1 = m.group(1)
        |    if s1 != 4 then return 0
        |    if e1 != 9 then return 0
        |    val s2, e2 = m.group(2)
        |    if s2 != 10 then return 0
        |    if e2 != 15 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.regex character class" ignore { // TODO: same as anchored
    llvmExitWithStd(
      """import std.regex.*
        |
        |main() -> int
        |    val m = match_regex("[0-9]+", "abc123def")
        |    if !m.matched() then return 0
        |    val s, e = m.group(0)
        |    if s != 3 then return 0
        |    if e != 6 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.regex quantifiers" ignore { // TODO: same as anchored
    llvmExitWithStd(
      """import std.regex.*
        |
        |main() -> int
        |    val m1 = match_regex("^ab+c$", "abbbbc")
        |    if !m1.matched() then return 0
        |    val m2 = match_regex("^ab?c$", "ac")
        |    if !m2.matched() then return 0
        |    val m3 = match_regex("^a{3}$", "aaa")
        |    if !m3.matched() then return 0
        |    val m4 = match_regex("^a{3}$", "aa")
        |    if m4.matched() then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.container.list new and empty" in {
    llvmExitWithStd(
      """import std.container.list.*
        |
        |main() -> int
        |    val l = new_list[int]()
        |    if l.len() != 0 then return 0
        |    if i64(l.front()) != 0 then return 0
        |    if i64(l.back()) != 0 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.container.list push_front and push_back" in {
    llvmOutputWithStd(
      """import std.container.list.*
        |
        |main()
        |    val l = new_list[int]()
        |    println(l.len())
        |    val a = l.push_front(1)
        |    println(l.len())
        |    println(a.value)
        |""".stripMargin) shouldBe "0\n1\n1"
  }

  "std.container.list remove" in {
    llvmExitWithStd(
      """import std.container.list.*
        |
        |main() -> int
        |    val l = new_list[int]()
        |    val x = l.push_back(10)
        |    val y = l.push_back(20)
        |    if l.remove(x) != 10 then return 0
        |    if l.len() != 1 then return 0
        |    if l.front() != y then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.container.list insert_before and after" in {
    llvmExitWithStd(
      """import std.container.list.*
        |
        |main() -> int
        |    val l = new_list[int]()
        |    val mid = l.push_back(2)
        |    val lo = l.insert_before(1, mid)
        |    val hi = l.insert_after(3, mid)
        |    if l.len() != 3 then return 0
        |    if lo.value != 1 then return 0
        |    if mid.value != 2 then return 0
        |    if hi.value != 3 then return 0
        |    if lo.next_elem() != mid then return 0
        |    if mid.next_elem() != hi then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "std.container.list move_to_front" in {
    llvmExitWithStd(
      """import std.container.list.*
        |
        |main() -> int
        |    val l = new_list[int]()
        |    val a = l.push_back(1)
        |    val b = l.push_back(2)
        |    val c = l.push_back(3)
        |    l.move_to_front(c)
        |    if l.front() != c then return 0
        |    if c.next_elem() != a then return 0
        |    if l.back() != b then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }
}
