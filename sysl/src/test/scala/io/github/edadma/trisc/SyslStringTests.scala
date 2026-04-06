package io.github.edadma.trisc

class SyslStringTests extends SyslTestHelpers {

  // ===== Char literals =====

  "char literal basic" in {
    eval("main() -> int = 'A'\n") shouldBe 65
  }

  "char literal in arithmetic" in {
    eval("main() -> int = 'A' + 1\n") shouldBe 66
  }

  "char literal comparison" in {
    eval("main() -> int = if 'A' < 'B' then 1 else 0\n") shouldBe 1
  }

  "char literal escape newline" in {
    eval("main() -> int = '\\n'\n") shouldBe 10
  }

  "char literal escape tab" in {
    eval("main() -> int = '\\t'\n") shouldBe 9
  }

  "char literal escape null" in {
    eval("main() -> int = '\\0'\n") shouldBe 0
  }

  "char literal escape backslash" in {
    eval("main() -> int = '\\\\'\n") shouldBe 92
  }

  "putchar with char literal" in {
    output(
      """main() -> int
        |    putchar('H')
        |    putchar('i')
        |    0
        |""".stripMargin) shouldBe "Hi"
  }

  "char conversion digit to int" in {
    eval("main() -> int = '5' - '0'\n") shouldBe 5
  }

  "char in array" in {
    output(
      """main() -> int
        |    msg: [3]int
        |    msg[0] = 'H'
        |    msg[1] = 'i'
        |    msg[2] = '!'
        |    i = 0
        |    while i < 3 do putchar(msg[i++])
        |    0
        |""".stripMargin) shouldBe "Hi!"
  }

  "char literal unicode BMP" in {
    eval("main() -> int = 'é'\n") shouldBe 233
  }

  "char literal unicode CJK" in {
    eval("main() -> int = '世'\n") shouldBe 19990
  }

  // ===== Byte arrays =====

  "byte array declaration" in {
    eval(
      """main() -> int
        |    buf: [4]byte
        |    buf[0] = 72
        |    buf[1] = 105
        |    buf[0] + buf[1]
        |""".stripMargin) shouldBe 177
  }

  // ===== String type =====

  "string literal indexing" in {
    eval(
      """main() -> int
        |    s = "Hello"
        |    s[0]
        |""".stripMargin) shouldBe 72 // 'H'
  }

  "string literal len" in {
    eval("main() -> int = len(\"Hello\")\n") shouldBe 5
  }

  "string literal len UTF-8" in {
    eval("main() -> int = len(\"café\")\n") shouldBe 5 // 'é' is 2 bytes in UTF-8
  }

  "string literal indexing returns byte" in {
    eval(
      """main() -> int
        |    s = "AB"
        |    s[0] + s[1]
        |""".stripMargin) shouldBe 131 // 65 + 66
  }

  "string bounds check" in {
    assertThrows[RuntimeException] {
      eval(
        """main() -> int
          |    s = "Hi"
          |    s[2]
          |""".stripMargin)
    }
  }

  "string negative index bounds check" in {
    assertThrows[RuntimeException] {
      eval(
        """main() -> int
          |    s = "Hi"
          |    s[-1]
          |""".stripMargin)
    }
  }

  "puts builtin" in {
    output(
      """main() -> int
        |    puts("Hello")
        |    0
        |""".stripMargin) shouldBe "Hello"
  }

  "puti builtin" in {
    output(
      """main() -> int
        |    puti(42)
        |    0
        |""".stripMargin) shouldBe "42"
  }

  "string variable" in {
    output(
      """main() -> int
        |    greeting: string = "Hi!"
        |    puts(greeting)
        |    0
        |""".stripMargin) shouldBe "Hi!"
  }

  "string passed to function" in {
    eval(
      """first(s: string) -> int = s[0]
        |
        |main() -> int = first("ABC")
        |""".stripMargin) shouldBe 65
  }

  "len in function" in {
    eval(
      """length(s: string) -> int = len(s)
        |
        |main() -> int = length("Hello")
        |""".stripMargin) shouldBe 5
  }

  // ===== Old-style *byte string functions (still work with raw pointers) =====

  "strlen implementation" in {
    eval(
      """strlen(s: *byte) -> int
        |    n = 0
        |    while s[n] != 0 do n++
        |    n
        |
        |main() -> int
        |    str: [6]int
        |    str[0] = 'H'
        |    str[1] = 'e'
        |    str[2] = 'l'
        |    str[3] = 'l'
        |    str[4] = 'o'
        |    str[5] = 0
        |    strlen(str)
        |""".stripMargin) shouldBe 5
  }

  "puts with byte array" in {
    output(
      """myputs(s: *byte)
        |    i = 0
        |    while s[i] != 0
        |        putchar(s[i])
        |        i += 1
        |
        |main() -> int
        |    str: [4]int
        |    str[0] = 'H'
        |    str[1] = 'i'
        |    str[2] = '!'
        |    str[3] = 0
        |    myputs(str)
        |    0
        |""".stripMargin) shouldBe "Hi!"
  }

  "strcmp implementation" in {
    eval(
      """strcmp(a: *byte, b: *byte) -> int
        |    i = 0
        |    while a[i] != 0 && a[i] == b[i] do i++
        |    int(a[i]) - int(b[i])
        |
        |main() -> int
        |    s1: [4]int
        |    s2: [4]int
        |    s3: [4]int
        |    s1[0] = 'a'
        |    s1[1] = 'b'
        |    s1[2] = 'c'
        |    s1[3] = 0
        |    s2[0] = 'a'
        |    s2[1] = 'b'
        |    s2[2] = 'c'
        |    s2[3] = 0
        |    s3[0] = 'a'
        |    s3[1] = 'b'
        |    s3[2] = 'd'
        |    s3[3] = 0
        |    eq = strcmp(s1, s2)
        |    lt = strcmp(s1, s3)
        |    if eq == 0 && lt < 0 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== String escape sequences =====

  "string with newline escape" in {
    output(
      """main() -> int
        |    puts("hi\n")
        |    0
        |""".stripMargin) shouldBe "hi\n"
  }

  "string with tab escape" in {
    output(
      """main() -> int
        |    puts("a\tb")
        |    0
        |""".stripMargin) shouldBe "a\tb"
  }

  "string with backslash escape" in {
    output(
      """main() -> int
        |    puts("a\\b")
        |    0
        |""".stripMargin) shouldBe "a\\b"
  }

  "string with null escape byte value" in {
    eval(
      """main() -> int
        |    val s = "ab\0cd"
        |    s[2]
        |""".stripMargin) shouldBe 0  // \0 = 0
  }

  "string with embedded quote escape" in {
    output(
      """main() -> int
        |    puts("say \"hi\"")
        |    0
        |""".stripMargin) shouldBe "say \"hi\""
  }

  "string escape in indexing" in {
    eval(
      """main() -> int
        |    val s = "a\nb"
        |    s[1]
        |""".stripMargin) shouldBe 10  // \n = 10
  }
}
