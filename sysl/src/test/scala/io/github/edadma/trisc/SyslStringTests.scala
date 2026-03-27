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

  // ===== Byte arrays and string literals =====

  "byte array declaration" in {
    eval(
      """main() -> int
        |    buf: [4]byte
        |    buf[0] = 72
        |    buf[1] = 105
        |    buf[0] + buf[1]
        |""".stripMargin) shouldBe 177
  }

  "string literal creates null-terminated byte array" in {
    eval(
      """main() -> int
        |    s = "Hello"
        |    s[0]
        |""".stripMargin) shouldBe 72 // 'H'
  }

  "string literal null terminated" in {
    eval(
      """main() -> int
        |    s = "Hi"
        |    s[2]
        |""".stripMargin) shouldBe 0
  }

  "string literal with strlen" in {
    eval(
      """strlen(s: *byte) -> int
        |    n = 0
        |    while s[n] != 0 do n++
        |    n
        |
        |main() -> int = strlen("Hello")
        |""".stripMargin) shouldBe 5
  }

  "string literal with puts" in {
    output(
      """puts(s: *byte)
        |    i = 0
        |    while s[i] != 0
        |        putchar(s[i])
        |        i += 1
        |
        |main() -> int
        |    puts("Hello")
        |    0
        |""".stripMargin) shouldBe "Hello"
  }

  "string literal passed to function" in {
    eval(
      """first(s: *byte) -> int = s[0]
        |
        |main() -> int = first("ABC")
        |""".stripMargin) shouldBe 65
  }

  "string literal UTF-8 encoding" in {
    eval(
      """strlen(s: *byte) -> int
        |    n = 0
        |    while s[n] != 0 do n++
        |    n
        |
        |main() -> int = strlen("café")
        |""".stripMargin) shouldBe 5 // 'é' is 2 bytes in UTF-8
  }

  "string literal in variable" in {
    output(
      """puts(s: *byte)
        |    i = 0
        |    while s[i] != 0
        |        putchar(s[i])
        |        i += 1
        |
        |main() -> int
        |    greeting = "Hi!"
        |    puts(greeting)
        |    0
        |""".stripMargin) shouldBe "Hi!"
  }

  // ===== String library functions (in sysl) =====

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

  "puts implementation" in {
    output(
      """puts(s: *byte)
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
        |    puts(str)
        |    0
        |""".stripMargin) shouldBe "Hi!"
  }

  "strcmp implementation" in {
    eval(
      """strcmp(a: *byte, b: *byte) -> int
        |    i = 0
        |    while a[i] != 0 && a[i] == b[i] do i++
        |    a[i] - b[i]
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
}
