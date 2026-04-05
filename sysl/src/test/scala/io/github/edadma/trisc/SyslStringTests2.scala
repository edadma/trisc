package io.github.edadma.trisc

class SyslStringTests2 extends SyslTestHelpers {

  // ===== String literals =====

  "string literal length" in {
    eval(
      """main() -> int
        |    val s = "hello"
        |    len(s)
        |""".stripMargin) shouldBe 5
  }

  "empty string length" in {
    eval(
      """main() -> int
        |    val s = ""
        |    len(s)
        |""".stripMargin) shouldBe 0
  }

  "string indexing" in {
    eval(
      """main() -> int
        |    val s = "ABC"
        |    int(s[0]) * 100 + int(s[1]) * 10 + int(s[2]) - 65 * 111
        |""".stripMargin) shouldBe 12
  }

  "string index bounds check" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    val s = "hi"
        |    s[2]
        |""".stripMargin)
  }

  "string index negative bounds check" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    val s = "hi"
        |    s[-1]
        |""".stripMargin)
  }

  // ===== String concatenation =====

  "string concatenation" in {
    output(
      """main() -> int
        |    val s = "hello" + " " + "world"
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "hello world"
  }

  "concat empty left" in {
    output(
      """main() -> int
        |    val s = "" + "abc"
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "abc"
  }

  "concat empty right" in {
    output(
      """main() -> int
        |    val s = "abc" + ""
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "abc"
  }

  "concat both empty" in {
    eval(
      """main() -> int
        |    val s = "" + ""
        |    len(s)
        |""".stripMargin) shouldBe 0
  }

  "concat length is sum" in {
    eval(
      """main() -> int
        |    val a = "abc"
        |    val b = "defgh"
        |    val c = a + b
        |    len(c)
        |""".stripMargin) shouldBe 8
  }

  "concat preserves originals" in {
    output(
      """main() -> int
        |    val a = "hello"
        |    val b = " world"
        |    val c = a + b
        |    puts(a)
        |    0
        |""".stripMargin) shouldBe "hello"
  }

  "concat in loop" in {
    output(
      """main() -> int
        |    var s = ""
        |    var i = 0
        |    while i < 3
        |        s = s + "ab"
        |        i++
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "ababab"
  }

  // ===== String comparison =====

  "string equality — same" in {
    eval(
      """main() -> int
        |    if "hello" == "hello" then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "string equality — different" in {
    eval(
      """main() -> int
        |    if "hello" == "world" then return 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "string inequality" in {
    eval(
      """main() -> int
        |    if "abc" != "def" then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "string equality — different lengths" in {
    eval(
      """main() -> int
        |    if "abc" == "abcd" then return 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "string equality — empty strings" in {
    eval(
      """main() -> int
        |    if "" == "" then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "string equality — concatenated" in {
    eval(
      """main() -> int
        |    val a = "hel" + "lo"
        |    if a == "hello" then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  // ===== String passed to function =====

  "string passed to function" in {
    eval(
      """length(s: string) -> int = len(s)
        |
        |main() -> int = length("hello")
        |""".stripMargin) shouldBe 5
  }

  "string returned from function" in {
    output(
      """greet(name: string) -> string = "hello " + name
        |
        |main() -> int
        |    puts(greet("world"))
        |    0
        |""".stripMargin) shouldBe "hello world"
  }

  "string built in function and compared" in {
    eval(
      """make(a: string, b: string) -> string = a + b
        |
        |main() -> int
        |    val s = make("foo", "bar")
        |    if s == "foobar" then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  // ===== String as *i8 decay =====

  "string decays to *i8" in {
    eval(
      """first_byte(p: *i8) -> int = p[0]
        |
        |main() -> int = first_byte("A")
        |""".stripMargin) shouldBe 65
  }

  // ===== Multiple strings =====

  "multiple string variables" in {
    eval(
      """main() -> int
        |    val a = "abc"
        |    val b = "def"
        |    len(a) + len(b)
        |""".stripMargin) shouldBe 6
  }

  "string reassignment" in {
    output(
      """main() -> int
        |    var s = "first"
        |    s = "second"
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "second"
  }

  "string in if/else" in {
    output(
      """main() -> int
        |    val flag = true
        |    val s = if flag then "yes" else "no"
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "yes"
  }

  // ===== String construction from pointer =====

  "string from *byte and length" in {
    eval(
      """main() -> int
        |    var buf: [3]byte
        |    buf[0] = 'H'
        |    buf[1] = 'i'
        |    buf[2] = '!'
        |    s = string(&buf[0], 3)
        |    len(s)
        |""".stripMargin) shouldBe 3
  }

  "string from *byte content" in {
    output(
      """main() -> int
        |    var buf: [5]byte
        |    buf[0] = 'h'
        |    buf[1] = 'e'
        |    buf[2] = 'l'
        |    buf[3] = 'l'
        |    buf[4] = 'o'
        |    s = string(&buf[0], 5)
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "hello"
  }
}
