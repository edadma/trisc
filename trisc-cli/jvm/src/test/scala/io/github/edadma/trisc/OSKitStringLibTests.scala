package io.github.edadma.trisc

class OSKitStringLibTests extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val stringSysl: String = readLsysl("oskit/lib/string.lsysl")

  def runStringTest(source: String, maxCycles: Int = 500000): (CPU, String) =
    runWithBoot(Map("oskit/lib/string" -> stringSysl, "main" -> source))

  "strtok: single token" in {
    val (_, output) = runStringTest(
      """import oskit.lib.*
        |
        |main() -> int
        |    var s: [6]i8
        |    s[0] = 104  // h
        |    s[1] = 101  // e
        |    s[2] = 108  // l
        |    s[3] = 108  // l
        |    s[4] = 111  // o
        |    s[5] = 0
        |    var d: [2]i8
        |    d[0] = 32
        |    d[1] = 0
        |    val t = strtok(&s[0], &d[0])
        |    if i64(t) != 0
        |        var i = 0
        |        while t[i] != 0
        |            putchar(t[i])
        |            i += 1
        |    0
        |""".stripMargin)
    output shouldBe "hello"
  }

  "strtok: multiple tokens" in {
    val (_, output) = runStringTest(
      """import oskit.lib.*
        |
        |main() -> int
        |    var s: [8]i8
        |    s[0] = 108  // l
        |    s[1] = 115  // s
        |    s[2] = 32   // space
        |    s[3] = 45   // -
        |    s[4] = 108  // l
        |    s[5] = 32   // space
        |    s[6] = 47   // /
        |    s[7] = 0
        |    var d: [2]i8
        |    d[0] = 32
        |    d[1] = 0
        |    var tok = strtok(&s[0], &d[0])
        |    var count = 0
        |    while i64(tok) != 0
        |        if count > 0
        |            putchar(44)  // comma
        |        var i = 0
        |        while tok[i] != 0
        |            putchar(tok[i])
        |            i += 1
        |        count += 1
        |        tok = strtok(*i8(0), &d[0])
        |    0
        |""".stripMargin)
    output shouldBe "ls,-l,/"
  }

  "strtok: leading and trailing spaces" in {
    val (_, output) = runStringTest(
      """import oskit.lib.*
        |
        |main() -> int
        |    var s: [8]i8
        |    s[0] = 32   // space
        |    s[1] = 32   // space
        |    s[2] = 104  // h
        |    s[3] = 105  // i
        |    s[4] = 32   // space
        |    s[5] = 32   // space
        |    s[6] = 0
        |    var d: [2]i8
        |    d[0] = 32
        |    d[1] = 0
        |    var tok = strtok(&s[0], &d[0])
        |    var count = 0
        |    while i64(tok) != 0
        |        var i = 0
        |        while tok[i] != 0
        |            putchar(tok[i])
        |            i += 1
        |        count += 1
        |        tok = strtok(*i8(0), &d[0])
        |    putchar(48 + count)  // print count
        |    0
        |""".stripMargin)
    output shouldBe "hi1"
  }

  "strtok: empty string" in {
    val (_, output) = runStringTest(
      """import oskit.lib.*
        |
        |main() -> int
        |    var s: [1]i8
        |    s[0] = 0
        |    var d: [2]i8
        |    d[0] = 32
        |    d[1] = 0
        |    val tok = strtok(&s[0], &d[0])
        |    if i64(tok) == 0
        |        putchar(89)  // Y
        |    else
        |        putchar(78)  // N
        |    0
        |""".stripMargin)
    output shouldBe "Y"
  }

  "streq: equal strings" in {
    val (_, output) = runStringTest(
      """import oskit.lib.*
        |
        |main() -> int
        |    var a: [3]i8
        |    a[0] = 108
        |    a[1] = 115
        |    a[2] = 0
        |    var b: [3]i8
        |    b[0] = 108
        |    b[1] = 115
        |    b[2] = 0
        |    if streq(&a[0], &b[0]) == 1
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin)
    output shouldBe "Y"
  }

  "streq: different strings" in {
    val (_, output) = runStringTest(
      """import oskit.lib.*
        |
        |main() -> int
        |    var a: [3]i8
        |    a[0] = 108
        |    a[1] = 115
        |    a[2] = 0
        |    var b: [3]i8
        |    b[0] = 99
        |    b[1] = 100
        |    b[2] = 0
        |    if streq(&a[0], &b[0]) == 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin)
    output shouldBe "Y"
  }
}
