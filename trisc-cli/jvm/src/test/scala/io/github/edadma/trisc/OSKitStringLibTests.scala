package io.github.edadma.trisc

class OSKitStringLibTests extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val stringSysl: String = readLsysl("oskit/lib/string.lsysl")

  def runStringTest(source: String, maxCycles: Int = 500000): (CPU, String) =
    runWithBoot(Map("oskit/lib/string" -> stringSysl, "posix/string/string" -> posixStringSysl, "main" -> source))

  "strtok: single token" in {
    val (_, output) = runStringTest(
      """import posix.string.*
        |
        |main() -> int
        |    var s: [6]i8
        |    s[0] = 'h'
        |    s[1] = 'e'
        |    s[2] = 'l'
        |    s[3] = 'l'
        |    s[4] = 'o'
        |    s[5] = 0
        |    var d: [2]i8
        |    d[0] = ' '
        |    d[1] = 0
        |    val t = strtok(s, d)
        |    if i64(t) != 0
        |        for var i = 0; t[i] != 0; i++
        |            putchar(t[i])
        |    0
        |""".stripMargin)
    output shouldBe "hello"
  }

  "strtok: multiple tokens" in {
    val (_, output) = runStringTest(
      """import posix.string.*
        |
        |main() -> int
        |    var s: [8]i8
        |    s[0] = 'l'
        |    s[1] = 's'
        |    s[2] = ' '
        |    s[3] = '-'
        |    s[4] = 'l'
        |    s[5] = ' '
        |    s[6] = '/'
        |    s[7] = 0
        |    var d: [2]i8
        |    d[0] = ' '
        |    d[1] = 0
        |    var tok = strtok(s, d)
        |    var count = 0
        |    while i64(tok) != 0
        |        if count > 0
        |            putchar(',')
        |        for var i = 0; tok[i] != 0; i++
        |            putchar(tok[i])
        |        count += 1
        |        tok = strtok(*i8(0), d)
        |    0
        |""".stripMargin)
    output shouldBe "ls,-l,/"
  }

  "strtok: leading and trailing spaces" in {
    val (_, output) = runStringTest(
      """import posix.string.*
        |
        |main() -> int
        |    var s: [8]i8
        |    s[0] = ' '
        |    s[1] = ' '
        |    s[2] = 'h'
        |    s[3] = 'i'
        |    s[4] = ' '
        |    s[5] = ' '
        |    s[6] = 0
        |    var d: [2]i8
        |    d[0] = ' '
        |    d[1] = 0
        |    var tok = strtok(s, d)
        |    var count = 0
        |    while i64(tok) != 0
        |        for var i = 0; tok[i] != 0; i++
        |            putchar(tok[i])
        |        count += 1
        |        tok = strtok(*i8(0), d)
        |    putchar('0' + count)
        |    0
        |""".stripMargin)
    output shouldBe "hi1"
  }

  "strtok: empty string" in {
    val (_, output) = runStringTest(
      """import posix.string.*
        |
        |main() -> int
        |    var s: [1]i8
        |    s[0] = 0
        |    var d: [2]i8
        |    d[0] = ' '
        |    d[1] = 0
        |    val tok = strtok(s, d)
        |    if i64(tok) == 0
        |        putchar('Y')
        |    else
        |        putchar('N')
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
        |    a[0] = 'l'
        |    a[1] = 's'
        |    a[2] = 0
        |    var b: [3]i8
        |    b[0] = 'l'
        |    b[1] = 's'
        |    b[2] = 0
        |    if streq(a, b) == 1
        |        putchar('Y')
        |    else
        |        putchar('N')
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
        |    a[0] = 'l'
        |    a[1] = 's'
        |    a[2] = 0
        |    var b: [3]i8
        |    b[0] = 'c'
        |    b[1] = 'd'
        |    b[2] = 0
        |    if streq(a, b) == 0
        |        putchar('Y')
        |    else
        |        putchar('N')
        |    0
        |""".stripMargin)
    output shouldBe "Y"
  }
}
