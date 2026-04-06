package io.github.edadma.trisc

class SyslLibStdlibConvTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/stdlib/stdlib" -> readSysl("posix/stdlib/stdlib.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.stdlib.*
       |$main
       |""".stripMargin)

  // ===== atoi =====

  "atoi simple" in {
    evalWith(
      """main() -> int
        |    s: [4]byte
        |    s[0] = '1'
        |    s[1] = '2'
        |    s[2] = '3'
        |    s[3] = 0
        |    atoi(s)
        |""".stripMargin) shouldBe 123
  }

  "atoi negative" in {
    evalWith(
      """main() -> int
        |    s: [5]byte
        |    s[0] = '-'
        |    s[1] = '4'
        |    s[2] = '2'
        |    s[3] = '0'
        |    s[4] = 0
        |    atoi(s)
        |""".stripMargin) shouldBe -420
  }

  "atoi with leading spaces" in {
    evalWith(
      """main() -> int
        |    s: [5]byte
        |    s[0] = ' '
        |    s[1] = ' '
        |    s[2] = '7'
        |    s[3] = '8'
        |    s[4] = 0
        |    atoi(s)
        |""".stripMargin) shouldBe 78
  }

  "atoi with plus sign" in {
    evalWith(
      """main() -> int
        |    s: [4]byte
        |    s[0] = '+'
        |    s[1] = '9'
        |    s[2] = '9'
        |    s[3] = 0
        |    atoi(s)
        |""".stripMargin) shouldBe 99
  }

  "atoi zero" in {
    evalWith(
      """main() -> int
        |    s: [2]byte
        |    s[0] = '0'
        |    s[1] = 0
        |    atoi(s)
        |""".stripMargin) shouldBe 0
  }

  "atoi stops at non-digit" in {
    evalWith(
      """main() -> int
        |    s: [5]byte
        |    s[0] = '1'
        |    s[1] = '2'
        |    s[2] = 'x'
        |    s[3] = '3'
        |    s[4] = 0
        |    atoi(s)
        |""".stripMargin) shouldBe 12
  }

  "atoi empty string" in {
    evalWith(
      """main() -> int
        |    s: [1]byte
        |    s[0] = 0
        |    atoi(s)
        |""".stripMargin) shouldBe 0
  }

  // ===== atol =====

  "atol simple" in {
    evalWith(
      """main() -> i64
        |    s: [7]byte
        |    s[0] = '1'
        |    s[1] = '0'
        |    s[2] = '0'
        |    s[3] = '0'
        |    s[4] = '0'
        |    s[5] = '0'
        |    s[6] = 0
        |    atol(s)
        |""".stripMargin) shouldBe 100000L
  }

  "atol negative" in {
    evalWith(
      """main() -> i64
        |    s: [4]byte
        |    s[0] = '-'
        |    s[1] = '5'
        |    s[2] = '5'
        |    s[3] = 0
        |    atol(s)
        |""".stripMargin) shouldBe -55L
  }

  // ===== strtol =====

  "strtol decimal" in {
    evalWith(
      """main() -> i64
        |    s: [5]byte
        |    s[0] = ' '
        |    s[1] = '2'
        |    s[2] = '5'
        |    s[3] = '5'
        |    s[4] = 0
        |    strtol(s, *byte(0), 10)
        |""".stripMargin) shouldBe 255L
  }

  "strtol hex explicit base" in {
    evalWith(
      """main() -> i64
        |    s: [3]byte
        |    s[0] = 'F'
        |    s[1] = 'F'
        |    s[2] = 0
        |    strtol(s, *byte(0), 16)
        |""".stripMargin) shouldBe 255L
  }

  "strtol hex with 0x prefix auto-detect" in {
    evalWith(
      """main() -> i64
        |    s: [5]byte
        |    s[0] = '0'
        |    s[1] = 'x'
        |    s[2] = '1'
        |    s[3] = 'A'
        |    s[4] = 0
        |    strtol(s, *byte(0), 0)
        |""".stripMargin) shouldBe 26L
  }

  "strtol octal auto-detect" in {
    evalWith(
      """main() -> i64
        |    s: [4]byte
        |    s[0] = '0'
        |    s[1] = '1'
        |    s[2] = '7'
        |    s[3] = 0
        |    strtol(s, *byte(0), 0)
        |""".stripMargin) shouldBe 15L
  }

  "strtol negative hex" in {
    evalWith(
      """main() -> i64
        |    s: [6]byte
        |    s[0] = '-'
        |    s[1] = '0'
        |    s[2] = 'x'
        |    s[3] = '1'
        |    s[4] = '0'
        |    s[5] = 0
        |    strtol(s, *byte(0), 0)
        |""".stripMargin) shouldBe -16L
  }

  "strtol base 2" in {
    evalWith(
      """main() -> i64
        |    s: [5]byte
        |    s[0] = '1'
        |    s[1] = '0'
        |    s[2] = '1'
        |    s[3] = '0'
        |    s[4] = 0
        |    strtol(s, *byte(0), 2)
        |""".stripMargin) shouldBe 10L
  }

  "strtol lowercase hex" in {
    evalWith(
      """main() -> i64
        |    s: [3]byte
        |    s[0] = 'f'
        |    s[1] = 'f'
        |    s[2] = 0
        |    strtol(s, *byte(0), 16)
        |""".stripMargin) shouldBe 255L
  }

  "strtol zero" in {
    evalWith(
      """main() -> i64
        |    s: [2]byte
        |    s[0] = '0'
        |    s[1] = 0
        |    strtol(s, *byte(0), 10)
        |""".stripMargin) shouldBe 0L
  }

  // ===== strtoul =====

  "strtoul decimal" in {
    evalWith(
      """main() -> i64
        |    s: [4]byte
        |    s[0] = '4'
        |    s[1] = '2'
        |    s[2] = '0'
        |    s[3] = 0
        |    strtoul(s, *byte(0), 10)
        |""".stripMargin) shouldBe 420L
  }

  "strtoul hex auto-detect" in {
    evalWith(
      """main() -> i64
        |    s: [5]byte
        |    s[0] = '0'
        |    s[1] = 'X'
        |    s[2] = 'C'
        |    s[3] = 'A'
        |    s[4] = 0
        |    strtoul(s, *byte(0), 0)
        |""".stripMargin) shouldBe 202L
  }

  "strtoul with plus sign" in {
    evalWith(
      """main() -> i64
        |    s: [4]byte
        |    s[0] = '+'
        |    s[1] = '5'
        |    s[2] = '0'
        |    s[3] = 0
        |    strtoul(s, *byte(0), 10)
        |""".stripMargin) shouldBe 50L
  }

  "strtoul octal" in {
    evalWith(
      """main() -> i64
        |    s: [4]byte
        |    s[0] = '0'
        |    s[1] = '7'
        |    s[2] = '7'
        |    s[3] = 0
        |    strtoul(s, *byte(0), 0)
        |""".stripMargin) shouldBe 63L
  }
}
