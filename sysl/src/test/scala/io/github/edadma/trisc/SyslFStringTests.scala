package io.github.edadma.trisc

class SyslFStringTests extends SyslTestHelpers {

  "f-string basic decimal" in {
    output(
      """main() -> int
        |    val n = 42
        |    puts(f"value=${n}%d")
        |    0
        |""".stripMargin) shouldBe "value=42"
  }

  "f-string hex lowercase" in {
    output(
      """main() -> int
        |    val n = 255
        |    puts(f"${n}%x")
        |    0
        |""".stripMargin) shouldBe "ff"
  }

  "f-string hex uppercase" in {
    output(
      """main() -> int
        |    val n = 255
        |    puts(f"${n}%X")
        |    0
        |""".stripMargin) shouldBe "FF"
  }

  "f-string zero-padded hex" in {
    output(
      """main() -> int
        |    val n = 10
        |    puts(f"0x${n}%04x")
        |    0
        |""".stripMargin) shouldBe "0x000a"
  }

  "f-string zero-padded decimal" in {
    output(
      """main() -> int
        |    val n = 42
        |    puts(f"${n}%08d")
        |    0
        |""".stripMargin) shouldBe "00000042"
  }

  "f-string binary" in {
    output(
      """main() -> int
        |    val n = 26
        |    puts(f"${n}%b")
        |    0
        |""".stripMargin) shouldBe "11010"
  }

  "f-string octal" in {
    output(
      """main() -> int
        |    val n = 511
        |    puts(f"${n}%o")
        |    0
        |""".stripMargin) shouldBe "777"
  }

  "f-string string with width" in {
    output(
      """main() -> int
        |    val s = "hi"
        |    puts(f"[${s}%10s]")
        |    0
        |""".stripMargin) shouldBe "[        hi]"
  }

  "f-string left-aligned string" in {
    output(
      """main() -> int
        |    val s = "hi"
        |    puts(f"[${s}%-10s]")
        |    0
        |""".stripMargin) shouldBe "[hi        ]"
  }

  "f-string mixed" in {
    output(
      """main() -> int
        |    val cp = 65
        |    val count = 3
        |    val name = "LATIN"
        |    puts(f"U+${cp}%04X count=${count}%d name=${name}%s")
        |    0
        |""".stripMargin) shouldBe "U+0041 count=3 name=LATIN"
  }

  "f-string percent escape" in {
    output(
      """main() -> int
        |    val n = 50
        |    puts(f"${n}%d%%")
        |    0
        |""".stripMargin) shouldBe "50%"
  }

  "f-string no spec defaults to %s" in {
    output(
      """main() -> int
        |    val n = 42
        |    puts(f"value=$n")
        |    0
        |""".stripMargin) shouldBe "value=42"
  }

  "f-string show sign" in {
    output(
      """main() -> int
        |    val n = 42
        |    puts(f"${n}%+d")
        |    0
        |""".stripMargin) shouldBe "+42"
  }

  "f-string bare name with spec" in {
    output(
      """main() -> int
        |    val n = 255
        |    puts(f"$n%x")
        |    0
        |""".stripMargin) shouldBe "ff"
  }

  "f-string bare name zero-padded" in {
    output(
      """main() -> int
        |    val cp = 65
        |    puts(f"U+$cp%04X")
        |    0
        |""".stripMargin) shouldBe "U+0041"
  }

  "f-string i64 hex" in {
    output(
      """main() -> int
        |    val n: i64 = 3735928559i64
        |    puts(f"${n}%x")
        |    0
        |""".stripMargin) shouldBe "deadbeef"
  }
}
