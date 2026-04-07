package io.github.edadma.trisc

class SyslByteArrayInitTests extends SyslTestHelpers {

  // ===== String literal initializer =====

  "byte array from string literal" in {
    eval(
      """main() -> int
        |    var buf: [5]byte = "hello"
        |    int(buf[0]) + int(buf[4])
        |""".stripMargin) shouldBe ('h' + 'o')
  }

  "byte array from string with zero fill" in {
    eval(
      """main() -> int
        |    var buf: [8]byte = "hi"
        |    int(buf[0]) + int(buf[1]) + int(buf[2]) + int(buf[7])
        |""".stripMargin) shouldBe ('h' + 'i' + 0 + 0)
  }

  "byte array from empty string" in {
    eval(
      """main() -> int
        |    var buf: [3]byte = ""
        |    int(buf[0]) + int(buf[1]) + int(buf[2])
        |""".stripMargin) shouldBe 0
  }

  "byte array string too long is error" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    var buf: [2]byte = "hello"
        |    0
        |""".stripMargin)
  }

  // ===== Character literal initializer =====

  "byte array from char literals" in {
    eval(
      """main() -> int
        |    var buf: [3]byte = ['a', 'b', 'c']
        |    int(buf[0]) + int(buf[1]) + int(buf[2])
        |""".stripMargin) shouldBe ('a' + 'b' + 'c')
  }

  "byte array from char literals with zero fill" in {
    eval(
      """main() -> int
        |    var buf: [5]byte = ['x', 'y']
        |    int(buf[0]) + int(buf[1]) + int(buf[2])
        |""".stripMargin) shouldBe ('x' + 'y' + 0)
  }

  "byte array char value out of range is error" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    var buf: [3]byte = [256, 0, 0]
        |    0
        |""".stripMargin)
  }

  // ===== Global byte array =====

  "global byte array from string" in {
    eval(
      """msg: [5]byte = "world"
        |
        |main() -> int
        |    int(msg[0])
        |""".stripMargin) shouldBe 'w'
  }

  "global byte array from chars" in {
    eval(
      """data: [3]byte = [0x41, 0x42, 0x43]
        |
        |main() -> int
        |    int(data[0]) + int(data[1]) + int(data[2])
        |""".stripMargin) shouldBe (0x41 + 0x42 + 0x43)
  }

  // ===== Exact size match =====

  "byte array exact size string" in {
    eval(
      """main() -> int
        |    var buf: [3]byte = "abc"
        |    int(buf[2])
        |""".stripMargin) shouldBe 'c'
  }
}
