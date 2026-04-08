package io.github.edadma.trisc

class SyslLLVMStringTests extends SyslLLVMTestHelpers {

  "string variable" in {
    llvmOutput(
      """main() -> int
        |    s = "hello"
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "hello"
  }

  "string parameter" in {
    llvmOutput(
      """greet(s: string) -> int
        |    puts(s)
        |    0
        |
        |main() -> int
        |    greet("world")
        |    0
        |""".stripMargin) shouldBe "world"
  }

  "string reassignment" in {
    llvmOutput(
      """main() -> int
        |    var s = "first"
        |    s = "second"
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "second"
  }

  "string in struct" in {
    llvmOutput(
      """struct Greeting
        |    msg: string
        |    count: int
        |
        |main() -> int
        |    g = Greeting("hi", 3)
        |    puts(g.msg)
        |    println(g.count)
        |    0
        |""".stripMargin) shouldBe "hi\n3"
  }

  "string concatenation result stored" in {
    llvmOutput(
      """main() -> int
        |    a = "hello"
        |    b = " world"
        |    c = a + b
        |    puts(c)
        |    0
        |""".stripMargin) shouldBe "hello world"
  }

  "string length" in {
    llvmExit(
      """main() -> int
        |    s = "hello"
        |    len(s)
        |""".stripMargin) shouldBe 5
  }

  "string in array constant index" in {
    llvmOutput(
      """main() -> int
        |    a = ["one", "two", "three"]
        |    puts(a[0])
        |    puts(a[1])
        |    puts(a[2])
        |    0
        |""".stripMargin) shouldBe "one\ntwo\nthree"
  }

  "string in array runtime index" in {
    llvmOutput(
      """main() -> int
        |    a = ["one", "two", "three"]
        |    for var i = 0; i < 3; i = i + 1
        |        puts(a[i])
        |    0
        |""".stripMargin) shouldBe "one\ntwo\nthree"
  }
}
