package io.github.edadma.trisc

class SyslSVMInterfaceTests extends SyslSVMCodegenHelpers {

  "simple interface dispatch" in {
    compileAndRun(
      """interface Shape
        |    area() -> i64
        |
        |struct Square
        |    side: i64
        |
        |Square.area() -> i64 = self.side * self.side
        |
        |main() -> i64
        |    var sq = Square(5)
        |    var s: Shape = sq
        |    s.area()
        |""".stripMargin) shouldBe 25
  }

  "two structs share interface" in {
    compileAndRun(
      """interface Greet
        |    greeting() -> i64
        |
        |struct Foo
        |    x: i64
        |
        |struct Bar
        |    y: i64
        |
        |Foo.greeting() -> i64 = 1
        |Bar.greeting() -> i64 = 2
        |
        |speak(g: Greet) -> i64 = g.greeting()
        |
        |main() -> i64
        |    var f = Foo(1)
        |    var b = Bar(2)
        |    speak(f) * 10 + speak(b)
        |""".stripMargin) shouldBe 12
  }
}
