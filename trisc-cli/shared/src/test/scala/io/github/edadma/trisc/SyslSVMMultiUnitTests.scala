package io.github.edadma.trisc

class SyslSVMMultiUnitTests extends SyslSVMCodegenHelpers {

  "call function in other unit" in {
    compileMultiAndRun(Map(
      "main.sysl" ->
        """extern add(a: i64, b: i64) -> i64
          |
          |main() -> i64 = add(3, 4)
          |""".stripMargin,
      "math.sysl" ->
        """add(a: i64, b: i64) -> i64 = a + b
          |""".stripMargin
    )) shouldBe 7
  }

  "shared global via function" in {
    compileMultiAndRun(Map(
      "main.sysl" ->
        """extern bump() -> i64
          |
          |main() -> i64
          |    bump()
          |    bump()
          |    bump()
          |""".stripMargin,
      "state.sysl" ->
        """var counter: i64 = 0
          |
          |bump() -> i64
          |    counter += 1
          |    counter
          |""".stripMargin
    )) shouldBe 3
  }

  "multi-unit with structs" in {
    compileMultiAndRun(Map(
      "main.sysl" ->
        """struct Point
          |    x: i64
          |    y: i64
          |
          |extern make_point(x: i64, y: i64) -> Point
          |
          |main() -> i64
          |    var p = make_point(10, 32)
          |    p.x + p.y
          |""".stripMargin,
      "factory.sysl" ->
        """struct Point
          |    x: i64
          |    y: i64
          |
          |make_point(x: i64, y: i64) -> Point = Point(x, y)
          |""".stripMargin
    )) shouldBe 42
  }

  "multi-unit with strings" in {
    compileMultiAndRun(Map(
      "main.sysl" ->
        """extern get_len(s: string) -> i64
          |
          |main() -> i64
          |    get_len("hello world")
          |""".stripMargin,
      "util.sysl" ->
        """get_len(s: string) -> i64 = len(s)
          |""".stripMargin
    )) shouldBe 11
  }
}
