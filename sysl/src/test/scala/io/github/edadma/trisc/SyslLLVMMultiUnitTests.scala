package io.github.edadma.trisc

class SyslLLVMMultiUnitTests extends SyslLLVMTestHelpers {

  "call function from another file" in {
    llvmExitMulti(Map(
      "math/math" ->
        """module math
          |
          |add(a: int, b: int) -> int = a + b
          |""".stripMargin,
      "main" ->
        """import math.*
          |
          |main() -> int
          |    add(17, 25)
          |""".stripMargin,
    )) shouldBe 42
  }

  "call multiple functions across files" in {
    llvmOutputMulti(Map(
      "math/math" ->
        """module math
          |
          |add(a: int, b: int) -> int = a + b
          |mul(a: int, b: int) -> int = a * b
          |""".stripMargin,
      "main" ->
        """import math.*
          |
          |main() -> int
          |    println(add(3, 4))
          |    println(mul(5, 6))
          |    0
          |""".stripMargin,
    )) shouldBe "7\n30"
  }

  "use struct from another file" in {
    llvmExitMulti(Map(
      "geom/geom" ->
        """module geom
          |
          |struct Point
          |    x: int
          |    y: int
          |
          |make_point(x: int, y: int) -> Point = Point(x, y)
          |""".stripMargin,
      "main" ->
        """import geom.*
          |
          |main() -> int
          |    p = make_point(3, 4)
          |    p.x + p.y
          |""".stripMargin,
    )) shouldBe 7
  }

  "global variable from another file" in {
    llvmExitMulti(Map(
      "constants/constants" ->
        """module constants
          |
          |val ANSWER = 42
          |""".stripMargin,
      "main" ->
        """import constants.*
          |
          |main() -> int
          |    ANSWER
          |""".stripMargin,
    )) shouldBe 42
  }

  "three-file chain: A imports B, B imports C" in {
    llvmExitMulti(Map(
      "base/base" ->
        """module base
          |
          |square(x: int) -> int = x * x
          |""".stripMargin,
      "mid/mid" ->
        """module mid
          |
          |import base.*
          |
          |sum_of_squares(a: int, b: int) -> int = square(a) + square(b)
          |""".stripMargin,
      "main" ->
        """import mid.*
          |
          |main() -> int
          |    sum_of_squares(3, 4)
          |""".stripMargin,
    )) shouldBe 25
  }

  "string function across files" in {
    llvmOutputMulti(Map(
      "greet/greet" ->
        """module greet
          |
          |hello(name: string) -> int
          |    puts(name)
          |    0
          |""".stripMargin,
      "main" ->
        """import greet.*
          |
          |main() -> int
          |    hello("world")
          |""".stripMargin,
    )) shouldBe "world"
  }
}
