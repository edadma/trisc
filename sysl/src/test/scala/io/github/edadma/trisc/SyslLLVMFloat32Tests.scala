package io.github.edadma.trisc

class SyslLLVMFloat32Tests extends SyslLLVMTestHelpers {

  "f32 variable round-trips through assignment" in {
    llvmExit(
      """main() -> int
        |    var x: f32 = 2.5
        |    int(x * 4.0)
        |""".stripMargin) shouldBe 10
  }

  "f32 arithmetic uses native float ops" in {
    llvmExit(
      """main() -> int
        |    var a: f32 = 3.5
        |    var b: f32 = 2.5
        |    int(a + b)
        |""".stripMargin) shouldBe 6
  }

  "f32 to f64 widening uses fpext" in {
    val ir = compileLLVM(
      """main() -> int
        |    var a: f32 = 1.5
        |    var b: f64 = a
        |    int(b)
        |""".stripMargin)
    assert(ir.contains("fpext"), s"expected fpext for f32→f64 widening, got:\n$ir")
  }

  "f64 to f32 narrowing with cast uses fptrunc" in {
    val ir = compileLLVM(
      """main() -> int
        |    var a: f64 = 1.5
        |    var b: f32 = f32(a)
        |    int(b)
        |""".stripMargin)
    assert(ir.contains("fptrunc"), s"expected fptrunc for f64→f32 narrowing, got:\n$ir")
  }

  "f32 emits LLVM 'float' type" in {
    val ir = compileLLVM(
      """main() -> int
        |    var x: f32 = 1.0
        |    0
        |""".stripMargin)
    assert(ir.contains("alloca float") || ir.contains(" float "),
           s"expected LLVM 'float' type for f32, got:\n$ir")
  }

  "f32 in struct uses 4-byte LLVM float field" in {
    val ir = compileLLVM(
      """struct Vec2
        |    x: f32
        |    y: f32
        |
        |main() -> int
        |    var v = Vec2(1.0, 2.0)
        |    0
        |""".stripMargin)
    // The struct layout should mention float, not double
    val structDef = ir.linesIterator.find(_.contains("%struct.Vec2")).getOrElse("")
    assert(!structDef.contains("double"),
           s"Vec2 struct should not contain double:\n$structDef\nfull IR:\n$ir")
  }

  "f32 function parameter and return" in {
    llvmExit(
      """sq(x: f32) -> f32 = x * x
        |
        |main() -> int
        |    var r: f32 = sq(4.0)
        |    int(r)
        |""".stripMargin) shouldBe 16
  }

  "int promotes to f32 in mixed arithmetic" in {
    llvmExit(
      """main() -> int
        |    var x: f32 = 2.5
        |    var n: int = 4
        |    int(x * f32(n))
        |""".stripMargin) shouldBe 10
  }

  "f32 print promotes to double for printf" in {
    val ir = compileLLVM(
      """main()
        |    var x: f32 = 2.5
        |    println(x)
        |""".stripMargin)
    // printf is variadic: f32 must be fpext'd to double before the call
    assert(ir.contains("fpext float"),
           s"expected fpext for f32 in println, got:\n$ir")
  }

  "f32 negation uses fneg float" in {
    val ir = compileLLVM(
      """main() -> int
        |    var x: f32 = 1.5
        |    var y: f32 = -x
        |    int(y)
        |""".stripMargin)
    assert(ir.contains("fneg float"),
           s"expected fneg float for f32 unary minus, got:\n$ir")
  }
}
