package io.github.edadma.trisc

class SyslCodegenConstTests extends SyslCodegenHelpers {

  "return constant 0" in {
    compileAndRun("main() -> int = 0\n") shouldBe 0
  }

  "return constant 42" in {
    compileAndRun("main() -> int = 42\n") shouldBe 42
  }

  "return constant 255" in {
    compileAndRun("main() -> int = 255\n") shouldBe 255
  }

  "return large constant" in {
    compileAndRun("main() -> int = 1000\n") shouldBe 1000
  }

  "unary minus" in {
    compileAndRun("main() -> int = -42\n") shouldBe -42
  }

  "global val with negative value" in {
    compileAndRun(
      """val NEG = -1
        |main() -> int = NEG
        |""".stripMargin) shouldBe -1
  }

  "global val with negative value used in comparison" in {
    compileAndRun(
      """val EMPTY = -1
        |var x = -1
        |
        |main() -> int
        |    if x == EMPTY
        |        42
        |    else
        |        0
        |""".stripMargin) shouldBe 42
  }

  "global val with negative value in array init" in {
    compileAndRun(
      """val NONE = -1
        |var arr: [4]int
        |
        |init()
        |    var i = 0
        |    while i < 4
        |        arr[i] = NONE
        |        i += 1
        |
        |main() -> int
        |    init()
        |    arr[0] + arr[1] + arr[2] + arr[3]
        |""".stripMargin) shouldBe -4
  }

  "global val with unary minus expression" in {
    compileAndRun(
      """val A = -42
        |val B = -100
        |main() -> int = A + B
        |""".stripMargin) shouldBe -142
  }

  "large i64 constant high bits via ldc" in {
    compileAndRun(
      """main() -> int
        |    val x: i64 = i64(0x20000FFFFFE0007F)
        |    int((x >> 32) & i64(0xFFFFFFFF))
        |""".stripMargin) shouldBe 0x20000FFFL
  }

  "large i64 constant bit test via ldc" in {
    compileAndRun(
      """main() -> int
        |    val mask: i64 = i64(0x20000FFFFFE0007F)
        |    if (mask & (i64(1) << 31)) != 0
        |        1
        |    else
        |        0
        |""".stripMargin) shouldBe 1
  }

  "large i64 constant cross-module via ldc" in {
    compileMultiAndRun(Map(
      "lib" ->
        """module lib
          |get_mask() -> i64
          |    i64(0x20000FFFFFE0007F)
          |""".stripMargin,
      "app" ->
        """import lib.get_mask
          |main() -> int
          |    val mask = get_mask()
          |    if (mask & (i64(1) << 31)) != 0
          |        1
          |    else
          |        0
          |""".stripMargin,
    )) shouldBe 1
  }

  "module-level val with bitwise OR and shift" in {
    compileAndRun(
      """val MASK = 0x7F | (7 << 21) | (0xFFFFF << 24)
        |main() -> int = (MASK >> 24) & 0xFFFFF
        |""".stripMargin) shouldBe 0xFFFFF
  }

  "module-level val with i64 shift and OR" in {
    compileMultiAndRun(Map(
      "lib" ->
        """module lib
          |val MASK: i64 = i64(0x7F) | (i64(7) << 21) | (i64(0xFFFFFFFF) << 24) | (i64(1) << 61)
          |get_mask() -> i64 = MASK
          |""".stripMargin,
      "app" ->
        """import lib.get_mask
          |main() -> int
          |    val mask = get_mask()
          |    int((mask >> 32) & i64(0xFFFFFFFF))
          |""".stripMargin,
    )) shouldBe 0x20FFFFFFL
  }

  "large i64 constant survives two-pass link" in {
    // Replicate the two-pass linking used by compileServerTrb
    val driver = new SyslDriver
    val result = driver.compile(Map(
      "lib" ->
        """module lib
          |
          |get_mask() -> i64
          |    i64(0x20000FFFFFE0007F)
          |""".stripMargin,
      "app" ->
        """import lib.get_mask
          |
          |main() -> int
          |    val mask = get_mask()
          |    if (mask & (i64(1) << 31)) != 0
          |        1
          |    else
          |        0
          |""".stripMargin,
    ))
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    // Pass 1: merge into relocatable
    val merged = Linker.link(tofs, relocatable = true)
    // Pass 2: link with boot TOF (simulates syscallTof + syslTof link)
    val linked = Linker.link(Seq(Runtime.bootTof, merged, Runtime.ioTof))

    val stdout = new Stdout(Runtime.stdoutAddress)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 100000; quiet = true }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 1
  }

  "multiple large i64 constants passed to function" in {
    // Replicates rs_set_server_priv: init multiple i64 locals, pass to function
    compileMultiAndRun(Map(
      "lib" ->
        """module lib
          |
          |check(lo: i64, hi: i64, ipc: int) -> int
          |    int((lo >> 32) & i64(0xFFFFFFFF))
          |""".stripMargin,
      "app" ->
        """import lib.check
          |
          |main() -> int
          |    var lo: i64 = i64(0x20000FFFFFE0007F)
          |    var hi: i64 = i64(0x182)
          |    var ipc = 0xFF
          |    check(lo, hi, ipc)
          |""".stripMargin,
    )) shouldBe 0x20000FFFL
  }

  "multiple large i64 constants with server-style link at 0xD0000" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  _default_: 0x0
        |  code: 0xD0000
        |  rodata
        |  data
        |  bss
        |ENTRY _start
        |""".stripMargin): @unchecked

    val driver = new SyslDriver
    val result = driver.compile(Map(
      "lib" ->
        """module lib
          |
          |check(lo: i64, hi: i64, ipc: int) -> int
          |    int((lo >> 32) & i64(0xFFFFFFFF))
          |""".stripMargin,
      "app" ->
        """import lib.check
          |
          |main() -> int
          |    var lo: i64 = i64(0x20000FFFFFE0007F)
          |    var hi: i64 = i64(0x182)
          |    var ipc = 0xFF
          |    check(lo, hi, ipc)
          |""".stripMargin,
    ))
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val merged = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(Runtime.bootTof, merged, Runtime.ioTof), script, 0)

    val stdout = new Stdout(Runtime.stdoutAddress)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 100000; quiet = true }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 0x20000FFFL
  }

  "large i64 constant survives server-style link at 0xD0000" in {
    // Replicate exact compileServerTrb pipeline: two-pass + linker script
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  _default_: 0x0
        |  code: 0xD0000
        |  rodata
        |  data
        |  bss
        |ENTRY _start
        |""".stripMargin): @unchecked

    val driver = new SyslDriver
    val result = driver.compile(Map(
      "lib" ->
        """module lib
          |
          |get_mask() -> i64
          |    i64(0x20000FFFFFE0007F)
          |""".stripMargin,
      "app" ->
        """import lib.get_mask
          |
          |main() -> int
          |    val mask = get_mask()
          |    int((mask >> 32) & i64(0xFFFFFFFF))
          |""".stripMargin,
    ))
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    // Pass 1: merge into relocatable
    val merged = Linker.link(tofs, relocatable = true)
    // Pass 2: link with boot TOF + linker script at 0xD0000
    val linked = Linker.link(Seq(Runtime.bootTof, merged, Runtime.ioTof), script, 0)

    val stdout = new Stdout(Runtime.stdoutAddress)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 100000; quiet = true }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 0x20000FFFL
  }
}
