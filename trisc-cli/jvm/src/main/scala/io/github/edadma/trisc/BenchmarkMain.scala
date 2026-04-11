package io.github.edadma.trisc

/** Standalone benchmark runner for TRISC CPU performance measurement.
  * Run: sbt "triscCliJVM/runMain io.github.edadma.trisc.BenchmarkMain"
  */
object BenchmarkMain:
  private def runBenchmark(name: String, source: String, expected: Long, maxCycles: Int = 500_000_000): Unit =
    val bootSource =
      s"""segment vectors
         |  dl 0xFFF8
         |  dl boot
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |  dl default_isr
         |segment code
         |boot
         |  movi r4, main
         |  jalr r6, r4
         |  halt
         |default_isr
         |  halt
         |""".stripMargin

    val bootTof = assemble(bootSource, relocatable = true)
    val driver = new SyslDriver
    val result = driver.compile(Map("main" -> source))
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val progTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(bootTof, progTof))

    val mem = new Memory("Memory", new RAM(0, Runtime.stdoutAddress.toInt))
    linked.load(mem)
    val cpu = new CPU(mem) { limit = maxCycles }
    cpu.reset()

    val startTime = System.nanoTime()
    cpu.run()
    val elapsed = System.nanoTime() - startTime

    val cycles = cpu.cycles
    val ms = elapsed / 1_000_000
    val mips = if elapsed > 0 then cycles.toDouble / (elapsed / 1_000_000_000.0) / 1_000_000 else 0
    val result_val = cpu.r(1).read
    val pass = if result_val == expected && cpu.state == State.Halt then "PASS" else "FAIL"

    println(f"$pass  $name%-40s  $cycles%,12d cycles  ${ms}%,6d ms  $mips%.1f MIPS")

  def main(args: Array[String]): Unit =
    println(f"${""}%-6s ${"Benchmark"}%-40s  ${"Cycles"}%12s  ${"Time"}%6s  ${""}%s")
    println("-" * 80)

    runBenchmark("bubble sort 500 elements", expected = 1,
      source = """main() -> int
        |    arr: [500]int
        |    for i = 0; i < 500; i++
        |        arr[i] = 500 - i
        |    n = 500
        |    while n > 1
        |        swapped = 0
        |        for j = 0; j < n - 1; j++
        |            if arr[j] > arr[j + 1] then
        |                tmp = arr[j]
        |                arr[j] = arr[j + 1]
        |                arr[j + 1] = tmp
        |                swapped = 1
        |        if swapped == 0 then n = 0
        |        else n -= 1
        |    ok = 1
        |    for i = 0; i < 499; i++
        |        if arr[i] > arr[i + 1] then ok = 0
        |    ok
        |""".stripMargin)

    runBenchmark("sieve of eratosthenes 10000", expected = 1229,
      source = """main() -> int
        |    sieve: [10000]byte
        |    for i = 0; i < 10000; i++
        |        sieve[i] = 1
        |    for i = 2; i * i < 10000; i++
        |        if sieve[i] == 1 then
        |            j = i * i
        |            while j < 10000
        |                sieve[j] = 0
        |                j += i
        |    count = 0
        |    for i = 2; i < 10000; i++
        |        if sieve[i] == 1 then count += 1
        |    count
        |""".stripMargin)

    runBenchmark("fibonacci iterative 10000", expected = 1,
      source = """main() -> int
        |    a = 0
        |    b = 1
        |    for i = 0; i < 10000; i++
        |        tmp = a + b
        |        a = b
        |        b = tmp
        |    if b != 0 then 1
        |    else 0
        |""".stripMargin)

    runBenchmark("nested loop 1000x1000", expected = 1,
      source = """main() -> int
        |    sum = 0
        |    for i = 0; i < 1000; i++
        |        for j = 0; j < 1000; j++
        |            sum += i + j
        |    if sum != 0 then 1
        |    else 0
        |""".stripMargin)
