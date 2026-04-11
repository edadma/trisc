package io.github.edadma.trisc

object CheckThreadStruct:
  def main(args: Array[String]): Unit =
    def readLsysl(path: String): String =
      LiterateRenderer.tangle(new LiterateParser().parse(scala.io.Source.fromFile(path).mkString))

    val sources = Map(
      "oskit/kernel/kernel" -> readLsysl("oskit/kernel/kernel.lsysl"),
      "oskit/services/services" -> readLsysl("oskit/services/services.lsysl"),
      "oskit/kernel/timer" -> readLsysl("oskit/kernel/timer.lsysl"),
      "oskit/sync/semaphore" -> readLsysl("oskit/sync/semaphore.lsysl"),
      "oskit/sync/mutex" -> readLsysl("oskit/sync/mutex.lsysl"),
      "oskit/ipc/ipc" -> readLsysl("oskit/ipc/ipc.lsysl"),
      "std/mem/mem" -> readLsysl("std/mem/mem.lsysl"),
      "oskit/hal/mem" -> readLsysl("oskit/hal/mem_dma.lsysl"),
      "std/debug/debug" -> readLsysl("std/debug/debug.lsysl"),
      "oskit/drivers/kbd/keyboard" -> readLsysl("oskit/drivers/kbd/keyboard.lsysl"),
      "oskit/drivers/tty/tty" -> readLsysl("oskit/drivers/tty/tty.lsysl"),
      "posix/unistd/sbrk" -> scala.io.Source.fromFile("oskit/lib/sbrk.sysl").mkString,
      "posix/stdlib/alloc" -> scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString,
      "posix/string/string" -> scala.io.Source.fromFile("posix/string/string.sysl").mkString,
      "posix/ctype/ctype" -> scala.io.Source.fromFile("posix/ctype/ctype.sysl").mkString,
      "app" -> "import oskit.kernel.*\nimport oskit.services.*\nkernel_main() -> int\n    0\n",
    )

    val driver = new SyslDriver
    val result = driver.compile(sources)
    val codegen = new SyslTriscCodegen

    // Dump FULL generated assembly for the timer module
    for unit <- result.units if unit.name == "oskit/kernel/timer" do
      val asm = codegen.generate(unit.typed)
      println("=== FULL timer module assembly ===")
      println(asm)

    // Also dump register_irq from kernel
    for unit <- result.units if unit.name == "oskit/kernel/kernel" do
      val asm = codegen.generate(unit.typed)
      val lines = asm.split("\n")
      val idx = lines.indexWhere(_.contains("register_irq:"))
      if idx >= 0 then
        println("\n=== register_irq function ===")
        for i <- idx to (idx + 20).min(lines.length - 1) do
          println(lines(i))
