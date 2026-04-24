package io.github.edadma.trisc

object OSKitTestData:
  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  lazy val bootAsm: String = scala.io.Source.fromFile("oskit/arch/trisc/boot.asm").mkString
  lazy val kernelSysl: String = readLsysl("oskit/kernel/kernel.lsysl")
  lazy val servicesSysl: String = readLsysl("oskit/services/services.lsysl")
  lazy val semaphoreSysl: String = readLsysl("oskit/sync/semaphore.lsysl")
  lazy val mutexSysl: String = readLsysl("oskit/sync/mutex.lsysl")
  lazy val condvarSysl: String = readLsysl("oskit/sync/condvar.lsysl")
  lazy val barrierSysl: String = readLsysl("oskit/sync/barrier.lsysl")
  lazy val rwlockSysl: String = readLsysl("oskit/sync/rwlock.lsysl")
  lazy val channelSysl: String = readLsysl("oskit/sync/channel.lsysl")
  lazy val mailboxSysl: String = readLsysl("oskit/sync/mailbox.lsysl")
  lazy val rbtreeSysl: String = readLsysl("oskit/kernel/rbtree.lsysl")
  lazy val rmutexSysl: String = readLsysl("oskit/sync/rmutex.lsysl")
  lazy val qsetSysl: String = readLsysl("oskit/sync/qset.lsysl")
  lazy val timerSysl: String = readLsysl("oskit/arch/trisc/timer.lsysl")
  lazy val pimutexSysl: String = readLsysl("oskit/sync/pimutex.lsysl")
  lazy val memSysl: String = readLsysl("std/mem/mem.lsysl")
  lazy val halMemSysl: String = readLsysl("oskit/hal/mem_dma.lsysl")
  lazy val archVmSysl: String = readLsysl("oskit/arch/trisc/vm.lsysl")
  lazy val archCpuSysl: String = readLsysl("oskit/arch/trisc/cpu.lsysl")
  lazy val configSysl: String = readSysl("oskit/config/config.sysl")
  private def readSysl(path: String): String = scala.io.Source.fromFile(path).mkString

  lazy val sbrkSysl: String = readSysl("oskit/lib/sbrk.sysl")
  lazy val posixStringSysl: String = readSysl("posix/string/string.sysl")
  lazy val posixCtypeSysl: String = readSysl("posix/ctype/ctype.sysl")
  lazy val posixAllocSysl: String = readSysl("posix/stdlib/alloc.sysl")

  lazy val linkerScript: LinkerScript =
    LinkerScriptParser.parse(scala.io.Source.fromFile("tos/linker.ld").mkString) match
      case Right(s) => s
      case Left(e)  => throw new RuntimeException(s"Failed to parse linker script: $e")
  lazy val tasksSysl: String = readLsysl("examples/tos-demo/tasks.lsysl")
  lazy val mainSysl: String = readLsysl("examples/tos-demo/main.lsysl")

  val minimalBoot: String =
    """STDOUT = 0x10000
      |
      |segment vectors
      |
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
      |
      |segment code
      |
      |extern main
      |
      |global boot, func
      |entry boot
      |
      |boot
      |  movi r4, main
      |  jalr r6, r4
      |  halt
      |
      |global putchar, func
      |
      |putchar
      |  movi r2, STDOUT
      |  stb r1, r2, r0
      |  jalr r0, r6
      |
      |global default_isr, func
      |
      |default_isr
      |  halt
      |""".stripMargin

end OSKitTestData
