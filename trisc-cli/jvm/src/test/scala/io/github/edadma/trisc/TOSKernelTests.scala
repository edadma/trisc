package io.github.edadma.trisc

class TOSKernelTests extends TOSTestHelpers {

  "TOS: putc syscall prints character" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |task()
          |    putc(72)
          |    putc(105)
          |    putc(10)
          |""".stripMargin
    ), maxCycles = 100000)

    output should startWith("Hi\n")
  }

  "TOS: thread exit works cleanly" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(task1, 0x6000, 0x5000, "t1")
          |    create_thread(task2, 0x8000, 0x7000, "t2")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |task1()
          |    putc(65)
          |
          |task2()
          |    putc(66)
          |""".stripMargin
    ), maxCycles = 100000)

    // Both tasks print and exit — output should contain both A and B
    output should include("A")
    output should include("B")
  }

  "TOS: sleep syscall delays output" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |task()
          |    putc(65)
          |    sleep(5)
          |    putc(66)
          |    sleep(5)
          |    putc(67)
          |""".stripMargin
    ))

    output should startWith("ABC")
  }

  "TOS: two tasks interleave with sleep" in {
    val (_, output) = runTOS(Map(
      "tasks" ->
        """import "services"
          |
          |task_a()
          |    var i = 0
          |    while i < 3
          |        putc(65)
          |        sleep(10)
          |        i += 1
          |
          |task_b()
          |    var i = 0
          |    while i < 3
          |        putc(66)
          |        sleep(20)
          |        i += 1
          |""".stripMargin,
      "app" ->
        """import "kernel"
          |import "tasks"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x6000, 0x5000, "a")
          |    create_thread(task_b, 0x8000, 0x7000, "b")
          |    timer_init(10)
          |    first_thread_ssp()
          |""".stripMargin
    ))

    // A prints at ticks 0,10,20 — B prints at ticks 0,20,40
    // Expected pattern: AB A AB A B (roughly 2:1)
    output.count(_ == 'A') shouldBe 3
    output.count(_ == 'B') shouldBe 3
  }

  "DEBUG: dump linked layout" ignore {
    val bootTof = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "kernel" -> kernelSysl, "services" -> servicesSysl, "timer" -> timerSysl, "semaphore" -> semaphoreSysl,
      "mutex" -> mutexSysl, "condvar" -> condvarSysl, "barrier" -> barrierSysl,
      "rwlock" -> rwlockSysl, "channel" -> channelSysl, "mailbox" -> mailboxSysl,
      "rmutex" -> rmutexSysl, "qset" -> qsetSysl,
      "app" -> """import "kernel"
        |import "services"
        |import "timer"
        |kernel_main() -> int
        |    create_thread(task, 0x6000, 0x5000, "t")
        |    timer_init(10)
        |    first_thread_ssp()
        |task()
        |    putc(65)
        |    sleep(5)
        |    putc(66)
        |""".stripMargin)
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(bootTof, syslTof), linkerScript, 0)

    info(linked.dumpLayout)
    linked.segments.size should be > 0
  }
}
