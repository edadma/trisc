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
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    putc('H')
          |    putc('i')
          |    putc('\n')
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
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task1()
          |    putc('A')
          |
          |task2()
          |    putc('B')
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
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    putc('A')
          |    sleep(5)
          |    putc('B')
          |    sleep(5)
          |    putc('C')
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
          |        putc('A')
          |        sleep(10)
          |        i += 1
          |
          |task_b()
          |    var i = 0
          |    while i < 3
          |        putc('B')
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
          |    timer_init(1000)
          |    first_thread_ssp()
          |""".stripMargin
    ))

    // A prints at ticks 0,10,20 — B prints at ticks 0,20,40
    // Expected pattern: AB A AB A B (roughly 2:1)
    output.count(_ == 'A') shouldBe 3
    output.count(_ == 'B') shouldBe 3
  }

}
