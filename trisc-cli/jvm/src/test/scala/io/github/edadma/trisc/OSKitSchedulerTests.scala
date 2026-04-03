package io.github.edadma.trisc

class OSKitSchedulerTests extends OSKitTestHelpers {

  "Scheduler: three threads same priority" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x10000, 0xF000, "a")
          |    create_thread(task_b, 0x14000, 0x13000, "b")
          |    create_thread(task_c, 0x18000, 0x17000, "c")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task_a()
          |    putc('A')
          |
          |task_b()
          |    putc('B')
          |
          |task_c()
          |    putc('C')
          |""".stripMargin
    ))

    info(s"output: '$output' state: ${cpu.state}")
    output should include("A")
    output should include("B")
    output should include("C")
  }

  "Scheduler: basic two-priority smoke test" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    create_thread_pri(task_b, 0x14000, 0x13000, "b", 1)
          |    create_thread_pri(task_a, 0x10000, 0xF000, "a", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task_a()
          |    putc('A')
          |
          |task_b()
          |    putc('B')
          |""".stripMargin
    ))

    info(s"output: '$output' state: ${cpu.state}")
    output should include("A")
    output should include("B")
    // A (pri=0, higher) should run before B (pri=1, lower)
    // But verify they both actually printed
    info(s"smoke output: '$output'")
    output.indexOf('A') should be < output.indexOf('B')
  }

  "Scheduler: quantum expiry causes round-robin rotation" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x10000, 0xF000, "a")
          |    create_thread(task_b, 0x14000, 0x13000, "b")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task_a()
          |    var i = 0
          |    while i < 4
          |        putc('A')
          |        sleep(10)
          |        i += 1
          |
          |task_b()
          |    var i = 0
          |    while i < 4
          |        putc('B')
          |        sleep(10)
          |        i += 1
          |""".stripMargin
    ))

    // Both should get equal time at same priority
    output.count(_ == 'A') shouldBe 4
    output.count(_ == 'B') shouldBe 4
  }

  "Scheduler: high priority preempts low priority" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    create_thread(high, 0x20000, 0x1F000, "high")
          |    create_thread_pri(low, 0x22000, 0x21000, "low", 2)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |high()
          |    putc('H')
          |    sleep(20)
          |    putc('H')
          |
          |low()
          |    putc('L')
          |    sleep(20)
          |    putc('L')
          |""".stripMargin
    ))

    // High (pri=0) runs first, then low runs while high sleeps
    output should include("H")
    output should include("L")
    output.indexOf('H') should be < output.indexOf('L')
  }

  "Scheduler: blocked thread removed from queue" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    create_thread(blocker, 0x10000, 0xF000, "blocker")
          |    create_thread(runner, 0x14000, 0x13000, "runner")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |blocker()
          |    putc('B')
          |    sleep(50)
          |    putc('B')
          |
          |runner()
          |    sleep(10)
          |    putc('R')
          |    sleep(10)
          |    putc('R')
          |""".stripMargin
    ))

    // Blocker prints B, sleeps. Runner prints R twice while blocker sleeps.
    // Blocker prints B again after wake.
    output.count(_ == 'B') shouldBe 2
    output.count(_ == 'R') shouldBe 2
  }

  "Scheduler: three priority levels strict ordering" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    create_thread_pri(high, 0x20000, 0x1F000, "high", 0)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(low, 0x24000, 0x23000, "low", 2)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |high()
          |    putc('H')
          |
          |med()
          |    putc('M')
          |
          |low()
          |    putc('L')
          |""".stripMargin
    ))

    info(s"output: '$output' state: ${cpu.state} pc: ${cpu.pc}")
    // Strict priority: H before M before L
    output should include("H")
    output should include("M")
    output should include("L")
    val h = output.indexOf('H')
    val m = output.indexOf('M')
    val l = output.indexOf('L')
    h should be < m
    m should be < l
  }

  "Scheduler: yield moves to back of queue" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x10000, 0xF000, "a")
          |    create_thread(task_b, 0x14000, 0x13000, "b")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task_a()
          |    putc('A')
          |    yield()
          |    putc('A')
          |
          |task_b()
          |    putc('B')
          |    yield()
          |    putc('B')
          |""".stripMargin
    ))

    // A prints, yields → B prints, yields → A prints → B prints: ABAB
    output shouldBe "ABAB"
  }

  "Scheduler: unblock higher priority preempts current" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    create_thread(high, 0x20000, 0x1F000, "high")
          |    create_thread_pri(low, 0x22000, 0x21000, "low", 2)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |high()
          |    putc('1')
          |    sleep(20)
          |    putc('2')
          |
          |low()
          |    var i = 0
          |    while i < 5
          |        putc('L')
          |        sleep(10)
          |        i += 1
          |""".stripMargin
    ))

    // High prints 1, sleeps. Low prints L. High wakes, preempts, prints 2.
    // Then low continues.
    output should include("1")
    output should include("2")
    output should include("L")
    // 2 must come before the last L's
    val idx2 = output.indexOf('2')
    val lastL = output.lastIndexOf('L')
    idx2 should be < lastL
  }
}
