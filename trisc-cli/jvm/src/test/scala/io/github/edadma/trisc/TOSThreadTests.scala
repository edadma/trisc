package io.github.edadma.trisc

class TOSThreadTests extends TOSTestHelpers {

  "TOS: thread join waits for completion" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(worker, 0x6000, 0x5000, "worker")
          |    create_thread(joiner, 0x8000, 0x7000, "joiner")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |worker()
          |    sleep(30)
          |    putc(87)
          |
          |joiner()
          |    join(0)
          |    putc(74)
          |""".stripMargin
    ))

    // W = worker done, J = joiner proceeds after join
    // W must appear before J
    output should include("W")
    output should include("J")
    output.indexOf('W') should be < output.indexOf('J')
  }

  "TOS: join on already-terminated thread returns immediately" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(fast, 0x6000, 0x5000, "fast")
          |    create_thread(slow, 0x8000, 0x7000, "slow")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |fast()
          |    putc(70)
          |
          |slow()
          |    sleep(50)
          |    join(0)
          |    putc(83)
          |""".stripMargin
    ))

    // F prints immediately, S prints after join (which should return immediately since fast is done)
    output should include("F")
    output should include("S")
  }

  "TOS: higher priority thread runs first" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread_pri(task_b, 0x8000, 0x7000, "b", 1)
          |    create_thread_pri(task_a, 0x6000, 0x5000, "a", 0)
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |task_a()
          |    putc(72)
          |
          |task_b()
          |    putc(76)
          |""".stripMargin
    ))

    // first_thread_ssp picks highest priority — H runs first
    output should include("H")
    output should include("L")
    output.indexOf('H') should be < output.indexOf('L')
  }

  "TOS: equal priority threads round-robin" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread_pri(task_a, 0x6000, 0x5000, "a", 1)
          |    create_thread_pri(task_b, 0x8000, 0x7000, "b", 1)
          |    timer_init(10)
          |    first_thread_ssp()
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
          |        sleep(10)
          |        i += 1
          |""".stripMargin
    ))

    // Both should run and interleave
    output.count(_ == 'A') shouldBe 3
    output.count(_ == 'B') shouldBe 3
  }

  "TOS: uptime returns a value" in {
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
          |    val t = uptime()
          |    putc(89)
          |""".stripMargin
    ))

    output should include("Y")
  }

  "TOS: uptime increases after sleep" in {
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
          |    val t1 = uptime()
          |    sleep(10)
          |    val t2 = uptime()
          |    if t2 > t1
          |        putc(89)
          |    else
          |        putc(78)
          |""".stripMargin
    ))

    println(s"DEBUG: state=${cpu.state} pc=${cpu.pc}")
    output should include("Y")
  }

  "TOS: thread_id returns current thread index" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(task0, 0x6000, 0x5000, "t0")
          |    create_thread(task1, 0x8000, 0x7000, "t1")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |task0()
          |    putc(48 + thread_id())
          |
          |task1()
          |    putc(48 + thread_id())
          |""".stripMargin
    ))

    // task0 prints '0', task1 prints '1'
    output should include("0")
    output should include("1")
  }

  "TOS: get_thread_count returns number of threads" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    create_thread(task2, 0x8000, 0x7000, "task2")
          |    create_thread(task3, 0xA000, 0x9000, "task3")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |task()
          |    putc(48 + get_thread_count())
          |
          |task2()
          |    sleep(100)
          |
          |task3()
          |    sleep(100)
          |""".stripMargin
    ))

    // 3 threads created, prints '3'
    output should include("3")
  }

  "TOS: get_thread_state returns correct states" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(checker, 0x6000, 0x5000, "checker")
          |    create_thread(sleeper, 0x8000, 0x7000, "sleeper")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |checker()
          |    sleep(10)
          |    // sleeper should be blocked (state 2)
          |    val s = get_thread_state(1)
          |    putc(48 + s)
          |
          |sleeper()
          |    sleep(100)
          |""".stripMargin
    ))

    // sleeper is blocked (state 2), prints '2'
    output should include("2")
  }

  "TOS: sleep_until blocks until absolute tick" in {
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
          |    val target = uptime() + 20
          |    sleep_until(target)
          |    val now = uptime()
          |    if now >= target
          |        putc(89)
          |    else
          |        putc(78)
          |""".stripMargin
    ))

    output should include("Y")
  }
}
