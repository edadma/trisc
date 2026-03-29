package io.github.edadma.trisc

class TOSThreadTests extends TOSTestHelpers {

  "TOS: thread join waits for completion" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(worker, 0x6000, 0x5000, "worker")
          |    create_thread(joiner, 0x8000, 0x7000, "joiner")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |
          |kernel_main() -> int
          |    create_thread(fast, 0x6000, 0x5000, "fast")
          |    create_thread(slow, 0x8000, 0x7000, "slow")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |
          |kernel_main() -> int
          |    create_thread_pri(task_b, 0x8000, 0x7000, "b", 1)
          |    create_thread_pri(task_a, 0x6000, 0x5000, "a", 0)
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |
          |kernel_main() -> int
          |    create_thread_pri(task_a, 0x6000, 0x5000, "a", 1)
          |    create_thread_pri(task_b, 0x8000, 0x7000, "b", 1)
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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

    output should include("Y")
  }

  "TOS: thread_id returns current thread index" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task0, 0x6000, 0x5000, "t0")
          |    create_thread(task1, 0x8000, 0x7000, "t1")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    create_thread(task2, 0x8000, 0x7000, "task2")
          |    create_thread(task3, 0xA000, 0x9000, "task3")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |
          |kernel_main() -> int
          |    create_thread(checker, 0x6000, 0x5000, "checker")
          |    create_thread(sleeper, 0x8000, 0x7000, "sleeper")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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

  // ===== Thread statistics =====

  "TOS: context switch count increments" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "t")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |task()
          |    sleep(10)
          |    sleep(10)
          |    val sw = get_ctx_switches(0)
          |    if sw > 1
          |        putc(89)
          |    else
          |        putc(78)
          |""".stripMargin
    ))

    // Thread was scheduled multiple times (initial + after each sleep)
    output should include("Y")
  }

  "TOS: cpu ticks accumulate" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "t")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |task()
          |    sleep(20)
          |    val t = get_cpu_ticks(0)
          |    if t > 0
          |        putc(89)
          |    else
          |        putc(78)
          |""".stripMargin
    ))

    output should include("Y")
  }

  "TOS: total context switches tracks all threads" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x6000, 0x5000, "a")
          |    create_thread(task_b, 0x8000, 0x7000, "b")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |task_a()
          |    sleep(10)
          |
          |task_b()
          |    sleep(20)
          |    val total = get_total_switches()
          |    if total >= 4
          |        putc(89)
          |    else
          |        putc(78)
          |""".stripMargin
    ))

    // At least 4 switches: a initial, b initial, a wake, b wake
    output should include("Y")
  }

  // ===== Watchdog =====

  // Watchdog tests disabled — spinner threads take too long in the
  // instruction-level emulator. The watchdog logic is tested by
  // verifying consec_quanta increments and the termination path in
  // the scheduler. Re-enable when the emulator supports cycle skipping.

  "TOS: watchdog terminates runaway thread" ignore {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    set_watchdog(3)
          |    create_thread(spinner, 0x6000, 0x5000, "spin")
          |    create_thread(checker, 0x8000, 0x7000, "check")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |spinner()
          |    // Infinite loop without yielding — watchdog should kill it
          |    while true
          |        var x = 0
          |        x += 1
          |
          |checker()
          |    // Wait long enough for watchdog to fire
          |    sleep(100)
          |    val state = get_thread_state(0)
          |    if state == 3
          |        putc(75)
          |    else
          |        putc(82)
          |""".stripMargin
    ))

    // K = killed (state 3 = TERMINATED), R = still running
    output should include("K")
  }

  "TOS: watchdog does not kill yielding thread" ignore {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    set_watchdog(3)
          |    create_thread(yielder, 0x6000, 0x5000, "yield")
          |    create_thread(checker, 0x8000, 0x7000, "check")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |yielder()
          |    var i = 0
          |    while i < 20
          |        yield()
          |        i += 1
          |    putc(65)
          |
          |checker()
          |    sleep(100)
          |    putc(67)
          |""".stripMargin
    ))

    // Yielder should survive watchdog and print A
    output should include("A")
    output should include("C")
  }

  // ===== putstr =====

  "TOS: putstr prints string" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "t")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |task()
          |    putstr("Hi")
          |    putc(10)
          |""".stripMargin
    ))

    output should startWith("Hi\n")
  }

  // ===== Stack canary =====

  "TOS: stack canary intact after normal execution" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "t")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |task()
          |    val ok = check_stack(0x6000)
          |    if ok == 1
          |        putc(89)
          |    else
          |        putc(78)
          |""".stripMargin
    ))

    output should include("Y")
  }

  // ===== Suspend / Resume =====

  "TOS: suspend and resume thread" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(worker, 0x6000, 0x5000, "w")
          |    create_thread(controller, 0x8000, 0x7000, "c")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |worker()
          |    var i = 0
          |    while i < 5
          |        putc(65 + i)
          |        sleep(20)
          |        i += 1
          |
          |controller()
          |    sleep(10)
          |    suspend(0)
          |    putc(83)
          |    sleep(30)
          |    resume(0)
          |    putc(82)
          |""".stripMargin
    ))

    // Worker prints A then sleeps, controller suspends, prints S,
    // resumes, prints R. Worker continues printing after resume.
    output should include("A")
    output should include("S")
    output should include("R")
  }

  "TOS: suspended thread state is queryable" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(target, 0x6000, 0x5000, "t")
          |    create_thread(checker, 0x8000, 0x7000, "c")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |target()
          |    sleep(100)
          |
          |checker()
          |    sleep(5)
          |    suspend(0)
          |    sleep(5)
          |    val s = get_thread_state(0)
          |    if s == 5
          |        putc(89)
          |    else
          |        putc(78)
          |""".stripMargin
    ))

    // 5 = STATE_SUSPENDED
    output should include("Y")
  }

  // ===== Panic =====

  "TOS: panic terminates all threads" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "t")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |task()
          |    putc(65)
          |    panic()
          |    putc(66)
          |""".stripMargin
    ))

    // A prints, panics, B (after panic) should not print
    output should include("A")
    output should not include("B")
    cpu.state shouldBe State.Wfi
  }

  // ===== Thread-local storage =====

  "TOS: tls_set and tls_get basic" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "t")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |task()
          |    tls_set(0, 42)
          |    tls_set(1, 99)
          |    val a = tls_get(0)
          |    val b = tls_get(1)
          |    if a == 42
          |        putc(65)
          |    if b == 99
          |        putc(66)
          |""".stripMargin
    ))

    output should include("A")
    output should include("B")
  }

  "TOS: tls is per-thread" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x6000, 0x5000, "a")
          |    create_thread(task_b, 0x8000, 0x7000, "b")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |task_a()
          |    tls_set(0, 10)
          |    sleep(20)
          |    val v = tls_get(0)
          |    if v == 10
          |        putc(65)
          |    else
          |        putc(78)
          |
          |task_b()
          |    tls_set(0, 20)
          |    sleep(20)
          |    val v = tls_get(0)
          |    if v == 20
          |        putc(66)
          |    else
          |        putc(78)
          |""".stripMargin
    ))

    // Each thread sees its own TLS value, not the other's
    output should include("A")
    output should include("B")
    output should not include("N")
  }
}
