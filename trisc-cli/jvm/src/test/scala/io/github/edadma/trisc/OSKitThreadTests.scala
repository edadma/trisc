package io.github.edadma.trisc

class OSKitThreadTests extends OSKitTestHelpers {

  "TOS: thread join waits for completion" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(worker, 0x10000, 0xF000, "worker")
          |    create_thread(joiner, 0x14000, 0x13000, "joiner")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |worker()
          |    sleep(30)
          |    putc('W')
          |
          |joiner()
          |    join(0)
          |    putc('J')
          |""".stripMargin
    ))

    // W = worker done, J = joiner proceeds after join
    // W must appear before J
    output should include("W")
    output should include("J")
    output.indexOf('W') should be < output.indexOf('J')
  }

  "TOS: join on already-terminated thread returns immediately" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(fast, 0x10000, 0xF000, "fast")
          |    create_thread(slow, 0x14000, 0x13000, "slow")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |fast()
          |    putc('F')
          |
          |slow()
          |    sleep(50)
          |    join(0)
          |    putc('S')
          |""".stripMargin
    ))

    // F prints immediately, S prints after join (which should return immediately since fast is done)
    output should include("F")
    output should include("S")
  }

  "TOS: higher priority thread runs first" taggedAs Slow in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.config.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread_pri(task_b, 0x14000, 0x13000, "b", 1, KERNEL_L1_BASE)
          |    create_thread_pri(task_a, 0x10000, 0xF000, "a", 0, KERNEL_L1_BASE)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task_a()
          |    putc('H')
          |
          |task_b()
          |    putc('L')
          |""".stripMargin
    ))

    // first_thread_ssp picks highest priority — H runs first
    output should include("H")
    output should include("L")
    output.indexOf('H') should be < output.indexOf('L')
  }

  "TOS: equal priority threads round-robin" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.config.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread_pri(task_a, 0x10000, 0xF000, "a", 1, KERNEL_L1_BASE)
          |    create_thread_pri(task_b, 0x14000, 0x13000, "b", 1, KERNEL_L1_BASE)
          |    timer_init(1000)
          |    first_thread_ssp()
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
          |        sleep(10)
          |        i += 1
          |""".stripMargin
    ))

    // Both should run and interleave
    output.count(_ == 'A') shouldBe 3
    output.count(_ == 'B') shouldBe 3
  }

  "TOS: uptime returns a value" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task, 0x10000, 0xF000, "task")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    val t = uptime()
          |    putc('Y')
          |""".stripMargin
    ))

    output should include("Y")
  }

  "TOS: uptime increases after sleep" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task, 0x10000, 0xF000, "task")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    val t1 = uptime()
          |    sleep(10)
          |    val t2 = uptime()
          |    if t2 > t1
          |        putc('Y')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("Y")
  }

  "TOS: thread_id returns current thread index" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task0, 0x10000, 0xF000, "t0")
          |    create_thread(task1, 0x14000, 0x13000, "t1")
          |    timer_init(1000)
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

  "TOS: get_thread_count returns number of threads" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task, 0x10000, 0xF000, "task")
          |    create_thread(task2, 0x14000, 0x13000, "task2")
          |    create_thread(task3, 0x18000, 0x17000, "task3")
          |    timer_init(1000)
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

  "TOS: get_thread_state returns correct states" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(checker, 0x10000, 0xF000, "checker")
          |    create_thread(sleeper, 0x14000, 0x13000, "sleeper")
          |    timer_init(1000)
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

  "TOS: sleep_until blocks until absolute tick" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task, 0x10000, 0xF000, "task")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    val target = uptime() + 20
          |    sleep_until(target)
          |    val now = uptime()
          |    if now >= target
          |        putc('Y')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("Y")
  }

  // ===== Thread statistics =====

  "TOS: context switch count increments" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    sleep(10)
          |    sleep(10)
          |    val sw = get_ctx_switches(0)
          |    if sw > 1
          |        putc('Y')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    // Thread was scheduled multiple times (initial + after each sleep)
    output should include("Y")
  }

  "TOS: cpu ticks accumulate" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    sleep(20)
          |    val t = get_cpu_ticks(0)
          |    if t > 0
          |        putc('Y')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("Y")
  }

  "TOS: total context switches tracks all threads" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x10000, 0xF000, "a")
          |    create_thread(task_b, 0x14000, 0x13000, "b")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task_a()
          |    sleep(10)
          |
          |task_b()
          |    sleep(20)
          |    val total = get_total_switches()
          |    if total >= 4
          |        putc('Y')
          |    else
          |        putc('N')
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

  "TOS: watchdog terminates runaway thread" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    set_watchdog(3)
          |    create_thread(spinner, 0x10000, 0xF000, "spin")
          |    create_thread(checker, 0x14000, 0x13000, "check")
          |    timer_init(1000)
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
          |        putc('K')
          |    else
          |        putc('R')
          |""".stripMargin
    ))

    // K = killed (state 3 = TERMINATED), R = still running
    output should include("K")
  }

  "TOS: watchdog does not kill yielding thread" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    set_watchdog(3)
          |    create_thread(yielder, 0x10000, 0xF000, "yield")
          |    create_thread(checker, 0x14000, 0x13000, "check")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |yielder()
          |    var i = 0
          |    while i < 20
          |        yield()
          |        i += 1
          |    putc('A')
          |
          |checker()
          |    sleep(100)
          |    putc('C')
          |""".stripMargin
    ))

    // Yielder should survive watchdog and print A
    output should include("A")
    output should include("C")
  }

  // ===== putstr =====

  "TOS: putstr prints string" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    putstr("Hi")
          |    putc('\n')
          |""".stripMargin
    ))

    output should startWith("Hi\n")
  }

  // ===== Stack canary =====

  "TOS: stack canary intact after normal execution" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    val ok = check_stack(0x10000)
          |    if ok == 1
          |        putc('Y')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("Y")
  }

  // ===== Suspend / Resume =====

  "TOS: suspend and resume thread" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(worker, 0x10000, 0xF000, "w")
          |    create_thread(controller, 0x14000, 0x13000, "c")
          |    timer_init(1000)
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
          |    putc('S')
          |    sleep(30)
          |    resume(0)
          |    putc('R')
          |""".stripMargin
    ))

    // Worker prints A then sleeps, controller suspends, prints S,
    // resumes, prints R. Worker continues printing after resume.
    output should include("A")
    output should include("S")
    output should include("R")
  }

  "TOS: suspended thread state is queryable" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(target, 0x10000, 0xF000, "t")
          |    create_thread(checker, 0x14000, 0x13000, "c")
          |    timer_init(1000)
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
          |        putc('Y')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    // 5 = STATE_SUSPENDED
    output should include("Y")
  }

  // ===== Panic =====

  "TOS: panic terminates all threads" taggedAs Slow in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    putc('A')
          |    panic()
          |    putc('B')
          |""".stripMargin
    ))

    // A prints, panics, B (after panic) should not print
    output should include("A")
    output should not include("B")
  }

  // ===== Thread-local storage =====

  "TOS: tls_set and tls_get basic" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    tls_set(0, 42)
          |    tls_set(1, 99)
          |    val a = tls_get(0)
          |    val b = tls_get(1)
          |    if a == 42
          |        putc('A')
          |    if b == 99
          |        putc('B')
          |""".stripMargin
    ))

    output should include("A")
    output should include("B")
  }

  "TOS: tls is per-thread" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x10000, 0xF000, "a")
          |    create_thread(task_b, 0x14000, 0x13000, "b")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task_a()
          |    tls_set(0, 10)
          |    sleep(20)
          |    val v = tls_get(0)
          |    if v == 10
          |        putc('A')
          |    else
          |        putc('N')
          |
          |task_b()
          |    tls_set(0, 20)
          |    sleep(20)
          |    val v = tls_get(0)
          |    if v == 20
          |        putc('B')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    // Each thread sees its own TLS value, not the other's
    output should include("A")
    output should include("B")
    output should not include("N")
  }

  "TOS: getuid returns current thread uid (default 0)" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(worker, 0x10000, 0xF000, "worker")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |worker()
          |    val u = getuid()
          |    if u == 0
          |        putc('Z')
          |    else
          |        putc('X')
          |""".stripMargin
    ))

    output should include("Z")
  }

  "TOS: setuid sets uid, getuid returns new value" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(worker, 0x10000, 0xF000, "worker")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |worker()
          |    setuid(42)
          |    val u = getuid()
          |    if u == 42
          |        putc('Y')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("Y")
    output should not include("N")
  }

  "TOS: child thread inherits uid from parent" taggedAs Slow in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
          |
          |kernel_main() -> int
          |    create_thread(parent, 0x10000, 0xF000, "parent")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |parent()
          |    setuid(7)
          |    create_thread(child, 0x14000, 0x13000, "child")
          |    sleep(20)
          |
          |child()
          |    val u = getuid()
          |    if u == 7
          |        putc('I')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("I")
    output should not include("N")
  }
}
