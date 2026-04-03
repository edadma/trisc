package io.github.edadma.trisc

// Investigating: implicit thread exit (returning from function → thread_exit trampoline)
// sometimes triggers a CPU exception (default_isr → halt).
//
// Known: the PI test with 3 threads (low/med/high), med doing sleep(15)+putc(98)
// then implicitly returning, halted. Adding exit() or an extra putc fixed it.
//
// Strategy: systematically vary function body, thread count, priorities,
// and imports to isolate the trigger.

class TOSExitBugTests extends TOSTestHelpers {

  // === Baseline: single thread, implicit return ===

  "Exit: single thread, no calls, implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |kernel_main() -> int
          |    create_thread(task, 0x20000, 0x1F000, "task")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    putc('A')
          |""".stripMargin
    ), maxCycles = 200000)

    info(s"output: '$output' state: ${cpu.state}")
    output shouldBe "A"
  }

  "Exit: single thread, sleep+putc, implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |kernel_main() -> int
          |    create_thread(task, 0x20000, 0x1F000, "task")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    sleep(10)
          |    putc('A')
          |""".stripMargin
    ), maxCycles = 500000)

    info(s"output: '$output' state: ${cpu.state}")
    output shouldBe "A"
  }

  // === Two threads, same priority, implicit return ===

  "Exit: two threads, both sleep+putc, implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x20000, 0x1F000, "a")
          |    create_thread(task_b, 0x22000, 0x21000, "b")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task_a()
          |    sleep(10)
          |    putc('A')
          |
          |task_b()
          |    sleep(20)
          |    putc('B')
          |""".stripMargin
    ), maxCycles = 500000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("A")
    output should include("B")
  }

  // === Three threads, same priority, implicit return ===

  "Exit: three threads same priority, sleep+putc, implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x20000, 0x1F000, "a")
          |    create_thread(task_b, 0x22000, 0x21000, "b")
          |    create_thread(task_c, 0x24000, 0x23000, "c")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task_a()
          |    sleep(10)
          |    putc('A')
          |
          |task_b()
          |    sleep(20)
          |    putc('B')
          |
          |task_c()
          |    sleep(30)
          |    putc('C')
          |""".stripMargin
    ), maxCycles = 500000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("A")
    output should include("B")
    output should include("C")
  }

  // === Three threads, different priorities, implicit return ===

  "Exit: three threads different priorities, sleep+putc, implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |kernel_main() -> int
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    sleep(30)
          |    putc('L')
          |
          |med()
          |    sleep(15)
          |    putc('M')
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |""".stripMargin
    ), maxCycles = 500000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("H")
    output should include("M")
    output should include("L")
  }

  // === Now add pimutex import — does that change behavior? ===

  "Exit: three threads different priorities, sleep+putc, implicit return, pimutex imported" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |kernel_main() -> int
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    sleep(30)
          |    putc('L')
          |
          |med()
          |    sleep(15)
          |    putc('M')
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |""".stripMargin
    ), maxCycles = 500000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("H")
    output should include("M")
    output should include("L")
  }

  // === Add PIMutex variable (changes BSS layout) ===

  "Exit: three threads diff pri, sleep+putc, implicit return, pimutex var declared" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    sleep(30)
          |    putc('L')
          |
          |med()
          |    sleep(15)
          |    putc('M')
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |""".stripMargin
    ), maxCycles = 500000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("H")
    output should include("M")
    output should include("L")
  }

  // === Use the mutex (low holds it, high blocks) — the original failing scenario ===

  "Exit: PI scenario, med implicit return (original failing case)" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    pimutex_lock(&mtx)
          |    putc('L')
          |    sleep(30)
          |    putc('l')
          |    pimutex_unlock(&mtx)
          |
          |med()
          |    sleep(15)
          |    putc('M')
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |    pimutex_lock(&mtx)
          |    putc('h')
          |    pimutex_unlock(&mtx)
          |""".stripMargin
    ), maxCycles = 5000000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("l")
    output should include("h")
  }

  // === Vary med's function body to find the boundary ===

  "Exit: PI scenario, med has only putc (no sleep), implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    pimutex_lock(&mtx)
          |    putc('L')
          |    sleep(30)
          |    putc('l')
          |    pimutex_unlock(&mtx)
          |
          |med()
          |    putc('M')
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |    pimutex_lock(&mtx)
          |    putc('h')
          |    pimutex_unlock(&mtx)
          |""".stripMargin
    ), maxCycles = 5000000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("l")
    output should include("h")
  }

  "Exit: PI scenario, med has putc+putc, implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    pimutex_lock(&mtx)
          |    putc('L')
          |    sleep(30)
          |    putc('l')
          |    pimutex_unlock(&mtx)
          |
          |med()
          |    putc('M')
          |    putc('.')
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |    pimutex_lock(&mtx)
          |    putc('h')
          |    pimutex_unlock(&mtx)
          |""".stripMargin
    ), maxCycles = 5000000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("l")
    output should include("h")
  }

  "Exit: PI scenario, med has sleep only (no putc), implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    pimutex_lock(&mtx)
          |    putc('L')
          |    sleep(30)
          |    putc('l')
          |    pimutex_unlock(&mtx)
          |
          |med()
          |    sleep(15)
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |    pimutex_lock(&mtx)
          |    putc('h')
          |    pimutex_unlock(&mtx)
          |""".stripMargin
    ), maxCycles = 5000000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("l")
    output should include("h")
  }

  "Exit: PI scenario, med has yield+putc, implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    pimutex_lock(&mtx)
          |    putc('L')
          |    sleep(30)
          |    putc('l')
          |    pimutex_unlock(&mtx)
          |
          |med()
          |    yield()
          |    putc('M')
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |    pimutex_lock(&mtx)
          |    putc('h')
          |    pimutex_unlock(&mtx)
          |""".stripMargin
    ), maxCycles = 5000000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("l")
    output should include("h")
  }

  // === Isolate: is it slow+fast path, or slow+slow? ===

  "Exit: PI scenario, med has sleep+yield (slow+slow), implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    pimutex_lock(&mtx)
          |    putc('L')
          |    sleep(30)
          |    putc('l')
          |    pimutex_unlock(&mtx)
          |
          |med()
          |    sleep(15)
          |    yield()
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |    pimutex_lock(&mtx)
          |    putc('h')
          |    pimutex_unlock(&mtx)
          |""".stripMargin
    ), maxCycles = 5000000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("l")
  }

  "Exit: PI scenario, med has sleep+sleep (slow+slow), implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    pimutex_lock(&mtx)
          |    putc('L')
          |    sleep(30)
          |    putc('l')
          |    pimutex_unlock(&mtx)
          |
          |med()
          |    sleep(15)
          |    sleep(1)
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |    pimutex_lock(&mtx)
          |    putc('h')
          |    pimutex_unlock(&mtx)
          |""".stripMargin
    ), maxCycles = 5000000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("l")
  }

  // === Does the bug happen WITHOUT pimutex, just with the right code shape? ===

  "Exit: three threads diff pri, no mutex, med sleep+putc implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |kernel_main() -> int
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    sleep(30)
          |    putc('L')
          |    sleep(30)
          |    putc('l')
          |
          |med()
          |    sleep(15)
          |    putc('M')
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |    sleep(30)
          |    putc('h')
          |""".stripMargin
    ), maxCycles = 5000000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("L")
    output should include("M")
    output should include("H")
    output should include("l")
    output should include("h")
  }

  // === Vary which thread has implicit return ===

  "Exit: PI scenario, LOW implicit return (instead of med)" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    pimutex_lock(&mtx)
          |    putc('L')
          |    sleep(30)
          |    putc('l')
          |    pimutex_unlock(&mtx)
          |
          |med()
          |    sleep(15)
          |    putc('M')
          |    exit()
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |    pimutex_lock(&mtx)
          |    putc('h')
          |    pimutex_unlock(&mtx)
          |    exit()
          |""".stripMargin
    ), maxCycles = 5000000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("l")
    output should include("h")
  }

  "Exit: PI scenario, HIGH implicit return (instead of med)" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    pimutex_lock(&mtx)
          |    putc('L')
          |    sleep(30)
          |    putc('l')
          |    pimutex_unlock(&mtx)
          |    exit()
          |
          |med()
          |    sleep(15)
          |    putc('M')
          |    exit()
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |    pimutex_lock(&mtx)
          |    putc('h')
          |    pimutex_unlock(&mtx)
          |""".stripMargin
    ), maxCycles = 5000000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("l")
    output should include("h")
  }

  // === ALL three implicit return ===

  "Exit: PI scenario, ALL threads implicit return" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import tos.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    pimutex_lock(&mtx)
          |    putc('L')
          |    sleep(30)
          |    putc('l')
          |    pimutex_unlock(&mtx)
          |
          |med()
          |    sleep(15)
          |    putc('M')
          |
          |high()
          |    sleep(10)
          |    putc('H')
          |    pimutex_lock(&mtx)
          |    putc('h')
          |    pimutex_unlock(&mtx)
          |""".stripMargin
    ), maxCycles = 5000000)

    info(s"output: '$output' state: ${cpu.state}")
    output should include("l")
    output should include("h")
  }
}
