package io.github.edadma.trisc

class OSKitPIMutexTests extends OSKitTestHelpers {

  "PIMutex: basic lock/unlock" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread(task, 0x20000, 0x1F000, "task")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    pimutex_lock(&mtx)
          |    putc('A')
          |    pimutex_unlock(&mtx)
          |    putc('B')
          |""".stripMargin
    ), maxCycles = 200000)

    output should include("A")
    output should include("B")
  }

  "PIMutex: mutual exclusion between two tasks" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread(task_a, 0x20000, 0x1F000, "a")
          |    create_thread(task_b, 0x22000, 0x21000, "b")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task_a()
          |    var i = 0
          |    while i < 3
          |        pimutex_lock(&mtx)
          |        putc('[')
          |        putc('A')
          |        putc(']')
          |        pimutex_unlock(&mtx)
          |        sleep(10)
          |        i += 1
          |
          |task_b()
          |    var i = 0
          |    while i < 3
          |        pimutex_lock(&mtx)
          |        putc('[')
          |        putc('B')
          |        putc(']')
          |        pimutex_unlock(&mtx)
          |        sleep(10)
          |        i += 1
          |""".stripMargin
    ))

    val groups = output.sliding(3).count(s => s == "[A]" || s == "[B]")
    groups shouldBe 6
  }

  "PIMutex: contended lock blocks and wakes" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread(holder, 0x20000, 0x1F000, "holder")
          |    create_thread(waiter, 0x22000, 0x21000, "waiter")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |holder()
          |    pimutex_lock(&mtx)
          |    putc('H')
          |    sleep(30)
          |    putc('h')
          |    pimutex_unlock(&mtx)
          |    putc('!')
          |    exit()
          |
          |waiter()
          |    sleep(10)
          |    putc('W')
          |    pimutex_lock(&mtx)
          |    putc('w')
          |    pimutex_unlock(&mtx)
          |    exit()
          |""".stripMargin
    ))

    info(s"contended output: '$output'")
    output should include("H")
    output should include("W")
    output should include("h")
    output should include("w")
    output.indexOf('h') should be < output.indexOf('w')
  }

  "PIMutex: contended lock with different priorities" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |var mtx: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 2)
          |    create_thread_pri(high, 0x22000, 0x21000, "high", 0)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    pimutex_lock(&mtx)
          |    putc('L')
          |    sleep(30)
          |    putc('l')
          |    pimutex_unlock(&mtx)
          |    putc('!')
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
    ))

    info(s"diff-pri contended output: '$output'")
    output should include("L")
    output should include("H")
    output should include("l")
    output should include("h")
    output.indexOf('l') should be < output.indexOf('h')
  }

  "PIMutex: priority inheritance prevents inversion" in {
    // Classic priority inversion scenario with active work (yield loop).
    //
    // Low (pri 2): locks mutex, actively works (yield loop)
    // High (pri 0): wakes, tries to lock → blocks, boosts low to pri 0
    // Med (pri 1): wakes during low's loop
    //
    // WITHOUT PI: med (pri 1) would preempt low (pri 2), delaying the
    //   critical section — high starves while med runs.
    // WITH PI: low runs at pri 0 (boosted), med can't preempt. Low
    //   finishes critical section promptly. High gets mutex.
    //
    // Key: low prints 'l' (end of critical section) before med prints 'M'.
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.*
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
          |    // Active work: yield loop. Each yield is one context switch.
          |    // High wakes at tick ~3, med wakes at tick ~8.
          |    // With PI, low stays at pri 0 through all of this.
          |    var i = 0
          |    while i < 20
          |        yield()
          |        i += 1
          |    putc('l')
          |    pimutex_unlock(&mtx)
          |    exit()
          |
          |med()
          |    sleep(8)
          |    putc('M')
          |    exit()
          |
          |high()
          |    sleep(3)
          |    putc('H')
          |    pimutex_lock(&mtx)
          |    putc('h')
          |    pimutex_unlock(&mtx)
          |    exit()
          |""".stripMargin
    ))

    info(s"PI test output: '$output'")
    output should include("L")
    output should include("H")
    output should include("l")
    output should include("h")
    output should include("M")
    // Low finishes critical section before med runs — PI prevented inversion
    output.indexOf('l') should be < output.indexOf('M')
    // High gets mutex after low releases
    output.indexOf('h') should be > output.indexOf('l')
  }

  "PIMutex: priority restored after unlock" in {
    // After unlock, holder's priority reverts to base.
    // Low holds mutex, gets boosted to pri 0. After unlock, reverts to pri 2.
    // Med (pri 1) wakes DURING low's critical section (active yield loop).
    // After unlock: high (pri 0) runs, then med (pri 1), then low (pri 2).
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.*
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
          |    var i = 0
          |    while i < 20
          |        yield()
          |        i += 1
          |    pimutex_unlock(&mtx)
          |    // After unlock, low is back to priority 2.
          |    putc('L')
          |    exit()
          |
          |med()
          |    sleep(8)
          |    putc('M')
          |    exit()
          |
          |high()
          |    sleep(3)
          |    pimutex_lock(&mtx)
          |    pimutex_unlock(&mtx)
          |    putc('H')
          |    exit()
          |""".stripMargin
    ))

    info(s"Restore test output: '$output'")
    // After unlock: high runs (pri 0), then med (pri 1), then low (pri 2)
    output should include("H")
    output should include("M")
    output should include("L")
    output.indexOf('H') should be < output.indexOf('M')
    output.indexOf('M') should be < output.indexOf('L')
  }

  "PIMutex: transitive inheritance (chain of two mutexes)" in {
    // Chain: high → mtx_b → mid → mtx_a → low
    // Transitive PI boosts low all the way to priority 0.
    // bg (priority 1) wakes but can't preempt boosted low.
    val (_, output) = runTOS(Map(
      "app" ->
        """import oskit.*
          |
          |var mtx_a: PIMutex
          |var mtx_b: PIMutex
          |
          |kernel_main() -> int
          |    pimutex_init(&mtx_a)
          |    pimutex_init(&mtx_b)
          |    create_thread_pri(low, 0x20000, 0x1F000, "low", 3)
          |    create_thread_pri(mid, 0x22000, 0x21000, "mid", 2)
          |    create_thread_pri(high, 0x24000, 0x23000, "high", 0)
          |    create_thread_pri(bg, 0x26000, 0x25000, "bg", 1)
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |low()
          |    pimutex_lock(&mtx_a)
          |    putc('L')
          |    // Active work so PI can be observed
          |    var i = 0
          |    while i < 30
          |        yield()
          |        i += 1
          |    putc('l')
          |    pimutex_unlock(&mtx_a)
          |    exit()
          |
          |mid()
          |    sleep(3)
          |    pimutex_lock(&mtx_b)
          |    sleep(5)
          |    pimutex_lock(&mtx_a)
          |    putc('m')
          |    pimutex_unlock(&mtx_a)
          |    pimutex_unlock(&mtx_b)
          |    exit()
          |
          |high()
          |    sleep(12)
          |    pimutex_lock(&mtx_b)
          |    putc('h')
          |    pimutex_unlock(&mtx_b)
          |    exit()
          |
          |bg()
          |    sleep(15)
          |    putc('B')
          |    exit()
          |""".stripMargin
    ))

    info(s"Transitive PI output: '$output'")
    // With transitive PI, low is boosted to pri 0 through the chain.
    // Low finishes critical section ('l') before bg ('B') runs.
    output should include("l")
    output should include("B")
    output.indexOf('l') should be < output.indexOf('B')
  }
}
