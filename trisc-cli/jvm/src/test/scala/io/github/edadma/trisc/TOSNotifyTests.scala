package io.github.edadma.trisc

class TOSNotifyTests extends TOSTestHelpers {

  // ===== Task notifications =====

  "TOS: notify_send wakes waiting thread" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(waiter, 0x6000, 0x5000, "w")
          |    create_thread(sender, 0x8000, 0x7000, "s")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |waiter()
          |    notify_wait()
          |    val v = notify_read_self()
          |    if v == 42
          |        putc(89)
          |    else
          |        putc(78)
          |
          |sender()
          |    sleep(10)
          |    notify_send_to(0, 42)
          |""".stripMargin
    ))

    output should include("Y")
  }

  "TOS: notification already pending skips wait" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "t")
          |    create_thread(sender, 0x8000, 0x7000, "s")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    sleep(20)
          |    notify_wait()
          |    putc(65)
          |
          |sender()
          |    sleep(5)
          |    notify_send_to(0, 1)
          |""".stripMargin
    ))

    // Sender sends before task waits — notification is pending,
    // so notify_wait returns immediately
    output should include("A")
  }

  // ===== Event groups =====

  "TOS: event_set wakes waiting thread" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(waiter, 0x6000, 0x5000, "w")
          |    create_thread(setter, 0x8000, 0x7000, "s")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |waiter()
          |    event_wait(3, 0)
          |    putc(87)
          |
          |setter()
          |    sleep(10)
          |    event_set(0, 1)
          |""".stripMargin
    ))

    // waiter waits for any of bits 0 or 1. setter sets bit 0.
    output should include("W")
  }

  "TOS: event_wait all bits" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |
          |kernel_main() -> int
          |    create_thread(waiter, 0x6000, 0x5000, "w")
          |    create_thread(setter, 0x8000, 0x7000, "s")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |waiter()
          |    event_wait(3, 1)
          |    putc(87)
          |
          |setter()
          |    sleep(10)
          |    event_set(0, 1)
          |    sleep(10)
          |    event_set(0, 2)
          |""".stripMargin
    ))

    // waiter waits for ALL of bits 0 and 1. setter sets them one at a time.
    output should include("W")
  }

  // ===== Recursive mutex =====

  "TOS: recursive mutex allows same-thread relock" in {
    val (_, output) = runTOS(Map(
      "rmutex" -> rmutexSysl,
      "app" ->
        """import "kernel"
          |import "services"
          |import "rmutex"
          |import "timer"
          |
          |var rm: RMutex
          |
          |kernel_main() -> int
          |    rmutex_init(&rm)
          |    create_thread(task, 0x6000, 0x5000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    rmutex_lock(&rm)
          |    rmutex_lock(&rm)
          |    rmutex_lock(&rm)
          |    putc(65)
          |    rmutex_unlock(&rm)
          |    rmutex_unlock(&rm)
          |    rmutex_unlock(&rm)
          |    putc(66)
          |""".stripMargin
    ))

    // Can lock 3 times and unlock 3 times without deadlock
    output should include("A")
    output should include("B")
  }

  "TOS: recursive mutex blocks other thread" in {
    val (_, output) = runTOS(Map(
      "rmutex" -> rmutexSysl,
      "app" ->
        """import "kernel"
          |import "services"
          |import "rmutex"
          |import "timer"
          |
          |var rm: RMutex
          |
          |kernel_main() -> int
          |    rmutex_init(&rm)
          |    create_thread(holder, 0x6000, 0x5000, "h")
          |    create_thread(waiter, 0x8000, 0x7000, "w")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |holder()
          |    rmutex_lock(&rm)
          |    putc(72)
          |    sleep(30)
          |    rmutex_unlock(&rm)
          |    putc(85)
          |
          |waiter()
          |    sleep(5)
          |    rmutex_lock(&rm)
          |    putc(87)
          |    rmutex_unlock(&rm)
          |""".stripMargin
    ))

    // H = holder locks, U = holder unlocks, W = waiter gets lock
    // W must come after U
    output should include("H")
    output should include("U")
    output should include("W")
    output.indexOf('W') should be > output.indexOf('U')
  }

  // ===== Queue sets =====

  "TOS: qset_wait returns ready slot" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "qset"
          |import "timer"
          |
          |var qs: QueueSet
          |
          |kernel_main() -> int
          |    val sid = qset_init(&qs)
          |    qset_add(&qs, sid, 10)
          |    qset_add(&qs, sid, 20)
          |    create_thread(waiter, 0x6000, 0x5000, "w")
          |    create_thread(poster, 0x8000, 0x7000, "p")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |waiter()
          |    val id = qset_wait(&qs, 0)
          |    if id == 20
          |        putc(89)
          |    else
          |        putc(78)
          |
          |poster()
          |    sleep(10)
          |    qset_notify(&qs, 1)
          |""".stripMargin
    ))

    // Poster notifies slot 1 (id=20). Waiter should get 20.
    output should include("Y")
  }

  "TOS: qset_wait returns first ready of multiple" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "qset"
          |import "timer"
          |
          |var qs: QueueSet
          |
          |kernel_main() -> int
          |    val sid = qset_init(&qs)
          |    qset_add(&qs, sid, 10)
          |    qset_add(&qs, sid, 20)
          |    qset_add(&qs, sid, 30)
          |    create_thread(waiter, 0x6000, 0x5000, "w")
          |    create_thread(poster, 0x8000, 0x7000, "p")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |waiter()
          |    val id1 = qset_wait(&qs, 0)
          |    val id2 = qset_wait(&qs, 0)
          |    if id1 == 10
          |        putc(65)
          |    if id2 == 30
          |        putc(66)
          |
          |poster()
          |    sleep(5)
          |    qset_notify(&qs, 0)
          |    qset_notify(&qs, 2)
          |""".stripMargin
    ))

    output should include("A")
    output should include("B")
  }
}
