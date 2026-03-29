package io.github.edadma.trisc

class TOSSyncTests extends TOSTestHelpers {

  // ===== Semaphore tests =====

  "TOS: sem_init and sem_wait/sem_post basic" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "semaphore"
          |
          |var sem: i64 = 0
          |
          |kernel_main() -> int
          |    sem_init(&sem, 1)
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |task()
          |    sem_wait(&sem)
          |    putc(65)
          |    sem_post(&sem)
          |    putc(66)
          |""".stripMargin
    ), maxCycles = 200000)

    output should include("A")
    output should include("B")
  }

  "TOS: semaphore enforces mutual exclusion between two tasks" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "semaphore"
          |
          |var mutex: i64 = 0
          |
          |kernel_main() -> int
          |    sem_init(&mutex, 1)
          |    create_thread(task_a, 0x6000, 0x5000, "a")
          |    create_thread(task_b, 0x8000, 0x7000, "b")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |task_a()
          |    var i = 0
          |    while i < 3
          |        sem_wait(&mutex)
          |        putc(91)
          |        putc(65)
          |        putc(93)
          |        sem_post(&mutex)
          |        sleep(10)
          |        i += 1
          |
          |task_b()
          |    var i = 0
          |    while i < 3
          |        sem_wait(&mutex)
          |        putc(91)
          |        putc(66)
          |        putc(93)
          |        sem_post(&mutex)
          |        sleep(10)
          |        i += 1
          |""".stripMargin
    ))

    // Each critical section prints [X] atomically — no interleaving like [A[B]
    // So output must be a sequence of [A] and [B] groups
    val groups = output.sliding(3).count(s => s == "[A]" || s == "[B]")
    groups shouldBe 6
  }

  "TOS: sem_trywait returns 0 when semaphore is zero" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "semaphore"
          |
          |var sem: i64 = 0
          |
          |kernel_main() -> int
          |    sem_init(&sem, 0)
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |task()
          |    val got = sem_trywait(&sem)
          |    if got == 0
          |        putc(78)
          |    else
          |        putc(89)
          |""".stripMargin
    ), maxCycles = 200000)

    output should include("N")
  }

  "TOS: counting semaphore allows N concurrent permits" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "semaphore"
          |
          |var sem: i64 = 0
          |var done_count: i64 = 0
          |
          |kernel_main() -> int
          |    sem_init(&sem, 2)
          |    done_count = 0
          |    create_thread(task_a, 0x6000, 0x5000, "a")
          |    create_thread(task_b, 0x8000, 0x7000, "b")
          |    create_thread(task_c, 0xA000, 0x9000, "c")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |task_a()
          |    sem_wait(&sem)
          |    putc(65)
          |    sleep(20)
          |    sem_post(&sem)
          |
          |task_b()
          |    sem_wait(&sem)
          |    putc(66)
          |    sleep(20)
          |    sem_post(&sem)
          |
          |task_c()
          |    sem_wait(&sem)
          |    putc(67)
          |    sleep(20)
          |    sem_post(&sem)
          |""".stripMargin
    ))

    // All three tasks should eventually acquire and print
    output should include("A")
    output should include("B")
    output should include("C")
  }

  "TOS: sem_wait blocks until sem_post from another thread" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "semaphore"
          |
          |var sem: i64 = 0
          |
          |kernel_main() -> int
          |    sem_init(&sem, 0)
          |    create_thread(waiter, 0x6000, 0x5000, "waiter")
          |    create_thread(poster, 0x8000, 0x7000, "poster")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |waiter()
          |    putc(87)
          |    sem_wait(&sem)
          |    putc(71)
          |
          |poster()
          |    sleep(30)
          |    putc(80)
          |    sem_post(&sem)
          |""".stripMargin
    ))

    // W = waiter starts, P = poster posts, G = waiter got it
    // P must appear before G (poster unblocks waiter)
    output should include("W")
    output should include("P")
    output should include("G")
    val pIdx = output.indexOf('P')
    val gIdx = output.indexOf('G')
    pIdx should be < gIdx
  }

  // ===== Mutex tests =====

  "TOS: mutex basic lock/unlock" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "mutex"
          |
          |var mtx: i64 = 0
          |
          |kernel_main() -> int
          |    mutex_init(&mtx)
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |task()
          |    mutex_lock(&mtx)
          |    putc(65)
          |    mutex_unlock(&mtx)
          |    putc(66)
          |""".stripMargin
    ), maxCycles = 200000)

    output should include("A")
    output should include("B")
  }

  "TOS: mutex_trylock fails when locked" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "mutex"
          |
          |var mtx: i64 = 0
          |
          |kernel_main() -> int
          |    mutex_init(&mtx)
          |    create_thread(holder, 0x6000, 0x5000, "holder")
          |    create_thread(trier, 0x8000, 0x7000, "trier")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |holder()
          |    mutex_lock(&mtx)
          |    putc(72)
          |    sleep(50)
          |    mutex_unlock(&mtx)
          |
          |trier()
          |    sleep(10)
          |    val got = mutex_trylock(&mtx)
          |    if got == 0
          |        putc(78)
          |    else
          |        putc(89)
          |""".stripMargin
    ))

    output should include("H")
    output should include("N")
  }

  // ===== Condition variable tests =====

  "TOS: condvar signal wakes one waiter" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "mutex"
          |import "condvar"
          |
          |var mtx: i64 = 0
          |var cv: Condvar
          |var ready = 0
          |
          |kernel_main() -> int
          |    mutex_init(&mtx)
          |    cond_init(&cv)
          |    create_thread(waiter, 0x6000, 0x5000, "waiter")
          |    create_thread(signaler, 0x8000, 0x7000, "signaler")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |waiter()
          |    mutex_lock(&mtx)
          |    while ready == 0
          |        cond_wait(&cv, &mtx)
          |    mutex_unlock(&mtx)
          |    putc(87)
          |
          |signaler()
          |    sleep(30)
          |    mutex_lock(&mtx)
          |    ready = 1
          |    cond_signal(&cv)
          |    mutex_unlock(&mtx)
          |    putc(83)
          |""".stripMargin
    ))

    output should include("S")
    output should include("W")
  }

  "TOS: condvar broadcast wakes all waiters" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "mutex"
          |import "condvar"
          |
          |var mtx: i64 = 0
          |var cv: Condvar
          |var go = 0
          |
          |kernel_main() -> int
          |    mutex_init(&mtx)
          |    cond_init(&cv)
          |    create_thread(waiter_a, 0x6000, 0x5000, "a")
          |    create_thread(waiter_b, 0x8000, 0x7000, "b")
          |    create_thread(broadcaster, 0xA000, 0x9000, "bc")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |waiter_a()
          |    mutex_lock(&mtx)
          |    while go == 0
          |        cond_wait(&cv, &mtx)
          |    mutex_unlock(&mtx)
          |    putc(65)
          |
          |waiter_b()
          |    mutex_lock(&mtx)
          |    while go == 0
          |        cond_wait(&cv, &mtx)
          |    mutex_unlock(&mtx)
          |    putc(66)
          |
          |broadcaster()
          |    sleep(30)
          |    mutex_lock(&mtx)
          |    go = 1
          |    cond_broadcast(&cv)
          |    mutex_unlock(&mtx)
          |""".stripMargin
    ))

    output should include("A")
    output should include("B")
  }

  // ===== Barrier tests =====

  "TOS: barrier synchronizes three threads" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "barrier"
          |
          |var bar: Barrier
          |
          |kernel_main() -> int
          |    barrier_init(&bar, 3)
          |    create_thread(task_a, 0x6000, 0x5000, "a")
          |    create_thread(task_b, 0x8000, 0x7000, "b")
          |    create_thread(task_c, 0xA000, 0x9000, "c")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |task_a()
          |    putc(49)
          |    barrier_wait(&bar)
          |    putc(65)
          |
          |task_b()
          |    sleep(20)
          |    putc(50)
          |    barrier_wait(&bar)
          |    putc(66)
          |
          |task_c()
          |    sleep(40)
          |    putc(51)
          |    barrier_wait(&bar)
          |    putc(67)
          |""".stripMargin
    ))

    // All three must print their number before any prints their letter
    // 1, 2, 3 all appear before A, B, C
    val numbers = "123".map(c => output.indexOf(c))
    val letters = "ABC".map(c => output.indexOf(c))
    numbers.max should be < letters.min
  }

  // ===== Reader-writer lock tests =====

  "TOS: rwlock allows concurrent readers" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "rwlock"
          |
          |var rw: RWLock
          |
          |kernel_main() -> int
          |    rwlock_init(&rw)
          |    create_thread(reader1, 0x6000, 0x5000, "r1")
          |    create_thread(reader2, 0x8000, 0x7000, "r2")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |reader1()
          |    read_lock(&rw)
          |    putc(65)
          |    sleep(20)
          |    read_unlock(&rw)
          |    putc(88)
          |
          |reader2()
          |    read_lock(&rw)
          |    putc(66)
          |    sleep(20)
          |    read_unlock(&rw)
          |    putc(89)
          |""".stripMargin
    ))

    // Both readers should acquire the lock (A and B both appear)
    output should include("A")
    output should include("B")
    output should include("X")
    output should include("Y")
  }

  "TOS: rwlock writer excludes readers" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "rwlock"
          |
          |var rw: RWLock
          |
          |kernel_main() -> int
          |    rwlock_init(&rw)
          |    create_thread(writer, 0x6000, 0x5000, "w")
          |    create_thread(reader, 0x8000, 0x7000, "r")
          |    timer_init(10)
          |    first_thread_ssp()
          |
          |writer()
          |    write_lock(&rw)
          |    putc(91)
          |    sleep(30)
          |    putc(93)
          |    write_unlock(&rw)
          |
          |reader()
          |    sleep(10)
          |    read_lock(&rw)
          |    putc(82)
          |    read_unlock(&rw)
          |""".stripMargin
    ))

    // Writer prints [ and ], reader prints R
    // R must appear after ] (writer holds lock during sleep)
    output should include("[")
    output should include("]")
    output should include("R")
    output.indexOf('R') should be > output.indexOf(']')
  }
}
