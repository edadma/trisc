package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TOSTests extends AnyFreeSpec with Matchers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  val bootAsm = scala.io.Source.fromFile("tos/boot.asm").mkString
  val kernelSysl = readLsysl("tos/kernel.lsysl")
  val servicesSysl = readLsysl("tos/services.lsysl")
  val semaphoreSysl = readLsysl("tos/semaphore.lsysl")
  val mutexSysl = readLsysl("tos/mutex.lsysl")
  val condvarSysl = readLsysl("tos/condvar.lsysl")
  val barrierSysl = readLsysl("tos/barrier.lsysl")
  val rwlockSysl = readLsysl("tos/rwlock.lsysl")
  val channelSysl = readLsysl("tos/channel.lsysl")
  val mailboxSysl = readLsysl("tos/mailbox.lsysl")
  val rbtreeSysl = readLsysl("tos/rbtree.lsysl")
  val tasksSysl = readLsysl("examples/tos-demo/tasks.lsysl")
  val mainSysl = readLsysl("examples/tos-demo/main.lsysl")

  "boot.asm assembles" in {
    val tof = assemble(bootAsm, relocatable = true)
    tof.segments should not be empty
  }

  "kernel.sysl parses" in {
    val parser = new SyslParser
    val result = parser.parseProgram(kernelSysl)
    result shouldBe a[Right[?, ?]]
  }

  "tasks.sysl parses" in {
    val parser = new SyslParser
    val result = parser.parseProgram(tasksSysl)
    result shouldBe a[Right[?, ?]]
  }

  "TOS compiles and links" in {
    // Step 1: Assemble boot.asm
    val bootTof = assemble(bootAsm, relocatable = true)

    // Step 2: Compile Sysl files together (kernel + demo)
    val driver = new SyslDriver
    val result = driver.compile(Map(
      "kernel" -> kernelSysl,
      "services" -> servicesSysl,
      "tasks" -> tasksSysl,
      "main" -> mainSysl,
    ))
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)

    // Step 3: Link with boot
    val linked = Linker.link(Seq(bootTof, syslTof))
    linked.segments should not be empty
    linked.entryAddress shouldBe defined
  }

  // ===== End-to-end: boot stub + Sysl program =====

  // Minimal boot stub for end-to-end tests.
  // Uses test memory layout (64KB RAM, stdout at 0x10000).
  val minimalBoot: String =
    """STDOUT = 0x10000
      |
      |segment vectors
      |
      |  dl 0xFFF8
      |  dl boot
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |
      |segment code
      |
      |extern main
      |
      |global boot, func
      |entry boot
      |
      |boot
      |  movi r4, main
      |  jalr r6, r4
      |  halt
      |
      |global putchar, func
      |
      |putchar
      |  movi r2, STDOUT
      |  stb r1, r2, r0
      |  jalr r0, r6
      |
      |global default_isr, func
      |
      |default_isr
      |  halt
      |""".stripMargin

  def compileSysl(source: String): TOF =
    val driver = new SyslDriver
    val result = driver.compile(Map("main" -> source))
    val unit = result.units.head
    val codegen = new SyslTriscCodegen()
    val asm = codegen.generate(unit.typed)
    assemble(asm, relocatable = true)

  def runWithBoot(syslSource: String): (CPU, String) = runWithBoot(Map("main" -> syslSource))

  def runWithBoot(sources: Map[String, String]): (CPU, String) =
    val bootTof = assemble(minimalBoot, relocatable = true)
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val progTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(bootTof, progTof))

    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = 0x10000
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val mem = new Memory("Memory", new RAM(0, 0x10000), stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 100000 }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  "boot + sysl hello: prints Hello and halts" in {
    val (cpu, output) = runWithBoot(
      """main() -> int
        |    putchar(72)
        |    putchar(101)
        |    putchar(108)
        |    putchar(108)
        |    putchar(111)
        |    putchar(10)
        |    0
        |""".stripMargin)

    output shouldBe "Hello\n"
    cpu.state shouldBe State.Halt
  }

  "boot + sysl program: returns value in r1" in {
    val (cpu, _) = runWithBoot(
      """main() -> int
        |    42
        |""".stripMargin)

    cpu.state shouldBe State.Halt
    cpu.r(1).read shouldBe 42
  }

  "boot + sysl program: computation and output" in {
    val (cpu, output) = runWithBoot(
      """main() -> int
        |    var i = 0
        |    while i < 5
        |        putchar(65 + i)
        |        i += 1
        |    putchar(10)
        |    0
        |""".stripMargin)

    output shouldBe "ABCDE\n"
    cpu.state shouldBe State.Halt
  }

  // ===== TOS integration tests =====

  /** Compile TOS kernel + user tasks, link with boot.asm, run on CPU with timer. */
  def runTOS(userSources: Map[String, String], maxCycles: Int = 500000000): (CPU, String) =
    // Assemble boot.asm
    val bootTof = assemble(bootAsm, relocatable = true)

    // Compile kernel + user sources together
    val allSources = Map(
      "kernel" -> kernelSysl, "services" -> servicesSysl, "semaphore" -> semaphoreSysl,
      "mutex" -> mutexSysl, "condvar" -> condvarSysl, "barrier" -> barrierSysl,
      "rwlock" -> rwlockSysl, "channel" -> channelSysl, "mailbox" -> mailboxSysl,
    ) ++ userSources
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)

    // Link all
    val linked = Linker.link(Seq(bootTof, syslTof))

    // Set up CPU with stdout + timer
    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = 0x100000
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val timer = new Timer(0x100020L)
    val mem = new Memory("Memory", new RAM(0, 0x100000), stdout, timer)
    linked.load(mem)
    val cpu = new CPU(mem, timer) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  "TOS: putc syscall prints character" in {
    val (cpu, output) = runTOS(Map(
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
          |
          |kernel_main() -> int
          |    create_thread(task1, 0x6000, 0x5000, "t1")
          |    create_thread(task2, 0x8000, 0x7000, "t2")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x6000, 0x5000, "a")
          |    create_thread(task_b, 0x8000, 0x7000, "b")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |""".stripMargin
    ))

    // A prints at ticks 0,10,20 — B prints at ticks 0,20,40
    // Expected pattern: AB A AB A B (roughly 2:1)
    output.count(_ == 'A') shouldBe 3
    output.count(_ == 'B') shouldBe 3
  }

  // ===== Semaphore tests =====

  "TOS: sem_init and sem_wait/sem_post basic" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "semaphore"
          |
          |var sem: i64 = 0
          |
          |kernel_main() -> int
          |    sem_init(&sem, 1)
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |import "semaphore"
          |
          |var mutex: i64 = 0
          |
          |kernel_main() -> int
          |    sem_init(&mutex, 1)
          |    create_thread(task_a, 0x6000, 0x5000, "a")
          |    create_thread(task_b, 0x8000, 0x7000, "b")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |import "semaphore"
          |
          |var sem: i64 = 0
          |
          |kernel_main() -> int
          |    sem_init(&sem, 0)
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |import "semaphore"
          |
          |var sem: i64 = 0
          |
          |kernel_main() -> int
          |    sem_init(&sem, 0)
          |    create_thread(waiter, 0x6000, 0x5000, "waiter")
          |    create_thread(poster, 0x8000, 0x7000, "poster")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |import "mutex"
          |
          |var mtx: i64 = 0
          |
          |kernel_main() -> int
          |    mutex_init(&mtx)
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |import "mutex"
          |
          |var mtx: i64 = 0
          |
          |kernel_main() -> int
          |    mutex_init(&mtx)
          |    create_thread(holder, 0x6000, 0x5000, "holder")
          |    create_thread(trier, 0x8000, 0x7000, "trier")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |import "barrier"
          |
          |var bar: Barrier
          |
          |kernel_main() -> int
          |    barrier_init(&bar, 3)
          |    create_thread(task_a, 0x6000, 0x5000, "a")
          |    create_thread(task_b, 0x8000, 0x7000, "b")
          |    create_thread(task_c, 0xA000, 0x9000, "c")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |import "rwlock"
          |
          |var rw: RWLock
          |
          |kernel_main() -> int
          |    rwlock_init(&rw)
          |    create_thread(reader1, 0x6000, 0x5000, "r1")
          |    create_thread(reader2, 0x8000, 0x7000, "r2")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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
          |import "rwlock"
          |
          |var rw: RWLock
          |
          |kernel_main() -> int
          |    rwlock_init(&rw)
          |    create_thread(writer, 0x6000, 0x5000, "w")
          |    create_thread(reader, 0x8000, 0x7000, "r")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
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

  // ===== Channel tests =====

  "TOS: channel send and receive" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "channel"
          |
          |var ch: Channel
          |var buf: [4]i64
          |
          |kernel_main() -> int
          |    chan_init(&ch, &buf[0], 4)
          |    create_thread(sender, 0x6000, 0x5000, "s")
          |    create_thread(receiver, 0x8000, 0x7000, "r")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |sender()
          |    chan_send(&ch, 72)
          |    chan_send(&ch, 105)
          |    chan_send(&ch, 10)
          |
          |receiver()
          |    putc(chan_recv(&ch))
          |    putc(chan_recv(&ch))
          |    putc(chan_recv(&ch))
          |""".stripMargin
    ))

    output should startWith("Hi\n")
  }

  "TOS: channel blocks sender when full" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "channel"
          |
          |var ch: Channel
          |var buf: [2]i64
          |
          |kernel_main() -> int
          |    chan_init(&ch, &buf[0], 2)
          |    create_thread(sender, 0x6000, 0x5000, "s")
          |    create_thread(receiver, 0x8000, 0x7000, "r")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |sender()
          |    chan_send(&ch, 65)
          |    chan_send(&ch, 66)
          |    chan_send(&ch, 67)
          |    putc(83)
          |
          |receiver()
          |    sleep(30)
          |    putc(chan_recv(&ch))
          |    putc(chan_recv(&ch))
          |    putc(chan_recv(&ch))
          |""".stripMargin
    ))

    // Sender blocks on 3rd send until receiver drains — all values arrive
    output should include("A")
    output should include("B")
    output should include("C")
    output should include("S")
  }

  // ===== Mailbox tests =====

  "TOS: mailbox send and receive" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "mailbox"
          |
          |var mb: Mailbox
          |
          |kernel_main() -> int
          |    mbox_init(&mb)
          |    create_thread(sender, 0x6000, 0x5000, "s")
          |    create_thread(receiver, 0x8000, 0x7000, "r")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |sender()
          |    mbox_send(&mb, 72)
          |    mbox_send(&mb, 105)
          |
          |receiver()
          |    putc(mbox_recv(&mb))
          |    putc(mbox_recv(&mb))
          |    putc(10)
          |""".stripMargin
    ))

    output should startWith("Hi\n")
  }

  "TOS: mailbox blocks sender until receiver drains" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "mailbox"
          |
          |var mb: Mailbox
          |
          |kernel_main() -> int
          |    mbox_init(&mb)
          |    create_thread(sender, 0x6000, 0x5000, "s")
          |    create_thread(receiver, 0x8000, 0x7000, "r")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |sender()
          |    mbox_send(&mb, 49)
          |    putc(65)
          |    mbox_send(&mb, 50)
          |    putc(66)
          |
          |receiver()
          |    sleep(20)
          |    putc(mbox_recv(&mb))
          |    putc(mbox_recv(&mb))
          |""".stripMargin
    ))

    // Sender sends 1, prints A, sends 2 (blocks until recv), prints B
    // Receiver wakes, receives 1, receives 2
    output should include("A")
    output should include("B")
    output should include("1")
    output should include("2")
  }

  // ===== Thread join tests =====

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

  // ===== Priority scheduling tests =====

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

  // ===== Thread management & timing syscall tests =====

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

  // ===== Red-black tree tests =====

  def runRBTest(appSource: String): (CPU, String) =
    runWithBoot(Map("rbtree" -> rbtreeSysl, "main" -> appSource))

  "RBTree: basic struct field access" in {
    val (_, output) = runRBTest(
      """import "rbtree"
        |
        |var pool: [4]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 4)
        |    // Check root value directly
        |    putchar(48 + tree.count)
        |    // test setting and reading root
        |    tree.root = 42
        |    if tree.root == 42
        |        putchar(65)
        |    else
        |        putchar(66)
        |    // test NIL
        |    tree.root = -1
        |    if tree.root == -1
        |        putchar(67)
        |    else
        |        putchar(68)
        |    0
        |""".stripMargin)
    output shouldBe "0AC"
  }

  "RBTree: tree_init sets root to NIL" in {
    val (_, output) = runRBTest(
      """import "rbtree"
        |
        |var pool: [4]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 4)
        |    putchar(48 + tree.count)
        |    if tree.root == -1
        |        putchar(65)
        |    else
        |        putchar(66)
        |    // also check free_head
        |    if tree.free_head == 0
        |        putchar(67)
        |    else
        |        putchar(68)
        |    0
        |""".stripMargin)
    output shouldBe "0AC"
  }

  "RBTree: struct field compound assign" in {
    val (_, output) = runWithBoot(
      """struct Counter
        |    n: int
        |
        |var c: Counter
        |
        |main() -> int
        |    c.n = 0
        |    var p: *Counter = &c
        |    p.n += 1
        |    p.n += 1
        |    p.n += 1
        |    putchar(48 + p.n)
        |    0
        |""".stripMargin)
    output shouldBe "3"
  }

  "RBTree: count via rb_insert" in {
    val (cpu, output) = runRBTest(
      """import "rbtree"
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |inc(t: *Tree)
        |    t.count += 1
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    putchar(48 + tree.count)
        |    inc(&tree)
        |    putchar(48 + tree.count)
        |    inc(&tree)
        |    putchar(48 + tree.count)
        |    // Now try actual insert
        |    rb_insert(&tree, 10, 1)
        |    putchar(48 + rb_count(&tree))
        |    rb_insert(&tree, 20, 2)
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    info(s"output: '$output' state: ${cpu.state}")
    output shouldBe "01234"
  }

  "RBTree: insert and find_min" in {
    val (_, output) = runRBTest(
      """import "rbtree"
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    rb_insert(&tree, 30, 3)
        |    rb_insert(&tree, 10, 1)
        |    rb_insert(&tree, 20, 2)
        |    val min = rb_find_min(&tree)
        |    putchar(48 + rb_node_value(&tree, min))
        |    0
        |""".stripMargin)
    output shouldBe "1"
  }

  "RBTree: count tracks insertions" in {
    val (_, output) = runRBTest(
      """import "rbtree"
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    rb_insert(&tree, 5, 0)
        |    rb_insert(&tree, 3, 0)
        |    rb_insert(&tree, 7, 0)
        |    rb_insert(&tree, 1, 0)
        |    rb_insert(&tree, 4, 0)
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    output shouldBe "5"
  }

  "RBTree: empty tree returns NIL for find_min" in {
    val (_, output) = runRBTest(
      """import "rbtree"
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    val min = rb_find_min(&tree)
        |    if min == -1
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin)
    output shouldBe "Y"
  }

  "RBTree: remove min and find new min" in {
    val (_, output) = runRBTest(
      """import "rbtree"
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    rb_insert(&tree, 30, 3)
        |    rb_insert(&tree, 10, 1)
        |    rb_insert(&tree, 20, 2)
        |    // Remove the min (key=10, value=1)
        |    val min1 = rb_find_min(&tree)
        |    rb_remove(&tree, min1)
        |    // New min should be key=20, value=2
        |    val min2 = rb_find_min(&tree)
        |    putchar(48 + rb_node_value(&tree, min2))
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    // value=2, count=2
    output shouldBe "22"
  }

  "RBTree: insert ascending sequence maintains balance" in {
    val (_, output) = runRBTest(
      """import "rbtree"
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    // Ascending insertion — worst case for naive BST
        |    var i: i64 = 1
        |    while i <= 7
        |        rb_insert(&tree, i, int(i))
        |        i += 1
        |    // Min should be 1
        |    val min = rb_find_min(&tree)
        |    putchar(48 + rb_node_value(&tree, min))
        |    // Count should be 7
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    output shouldBe "17"
  }

  "RBTree: remove all nodes leaves empty tree" in {
    val (_, output) = runRBTest(
      """import "rbtree"
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    rb_insert(&tree, 50, 5)
        |    rb_insert(&tree, 30, 3)
        |    rb_insert(&tree, 70, 7)
        |    rb_insert(&tree, 20, 2)
        |    // Remove all by repeatedly removing min
        |    var i = 0
        |    while i < 4
        |        val min = rb_find_min(&tree)
        |        rb_remove(&tree, min)
        |        i += 1
        |    putchar(48 + rb_empty(&tree))
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    output shouldBe "10"
  }

  "RBTree: insert descending sequence" in {
    val (_, output) = runRBTest(
      """import "rbtree"
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    var i: i64 = 8
        |    while i >= 1
        |        rb_insert(&tree, i, int(i))
        |        i -= 1
        |    val min = rb_find_min(&tree)
        |    putchar(48 + rb_node_value(&tree, min))
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    output shouldBe "18"
  }

  "RBTree: remove middle nodes preserves structure" in {
    val (_, output) = runRBTest(
      """import "rbtree"
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |var nodes: [5]int
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    nodes[0] = rb_insert(&tree, 10, 1)
        |    nodes[1] = rb_insert(&tree, 20, 2)
        |    nodes[2] = rb_insert(&tree, 30, 3)
        |    nodes[3] = rb_insert(&tree, 40, 4)
        |    nodes[4] = rb_insert(&tree, 50, 5)
        |    // Remove key=30 (middle)
        |    rb_remove(&tree, nodes[2])
        |    // Remove key=10 (min)
        |    rb_remove(&tree, nodes[0])
        |    // New min should be key=20, value=2
        |    val min = rb_find_min(&tree)
        |    putchar(48 + rb_node_value(&tree, min))
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    output shouldBe "23"
  }

  "RBTree: node reuse after remove" in {
    val (_, output) = runRBTest(
      """import "rbtree"
        |
        |var pool: [4]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 4)
        |    // Fill pool
        |    val a = rb_insert(&tree, 10, 1)
        |    val b = rb_insert(&tree, 20, 2)
        |    val c = rb_insert(&tree, 30, 3)
        |    val d = rb_insert(&tree, 40, 4)
        |    // Pool is full — next insert should fail
        |    val e = rb_insert(&tree, 50, 5)
        |    if e == -1
        |        putchar(70)
        |    // Remove one, then insert should succeed
        |    rb_remove(&tree, a)
        |    val f = rb_insert(&tree, 5, 0)
        |    if f != -1
        |        putchar(82)
        |    val min = rb_find_min(&tree)
        |    putchar(48 + rb_node_value(&tree, min))
        |    0
        |""".stripMargin)
    // F=full, R=reuse succeeded, 0=min value
    output shouldBe "FR0"
  }

  // ===== MINIX-style scheduler tests =====

  "Scheduler: three threads same priority" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x6000, 0x5000, "a")
          |    create_thread(task_b, 0x8000, 0x7000, "b")
          |    create_thread(task_c, 0xA000, 0x9000, "c")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |task_a()
          |    putc(65)
          |
          |task_b()
          |    putc(66)
          |
          |task_c()
          |    putc(67)
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
          |    putc(65)
          |
          |task_b()
          |    putc(66)
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
          |    var i = 0
          |    while i < 4
          |        putc(65)
          |        sleep(10)
          |        i += 1
          |
          |task_b()
          |    var i = 0
          |    while i < 4
          |        putc(66)
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
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(high, 0x20000, 0x1F000, "high")
          |    create_thread_pri(low, 0x22000, 0x21000, "low", 2)
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |high()
          |    putc(72)
          |    sleep(20)
          |    putc(72)
          |
          |low()
          |    putc(76)
          |    sleep(20)
          |    putc(76)
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
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(blocker, 0x6000, 0x5000, "blocker")
          |    create_thread(runner, 0x8000, 0x7000, "runner")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |blocker()
          |    putc(66)
          |    sleep(50)
          |    putc(66)
          |
          |runner()
          |    sleep(10)
          |    putc(82)
          |    sleep(10)
          |    putc(82)
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
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread_pri(high, 0x20000, 0x1F000, "high", 0)
          |    create_thread_pri(med, 0x22000, 0x21000, "med", 1)
          |    create_thread_pri(low, 0x24000, 0x23000, "low", 2)
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |high()
          |    putc(72)
          |
          |med()
          |    putc(77)
          |
          |low()
          |    putc(76)
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
          |    putc(65)
          |    yield()
          |    putc(65)
          |
          |task_b()
          |    putc(66)
          |    yield()
          |    putc(66)
          |""".stripMargin
    ))

    // A prints, yields → B prints, yields → A prints → B prints: ABAB
    output shouldBe "ABAB"
  }

  "Scheduler: unblock higher priority preempts current" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(high, 0x20000, 0x1F000, "high")
          |    create_thread_pri(low, 0x22000, 0x21000, "low", 2)
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |high()
          |    putc(49)
          |    sleep(20)
          |    putc(50)
          |
          |low()
          |    var i = 0
          |    while i < 5
          |        putc(76)
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
