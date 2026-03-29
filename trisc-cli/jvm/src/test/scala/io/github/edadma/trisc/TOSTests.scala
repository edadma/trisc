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
  // Uses test memory layout (0x1000 total, stdout at 0xFF8).
  val minimalBoot: String =
    """STDOUT = 0xFF8
      |
      |segment vectors
      |
      |  dl 0xFF0
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

  def runWithBoot(syslSource: String): (CPU, String) =
    val bootTof = assemble(minimalBoot, relocatable = true)
    val progTof = compileSysl(syslSource)
    val linked = Linker.link(Seq(bootTof, progTof))

    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = 0xFF8
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val mem = new Memory("Memory", new RAM(0, 0xFF8), stdout)
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
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
    val allSources = Map("kernel" -> kernelSysl, "services" -> servicesSysl, "semaphore" -> semaphoreSysl) ++ userSources
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
    val cpu = new CPU(mem, Seq(timer)) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  "TOS: putc syscall prints character" in {
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
}
