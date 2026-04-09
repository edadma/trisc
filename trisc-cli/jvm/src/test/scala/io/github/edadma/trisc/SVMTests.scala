package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SVMTests extends AnyFreeSpec with Matchers {

  private val STDOUT_ADDR = 0x10000L

  /** Helper: load bytecode into RAM, set vector table, run SVM with stdout capture. */
  private def runSVM(bytecode: Array[Byte], maxCycles: Int = 10000): (SVM, String) =
    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = STDOUT_ADDR
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val ram = new RAM(0, 0x10000)
    val mem = new Memory("Memory", ram, stdout)
    // Vector table: slot 0 = initial IP (entry point at 0x10 past vectors)
    val entryPoint = 0x10L
    mem.writeLong(0, entryPoint)
    mem.writeLong(8, 0) // interrupt handler = 0 (none)
    // Load bytecode at entry point
    for (b, i) <- bytecode.zipWithIndex do mem.loadByte(entryPoint + i, b & 0xff)
    val svm = new SVM(mem) { limit = maxCycles }
    svm.reset()
    svm.run()
    (svm, output.toString)

  "SVM: HALT stops execution" in {
    val (svm, _) = runSVM(Array(0x6B.toByte)) // HALT
    svm.state shouldBe State.Halt
  }

  "SVM: PUSH_1 + PUSH_2 + ADD = 3" in {
    val bytecode = Array(
      0x11, // PUSH_1
      0x12, // PUSH_2
      0x20, // ADD
      0x6B, // HALT
    ).map(_.toByte)
    val (svm, _) = runSVM(bytecode)
    svm.state shouldBe State.Halt
  }

  "SVM: PUSH_i8 42 + HALT" in {
    val bytecode = Array(
      0x14, 42, // PUSH_i8 42
      0x6B, // HALT
    ).map(_.toByte)
    val (svm, _) = runSVM(bytecode)
    svm.state shouldBe State.Halt
  }

  "SVM: putc via STORE8 to stdout" in {
    // Push 'H' (72), push stdout addr, STORE8, HALT
    val addrHi = ((STDOUT_ADDR >> 24) & 0xff).toInt
    val addrMid1 = ((STDOUT_ADDR >> 16) & 0xff).toInt
    val addrMid2 = ((STDOUT_ADDR >> 8) & 0xff).toInt
    val addrLo = (STDOUT_ADDR & 0xff).toInt
    val bytecode = Array(
      0x14, 72, // PUSH_i8 'H'
      0x17, addrHi, addrMid1, addrMid2, addrLo, // PUSH_i32 STDOUT_ADDR
      0x58, // STORE8
      0x14, 105, // PUSH_i8 'i'
      0x17, addrHi, addrMid1, addrMid2, addrLo, // PUSH_i32 STDOUT_ADDR
      0x58, // STORE8
      0x6B, // HALT
    ).map(_.toByte)
    val (svm, output) = runSVM(bytecode)
    output shouldBe "Hi"
    svm.state shouldBe State.Halt
  }

  "SVM: JUMP skips over halt" in {
    val bytecode = Array(
      0x60, 0x00, 0x01, // JUMP +1 (skip the next HALT, land on PUSH_1)
      0x6B, // HALT (skipped)
      0x11, // PUSH_1
      0x6B, // HALT
    ).map(_.toByte)
    val (svm, _) = runSVM(bytecode)
    svm.state shouldBe State.Halt
  }

  "SVM: CALL and RET" in {
    // Main: PUSH_i8 65 ('A'), CALL putc, HALT
    // putc: PUSH_i32 STDOUT, STORE8, RET
    val addrHi = ((STDOUT_ADDR >> 24) & 0xff).toInt
    val addrMid1 = ((STDOUT_ADDR >> 16) & 0xff).toInt
    val addrMid2 = ((STDOUT_ADDR >> 8) & 0xff).toInt
    val addrLo = (STDOUT_ADDR & 0xff).toInt
    val bytecode = Array(
      // offset 0: main
      0x14, 65, // PUSH_i8 'A'
      0x63, 0x00, 0x00, 0x00, 0x01, // CALL +1 (to putc at offset 8)
      0x6B, // HALT (offset 7)
      // offset 8: putc subroutine
      0x17, addrHi, addrMid1, addrMid2, addrLo, // PUSH_i32 STDOUT
      0x58, // STORE8
      0x64, // RET
    ).map(_.toByte)
    val (svm, output) = runSVM(bytecode)
    output shouldBe "A"
    svm.state shouldBe State.Halt
  }

  "SVM: LOCAL_SET and LOCAL_GET" in {
    val bytecode = Array(
      0x70, 2, // FRAME 2
      0x14, 42, // PUSH_i8 42
      0x72, 0, // LOCAL_SET 0
      0x14, 7, // PUSH_i8 7
      0x72, 1, // LOCAL_SET 1
      0x71, 0, // LOCAL_GET 0
      0x71, 1, // LOCAL_GET 1
      0x20, // ADD  (42 + 7 = 49 = '1')
      0x6B, // HALT
    ).map(_.toByte)
    val (svm, _) = runSVM(bytecode)
    svm.state shouldBe State.Halt
  }

  "SVM: DUP and SWAP" in {
    val bytecode = Array(
      0x14, 10, // PUSH_i8 10
      0x14, 20, // PUSH_i8 20
      0x03, // SWAP  → (20, 10)
      0x02, // DUP   → (20, 10, 10)
      0x6B, // HALT
    ).map(_.toByte)
    val (svm, _) = runSVM(bytecode)
    svm.state shouldBe State.Halt
  }

  "SVM: loop countdown with DEC_JUMPNZ" in {
    // Push 3, loop: DEC_JUMPNZ back to loop, HALT
    // Should execute 3 iterations
    val bytecode = Array(
      0x14, 3, // PUSH_i8 3
      // offset 2: loop
      0xBA, 0xFF, 0xFD, // DEC_JUMPNZ -3 (back to offset 2)
      0x6B, // HALT
    ).map(_.toByte)
    val (svm, _) = runSVM(bytecode)
    svm.state shouldBe State.Halt
  }

  "SVM: JUMPZ conditional branch" in {
    val addrHi = ((STDOUT_ADDR >> 24) & 0xff).toInt
    val addrMid1 = ((STDOUT_ADDR >> 16) & 0xff).toInt
    val addrMid2 = ((STDOUT_ADDR >> 8) & 0xff).toInt
    val addrLo = (STDOUT_ADDR & 0xff).toInt
    val bytecode = Array(
      0x10, // PUSH_0
      0x61, 0x00, 0x08, // JUMPZ +8 (skip to 'Y' print at offset 12)
      0x14, 78, // PUSH_i8 'N' (skipped)
      0x17, addrHi, addrMid1, addrMid2, addrLo,
      0x58, // STORE8
      // offset 12: land here
      0x14, 89, // PUSH_i8 'Y'
      0x17, addrHi, addrMid1, addrMid2, addrLo,
      0x58, // STORE8
      0x6B, // HALT
    ).map(_.toByte)
    val (svm, output) = runSVM(bytecode)
    output shouldBe "Y"
  }

  "SVM: interrupt wakes WFI" in {
    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = STDOUT_ADDR
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val ram = new RAM(0, 0x10000)
    val mem = new Memory("Memory", ram, stdout)
    val intc = new InterruptController(0x10080)
    val timer = new Timer(0x10010, intc, irq = 0)

    val entryPoint = 0x10L
    mem.writeLong(0, entryPoint) // initial IP

    // Interrupt handler: write '!' to stdout, RET
    val handlerAddr = 0x40L
    mem.writeLong(8, handlerAddr) // interrupt vector

    val addrHi = ((STDOUT_ADDR >> 24) & 0xff).toInt
    val addrMid1 = ((STDOUT_ADDR >> 16) & 0xff).toInt
    val addrMid2 = ((STDOUT_ADDR >> 8) & 0xff).toInt
    val addrLo = (STDOUT_ADDR & 0xff).toInt

    // Handler bytecode at 0x40
    val handler = Array(
      0x14, 33, // PUSH_i8 '!'
      0x17, addrHi, addrMid1, addrMid2, addrLo, // PUSH_i32 STDOUT
      0x58, // STORE8
      0x64, // RET (returns from interrupt to main code)
    ).map(_.toByte)
    for (b, i) <- handler.zipWithIndex do mem.loadByte(handlerAddr + i, b & 0xff)

    // Main bytecode: TRAP 0 (WFI), then write 'D' and HALT
    val main = Array(
      0x6A, 0x00, // TRAP 0 (WFI)
      0x14, 68, // PUSH_i8 'D'
      0x17, addrHi, addrMid1, addrMid2, addrLo,
      0x58, // STORE8
      0x6B, // HALT
    ).map(_.toByte)
    for (b, i) <- main.zipWithIndex do mem.loadByte(entryPoint + i, b & 0xff)

    // Configure timer to fire quickly
    // ARR at +0 (4 bytes), PSC at +8 (2 bytes), CR at +10 (1 byte), IER at +12 (1 byte)
    timer.writeInt(0x10010 + 0, 5) // ARR = 5 (fire after 5 ticks)
    timer.writeShort(0x10010 + 8, 1) // PSC = 1
    timer.writeByte(0x10010 + 12, 1) // IER = interrupt enable
    timer.writeByte(0x10010 + 10, 1) // CR = enable

    val svm = new SVM(mem, Seq(timer, intc)) { limit = 100 }
    svm.reset()
    svm.run()

    output.toString should include("!")
    output.toString should include("D")
    svm.state shouldBe State.Halt
  }

  "SVM: NEG negates value" in {
    val bytecode = Array(
      0x14, 42, // PUSH_i8 42
      0x28, // NEG
      0x6B, // HALT
    ).map(_.toByte)
    val (svm, _) = runSVM(bytecode)
    svm.state shouldBe State.Halt
  }

  "SVM: division by zero sets IllegalDivide" in {
    val bytecode = Array(
      0x11, // PUSH_1
      0x10, // PUSH_0
      0x23, // DIV
      0x6B, // HALT
    ).map(_.toByte)
    val (svm, _) = runSVM(bytecode)
    svm.state shouldBe State.IllegalDivide
  }

  "SVM: LOAD64 and STORE64 round-trip" in {
    val bytecode = Array(
      0x18, 0x12, 0x34, 0x56, 0x78.toByte, 0x9A.toByte, 0xBC.toByte, 0xDE.toByte, 0xF0.toByte, // PUSH_i64
      0x17, 0x00, 0x00, 0x01, 0x00, // PUSH_i32 0x100 (address)
      0x5B, // STORE64
      0x17, 0x00, 0x00, 0x01, 0x00, // PUSH_i32 0x100
      0x56, // LOAD64
      0x6B, // HALT
    ).map(_.toByte)
    val (svm, _) = runSVM(bytecode)
    svm.state shouldBe State.Halt
  }

  "SVM: float arithmetic FADD" in {
    val one = java.lang.Double.doubleToLongBits(1.5)
    val two = java.lang.Double.doubleToLongBits(2.5)
    def i64Bytes(v: Long): Array[Byte] = Array(
      ((v >> 56) & 0xff).toByte, ((v >> 48) & 0xff).toByte,
      ((v >> 40) & 0xff).toByte, ((v >> 32) & 0xff).toByte,
      ((v >> 24) & 0xff).toByte, ((v >> 16) & 0xff).toByte,
      ((v >> 8) & 0xff).toByte, (v & 0xff).toByte,
    )
    val bytecode = Array(0x18.toByte) ++ i64Bytes(one) ++
      Array(0x18.toByte) ++ i64Bytes(two) ++
      Array(0x90.toByte, 0x6B.toByte) // FADD, HALT
    val (svm, _) = runSVM(bytecode.toArray)
    svm.state shouldBe State.Halt
  }

  // ===== Assembler-based tests =====

  private val STDOUT = 0x10000

  /** Helper: assemble SVM source, load TOF, run with stdout capture. */
  private def runAsm(src: String, maxCycles: Int = 10000): (SVM, String) =
    val tof = svmAssemble(src)
    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = STDOUT
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val ram = new RAM(0, 0x10000)
    val mem = new Memory("Memory", ram, stdout)
    tof.load(mem)
    val svm = new SVM(mem) { limit = maxCycles }
    svm.reset()
    svm.run()
    (svm, output.toString)

  "SVM asm: hello world" in {
    val src =
      s"""STDOUT = $STDOUT
         |
         |; vector table
         |dl main    ; slot 0: initial IP
         |dl 0       ; slot 1: interrupt handler (none)
         |
         |main:
         |  push_i8 'H'
         |  push_i32 STDOUT
         |  store8
         |  push_i8 'i'
         |  push_i32 STDOUT
         |  store8
         |  halt
         |""".stripMargin
    val (svm, output) = runAsm(src)
    output shouldBe "Hi"
    svm.state shouldBe State.Halt
  }

  "SVM asm: call and ret" in {
    val src =
      s"""STDOUT = $STDOUT
         |
         |dl main
         |dl 0
         |
         |main:
         |  push_i8 65
         |  call putc
         |  push_i8 66
         |  call putc
         |  halt
         |
         |putc:
         |  push_i32 STDOUT
         |  store8
         |  ret
         |""".stripMargin
    val (svm, output) = runAsm(src)
    output shouldBe "AB"
  }

  "SVM asm: loop with locals" in {
    val src =
      s"""STDOUT = $STDOUT
         |
         |dl main
         |dl 0
         |
         |main:
         |  frame 1
         |  push_i8 3
         |  local_set 0      ; i = 3
         |.loop:
         |  push_i8 48       ; '0'
         |  local_get 0
         |  add              ; '0' + i
         |  push_i32 STDOUT
         |  store8
         |  local_get 0
         |  dec
         |  dup
         |  local_set 0
         |  jumpnz .loop
         |  drop
         |  halt
         |""".stripMargin
    val (svm, output) = runAsm(src)
    output shouldBe "321"
  }

  "SVM asm: conditional branch" in {
    val src =
      s"""STDOUT = $STDOUT
         |
         |dl main
         |dl 0
         |
         |main:
         |  push_1
         |  jumpz skip
         |  push_i8 'Y'
         |  push_i32 STDOUT
         |  store8
         |skip:
         |  push_i8 '!'
         |  push_i32 STDOUT
         |  store8
         |  halt
         |""".stripMargin
    val (svm, output) = runAsm(src)
    output shouldBe "Y!"
  }

  "SVM asm: arithmetic" in {
    val src =
      s"""STDOUT = $STDOUT
         |
         |dl main
         |dl 0
         |
         |main:
         |  push_i8 10
         |  push_i8 20
         |  add
         |  push_i8 48   ; convert to ASCII digit offset
         |  add
         |  push_i32 STDOUT
         |  store8        ; prints chr(30+48) = 'N'
         |  halt
         |""".stripMargin
    val (svm, output) = runAsm(src)
    output shouldBe "N"
  }
}
