package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Smoke tests for the Trisc16CPU subclass. The TRISC16 ISA shares
  * encodings with TRISC, so we hand-encode a few instructions and verify
  * that the subclass enforces 16-bit register width, ×2 AUIPC scaling,
  * 16-bit LD/ST size, the 4-byte boot vector, and the simplified
  * exception model. */
class Trisc16Tests extends AnyFreeSpec with Matchers:

  // Tiny memory model: 64KB RAM. Boot vector at 0/2 (16-bit each).
  private def freshCPU(prog: Iterable[Int], sp: Int = 0xFF00, pcStart: Int = 0x10): Trisc16CPU =
    val mem = new Memory("Memory", new RAM(0, 0x10000))
    mem.writeShort(0, sp)
    mem.writeShort(2, pcStart)
    var addr = pcStart
    for w <- prog do
      mem.writeShort(addr, w)
      addr += 2
    val cpu = new Trisc16CPU(mem)
    cpu.limit = 1000
    cpu.quiet = true
    cpu.reset()
    cpu

  private def runUntilHalt(cpu: Trisc16CPU): Unit =
    cpu.run()

  /** Build a CPU whose exception handler at 0x4 is a single HALT, so any fault
    * delivers cleanly into State.Halt. The body program loads at 0x10. */
  private def withHandler(prog: Iterable[Int], sp: Int = 0xFF00): Trisc16CPU =
    val mem = new Memory("Memory", new RAM(0, 0x10000))
    mem.writeShort(0, sp)
    mem.writeShort(2, 0x10)
    mem.writeShort(0x4, HALT_W)
    var addr = 0x10
    for w <- prog do
      mem.writeShort(addr, w)
      addr += 2
    val cpu = new Trisc16CPU(mem)
    cpu.limit = 100
    cpu.quiet = true
    cpu.reset()
    cpu

  // Encoding helpers — see cpu/TRISC16.md for the bit layout.
  private def ldi(r: Int, imm: Int): Int = 0xE000 | (r << 10) | (imm & 0xFF)
  private def sli(r: Int, imm: Int): Int = 0xE000 | (r << 10) | (2 << 8) | (imm & 0xFF)
  private def auipc(r: Int, imm: Int): Int = 0xE000 | (r << 10) | (1 << 8) | (imm & 0xFF)
  private def addRRR(d: Int, a: Int, b: Int): Int = (d << 10) | (a << 7) | (b << 4) | 8
  private def neg(a: Int, b: Int): Int = 0xC000 | (a << 10) | (b << 7) | 0x07
  private def lds(d: Int, a: Int, b: Int): Int = (d << 10) | (a << 7) | (b << 4) | 2
  private def sts(a: Int, b: Int, c: Int): Int = (a << 10) | (b << 7) | (c << 4) | 3
  private def trap(n: Int): Int = 0xE018 | (n & 7)
  private def gepc(r: Int): Int = 0xE000 | (r << 7) | 0x0C        // 111 000 rrr 0001100
  private def gcause(r: Int): Int = 0xE000 | (r << 7) | 0x0D      // 111 000 rrr 0001101
  private def pshr(n: Int): Int = 0xE000 | (n << 7) | 0x10        // 111 000 rrr 0010000
  private def popr(n: Int): Int = 0xE000 | (n << 7) | 0x11        // 111 000 rrr 0010001
  private val RTE_W: Int = 0xE00A                                 // 111 000 000 0001010
  private val HALT_W: Int = 0xC000

  "writeMask truncates register writes to 16 bits" in {
    // ldi r1, 0xFF; sli r1, 0xAB; sli r1, 0xCD; sli r1, 0xEF; halt
    // After three slis, the unmasked value would be 0xFFABCDEF (32 bits).
    // With a 16-bit mask, the final register holds 0xCDEF.
    val cpu = freshCPU(Seq(
      ldi(1, 0xFF),
      sli(1, 0xAB),
      sli(1, 0xCD),
      sli(1, 0xEF),
      HALT_W,
    ))
    runUntilHalt(cpu)
    cpu.r(1).read shouldBe 0xCDEFL
  }

  "add wraps at 16 bits" in {
    // r1 = 0xFFFF, r2 = 1, r3 = r1 + r2 → 0 (after 16-bit truncation on write)
    val cpu = freshCPU(Seq(
      ldi(1, 0xFF), sli(1, 0xFF),     // r1 = 0xFFFF
      ldi(2, 1),                       // r2 = 1
      addRRR(3, 1, 2),                 // r3 = r1 + r2
      HALT_W,
    ))
    runUntilHalt(cpu)
    cpu.r(3).read shouldBe 0L
  }

  "auipc uses ×2 scaling" in {
    // pcStart=0x10. After auipc r1, 5 the instruction's PC is 0x10, post-fetch
    // pc-2 = 0x10. Result: r1 = 0x10 + 5*2 = 0x1A.
    val cpu = freshCPU(Seq(
      auipc(1, 5),
      HALT_W,
    ))
    runUntilHalt(cpu)
    cpu.r(1).read shouldBe 0x1AL
  }

  "neg masks negative result to 16 bits" in {
    // r1 = 1, r2 = -r1 → 0xFFFF in 16-bit register (since high bits are masked off)
    val cpu = freshCPU(Seq(
      ldi(1, 1),
      neg(2, 1),
      HALT_W,
    ))
    runUntilHalt(cpu)
    cpu.r(2).read shouldBe 0xFFFFL
  }

  "lds/sts work with 16-bit memory accesses" in {
    // r1 = 0xBEEF (via ldi+sli)
    // r2 = 0x100 (data address, must be even-aligned: 0x100)
    // r3 = 0   (offset register)
    // sts r1, r2, r3
    // lds r4, r2, r3
    // halt
    val cpu = freshCPU(Seq(
      ldi(1, 0xBE), sli(1, 0xEF),     // r1 = 0xBEEF
      ldi(2, 1), sli(2, 0),            // r2 = 0x100
      // r3 stays at 0
      sts(1, 2, 0),                    // mem[r2 + r3] = r1 (16-bit)
      lds(4, 2, 0),                    // r4 = mem[r2 + r3] (16-bit)
      HALT_W,
    ))
    runUntilHalt(cpu)
    cpu.r(4).read shouldBe 0xBEEFL
  }

  "boot vector loads SP from 0x0 and PC from 0x2 as 16-bit values" in {
    val cpu = freshCPU(Seq(HALT_W), sp = 0xABCD, pcStart = 0x10)
    runUntilHalt(cpu)
    cpu.r(7).read shouldBe 0xABCDL
    cpu.pc shouldBe 0x12L  // 0x10 (start) + 2 (after fetching halt)
    cpu.state shouldBe State.Halt
  }

  "trap0 enters TRISC16 simplified exception: EPC=faulting PC, ECAUSE=8, PC=0x4" in {
    // Place a halt at 0x04 so the handler exits cleanly.
    // Program at 0x10: trap0; halt
    val cpu = withHandler(Seq(
      trap(0),
      HALT_W,                          // unreachable
    ))
    cpu.run()
    cpu.epc shouldBe 0x10L             // PC at the time of the trap
    cpu.ecause shouldBe 8              // trap0 cause code
    (cpu.psr & 1) shouldBe 1           // PSR.E set
    cpu.state shouldBe State.Halt      // handler ran HALT
  }

  "gepc and gcause expose EPC/ECAUSE in the handler" in {
    // Handler at 0x4: gepc r1; gcause r2; halt
    // Body at 0x10: trap5 (cause 13)
    val mem = new Memory("Memory", new RAM(0, 0x10000))
    mem.writeShort(0, 0xFF00)
    mem.writeShort(2, 0x10)
    mem.writeShort(0x4, gepc(1))
    mem.writeShort(0x6, gcause(2))
    mem.writeShort(0x8, HALT_W)
    mem.writeShort(0x10, trap(5))
    val cpu = new Trisc16CPU(mem)
    cpu.limit = 100
    cpu.quiet = true
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 0x10L       // EPC = trap site
    cpu.r(2).read shouldBe 13L         // trap5 → cause 13
    cpu.state shouldBe State.Halt
  }

  "rte restores PC from EPC and clears PSR.E" in {
    // No software path to write EPC, so set it directly. After rte we expect
    // PC ← 0x40 (the value we pre-loaded into EPC) and PSR.E ← 0.
    val mem = new Memory("Memory", new RAM(0, 0x10000))
    mem.writeShort(0, 0xFF00)
    mem.writeShort(2, 0x10)
    mem.writeShort(0x10, RTE_W)
    mem.writeShort(0x40, HALT_W)       // landing pad after rte
    val cpu = new Trisc16CPU(mem)
    cpu.limit = 100
    cpu.quiet = true
    cpu.reset()
    cpu.epc = 0x40L
    cpu.psr = cpu.psr | 1              // pretend we are in a handler
    cpu.run()
    cpu.pc shouldBe 0x42L              // 0x40 + 2 (after fetching halt at 0x40)
    (cpu.psr & 1) shouldBe 0           // PSR.E cleared by rte
    cpu.state shouldBe State.Halt
  }

  "Trisc16Decode rejects TRISC-only encodings as illegal-instruction" in {
    // fadd, ldd, tlbi each fall outside TRISC16's allow-list. Each should
    // decode to IllegalInstruction → State.UnimplementedOpcode → cause code 1
    // delivered to the handler.
    for (encoding, label) <- Seq(
      (0x253B, "fadd r1,r2,r3"),       // RRR-001 1011 — float add
      (0x0536, "ldd r1,r2,r3"),        // RRR-000 0110 — 64-bit load
      (0xC521, "tlbi r1,r2"),          // RR-01 00001 — MMU
    ) do
      val cpu = withHandler(Seq(encoding))
      cpu.run()
      withClue(s"$label: ") {
        cpu.ecause shouldBe 1          // illegal/unimplemented
        cpu.state shouldBe State.Halt  // handler at 0x4 halted
      }
  }

  "pshr/popr round-trip 16-bit registers in 2-byte slots" in {
    // ldi r1=0x11; sli r1=0x22; ldi r2=0x33; sli r2=0x44; ldi r3=0x55; sli r3=0x66
    // pshr r3 — push r1, r2, r3 (3 * 2 = 6 bytes)
    // ldi r1=0; ldi r2=0; ldi r3=0  (clobber)
    // popr r3 — restore
    // halt
    val cpu = freshCPU(Seq(
      ldi(1, 0x11), sli(1, 0x22),      // r1 = 0x1122
      ldi(2, 0x33), sli(2, 0x44),      // r2 = 0x3344
      ldi(3, 0x55), sli(3, 0x66),      // r3 = 0x5566
      pshr(3),                         // push r1..r3 (6 bytes)
      ldi(1, 0), ldi(2, 0), ldi(3, 0), // clobber
      popr(3),                         // restore r1..r3
      HALT_W,
    ), sp = 0xFF00)
    runUntilHalt(cpu)
    cpu.r(1).read shouldBe 0x1122L
    cpu.r(2).read shouldBe 0x3344L
    cpu.r(3).read shouldBe 0x5566L
    cpu.r(7).read shouldBe 0xFF00L     // SP back to start (push 6 + pop 6)
  }

  "fault inside a handler triggers DoubleFault" in {
    // trap0 at 0x10 → handler at 0x4 issues another trap0 → PSR.E already set,
    // Trisc16CPU.enterException short-circuits to DoubleFault.
    val mem = new Memory("Memory", new RAM(0, 0x10000))
    mem.writeShort(0, 0xFF00)
    mem.writeShort(2, 0x10)
    mem.writeShort(0x4, trap(0))       // handler does another trap (nested)
    mem.writeShort(0x6, HALT_W)        // unreachable
    mem.writeShort(0x10, trap(0))
    val cpu = new Trisc16CPU(mem)
    cpu.limit = 100
    cpu.quiet = true
    cpu.reset()
    cpu.run()
    cpu.state shouldBe State.DoubleFault
  }
