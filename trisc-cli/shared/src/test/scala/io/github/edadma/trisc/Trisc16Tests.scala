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

  // Encoding helpers — see cpu/TRISC16.md for the bit layout.
  private def ldi(r: Int, imm: Int): Int = 0xE000 | (r << 10) | (imm & 0xFF)
  private def sli(r: Int, imm: Int): Int = 0xE000 | (r << 10) | (2 << 8) | (imm & 0xFF)
  private def auipc(r: Int, imm: Int): Int = 0xE000 | (r << 10) | (1 << 8) | (imm & 0xFF)
  private def addRRR(d: Int, a: Int, b: Int): Int = (d << 10) | (a << 7) | (b << 4) | 8
  private def neg(a: Int, b: Int): Int = 0xC000 | (a << 10) | (b << 7) | 0x07
  private def lds(d: Int, a: Int, b: Int): Int = (d << 10) | (a << 7) | (b << 4) | 2
  private def sts(a: Int, b: Int, c: Int): Int = (a << 10) | (b << 7) | (c << 4) | 3
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
    // trap0 encoding: "111 000 nnn 0011 nnn"
    // bits 15-13=111, 12-10=000, 9-7=rrr (must be 000 for trap0?),
    //   actually pattern is "111 000 rrr 0011 iii" — rrr is unused (always 0?), iii=trap number
    //   Wait, looking at TRISC trap pattern in CPU.scala: `"111 000 rrr 0011 iii"`. The 'rrr' field
    //   isn't constrained — but the trap number is encoded in 'iii'. For trap0, iii=000.
    //   Bits 15-13=111, 12-10=000, 9-7=000, 6-3=0011, 2-0=000 → 0xE018
    val trap0 = 0xE018
    val mem = new Memory("Memory", new RAM(0, 0x10000))
    mem.writeShort(0, 0xFF00)         // SP
    mem.writeShort(2, 0x10)           // PC
    mem.writeShort(4, HALT_W)         // exception handler at 0x4 just halts
    mem.writeShort(0x10, trap0)
    mem.writeShort(0x12, HALT_W)      // unreachable
    val cpu = new Trisc16CPU(mem)
    cpu.limit = 100
    cpu.quiet = true
    cpu.reset()
    cpu.run()
    cpu.epc shouldBe 0x10L            // PC at the time of the trap
    cpu.ecause shouldBe 8             // trap0 cause code
    (cpu.psr & 1) shouldBe 1          // PSR.E set
    cpu.state shouldBe State.Halt     // handler ran HALT
  }
