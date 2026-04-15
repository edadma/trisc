package io.github.edadma.trisc

class IPITests extends TestHelpers {

  val IPI_BASE = 0x300L

  def mkIPI(numCores: Int = 4): (Array[IPI], Array[InterruptController]) =
    val intcs = Array.tabulate(numCores)(i => new InterruptController(0x200 + i * 0x100))
    val ipis = Array.tabulate(numCores)(i => new IPI(IPI_BASE + i * 16, i, intcs))
    (ipis, intcs)

  // ===== Basic properties =====

  "IPI has correct size" in {
    val (ipis, _) = mkIPI()
    ipis(0).size shouldBe 16
  }

  "SELF_ID returns correct core ID" in {
    val (ipis, _) = mkIPI()
    ipis(0).readInt(IPI_BASE + 12) shouldBe 0
    ipis(1).readInt(IPI_BASE + 16 + 12) shouldBe 1
    ipis(2).readInt(IPI_BASE + 32 + 12) shouldBe 2
    ipis(3).readInt(IPI_BASE + 48 + 12) shouldBe 3
  }

  // ===== Register read/write =====

  "TARGET register read/write" in {
    val (ipis, _) = mkIPI()
    val ipi = ipis(0)
    ipi.writeInt(IPI_BASE, 2)
    ipi.readInt(IPI_BASE) shouldBe 2
  }

  "VECTOR register read/write" in {
    val (ipis, _) = mkIPI()
    val ipi = ipis(0)
    ipi.writeInt(IPI_BASE + 4, 0x42)
    ipi.readInt(IPI_BASE + 4) shouldBe 0x42
  }

  // ===== IPI send =====

  "core 0 sends IPI to core 1" in {
    val (ipis, intcs) = mkIPI()
    val ipi0 = ipis(0)

    // Set target = core 1
    ipi0.writeInt(IPI_BASE, 1)
    // Send IPI
    ipi0.writeByte(IPI_BASE + 8, 1)

    // Status should be success
    ipi0.readByte(IPI_BASE + 9) shouldBe 1

    // Core 1's INTC should have a pending interrupt on IRQ 7
    (intcs(1).readByte(intcs(1).base) & (1 << 7)) shouldBe (1 << 7)
  }

  "core 2 sends IPI to core 0" in {
    val (ipis, intcs) = mkIPI()
    val ipi2 = ipis(2)
    val base2 = IPI_BASE + 2 * 16

    ipi2.writeInt(base2, 0) // target = core 0
    ipi2.writeByte(base2 + 8, 1) // send

    ipi2.readByte(base2 + 9) shouldBe 1 // success
    (intcs(0).readByte(intcs(0).base) & (1 << 7)) shouldBe (1 << 7)
  }

  "IPI to self works" in {
    val (ipis, intcs) = mkIPI()
    val ipi0 = ipis(0)

    ipi0.writeInt(IPI_BASE, 0) // target = self
    ipi0.writeByte(IPI_BASE + 8, 1)

    ipi0.readByte(IPI_BASE + 9) shouldBe 1
    (intcs(0).readByte(intcs(0).base) & (1 << 7)) shouldBe (1 << 7)
  }

  "IPI to invalid core sets error status" in {
    val (ipis, _) = mkIPI()
    val ipi0 = ipis(0)

    ipi0.writeInt(IPI_BASE, 99) // invalid core ID
    ipi0.writeByte(IPI_BASE + 8, 1)

    ipi0.readByte(IPI_BASE + 9) shouldBe 2 // invalid target
  }

  "multiple IPIs in sequence" in {
    val (ipis, intcs) = mkIPI()
    val ipi0 = ipis(0)

    // Send to core 1
    ipi0.writeInt(IPI_BASE, 1)
    ipi0.writeByte(IPI_BASE + 8, 1)
    (intcs(1).readByte(intcs(1).base) & (1 << 7)) shouldBe (1 << 7)

    // Claim the interrupt on core 1 (clears pending)
    intcs(1).readByte(intcs(1).base + 2) // CLAIM

    // Send to core 2
    ipi0.writeInt(IPI_BASE, 2)
    ipi0.writeByte(IPI_BASE + 8, 1)
    (intcs(2).readByte(intcs(2).base) & (1 << 7)) shouldBe (1 << 7)
  }

  "COMMAND=0 does not send" in {
    val (ipis, intcs) = mkIPI()
    val ipi0 = ipis(0)

    ipi0.writeInt(IPI_BASE, 1)
    ipi0.writeByte(IPI_BASE + 8, 0) // no send

    // No interrupt on core 1
    (intcs(1).readByte(intcs(1).base) & (1 << 7)) shouldBe 0
  }

  // ===== Byte-level register access =====

  "TARGET via byte writes" in {
    val (ipis, _) = mkIPI()
    val ipi = ipis(0)

    ipi.writeByte(IPI_BASE, 3) // low byte
    ipi.readInt(IPI_BASE) shouldBe 3
  }

  "STATUS is read-only (write ignored)" in {
    val (ipis, _) = mkIPI()
    val ipi = ipis(0)

    // Send a valid IPI first to set status=1
    ipi.writeInt(IPI_BASE, 0)
    ipi.writeByte(IPI_BASE + 8, 1)
    ipi.readByte(IPI_BASE + 9) shouldBe 1

    // Try to write STATUS
    ipi.writeByte(IPI_BASE + 9, 0)
    ipi.readByte(IPI_BASE + 9) shouldBe 1 // unchanged
  }
}
