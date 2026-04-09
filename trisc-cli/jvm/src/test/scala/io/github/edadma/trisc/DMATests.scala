package io.github.edadma.trisc

class DMATests extends TestHelpers {

  val BASE = 0x400L

  def mkDMA(): (DMA, InterruptController, RAM) =
    val intc = new InterruptController(0x200)
    val ram = new RAM(0, 0x10000)
    val mem = new Memory("mem", ram)
    val dma = new DMA(BASE, mem, intc, irq = 6)
    (dma, intc, ram)

  // Channel register helpers (channel 0)
  def ch(n: Int, field: Int): Long = BASE + n * 16 + field
  val READ_ADDR = 0
  val WRITE_ADDR = 4
  val TRANS_COUNT = 8
  val CTRL_TRIG = 12

  // CTRL bits
  val SZ_BYTE  = 0x00
  val SZ_SHORT = 0x01
  val SZ_WORD  = 0x02
  val SZ_LONG  = 0x03
  val INCR_R   = 0x04
  val INCR_W   = 0x08
  val ENABLE   = 0x200

  def chainTo(ch: Int): Int = (ch & 0xF) << 4
  val NO_CHAIN = 0xF << 4
  val IRQ_QUIET = 0x100

  // ===== Basic properties =====

  "DMA has correct size" in {
    val (dma, _, _) = mkDMA()
    dma.size shouldBe 202
  }

  // ===== Byte transfer =====

  "byte memcpy via DMA" in {
    val (dma, _, ram) = mkDMA()
    // Fill source
    for i <- 0 until 64 do ram.writeByte(0x1000 + i, (i & 0xFF).toByte)

    dma.writeInt(ch(0, READ_ADDR), 0x1000)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 64)
    dma.writeInt(ch(0, CTRL_TRIG), SZ_BYTE | INCR_R | INCR_W | NO_CHAIN | ENABLE)

    // Run 64 ticks
    for _ <- 1 to 64 do dma.apply(null)

    for i <- 0 until 64 do
      (ram.readByte(0x2000 + i) & 0xFF) shouldBe (i & 0xFF)
  }

  // ===== Word transfer (4x faster) =====

  "word memcpy via DMA (4 bytes per tick)" in {
    val (dma, _, ram) = mkDMA()
    for i <- 0 until 256 do ram.writeByte(0x1000 + i, (i & 0xFF).toByte)

    dma.writeInt(ch(0, READ_ADDR), 0x1000)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 64) // 64 words = 256 bytes
    dma.writeInt(ch(0, CTRL_TRIG), SZ_WORD | INCR_R | INCR_W | NO_CHAIN | ENABLE)

    for _ <- 1 to 64 do dma.apply(null)

    for i <- 0 until 256 do
      (ram.readByte(0x2000 + i) & 0xFF) shouldBe (i & 0xFF)
  }

  // ===== Long transfer (8 bytes per tick) =====

  "long memcpy via DMA (8 bytes per tick)" in {
    val (dma, _, ram) = mkDMA()
    for i <- 0 until 128 do ram.writeByte(0x1000 + i, ((i * 3) & 0xFF).toByte)

    dma.writeInt(ch(0, READ_ADDR), 0x1000)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 16) // 16 longs = 128 bytes
    dma.writeInt(ch(0, CTRL_TRIG), SZ_LONG | INCR_R | INCR_W | NO_CHAIN | ENABLE)

    for _ <- 1 to 16 do dma.apply(null)

    for i <- 0 until 128 do
      (ram.readByte(0x2000 + i) & 0xFF) shouldBe ((i * 3) & 0xFF)
  }

  // ===== Fixed address (device I/O pattern) =====

  "fixed write address (scatter to single location)" in {
    val (dma, _, ram) = mkDMA()
    for i <- 0 until 4 do ram.writeByte(0x1000 + i, (0x10 + i).toByte)

    // INCR_R but NOT INCR_W — writes all to same address
    dma.writeInt(ch(0, READ_ADDR), 0x1000)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 4)
    dma.writeInt(ch(0, CTRL_TRIG), SZ_BYTE | INCR_R | NO_CHAIN | ENABLE)

    for _ <- 1 to 4 do dma.apply(null)

    // Last byte written wins
    (ram.readByte(0x2000) & 0xFF) shouldBe 0x13
  }

  // ===== Completion =====

  "ENABLE cleared on completion" in {
    val (dma, _, ram) = mkDMA()
    dma.writeInt(ch(0, READ_ADDR), 0x1000)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 2)
    dma.writeInt(ch(0, CTRL_TRIG), SZ_BYTE | INCR_R | INCR_W | NO_CHAIN | ENABLE)

    dma.apply(null)
    (dma.readInt(ch(0, CTRL_TRIG)) & ENABLE) should not be 0 // still active

    dma.apply(null) // completes
    (dma.readInt(ch(0, CTRL_TRIG)) & ENABLE) shouldBe 0
  }

  "does not transfer when disabled" in {
    val (dma, _, ram) = mkDMA()
    ram.writeByte(0x1000, 0x42)
    dma.writeInt(ch(0, READ_ADDR), 0x1000)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 1)
    dma.writeInt(ch(0, CTRL_TRIG), SZ_BYTE | INCR_R | INCR_W | NO_CHAIN) // no ENABLE

    for _ <- 1 to 10 do dma.apply(null)
    ram.readByte(0x2000) shouldBe 0
  }

  // ===== Interrupt =====

  "completion raises interrupt" in {
    val (dma, intc, _) = mkDMA()
    // Enable interrupt for channel 0
    dma.writeShort(BASE + 194, 0x01) // INTE: channel 0
    dma.writeInt(ch(0, READ_ADDR), 0x1000)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 1)
    dma.writeInt(ch(0, CTRL_TRIG), SZ_BYTE | NO_CHAIN | ENABLE)

    dma.apply(null)
    (intc.readByte(0x200) & (1 << 6)) should not be 0 // IRQ 6
  }

  "IRQ_QUIET suppresses interrupt" in {
    val (dma, intc, _) = mkDMA()
    dma.writeShort(BASE + 194, 0x01)
    dma.writeInt(ch(0, READ_ADDR), 0x1000)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 1)
    dma.writeInt(ch(0, CTRL_TRIG), SZ_BYTE | NO_CHAIN | IRQ_QUIET | ENABLE)

    dma.apply(null)
    (intc.readByte(0x200) & (1 << 6)) shouldBe 0
  }

  "interrupt clear via INTC register" in {
    val (dma, intc, _) = mkDMA()
    dma.writeShort(BASE + 194, 0x01)
    dma.writeInt(ch(0, READ_ADDR), 0x1000)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 1)
    dma.writeInt(ch(0, CTRL_TRIG), SZ_BYTE | NO_CHAIN | ENABLE)
    dma.apply(null)

    // Clear interrupt
    dma.writeShort(BASE + 198, 0x01)
    // Read masked status
    dma.readShort(BASE + 192) shouldBe 0
  }

  // ===== Channel chaining =====

  "channel chain triggers next on completion" in {
    val (dma, _, ram) = mkDMA()
    // Source data
    for i <- 0 until 8 do ram.writeByte(0x1000 + i, (0xA0 + i).toByte)
    for i <- 0 until 8 do ram.writeByte(0x1100 + i, (0xB0 + i).toByte)

    // Channel 0: copy 8 bytes from 0x1000 to 0x2000, chain to channel 1
    dma.writeInt(ch(0, READ_ADDR), 0x1000)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 8)
    dma.writeInt(ch(0, CTRL_TRIG), SZ_BYTE | INCR_R | INCR_W | chainTo(1) | ENABLE)

    // Channel 1: copy 8 bytes from 0x1100 to 0x2100, no chain
    // Pre-configure but DON'T enable — chain will enable it
    dma.writeInt(ch(1, READ_ADDR), 0x1100)
    dma.writeInt(ch(1, WRITE_ADDR), 0x2100)
    dma.writeInt(ch(1, TRANS_COUNT), 8)
    dma.writeInt(ch(1, CTRL_TRIG), SZ_BYTE | INCR_R | INCR_W | NO_CHAIN) // no ENABLE

    // Run channel 0 to completion
    for _ <- 1 to 8 do dma.apply(null)

    // Channel 1 should now be enabled by chain
    (dma.readInt(ch(1, CTRL_TRIG)) & ENABLE) should not be 0

    // Run channel 1 to completion
    for _ <- 1 to 8 do dma.apply(null)

    // Verify both copies
    for i <- 0 until 8 do
      (ram.readByte(0x2000 + i) & 0xFF) shouldBe (0xA0 + i)
    for i <- 0 until 8 do
      (ram.readByte(0x2100 + i) & 0xFF) shouldBe (0xB0 + i)
  }

  // ===== Multiple channels simultaneously =====

  "two channels transfer in parallel" in {
    val (dma, _, ram) = mkDMA()
    for i <- 0 until 16 do ram.writeByte(0x1000 + i, (i & 0xFF).toByte)
    for i <- 0 until 16 do ram.writeByte(0x1100 + i, ((i + 0x80) & 0xFF).toByte)

    // Channel 0
    dma.writeInt(ch(0, READ_ADDR), 0x1000)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 16)
    dma.writeInt(ch(0, CTRL_TRIG), SZ_BYTE | INCR_R | INCR_W | NO_CHAIN | ENABLE)

    // Channel 1
    dma.writeInt(ch(1, READ_ADDR), 0x1100)
    dma.writeInt(ch(1, WRITE_ADDR), 0x2100)
    dma.writeInt(ch(1, TRANS_COUNT), 16)
    dma.writeInt(ch(1, CTRL_TRIG), SZ_BYTE | INCR_R | INCR_W | NO_CHAIN | ENABLE)

    for _ <- 1 to 16 do dma.apply(null)

    for i <- 0 until 16 do
      (ram.readByte(0x2000 + i) & 0xFF) shouldBe (i & 0xFF)
    for i <- 0 until 16 do
      (ram.readByte(0x2100 + i) & 0xFF) shouldBe ((i + 0x80) & 0xFF)
  }

  // ===== Abort =====

  "abort stops transfer" in {
    val (dma, _, ram) = mkDMA()
    for i <- 0 until 100 do ram.writeByte(0x1000 + i, 0xFF.toByte)

    dma.writeInt(ch(0, READ_ADDR), 0x1000)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 100)
    dma.writeInt(ch(0, CTRL_TRIG), SZ_BYTE | INCR_R | INCR_W | NO_CHAIN | ENABLE)

    // Run 10 ticks then abort
    for _ <- 1 to 10 do dma.apply(null)
    dma.writeShort(BASE + 200, 0x01) // ABORT channel 0

    // Run more ticks — should not transfer
    for _ <- 1 to 50 do dma.apply(null)

    // Only first 10 bytes should be copied
    (ram.readByte(0x2009) & 0xFF) shouldBe 0xFF
    ram.readByte(0x200A) shouldBe 0
  }

  // ===== Register readback =====

  "register readback" in {
    val (dma, _, _) = mkDMA()
    dma.writeInt(ch(0, READ_ADDR), 0x12345678)
    dma.writeInt(ch(0, WRITE_ADDR), 0xABCD0000)
    dma.writeInt(ch(0, TRANS_COUNT), 42)

    dma.readInt(ch(0, READ_ADDR)) shouldBe 0x12345678
    dma.readInt(ch(0, WRITE_ADDR)) shouldBe 0xABCD0000L.toInt
    dma.readInt(ch(0, TRANS_COUNT)) shouldBe 42
  }

  // ===== Memset pattern (fixed read, increment write) =====

  "memset pattern: fill memory with constant" in {
    val (dma, _, ram) = mkDMA()
    // Put the fill value at a known address
    ram.writeByte(0x0F00, 0x42)

    // Fixed read (no INCR_R), increment write
    dma.writeInt(ch(0, READ_ADDR), 0x0F00)
    dma.writeInt(ch(0, WRITE_ADDR), 0x2000)
    dma.writeInt(ch(0, TRANS_COUNT), 32)
    dma.writeInt(ch(0, CTRL_TRIG), SZ_BYTE | INCR_W | NO_CHAIN | ENABLE)

    for _ <- 1 to 32 do dma.apply(null)

    for i <- 0 until 32 do
      (ram.readByte(0x2000 + i) & 0xFF) shouldBe 0x42
  }
}
