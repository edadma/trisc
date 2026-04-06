package io.github.edadma.trisc

class PL011Tests extends TestHelpers {

  def mkPL011(fifoDepth: Int = 16): (PL011, InterruptController, StringBuilder) =
    val intc = new InterruptController(0x200)
    val output = new StringBuilder
    val uart = new PL011(0x09000000L, intc, irq = 1, onTx = b => output += b.toChar, fifoDepth = fifoDepth)
    // Enable UART: UARTEN | TXE | RXE
    uart.writeInt(0x09000030L, 0x0301) // CR = UARTEN + TXE + RXE
    (uart, intc, output)

  // ===== Basic properties =====

  "PL011 has 4KB size" in {
    val (uart, _, _) = mkPL011()
    uart.size shouldBe 0x1000
  }

  "PL011 flag register shows TX empty, RX empty initially" in {
    val (uart, _, _) = mkPL011()
    val fr = uart.readInt(0x09000018L)
    (fr & 0x80) should not be 0 // TXFE
    (fr & 0x10) should not be 0 // RXFE
    (fr & 0x20) shouldBe 0      // not TXFF
    (fr & 0x40) shouldBe 0      // not RXFF
  }

  // ===== TX =====

  "TX writes byte to FIFO and transmits on tick" in {
    val (uart, _, output) = mkPL011()
    uart.writeInt(0x09000000L, 'A') // UARTDR
    uart.apply(null) // tick transmits
    output.toString shouldBe "A"
  }

  "TX FIFO sends multiple bytes in order" in {
    val (uart, _, output) = mkPL011()
    uart.writeInt(0x09000000L, 'H')
    uart.writeInt(0x09000000L, 'i')
    uart.apply(null)
    uart.apply(null)
    output.toString shouldBe "Hi"
  }

  "TX flag shows full when FIFO is full" in {
    val (uart, _, _) = mkPL011(fifoDepth = 2)
    uart.writeInt(0x09000000L, 'A')
    uart.writeInt(0x09000000L, 'B')
    val fr = uart.readInt(0x09000018L)
    (fr & 0x20) should not be 0 // TXFF
  }

  "TX does nothing when UART disabled" in {
    val intc = new InterruptController(0x200)
    val output = new StringBuilder
    val uart = new PL011(0x09000000L, intc, irq = 1, onTx = b => output += b.toChar)
    // CR = 0 (disabled)
    uart.writeInt(0x09000000L, 'A')
    uart.apply(null)
    output.toString shouldBe ""
  }

  "TX interrupt fires when FIFO empties" in {
    val (uart, intc, _) = mkPL011()
    uart.writeInt(0x09000038L, 0x20) // IMSC: enable TX interrupt
    uart.writeInt(0x09000000L, 'A')
    uart.apply(null) // transmit A, FIFO now empty
    val mis = uart.readInt(0x09000040L)
    (mis & 0x20) should not be 0 // TX interrupt masked status
    (intc.readByte(0x200) & (1 << 1)) should not be 0 // IRQ 1
  }

  // ===== RX =====

  "RX receive makes byte available via UARTDR" in {
    val (uart, _, _) = mkPL011()
    uart.receive('Z')
    val fr = uart.readInt(0x09000018L)
    (fr & 0x10) shouldBe 0 // RXFE cleared (data available)
    val data = uart.readInt(0x09000000L)
    data shouldBe 'Z'
  }

  "RX FIFO preserves order" in {
    val (uart, _, _) = mkPL011()
    uart.receive('A')
    uart.receive('B')
    uart.receive('C')
    uart.readInt(0x09000000L) shouldBe 'A'
    uart.readInt(0x09000000L) shouldBe 'B'
    uart.readInt(0x09000000L) shouldBe 'C'
  }

  "RX returns 0 when FIFO empty" in {
    val (uart, _, _) = mkPL011()
    uart.readInt(0x09000000L) shouldBe 0
  }

  "RX RXFE flag set after reading last byte" in {
    val (uart, _, _) = mkPL011()
    uart.receive('X')
    uart.readInt(0x09000000L)
    val fr = uart.readInt(0x09000018L)
    (fr & 0x10) should not be 0 // RXFE set again
  }

  "RX interrupt fires on receive" in {
    val (uart, intc, _) = mkPL011()
    uart.writeInt(0x09000038L, 0x10) // IMSC: enable RX interrupt
    uart.receive('A')
    val mis = uart.readInt(0x09000040L)
    (mis & 0x10) should not be 0 // RX interrupt
    (intc.readByte(0x200) & (1 << 1)) should not be 0
  }

  "RX interrupt cleared via ICR" in {
    val (uart, intc, _) = mkPL011()
    uart.writeInt(0x09000038L, 0x10) // IMSC: enable RX interrupt
    uart.receive('A')
    uart.readInt(0x09000000L) // read the data
    uart.writeInt(0x09000044L, 0x10) // ICR: clear RX interrupt
    val mis = uart.readInt(0x09000040L)
    (mis & 0x10) shouldBe 0
  }

  "RX no interrupt when mask disabled" in {
    val (uart, intc, _) = mkPL011()
    // IMSC = 0 (all masked)
    uart.receive('A')
    (intc.readByte(0x200) & (1 << 1)) shouldBe 0
  }

  // ===== Register readback =====

  "IBRD register readback" in {
    val (uart, _, _) = mkPL011()
    uart.writeInt(0x09000024L, 26)
    uart.readInt(0x09000024L) shouldBe 26
  }

  "FBRD register readback" in {
    val (uart, _, _) = mkPL011()
    uart.writeInt(0x09000028L, 3)
    uart.readInt(0x09000028L) shouldBe 3
  }

  "LCR_H register readback" in {
    val (uart, _, _) = mkPL011()
    uart.writeInt(0x0900002CL, 0x70) // 8-bit, FIFO enable
    uart.readInt(0x0900002CL) shouldBe 0x70
  }

  "CR register readback" in {
    val (uart, _, _) = mkPL011()
    uart.readInt(0x09000030L) shouldBe 0x0301 // UARTEN + TXE + RXE
  }

  "IMSC register readback" in {
    val (uart, _, _) = mkPL011()
    uart.writeInt(0x09000038L, 0x30) // RX + TX
    uart.readInt(0x09000038L) shouldBe 0x30
  }

  "RIS shows raw status" in {
    val (uart, _, _) = mkPL011()
    uart.receive('A')
    val ris = uart.readInt(0x0900003CL)
    (ris & 0x10) should not be 0 // RX raw interrupt
  }

  "MIS is RIS masked by IMSC" in {
    val (uart, _, _) = mkPL011()
    uart.receive('A')
    // Without mask: MIS should be 0
    uart.readInt(0x09000040L) shouldBe 0
    // Enable RX mask
    uart.writeInt(0x09000038L, 0x10)
    (uart.readInt(0x09000040L) & 0x10) should not be 0
  }

  // ===== Error clear =====

  "RSR cleared by write" in {
    val (uart, _, _) = mkPL011()
    // RSR starts at 0
    uart.readInt(0x09000004L) shouldBe 0
    // Write anything to clear (no-op since already 0, but shouldn't error)
    uart.writeInt(0x09000004L, 0xFF)
    uart.readInt(0x09000004L) shouldBe 0
  }

  // ===== Loopback =====

  "loopback: TX callback feeds RX" in {
    val intc = new InterruptController(0x200)
    var uart: PL011 = null
    uart = new PL011(0x09000000L, intc, irq = 1, onTx = b => uart.receive(b))
    uart.writeInt(0x09000030L, 0x0301) // enable
    uart.writeInt(0x09000000L, 'L')
    uart.apply(null) // transmit → loopback to RX
    uart.readInt(0x09000000L) shouldBe 'L'
  }

  // ===== Byte-level access =====

  "byte write to UARTDR transmits" in {
    val (uart, _, output) = mkPL011()
    uart.writeByte(0x09000000L, 'B')
    uart.apply(null)
    output.toString shouldBe "B"
  }

  "byte read from flag register" in {
    val (uart, _, _) = mkPL011()
    // FR at 0x18 — byte 0 should have TXFE(0x80) and RXFE(0x10) = 0x90
    val b = uart.readByte(0x09000018L)
    (b & 0x80) should not be 0 // TXFE
    (b & 0x10) should not be 0 // RXFE
  }
}
