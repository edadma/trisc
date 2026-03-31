package io.github.edadma.trisc

class UARTTests extends TestHelpers {

  def mkUART(divisor: Int = 1, fifoDepth: Int = 16): (UART, InterruptController, StringBuilder) =
    val intc = new InterruptController(0x200)
    val output = new StringBuilder
    val uart = new UART(0x100, intc, txIrq = 2, rxIrq = 3, onTx = b => output += b.toChar, fifoDepth = fifoDepth)
    // Set divisor
    uart.writeByte(0x107, divisor)
    (uart, intc, output)

  // ===== Basic properties =====

  "UART has correct size" in {
    val (uart, _, _) = mkUART()
    uart.size shouldBe 8
  }

  "UART status shows TX empty initially" in {
    val (uart, _, _) = mkUART()
    (uart.readByte(0x102) & 0x01) shouldBe 0x01 // TX empty
  }

  "UART status shows RX not ready initially" in {
    val (uart, _, _) = mkUART()
    (uart.readByte(0x102) & 0x02) shouldBe 0x00 // RX not ready
  }

  // ===== TX =====

  "TX transmits byte after divisor ticks" in {
    val (uart, _, output) = mkUART(divisor = 5)
    uart.writeByte(0x103, 0x04) // enable
    uart.writeByte(0x100, 'A')  // write to TX
    for _ <- 1 to 4 do uart.apply(null)
    output.toString shouldBe "" // not yet
    uart.apply(null)            // 5th tick
    output.toString shouldBe "A"
  }

  "TX with divisor=1 transmits immediately" in {
    val (uart, _, output) = mkUART(divisor = 1)
    uart.writeByte(0x103, 0x04) // enable
    uart.writeByte(0x100, 'X')
    uart.apply(null)
    output.toString shouldBe "X"
  }

  "TX FIFO sends multiple bytes in order" in {
    val (uart, _, output) = mkUART(divisor = 1)
    uart.writeByte(0x103, 0x04)
    uart.writeByte(0x100, 'H')
    uart.writeByte(0x100, 'i')
    uart.apply(null)
    uart.apply(null)
    output.toString shouldBe "Hi"
  }

  "TX status shows full when FIFO is full" in {
    val (uart, _, _) = mkUART(divisor = 1, fifoDepth = 2)
    uart.writeByte(0x103, 0x04)
    uart.writeByte(0x100, 'A')
    uart.writeByte(0x100, 'B')
    (uart.readByte(0x102) & 0x04) should not be 0 // TX full
  }

  "TX drops byte when FIFO is full" in {
    val (uart, _, output) = mkUART(divisor = 1, fifoDepth = 2)
    uart.writeByte(0x103, 0x04)
    uart.writeByte(0x100, 'A')
    uart.writeByte(0x100, 'B')
    uart.writeByte(0x100, 'C') // dropped
    uart.apply(null)
    uart.apply(null)
    uart.apply(null)
    output.toString shouldBe "AB"
  }

  "TX empty interrupt fires when FIFO drains" in {
    val (uart, intc, _) = mkUART(divisor = 1)
    uart.writeByte(0x103, 0x05) // enable + TX IE
    uart.writeByte(0x100, 'A')
    uart.apply(null) // transmit A, FIFO now empty
    // INTC should have TX IRQ pending (bit 2)
    (intc.readByte(0x200) & (1 << 2)) should not be 0
  }

  "TX no interrupt when TX IE disabled" in {
    val (uart, intc, _) = mkUART(divisor = 1)
    uart.writeByte(0x103, 0x04) // enable only, no TX IE
    uart.writeByte(0x100, 'A')
    uart.apply(null)
    (intc.readByte(0x200) & (1 << 2)) shouldBe 0
  }

  "TX does nothing when disabled" in {
    val (uart, _, output) = mkUART(divisor = 1)
    // control = 0 (disabled)
    uart.writeByte(0x100, 'A')
    uart.apply(null)
    uart.apply(null)
    output.toString shouldBe ""
  }

  // ===== RX =====

  "RX receive makes byte available" in {
    val (uart, _, _) = mkUART()
    uart.receive('Z')
    (uart.readByte(0x102) & 0x02) should not be 0 // RX ready
    uart.readByte(0x101) shouldBe 'Z'
  }

  "RX FIFO preserves order" in {
    val (uart, _, _) = mkUART()
    uart.receive('A')
    uart.receive('B')
    uart.receive('C')
    uart.readByte(0x101) shouldBe 'A'
    uart.readByte(0x101) shouldBe 'B'
    uart.readByte(0x101) shouldBe 'C'
  }

  "RX returns 0 when FIFO empty" in {
    val (uart, _, _) = mkUART()
    uart.readByte(0x101) shouldBe 0
  }

  "RX ready clears after reading last byte" in {
    val (uart, _, _) = mkUART()
    uart.receive('X')
    uart.readByte(0x101) // read it
    (uart.readByte(0x102) & 0x02) shouldBe 0 // RX not ready
  }

  "RX overrun when FIFO full" in {
    val (uart, _, _) = mkUART(fifoDepth = 2)
    uart.receive('A')
    uart.receive('B')
    uart.receive('C') // overflow
    (uart.readByte(0x102) & 0x08) should not be 0 // overrun flag
  }

  "RX overrun clears on status write" in {
    val (uart, _, _) = mkUART(fifoDepth = 2)
    uart.receive('A')
    uart.receive('B')
    uart.receive('C')
    uart.writeByte(0x102, 0) // clear overrun
    (uart.readByte(0x102) & 0x08) shouldBe 0
  }

  "RX interrupt fires on first byte" in {
    val (uart, intc, _) = mkUART()
    uart.writeByte(0x103, 0x06) // enable + RX IE
    uart.receive('A')
    (intc.readByte(0x200) & (1 << 3)) should not be 0 // RX IRQ
  }

  "RX interrupt does not fire when second byte arrives" in {
    val (uart, intc, _) = mkUART()
    uart.writeByte(0x103, 0x06) // enable + RX IE
    uart.receive('A')
    // Clear the IRQ manually
    intc.writeByte(0x203, 3) // ACK IRQ 3
    uart.receive('B') // FIFO was not empty, no new interrupt
    (intc.readByte(0x200) & (1 << 3)) shouldBe 0
  }

  "RX no interrupt when RX IE disabled" in {
    val (uart, intc, _) = mkUART()
    uart.writeByte(0x103, 0x04) // enable only
    uart.receive('A')
    (intc.readByte(0x200) & (1 << 3)) shouldBe 0
  }

  "RX no interrupt when device disabled" in {
    val (uart, intc, _) = mkUART()
    uart.writeByte(0x103, 0x02) // RX IE but not enabled
    uart.receive('A')
    (intc.readByte(0x200) & (1 << 3)) shouldBe 0
  }

  // ===== Divisor =====

  "divisor register readback" in {
    val (uart, _, _) = mkUART()
    uart.writeByte(0x104, 0x00)
    uart.writeByte(0x105, 0x01)
    uart.writeByte(0x106, 0x00)
    uart.writeByte(0x107, 0x00)
    uart.readByte(0x104) shouldBe 0x00
    uart.readByte(0x105) shouldBe 0x01
    uart.readByte(0x106) shouldBe 0x00
    uart.readByte(0x107) shouldBe 0x00
  }

  "divisor controls TX timing" in {
    val (uart, _, output) = mkUART(divisor = 3)
    uart.writeByte(0x103, 0x04) // enable
    uart.writeByte(0x100, 'A')
    uart.writeByte(0x100, 'B')
    // A should take 3 ticks, B should take 3 more
    uart.apply(null) // 1
    uart.apply(null) // 2
    output.toString shouldBe ""
    uart.apply(null) // 3 → A sent
    output.toString shouldBe "A"
    uart.apply(null) // 4
    uart.apply(null) // 5
    uart.apply(null) // 6 → B sent
    output.toString shouldBe "AB"
  }

  // ===== Control register =====

  "control register readback" in {
    val (uart, _, _) = mkUART()
    uart.writeByte(0x103, 0x07)
    uart.readByte(0x103) shouldBe 0x07
  }

  // ===== Loopback pattern =====

  "loopback: TX callback feeds RX" in {
    val intc = new InterruptController(0x200)
    var uart: UART = null
    uart = new UART(0x100, intc, txIrq = 2, rxIrq = 3, onTx = b => uart.receive(b))
    uart.writeByte(0x107, 1)    // divisor = 1
    uart.writeByte(0x103, 0x04) // enable
    uart.writeByte(0x100, 'L')
    uart.apply(null)
    uart.readByte(0x101) shouldBe 'L'
  }
}
