package io.github.edadma.trisc

class GPIOTests extends TestHelpers {

  def mkGPIO(width: Int = 8): (GPIO, InterruptController) =
    val intc = new InterruptController(0x200)
    val gpio = new GPIO(0x100, width, intc, irq = 1)
    (gpio, intc)

  // ===== Basic register access =====

  "GPIO has correct size" in {
    val (gpio, _) = mkGPIO()
    gpio.size shouldBe 10
  }

  "DDR defaults to all input" in {
    val (gpio, _) = mkGPIO()
    gpio.readByte(0x100) shouldBe 0
  }

  "OUT defaults to zero" in {
    val (gpio, _) = mkGPIO()
    gpio.readByte(0x101) shouldBe 0
  }

  // ===== Direction and I/O =====

  "output pins reflect OUT when DDR=1" in {
    val (gpio, _) = mkGPIO()
    gpio.writeByte(0x100, 0xFF) // all output
    gpio.writeByte(0x101, 0xA5) // OUT = 0xA5
    gpio.readByte(0x102) shouldBe 0xA5 // IN reflects OUT
  }

  "input pins reflect host when DDR=0" in {
    val (gpio, _) = mkGPIO()
    gpio.writeByte(0x100, 0x00) // all input
    gpio.setInput(0x3C)
    gpio.readByte(0x102) shouldBe 0x3C
  }

  "mixed direction: output bits from OUT, input bits from host" in {
    val (gpio, _) = mkGPIO()
    gpio.writeByte(0x100, 0x0F) // low nibble = output, high nibble = input
    gpio.writeByte(0x101, 0xAB) // OUT = 0xAB, but only low nibble visible as output
    gpio.setInput(0xC0)         // host drives high nibble
    gpio.readByte(0x102) shouldBe 0xCB // 0xC0 & 0xF0 | 0x0B & 0x0F
  }

  // ===== Width masking =====

  "4-bit GPIO masks writes" in {
    val (gpio, _) = mkGPIO(4)
    gpio.writeByte(0x100, 0xFF) // DDR — only low 4 bits matter
    gpio.writeByte(0x101, 0xFF) // OUT — only low 4 bits matter
    gpio.readByte(0x102) shouldBe 0x0F
  }

  "4-bit GPIO masks input" in {
    val (gpio, _) = mkGPIO(4)
    gpio.setInput(0xFF)
    gpio.readByte(0x102) shouldBe 0x0F
  }

  // ===== Atomic set/clear/XOR =====

  "SET sets bits in OUT" in {
    val (gpio, _) = mkGPIO()
    gpio.writeByte(0x100, 0xFF) // all output
    gpio.writeByte(0x101, 0x0F) // OUT = 0x0F
    gpio.writeByte(0x103, 0x30) // SET 0x30
    gpio.readByte(0x102) shouldBe 0x3F
  }

  "CLEAR clears bits in OUT" in {
    val (gpio, _) = mkGPIO()
    gpio.writeByte(0x100, 0xFF) // all output
    gpio.writeByte(0x101, 0xFF) // OUT = 0xFF
    gpio.writeByte(0x104, 0x0F) // CLEAR low nibble
    gpio.readByte(0x102) shouldBe 0xF0
  }

  "XOR toggles bits in OUT" in {
    val (gpio, _) = mkGPIO()
    gpio.writeByte(0x100, 0xFF) // all output
    gpio.writeByte(0x101, 0xAA) // OUT = 0xAA
    gpio.writeByte(0x105, 0xFF) // XOR all bits
    gpio.readByte(0x102) shouldBe 0x55
  }

  // ===== onChange callback =====

  "onChange fires on OUT write" in {
    var captured = -1
    val intc = new InterruptController(0x200)
    val gpio = new GPIO(0x100, 8, intc, irq = 1, onChange = v => captured = v)
    gpio.writeByte(0x100, 0xFF) // all output
    gpio.writeByte(0x101, 0x42)
    captured shouldBe 0x42
  }

  "onChange fires on SET" in {
    var captured = -1
    val intc = new InterruptController(0x200)
    val gpio = new GPIO(0x100, 8, intc, irq = 1, onChange = v => captured = v)
    gpio.writeByte(0x100, 0xFF)
    gpio.writeByte(0x101, 0x01)
    gpio.writeByte(0x103, 0x80) // SET bit 7
    captured shouldBe 0x81
  }

  "onChange fires on CLEAR" in {
    var captured = -1
    val intc = new InterruptController(0x200)
    val gpio = new GPIO(0x100, 8, intc, irq = 1, onChange = v => captured = v)
    gpio.writeByte(0x100, 0xFF)
    gpio.writeByte(0x101, 0xFF)
    gpio.writeByte(0x104, 0x01) // CLEAR bit 0
    captured shouldBe 0xFE
  }

  "onChange fires on XOR" in {
    var captured = -1
    val intc = new InterruptController(0x200)
    val gpio = new GPIO(0x100, 8, intc, irq = 1, onChange = v => captured = v)
    gpio.writeByte(0x100, 0xFF)
    gpio.writeByte(0x101, 0x00)
    gpio.writeByte(0x105, 0x0F) // XOR low nibble
    captured shouldBe 0x0F
  }

  // ===== Edge interrupts =====

  "rising edge interrupt fires on low-to-high transition" in {
    val (gpio, intc) = mkGPIO()
    gpio.writeByte(0x106, 0x01) // INT_MASK: pin 0
    gpio.writeByte(0x108, 0x01) // INT_MODE: edge
    gpio.writeByte(0x109, 0x01) // INT_POL: rising

    gpio.setInput(0x00)
    gpio.apply(null) // establish baseline
    gpio.setInput(0x01)
    gpio.apply(null) // should detect rising edge
    gpio.readByte(0x107) shouldBe 0x01 // INT_STATUS
  }

  "falling edge interrupt fires on high-to-low transition" in {
    val (gpio, intc) = mkGPIO()
    gpio.writeByte(0x106, 0x01) // INT_MASK: pin 0
    gpio.writeByte(0x108, 0x01) // INT_MODE: edge
    gpio.writeByte(0x109, 0x00) // INT_POL: falling

    gpio.setInput(0x01)
    gpio.apply(null) // establish baseline
    gpio.setInput(0x00)
    gpio.apply(null) // should detect falling edge
    gpio.readByte(0x107) shouldBe 0x01
  }

  "rising edge does not fire on high-to-low" in {
    val (gpio, intc) = mkGPIO()
    gpio.writeByte(0x106, 0x01) // INT_MASK: pin 0
    gpio.writeByte(0x108, 0x01) // INT_MODE: edge
    gpio.writeByte(0x109, 0x01) // INT_POL: rising

    gpio.setInput(0x01)
    gpio.apply(null)            // rising edge fires here (0→1)
    gpio.writeByte(0x107, 0x01) // clear status
    gpio.setInput(0x00)         // falling — should not fire
    gpio.apply(null)
    gpio.readByte(0x107) shouldBe 0x00
  }

  "edge interrupt does not re-fire while pin stays high" in {
    val (gpio, intc) = mkGPIO()
    gpio.writeByte(0x106, 0x01) // INT_MASK: pin 0
    gpio.writeByte(0x108, 0x01) // INT_MODE: edge
    gpio.writeByte(0x109, 0x01) // INT_POL: rising

    gpio.setInput(0x00)
    gpio.apply(null)
    gpio.setInput(0x01)
    gpio.apply(null) // fires
    gpio.writeByte(0x107, 0x01) // clear status
    gpio.apply(null) // pin still high — should not re-fire
    gpio.readByte(0x107) shouldBe 0x00
  }

  // ===== Level interrupts =====

  "level-high interrupt fires while pin is high" in {
    val (gpio, intc) = mkGPIO()
    gpio.writeByte(0x106, 0x01) // INT_MASK: pin 0
    gpio.writeByte(0x108, 0x00) // INT_MODE: level
    gpio.writeByte(0x109, 0x01) // INT_POL: high

    gpio.setInput(0x01)
    gpio.apply(null)
    gpio.readByte(0x107) shouldBe 0x01
  }

  "level-high interrupt does not fire while pin is low" in {
    val (gpio, intc) = mkGPIO()
    gpio.writeByte(0x106, 0x01) // INT_MASK: pin 0
    gpio.writeByte(0x108, 0x00) // INT_MODE: level
    gpio.writeByte(0x109, 0x01) // INT_POL: high

    gpio.setInput(0x00)
    gpio.apply(null)
    gpio.readByte(0x107) shouldBe 0x00
  }

  "level-low interrupt fires while pin is low" in {
    val (gpio, intc) = mkGPIO()
    gpio.writeByte(0x106, 0x01) // INT_MASK: pin 0
    gpio.writeByte(0x108, 0x00) // INT_MODE: level
    gpio.writeByte(0x109, 0x00) // INT_POL: low

    gpio.setInput(0x00)
    gpio.apply(null)
    gpio.readByte(0x107) shouldBe 0x01
  }

  "level interrupt re-fires after clear while condition holds" in {
    val (gpio, intc) = mkGPIO()
    gpio.writeByte(0x106, 0x01) // INT_MASK: pin 0
    gpio.writeByte(0x108, 0x00) // INT_MODE: level
    gpio.writeByte(0x109, 0x01) // INT_POL: high

    gpio.setInput(0x01)
    gpio.apply(null)
    gpio.readByte(0x107) shouldBe 0x01
    gpio.writeByte(0x107, 0x01) // clear status
    gpio.apply(null) // should re-fire because pin is still high
    gpio.readByte(0x107) shouldBe 0x01
  }

  // ===== INT_MASK =====

  "masked pin does not generate interrupt" in {
    val (gpio, intc) = mkGPIO()
    gpio.writeByte(0x106, 0x00) // INT_MASK: nothing enabled
    gpio.writeByte(0x108, 0x01) // INT_MODE: edge
    gpio.writeByte(0x109, 0x01) // INT_POL: rising

    gpio.setInput(0x00)
    gpio.apply(null)
    gpio.setInput(0x01) // transition — but masked
    gpio.apply(null)
    gpio.readByte(0x107) shouldBe 0x00
  }

  // ===== INTC integration =====

  "GPIO raises INTC line on interrupt" in {
    val (gpio, intc) = mkGPIO()
    gpio.writeByte(0x106, 0x01) // INT_MASK: pin 0
    gpio.writeByte(0x108, 0x00) // INT_MODE: level
    gpio.writeByte(0x109, 0x01) // INT_POL: high

    gpio.setInput(0x01)
    gpio.apply(null)
    // INTC pending register should have IRQ 1 set
    intc.readByte(0x200) shouldBe 0x02 // bit 1
  }

  // ===== Write-1-to-clear =====

  "INT_STATUS write-1-to-clear only clears written bits" in {
    val (gpio, intc) = mkGPIO()
    gpio.writeByte(0x106, 0x03) // INT_MASK: pins 0,1
    gpio.writeByte(0x108, 0x03) // INT_MODE: edge
    gpio.writeByte(0x109, 0x03) // INT_POL: rising

    gpio.setInput(0x00)
    gpio.apply(null)
    gpio.setInput(0x03) // both rise
    gpio.apply(null)
    gpio.readByte(0x107) shouldBe 0x03
    gpio.writeByte(0x107, 0x01) // clear only pin 0
    gpio.readByte(0x107) shouldBe 0x02 // pin 1 still pending
  }

  // ===== Multi-pin edge detection =====

  "multiple pins can trigger independently" in {
    val (gpio, intc) = mkGPIO()
    gpio.writeByte(0x106, 0x05) // INT_MASK: pins 0,2
    gpio.writeByte(0x108, 0x05) // INT_MODE: edge
    gpio.writeByte(0x109, 0x05) // INT_POL: rising

    gpio.setInput(0x00)
    gpio.apply(null)
    gpio.setInput(0x01) // only pin 0 rises
    gpio.apply(null)
    gpio.readByte(0x107) shouldBe 0x01

    gpio.writeByte(0x107, 0x01) // clear pin 0
    gpio.setInput(0x05) // pin 2 rises
    gpio.apply(null)
    gpio.readByte(0x107) shouldBe 0x04
  }
}
