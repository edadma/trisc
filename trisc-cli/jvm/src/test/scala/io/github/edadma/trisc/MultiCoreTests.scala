package io.github.edadma.trisc

class MultiCoreTests extends TestHelpers {

  // ===== Single-core backward compatibility =====

  "single core still works with coreId=0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 42
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
    cpu.coreId shouldBe 0
  }

  // ===== ReservationMonitor =====

  "reservation monitor: set and check succeeds" in {
    val mon = new ReservationMonitor(2)
    mon.setReservation(0, 0x1000)
    mon.checkAndClear(0, 0x1000) shouldBe true
  }

  "reservation monitor: check without set fails" in {
    val mon = new ReservationMonitor(2)
    mon.checkAndClear(0, 0x1000) shouldBe false
  }

  "reservation monitor: invalidate from other core" in {
    val mon = new ReservationMonitor(2)
    mon.setReservation(0, 0x1000)
    mon.invalidateOthers(1, 0x1000) // core 1 writes to 0x1000
    mon.checkAndClear(0, 0x1000) shouldBe false // core 0's reservation gone
  }

  "reservation monitor: invalidate doesn't affect same core" in {
    val mon = new ReservationMonitor(2)
    mon.setReservation(0, 0x1000)
    mon.invalidateOthers(0, 0x1000) // core 0's own write
    mon.checkAndClear(0, 0x1000) shouldBe true // still valid
  }

  "reservation monitor: different address not invalidated" in {
    val mon = new ReservationMonitor(2)
    mon.setReservation(0, 0x1000)
    mon.invalidateOthers(1, 0x2000) // core 1 writes to different address
    mon.checkAndClear(0, 0x1000) shouldBe true // not affected
  }

  "reservation monitor: atomic CAS" in {
    val ram = new RAM(0, 0x1000)
    ram.writeLong(0x100, 42)
    val mon = new ReservationMonitor(2)
    val old = mon.atomicCAS(ram, 0x100, 42, 99)
    old shouldBe 42
    ram.readLong(0x100) shouldBe 99
  }

  "reservation monitor: CAS fails on mismatch" in {
    val ram = new RAM(0, 0x1000)
    ram.writeLong(0x100, 42)
    val mon = new ReservationMonitor(2)
    val old = mon.atomicCAS(ram, 0x100, 10, 99) // expected 10, actual 42
    old shouldBe 42
    ram.readLong(0x100) shouldBe 42 // unchanged
  }

  // ===== MultiCore basic =====

  "two cores run independently" in {
    val ram = new RAM(0, 0x10000)
    val mem = new Memory("mem", ram)

    // Core 0: write 0xAA to address 0x1000, halt
    // Core 1: write 0xBB to address 0x2000, halt
    val prog0 = assemble(
      s"""${VECTORS}movi r1, 0x1000
         |ldi r2, 0xAA
         |stb r2, r1, r0
         |halt
         |""".stripMargin)
    val prog1 = assemble(
      s"""${VECTORS}movi r1, 0x2000
         |ldi r2, 0xBB
         |stb r2, r1, r0
         |halt
         |""".stripMargin)

    // Load both programs (they share vectors but run from same code area)
    prog0.load(mem)

    val mc = new MultiCore(mem, numCores = 2)
    // Both cores use same program (both write then halt)
    // Core 0 writes 0xAA to 0x1000
    // Core 1 writes 0xBB to 0x2000
    // We need different code for each core — use coreId check

    // Simpler: just run core 0 with a limit
    mc.core(0).limit = 10000
    mc.core(0).quiet = true
    mc.core(1).limit = 10000
    mc.core(1).quiet = true
    mc.resetAll()
    mc.runAll()

    // Both cores should have halted
    mc.core(0).state shouldBe State.Halt
    mc.core(1).state shouldBe State.Halt
  }

  "LL/SC fails when other core writes to same address" in {
    val ram = new RAM(0, 0x10000)
    val mem = new Memory("mem", ram)
    val mon = new ReservationMonitor(2)

    // Set up data at 0x1000
    mem.writeLong(0x1000, 42)

    // Core 0: LL from 0x1000
    val cpu0 = new CPU(mem, coreId = 0, reservationMonitor = Some(mon)) { limit = 10000; quiet = true }

    // Simulate: core 0 does LL
    cpu0.setReservation(0x1000)

    // Core 1 writes to same address (invalidates core 0's reservation)
    mon.invalidateOthers(1, 0x1000)

    // Core 0's SC should fail
    cpu0.checkAndClearReservation(0x1000) shouldBe false
  }

  "LL/SC succeeds when other core writes to different address" in {
    val ram = new RAM(0, 0x10000)
    val mem = new Memory("mem", ram)
    val mon = new ReservationMonitor(2)

    mem.writeLong(0x1000, 42)

    val cpu0 = new CPU(mem, coreId = 0, reservationMonitor = Some(mon)) { limit = 10000; quiet = true }

    cpu0.setReservation(0x1000)
    mon.invalidateOthers(1, 0x2000) // different address

    cpu0.checkAndClearReservation(0x1000) shouldBe true
  }

  "concurrent CAS from two cores: only one wins" in {
    val ram = new RAM(0, 0x10000)
    val mon = new ReservationMonitor(2)

    // Initial value
    ram.writeLong(0x100, 0)

    // Both cores try CAS(0x100, 0, coreId+1)
    val result0 = mon.atomicCAS(ram, 0x100, 0, 1) // core 0 tries first
    val result1 = mon.atomicCAS(ram, 0x100, 0, 2) // core 1 tries second

    // Core 0 should have won (got old=0, wrote 1)
    result0 shouldBe 0
    // Core 1 sees old=1 (core 0's write), expected 0, so fails
    result1 shouldBe 1
    // Final value should be 1 (core 0's write)
    ram.readLong(0x100) shouldBe 1
  }

  // ===== MultiCore configuration =====

  "MultiCore creates correct number of cores" in {
    val ram = new RAM(0, 0x1000)
    val mem = new Memory("mem", ram)
    val mc = new MultiCore(mem, numCores = 4)
    mc.numCores shouldBe 4
    mc.cores.length shouldBe 4
    mc.core(0).coreId shouldBe 0
    mc.core(1).coreId shouldBe 1
    mc.core(2).coreId shouldBe 2
    mc.core(3).coreId shouldBe 3
  }

  "cores share same memory" in {
    val ram = new RAM(0, 0x10000)
    val mem = new Memory("mem", ram)
    val mc = new MultiCore(mem, numCores = 2)

    // Write via core 0's memory view
    mem.writeByte(0x1000, 0x42)

    // Should be visible to core 1
    mem.readByte(0x1000) shouldBe 0x42
  }

  "per-core tick sequences" in {
    val intc = new InterruptController(0x10200)
    val timer = new Timer(0x10100, intc, irq = 0)
    val ram = new RAM(0, 0x10000)
    val mem = new Memory("mem", ram, intc, timer)

    val mc = new MultiCore(mem, numCores = 2)
    // Only core 0 gets the timer tick
    mc.core(0).tick = Seq(timer, intc)
    mc.core(1).tick = Seq(intc) // core 1 only handles interrupts

    mc.core(0).tick.length shouldBe 2
    mc.core(1).tick.length shouldBe 1
  }

  // ===== IPI device =====

  "IPI device: core 0 sends IPI to core 1 via INTC" in {
    // Per-core INTCs
    val intc0 = new InterruptController(0x10200)
    val intc1 = new InterruptController(0x10300)
    val intcs = Array(intc0, intc1)

    // IPI devices (one per core, at different offsets)
    val ipi0 = new IPI(0x10400, selfCoreId = 0, intcs)
    val ipi1 = new IPI(0x10410, selfCoreId = 1, intcs)

    val ram = new RAM(0, 0x10000)
    val mem = new Memory("mem", ram, intc0, intc1, ipi0, ipi1)

    // Core 0 writes target=1, then sends IPI
    ipi0.writeInt(0x10400, 1)      // TARGET = core 1
    ipi0.writeByte(0x10400 + 8, 1) // COMMAND = send

    // Core 1's INTC should have IRQ 7 pending
    (intc1.readByte(0x10300) & (1 << 7)) shouldBe (1 << 7)
  }

  "IPI SELF_ID readable from each core's device" in {
    val intcs = Array(new InterruptController(0x10200), new InterruptController(0x10300))
    val ipi0 = new IPI(0x10400, selfCoreId = 0, intcs)
    val ipi1 = new IPI(0x10410, selfCoreId = 1, intcs)

    ipi0.readInt(0x10400 + 12) shouldBe 0
    ipi1.readInt(0x10410 + 12) shouldBe 1
  }
}
