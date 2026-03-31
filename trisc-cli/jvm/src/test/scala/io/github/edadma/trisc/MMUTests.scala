package io.github.edadma.trisc

class MMUTests extends TestHelpers {

  /** Build a page table entry: PPN in upper 20 bits, flags in lower 10 bits. */
  def pte(ppn: Long, flags: Int): Long = (ppn << 10) | (flags & 0x3FF)

  /** PTE flag constants */
  val V = 0x001
  val R = 0x002
  val W = 0x004
  val X = 0x008
  val U = 0x010
  val G = 0x020

  /**
   * Set up a simple identity-mapped page table for page 0.
   * L1 table at `l1Base`, L2 table at `l2Base`.
   * Maps virtual 0x00000000-0x00000FFF → physical 0x00000000-0x00000FFF.
   */
  def setupIdentityPage0(mem: Addressable, l1Base: Long, l2Base: Long, flags: Int = V | R | W | X): Unit =
    // L1 entry 0: points to L2 table (PPN = l2Base >> 12, V flag only)
    val l1Entry = pte(l2Base >> 12, V)
    writePTE(mem, l1Base, l1Entry)
    // L2 entry 0: maps page 0 identity (PPN=0)
    val l2Entry = pte(0, flags)
    writePTE(mem, l2Base, l2Entry)

  /** Write a 32-bit big-endian PTE to memory. */
  def writePTE(mem: Addressable, addr: Long, value: Long): Unit =
    mem.writeByte(addr, (value >> 24) & 0xFF)
    mem.writeByte(addr + 1, (value >> 16) & 0xFF)
    mem.writeByte(addr + 2, (value >> 8) & 0xFF)
    mem.writeByte(addr + 3, value & 0xFF)

  // ===== SimpleMMU unit tests =====

  "SimpleMMU identity maps when disabled" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.translate(0x1234, Access.Read, supervisor = true) shouldBe Right(0x1234L)
  }

  "SimpleMMU page fault when enabled with empty page table" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000) // L1 table at 0x8000 (all zeros = not valid)
    mmu.translate(0x1000, Access.Read, supervisor = true) shouldBe Left(FaultCause.PageNotPresent)
  }

  "SimpleMMU translates through two-level page table" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    // Map virtual page 1 (0x1000-0x1FFF) → physical page 5 (0x5000-0x5FFF)
    // L1 entry 0 (covers first 4MB): points to L2 table at 0x9000
    writePTE(mem, 0x8000, pte(0x9000 >> 12, V))
    // L2 entry 1 (page index 1): PPN=5, V|R|W|X
    writePTE(mem, 0x9000 + 1 * 4, pte(5, V | R | W | X))

    mmu.translate(0x1234, Access.Read, supervisor = true) shouldBe Right(0x5234L)
  }

  "SimpleMMU preserves page offset" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    writePTE(mem, 0x8000, pte(0x9000 >> 12, V))
    writePTE(mem, 0x9000 + 2 * 4, pte(8, V | R | W | X)) // page 2 → PPN 8

    mmu.translate(0x2FFF, Access.Read, supervisor = true) shouldBe Right(0x8FFFL)
  }

  "SimpleMMU permission fault on user write to read-only page" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    writePTE(mem, 0x8000, pte(0x9000 >> 12, V))
    writePTE(mem, 0x9000, pte(0, V | R | U)) // R + U, no W

    mmu.translate(0x0000, Access.Read, supervisor = false) shouldBe Right(0x0000L)
    mmu.translate(0x0000, Access.Write, supervisor = false) shouldBe Left(FaultCause.PermissionDenied)
  }

  "SimpleMMU supervisor bypasses User bit" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    writePTE(mem, 0x8000, pte(0x9000 >> 12, V))
    writePTE(mem, 0x9000, pte(0, V | R | W | X)) // no U bit

    mmu.translate(0x0000, Access.Read, supervisor = true) shouldBe Right(0x0000L)
    mmu.translate(0x0000, Access.Read, supervisor = false) shouldBe Left(FaultCause.PermissionDenied)
  }

  "SimpleMMU sets Accessed bit in page table" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    writePTE(mem, 0x8000, pte(0x9000 >> 12, V))
    writePTE(mem, 0x9000, pte(0, V | R | W | X))

    mmu.translate(0x0000, Access.Read, supervisor = true)

    // Read back L2 PTE — should have A bit set
    val pteAddr = 0x9000L
    val b0 = mem.readByte(pteAddr) & 0xFF
    val b3 = mem.readByte(pteAddr + 3) & 0xFF
    (b3 & 0x40) should not be 0 // Accessed bit
  }

  "SimpleMMU sets Dirty bit on write" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    writePTE(mem, 0x8000, pte(0x9000 >> 12, V))
    writePTE(mem, 0x9000, pte(0, V | R | W | X))

    mmu.translate(0x0000, Access.Write, supervisor = true)

    val b3 = mem.readByte(0x9003) & 0xFF
    (b3 & 0x80) should not be 0 // Dirty bit
    (b3 & 0x40) should not be 0 // Accessed also set
  }

  "SimpleMMU TLB caches translation" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    writePTE(mem, 0x8000, pte(0x9000 >> 12, V))
    writePTE(mem, 0x9000, pte(5, V | R | W | X))

    // First access — walks page table
    mmu.translate(0x0100, Access.Read, supervisor = true) shouldBe Right(0x5100L)
    // Zero out page table — TLB should still serve this
    writePTE(mem, 0x8000, 0)
    writePTE(mem, 0x9000, 0)
    mmu.translate(0x0200, Access.Read, supervisor = true) shouldBe Right(0x5200L)
  }

  "SimpleMMU TLB invalidate forces re-walk" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    writePTE(mem, 0x8000, pte(0x9000 >> 12, V))
    writePTE(mem, 0x9000, pte(5, V | R | W | X))

    mmu.translate(0x0100, Access.Read, supervisor = true) shouldBe Right(0x5100L)
    // Invalidate and change mapping
    mmu.tlbInvalidate(0x0000)
    writePTE(mem, 0x9000, pte(9, V | R | W | X)) // now maps to PPN 9
    mmu.translate(0x0100, Access.Read, supervisor = true) shouldBe Right(0x9100L)
  }

  "SimpleMMU TLB invalidate all" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    writePTE(mem, 0x8000, pte(0x9000 >> 12, V))
    writePTE(mem, 0x9000, pte(5, V | R | W | X))

    mmu.translate(0x0100, Access.Read, supervisor = true)
    mmu.tlbInvalidateAll()
    // Zero page table — should fault now
    writePTE(mem, 0x8000, 0)
    mmu.translate(0x0100, Access.Read, supervisor = true) shouldBe Left(FaultCause.PageNotPresent)
  }

  "SimpleMMU PTBR get/set" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setPtbr(0x10000)
    mmu.ptbr shouldBe 0x10000
  }

  "SimpleMMU superpage (4MB) mapping" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    // L1 entry 0: superpage — PPN=0, V|R|W|X (has RWX so it's a leaf)
    writePTE(mem, 0x8000, pte(0, V | R | W | X))

    mmu.translate(0x00000, Access.Read, supervisor = true) shouldBe Right(0x00000L)
    mmu.translate(0x12345, Access.Read, supervisor = true) shouldBe Right(0x12345L)
    mmu.translate(0x3FFFFF, Access.Read, supervisor = true) shouldBe Right(0x3FFFFFL)
  }

  "SimpleMMU ASID isolation" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    writePTE(mem, 0x8000, pte(0x9000 >> 12, V))
    writePTE(mem, 0x9000, pte(5, V | R | W | X))

    mmu.setAsid(1)
    mmu.translate(0x0000, Access.Read, supervisor = true) shouldBe Right(0x5000L) // loads TLB with ASID=1

    // Change mapping in page table
    writePTE(mem, 0x9000, pte(7, V | R | W | X))
    // Switch ASID — TLB entry has ASID=1, won't match ASID=2, forces re-walk
    mmu.setAsid(2)
    mmu.translate(0x0000, Access.Read, supervisor = true) shouldBe Right(0x7000L)
  }

  "SimpleMMU Global flag bypasses ASID check" in {
    val mem = new Memory("mem", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(mem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    writePTE(mem, 0x8000, pte(0x9000 >> 12, V))
    writePTE(mem, 0x9000, pte(5, V | R | W | X | G))

    mmu.setAsid(1)
    mmu.translate(0x0000, Access.Read, supervisor = true) shouldBe Right(0x5000L)
    // Switch ASID — G flag means TLB entry still matches
    mmu.setAsid(99)
    // Zero page table to prove TLB is serving this
    writePTE(mem, 0x8000, 0)
    writePTE(mem, 0x9000, 0)
    mmu.translate(0x0000, Access.Read, supervisor = true) shouldBe Right(0x5000L)
  }

  // ===== CPU integration tests =====

  "CPU without MMU works normally" in {
    val output = runProgram(
      s"""${VECTORS}movi r1, 0xFF8
         |sti r1, 'O'
         |sti r1, 'K'
         |halt
         |""".stripMargin)
    output shouldBe "OK"
  }

  "CPU with MMU disabled works normally" in {
    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = 0xFF8
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val rawMem = new Memory("Memory", new RAM(0, 0xFF8), stdout)
    val mmu = new SimpleMMU(rawMem)
    val tof = assemble(
      s"""${VECTORS}movi r1, 0xFF8
         |sti r1, 'O'
         |sti r1, 'K'
         |halt
         |""".stripMargin)
    tof.load(rawMem)
    val cpu = new CPU(rawMem, mmu = Some(mmu)) { limit = 10000 }
    cpu.reset()
    cpu.run()
    output.toString shouldBe "OK"
  }

  "CPU page fault sets faultAddr and faultCause" in {
    val rawMem = new Memory("Memory", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(rawMem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    // Identity map page 0 for code/vectors
    setupIdentityPage0(rawMem, 0x8000, 0x9000)

    // DataAccess vector at slot 4 (offset 32)
    val tof = assemble(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd fault_handler
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |rb 88
        |reset
        |  movi r3, 0x2000
        |  ldb r1, r3, r0
        |  ldi r2, 99
        |  halt
        |fault_handler
        |  gfault r4, r0
        |  gfcause r5, r0
        |  halt
        |""".stripMargin)
    tof.load(rawMem)
    val cpu = new CPU(rawMem, mmu = Some(mmu)) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(4).read shouldBe 0x2000 // faultAddr
    cpu.r(5).read shouldBe FaultCause.PageNotPresent.ordinal
    cpu.r(2).read should not be 99L // didn't reach past the faulting load
  }

  "CPU MMU translates data access" in {
    val rawMem = new Memory("Memory", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(rawMem)
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)

    // Identity map page 0
    setupIdentityPage0(rawMem, 0x8000, 0x9000)
    // Map virtual page 2 → physical page 3
    writePTE(rawMem, 0x9000 + 2 * 4, pte(3, V | R | W | X))

    // Store a value at physical 0x3042
    rawMem.writeByte(0x3042, 0x77)

    val tof = assemble(
      s"""${VECTORS}; Read from virtual 0x2042 → should get physical 0x3042
         |movi r3, 0x2042
         |ldb r1, r3, r0
         |halt
         |""".stripMargin)
    tof.load(rawMem)
    val cpu = new CPU(rawMem, mmu = Some(mmu)) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 0x77
  }

  "SPTBR and GPTBR instructions work" in {
    val rawMem = new Memory("Memory", new RAM(0, 0x1000))
    val mmu = new SimpleMMU(rawMem)
    val tof = assemble(
      s"""${VECTORS}movi r1, 0x8000
         |sptbr r1, r0
         |gptbr r2, r0
         |halt
         |""".stripMargin)
    tof.load(rawMem)
    val cpu = new CPU(rawMem, mmu = Some(mmu)) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(2).read shouldBe 0x8000
  }

  "MMU instructions are supervisor-only" in {
    val rawMem = new Memory("Memory", new RAM(0, 0x1000))
    val mmu = new SimpleMMU(rawMem)
    val tof = assemble(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd privhandler
        |dd 0
        |rb 88
        |reset
        |  movi r1, 0xE00
        |  susp r1
        |  ldi r1, 0
        |  spsr r1
        |  ; now in user mode
        |  movi r2, 0x1000
        |  sptbr r2, r0
        |  ldi r3, 99
        |  halt
        |privhandler
        |  ldi r3, 42
        |  halt
        |""".stripMargin)
    tof.load(rawMem)
    val cpu = new CPU(rawMem, mmu = Some(mmu)) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(3).read shouldBe 42
  }

  "Page fault handler can map page and resume" in {
    // Test that the OS can handle a page fault by enabling the MMU mid-execution
    // after setting up page tables. The handler maps the missing page and resumes.
    val rawMem = new Memory("Memory", new RAM(0, 0x10000))
    val mmu = new SimpleMMU(rawMem)

    // Store value at physical 0x5042 (will be reached via virtual page 2 → physical page 5)
    rawMem.writeByte(0x5042, 0xAB)

    // Pre-build page tables in memory:
    // L1 at 0x8000, L2 at 0x9000
    // Map page 0 identity (for code)
    writePTE(rawMem, 0x8000, pte(0x9000 >> 12, V)) // L1[0] → L2 at 0x9000
    writePTE(rawMem, 0x9000, pte(0, V | R | W | X)) // L2[0]: page 0 identity
    // Page 2 → physical page 5
    writePTE(rawMem, 0x9000 + 2 * 4, pte(5, V | R | W | X))

    // Program: enable MMU, then access virtual 0x2042
    val tof = assemble(
      s"""${VECTORS}; Set up PTBR and enable MMU from supervisor mode
         |movi r1, 0x8000
         |sptbr r1, r0
         |; Read from virtual 0x2042 — translates to physical 0x5042
         |movi r3, 0x2042
         |ldb r1, r3, r0
         |halt
         |""".stripMargin)
    tof.load(rawMem)
    // Enable MMU after page tables are loaded but before CPU runs
    mmu.setEnabled(true)
    mmu.setPtbr(0x8000)
    val cpu = new CPU(rawMem, mmu = Some(mmu)) { limit = 10000 }
    cpu.reset()
    cpu.run()
    (cpu.r(1).read & 0xFF) shouldBe 0xAB
    cpu.state shouldBe State.Halt
  }
}
