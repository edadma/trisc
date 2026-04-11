package io.github.edadma.trisc

/** Fault causes set by the MMU when translation fails. */
enum FaultCause:
  case None, PageNotPresent, PermissionDenied

/**
 * Abstract memory management unit with hardware page table walk.
 *
 * Implementations define TLB caching and page table format.
 * The MMU translates virtual addresses to physical addresses transparently.
 * On success, returns the physical address. On failure, returns a FaultCause
 * and the CPU raises the appropriate exception (DataAccess or InstructionAccess).
 */
trait MMU:
  /** Translate a virtual address. Returns Right(physAddr) or Left(faultCause). */
  def translate(vaddr: Long, access: Access, supervisor: Boolean): Either[FaultCause, Long]

  /** Invalidate a single TLB entry by virtual address. */
  def tlbInvalidate(vaddr: Long): Unit

  /** Invalidate all TLB entries. */
  def tlbInvalidateAll(): Unit

  /** Whether the MMU is currently enabled. */
  def enabled: Boolean

  /** Enable or disable the MMU. */
  def setEnabled(en: Boolean): Unit

  /** Get the page table base register (physical address of L1 table). */
  def ptbr: Long

  /** Set the page table base register. */
  def setPtbr(base: Long): Unit

/**
 * Hardware-walked MMU with TLB, inspired by RISC-V Sv32 and ARMv7.
 *
 * Two-level page table, 4KB pages:
 *   Virtual address (32-bit): [L1 index (10 bits) | L2 index (10 bits) | offset (12 bits)]
 *
 * Page Table Entry (32-bit, stored as 4 bytes big-endian in memory):
 *   [PPN (20 bits) | reserved (2 bits) | flags (10 bits)]
 *
 * PTE flags (low 10 bits):
 *   bit 0: V (Valid)
 *   bit 1: R (Read)
 *   bit 2: W (Write)
 *   bit 3: X (Execute)
 *   bit 4: U (User — accessible in user mode)
 *   bit 5: G (Global — not flushed by ASID switch)
 *   bit 6: A (Accessed — set by hardware on any access)
 *   bit 7: D (Dirty — set by hardware on write)
 *
 * L1 table: 1024 entries × 4 bytes = 4KB, pointed to by PTBR.
 * Each L1 entry points to an L2 table (PPN field × 4096 = physical address of L2 table).
 * L2 table: 1024 entries × 4 bytes = 4KB.
 * Each L2 entry maps a 4KB page (PPN field × 4096 = physical address of page).
 *
 * L1 entries only need the V flag set (R/W/X/U are in L2).
 * Superpage support: if an L1 entry has R, W, or X set, it maps a 4MB superpage directly.
 *
 * ASID: 8-bit address space identifier. TLB entries are tagged with the ASID
 * that was active when loaded. Entries with G flag match any ASID.
 *
 * The MMU reads physical memory directly (bypassing itself) for page table walks.
 *
 * @param mem Physical memory, used for page table walks
 * @param tlbEntries Number of TLB entries (default 16)
 */
class SimpleMMU(mem: Addressable, tlbEntries: Int = 16) extends MMU:
  require(tlbEntries >= 1 && tlbEntries <= 64, "TLB size must be 1-64")

  private val PAGE_SHIFT = 12
  private val OFFSET_MASK = 0xFFFL
  private val L1_SHIFT = 22
  private val L2_SHIFT = 12
  private val INDEX_MASK = 0x3FF // 10 bits

  // PTE flags
  private val PTE_V = 0x001
  private val PTE_R = 0x002
  private val PTE_W = 0x004
  private val PTE_X = 0x008
  private val PTE_U = 0x010
  private val PTE_G = 0x020
  private val PTE_A = 0x040
  private val PTE_D = 0x080

  // TLB: fully associative
  private case class TLBEntry(var vpn: Long = 0, var ppn: Long = 0, var flags: Int = 0, var asid: Int = 0, var valid: Boolean = false)
  private val tlb = Array.fill(tlbEntries)(TLBEntry())
  private var nextEvict = 0

  private var _enabled: Boolean = false
  private var _ptbr: Long = 0
  private var _asid: Int = 0

  // Fast path: cache the last successful translation to skip TLB scan
  private var _lastVpn: Long = -1L
  private var _lastPpn: Long = 0L
  private var _lastFlags: Int = 0

  // Identity-map optimization: when the kernel page table is active and
  // all accesses are supervisor-mode to identity-mapped superpages,
  // skip translation entirely. Set via setIdentityRange().
  private var _identityPtbr: Long = 0
  private var _identityEnd: Long = 0

  /** Configure identity-map fast path. When ptbr == identityPtbr and
    * vaddr < identityEnd and supervisor mode, return vaddr unchanged. */
  def setIdentityRange(ptbr: Long, end: Long): Unit =
    _identityPtbr = ptbr
    _identityEnd = end

  def enabled: Boolean = _enabled
  def setEnabled(en: Boolean): Unit = _enabled = en
  def ptbr: Long = _ptbr
  def setPtbr(base: Long): Unit = _ptbr = base
  def asid: Int = _asid
  def setAsid(id: Int): Unit =
    val newAsid = id & 0xFF
    if newAsid != _asid then _lastVpn = -1L
    _asid = newAsid

  def translate(vaddr: Long, access: Access, supervisor: Boolean): Either[FaultCause, Long] =
    if !_enabled then return Right(vaddr)

    // Fast path: kernel identity mapping — no translation needed
    if _ptbr == _identityPtbr && supervisor && vaddr < _identityEnd then
      return Right(vaddr)

    val vpn = vaddr >>> PAGE_SHIFT
    val offset = vaddr & OFFSET_MASK

    // Fast path: last-translation cache (covers ~90% of accesses)
    if vpn == _lastVpn then
      val permitted = access match
        case Access.Read    => (_lastFlags & PTE_R) != 0 && (supervisor || (_lastFlags & PTE_U) != 0)
        case Access.Write   => (_lastFlags & PTE_W) != 0 && (supervisor || (_lastFlags & PTE_U) != 0)
        case Access.Execute => (_lastFlags & PTE_X) != 0 && (supervisor || (_lastFlags & PTE_U) != 0)
      if permitted then return Right((_lastPpn << PAGE_SHIFT) | offset)

    // TLB lookup
    var i = 0
    while i < tlbEntries do
      val e = tlb(i)
      if e.valid && e.vpn == vpn && ((e.flags & PTE_G) != 0 || e.asid == _asid) then
        _lastVpn = vpn; _lastPpn = e.ppn; _lastFlags = e.flags
        return checkPermissions(e.ppn, e.flags, offset, access, supervisor, i)
      i += 1

    // TLB miss — hardware page table walk
    walk(vaddr, vpn, offset, access, supervisor)

  private def checkPermissions(ppn: Long, flags: Int, offset: Long, access: Access, supervisor: Boolean, tlbIdx: Int): Either[FaultCause, Long] =
    val permitted = access match
      case Access.Read    => (flags & PTE_R) != 0 && (supervisor || (flags & PTE_U) != 0)
      case Access.Write   => (flags & PTE_W) != 0 && (supervisor || (flags & PTE_U) != 0)
      case Access.Execute => (flags & PTE_X) != 0 && (supervisor || (flags & PTE_U) != 0)

    if !permitted then Left(FaultCause.PermissionDenied)
    else
      // Set A/D bits in TLB (hardware updates these)
      tlb(tlbIdx).flags |= PTE_A
      if access == Access.Write then tlb(tlbIdx).flags |= PTE_D
      Right((ppn << PAGE_SHIFT) | offset)

  /** Read a 32-bit PTE from physical memory (big-endian). */
  private def readPTE(paddr: Long): Long =
    ((mem.readByte(paddr).toLong & 0xFF) << 24) |
    ((mem.readByte(paddr + 1).toLong & 0xFF) << 16) |
    ((mem.readByte(paddr + 2).toLong & 0xFF) << 8) |
    (mem.readByte(paddr + 3).toLong & 0xFF)

  /** Write a 32-bit PTE to physical memory (big-endian), for A/D bit updates. */
  private def writePTE(paddr: Long, pte: Long): Unit =
    mem.writeByte(paddr, (pte >> 24) & 0xFF)
    mem.writeByte(paddr + 1, (pte >> 16) & 0xFF)
    mem.writeByte(paddr + 2, (pte >> 8) & 0xFF)
    mem.writeByte(paddr + 3, pte & 0xFF)

  private def walk(vaddr: Long, vpn: Long, offset: Long, access: Access, supervisor: Boolean): Either[FaultCause, Long] =
    val l1Index = ((vaddr >>> L1_SHIFT) & INDEX_MASK).toInt
    val l2Index = ((vaddr >>> L2_SHIFT) & INDEX_MASK).toInt

    // Read L1 entry
    val l1Addr = _ptbr + l1Index * 4
    val l1PTE = readPTE(l1Addr)
    val l1Flags = (l1PTE & 0x3FF).toInt

    if (l1Flags & PTE_V) == 0 then return Left(FaultCause.PageNotPresent)

    // Superpage: if L1 has R, W, or X, it's a 4MB mapping
    if (l1Flags & (PTE_R | PTE_W | PTE_X)) != 0 then
      val ppn = (l1PTE >>> 10) & 0xFFFFF
      val superOffset = vaddr & 0x3FFFFFL // low 22 bits
      val permitted = access match
        case Access.Read    => (l1Flags & PTE_R) != 0 && (supervisor || (l1Flags & PTE_U) != 0)
        case Access.Write   => (l1Flags & PTE_W) != 0 && (supervisor || (l1Flags & PTE_U) != 0)
        case Access.Execute => (l1Flags & PTE_X) != 0 && (supervisor || (l1Flags & PTE_U) != 0)
      if !permitted then return Left(FaultCause.PermissionDenied)
      // Update A/D in page table
      val newFlags = l1Flags | PTE_A | (if access == Access.Write then PTE_D else 0)
      if newFlags != l1Flags then writePTE(l1Addr, (l1PTE & ~0x3FFL) | newFlags)
      // Cache in last-translation (as 4KB page within the superpage)
      _lastVpn = vpn
      _lastPpn = (ppn << 10) | ((vaddr >>> PAGE_SHIFT) & INDEX_MASK)
      _lastFlags = newFlags
      return Right((ppn << L1_SHIFT) | superOffset)

    // L1 is a pointer to L2 table
    val l2Base = (l1PTE >>> 10) << PAGE_SHIFT
    val l2Addr = l2Base + l2Index * 4
    val l2PTE = readPTE(l2Addr)
    val l2Flags = (l2PTE & 0x3FF).toInt

    if (l2Flags & PTE_V) == 0 then return Left(FaultCause.PageNotPresent)
    if (l2Flags & (PTE_R | PTE_W | PTE_X)) == 0 then return Left(FaultCause.PageNotPresent) // leaf must have permissions

    val ppn = (l2PTE >>> 10) & 0xFFFFF
    val permitted = access match
      case Access.Read    => (l2Flags & PTE_R) != 0 && (supervisor || (l2Flags & PTE_U) != 0)
      case Access.Write   => (l2Flags & PTE_W) != 0 && (supervisor || (l2Flags & PTE_U) != 0)
      case Access.Execute => (l2Flags & PTE_X) != 0 && (supervisor || (l2Flags & PTE_U) != 0)
    if !permitted then return Left(FaultCause.PermissionDenied)

    // Update A/D in page table
    val newFlags = l2Flags | PTE_A | (if access == Access.Write then PTE_D else 0)
    if newFlags != l2Flags then writePTE(l2Addr, (l2PTE & ~0x3FFL) | newFlags)

    // Load into TLB and last-translation cache
    loadTLB(vpn, ppn, newFlags)
    _lastVpn = vpn; _lastPpn = ppn; _lastFlags = newFlags

    Right((ppn << PAGE_SHIFT) | offset)

  private def loadTLB(vpn: Long, ppn: Long, flags: Int): Unit =
    // Check for existing entry to update
    var i = 0
    while i < tlbEntries do
      val e = tlb(i)
      if e.valid && e.vpn == vpn && ((e.flags & PTE_G) != 0 || e.asid == _asid) then
        e.ppn = ppn
        e.flags = flags
        e.asid = _asid
        return
      i += 1
    // Round-robin eviction
    val e = tlb(nextEvict)
    e.vpn = vpn
    e.ppn = ppn
    e.flags = flags
    e.asid = _asid
    e.valid = true
    nextEvict = (nextEvict + 1) % tlbEntries

  def tlbInvalidate(vaddr: Long): Unit =
    val vpn = vaddr >>> PAGE_SHIFT
    var i = 0
    while i < tlbEntries do
      if tlb(i).valid && tlb(i).vpn == vpn then tlb(i).valid = false
      i += 1
    if _lastVpn == vpn then _lastVpn = -1L

  def tlbInvalidateAll(): Unit =
    var i = 0
    while i < tlbEntries do
      tlb(i).valid = false
      i += 1
    _lastVpn = -1L
