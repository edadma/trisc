package io.github.edadma.sfs

import Constants.*

/** Allocator over a contiguous run of bitmap blocks on a [[BlockDevice]].
  *
  * Used for both the block bitmap (one bit per disk block, ~64 MiB on a
  * full 2 TB volume) and the inode bitmap (one bit per inode, ~128 KiB at
  * the default 1 Mi inodes). The whole region is cached in memory; reads
  * are answered out of the cache, writes flip cache bytes and mark
  * touched blocks dirty, and [[flush]] writes only those dirty blocks
  * back to the device.
  *
  * Bit layout is the standard "byte-LE" convention: bit `b` lives in byte
  * `b >> 3` at bit position `b & 7`, with bit 0 being the LSB. This is
  * the same convention used by ext2/3/4, so on-disk bitmap blocks are
  * directly inspectable with off-the-shelf tools.
  *
  * `freeCount` is maintained incrementally on every set/clear so statfs
  * is O(1). On [[load]] it is recomputed from scratch via popcount.
  */
final class Bitmap(
    val device: BlockDevice,
    val startBlock: Long,
    val lengthBlocks: Int,
    val totalBits: Int,
):
  require(lengthBlocks >= 1, s"lengthBlocks must be ≥ 1, got $lengthBlocks")
  require(totalBits >= 0, s"totalBits must be ≥ 0, got $totalBits")
  require(startBlock >= 0, s"startBlock must be ≥ 0, got $startBlock")
  require(
    totalBits.toLong <= lengthBlocks.toLong * BlockSize * 8L,
    s"totalBits $totalBits exceeds capacity ${lengthBlocks.toLong * BlockSize * 8L}",
  )

  /** In-memory cache: one [[BlockSize]]-byte buffer per bitmap block. */
  private val blocks: Array[Array[Byte]] =
    Array.fill(lengthBlocks)(new Array[Byte](BlockSize))

  /** Per-block dirty flag — only blocks marked dirty are written by flush. */
  private val dirty: Array[Boolean] = new Array[Boolean](lengthBlocks)

  /** Live count of clear bits in [0, totalBits). Maintained incrementally. */
  private var _freeCount: Int = totalBits

  def freeCount: Int = _freeCount

  /** Read every bitmap block from the device into the cache, then recount
    * free bits from scratch. Bits outside [0, totalBits) are ignored. */
  def load(): Unit =
    var i = 0
    while i < lengthBlocks do
      device.readBlock(startBlock + i, blocks(i))
      dirty(i) = false
      i += 1
    _freeCount = totalBits - countSet()

  /** Write every dirty block back to the device. Clean blocks are skipped.
    * Marks all blocks clean afterwards; further writes will re-dirty them. */
  def flush(): Unit =
    var i = 0
    while i < lengthBlocks do
      if dirty(i) then
        device.writeBlock(startBlock + i, blocks(i))
        dirty(i) = false
      i += 1

  /** Stage every dirty bitmap block into the given [[Transaction]] and
    * mark them clean. After this returns, a subsequent `tx.commit()`
    * will write the bitmap blocks through the journal and to their
    * on-disk locations.
    *
    * Used by [[Sfs.withTransaction]] just before commit, so bitmap
    * mutations from any per-op work are atomically journaled with the
    * other metadata in the same transaction. */
  def stageInto(tx: Transaction): Unit =
    var i = 0
    while i < lengthBlocks do
      if dirty(i) then
        tx.writeMetadata(startBlock + i, blocks(i))
        dirty(i) = false
      i += 1

  def isSet(bit: Int): Boolean =
    requireRange(bit)
    val byteOff = bit >>> 3
    val blockIdx = byteOff >>> BlockShift
    val byteInBlock = byteOff & BlockMask
    val mask = 1 << (bit & 7)
    (blocks(blockIdx)(byteInBlock) & mask) != 0

  /** Mark `bit` as allocated. No-op if already set. */
  def set(bit: Int): Unit =
    requireRange(bit)
    val byteOff = bit >>> 3
    val blockIdx = byteOff >>> BlockShift
    val byteInBlock = byteOff & BlockMask
    val mask = (1 << (bit & 7)).toByte
    val cur = blocks(blockIdx)(byteInBlock)
    if (cur & mask) == 0 then
      blocks(blockIdx)(byteInBlock) = (cur | mask).toByte
      dirty(blockIdx) = true
      _freeCount -= 1

  /** Mark `bit` as free. No-op if already clear. */
  def clear(bit: Int): Unit =
    requireRange(bit)
    val byteOff = bit >>> 3
    val blockIdx = byteOff >>> BlockShift
    val byteInBlock = byteOff & BlockMask
    val mask = (1 << (bit & 7)).toByte
    val cur = blocks(blockIdx)(byteInBlock)
    if (cur & mask) != 0 then
      blocks(blockIdx)(byteInBlock) = (cur & ~mask).toByte
      dirty(blockIdx) = true
      _freeCount += 1

  /** Alias for [[clear]] — reads more naturally at the call site. */
  def free(bit: Int): Unit = clear(bit)

  /** Mark `n` consecutive bits starting at `start` as free. */
  def freeRange(start: Int, n: Int): Unit =
    require(n >= 0, s"n must be ≥ 0, got $n")
    require(
      start >= 0 && start.toLong + n <= totalBits,
      s"freeRange [$start, ${start + n}) outside [0, $totalBits)",
    )
    var i = 0
    while i < n do
      clear(start + i)
      i += 1

  /** Find the first clear bit, mark it set, return its index. Returns
    * `None` if every bit is allocated. */
  def allocate(): Option[Int] = allocate(0)

  /** Like [[allocate]] but starts the linear scan at `hint` (locality
    * heuristic) and wraps around to 0 if nothing is found above the hint.
    * `hint` may equal `totalBits` to mean "no hint". */
  def allocate(hint: Int): Option[Int] =
    require(hint >= 0 && hint <= totalBits, s"hint $hint outside [0, $totalBits]")
    if _freeCount == 0 then None
    else
      var bit = scanFirstClear(hint, totalBits)
      if bit < 0 then bit = scanFirstClear(0, hint)
      if bit < 0 then None
      else
        set(bit)
        Some(bit)

  /** Find a contiguous run of `n` clear bits, mark them all set, return
    * the start index. Returns `None` if no such run exists. */
  def allocateRange(n: Int): Option[Int] =
    require(n >= 1, s"n must be ≥ 1, got $n")
    if _freeCount < n then None
    else
      val start = scanContiguousClear(0, totalBits, n)
      if start < 0 then None
      else
        var i = 0
        while i < n do
          set(start + i)
          i += 1
        Some(start)

  // ---- private helpers --------------------------------------------------

  private inline def BlockShift: Int = 12 // BlockSize == 1 << 12 == 4096
  private inline def BlockMask: Int = BlockSize - 1

  private def requireRange(bit: Int): Unit =
    if bit < 0 || bit >= totalBits then
      throw new IndexOutOfBoundsException(s"bit $bit outside [0, $totalBits)")

  private def byteAt(globalByteOff: Int): Int =
    val blk = globalByteOff >>> BlockShift
    val off = globalByteOff & BlockMask
    blocks(blk)(off) & 0xff

  /** Popcount across [0, totalBits), ignoring bits past the end. */
  private def countSet(): Int =
    var n = 0
    val fullBytes = totalBits >>> 3
    val tail = totalBits & 7
    var i = 0
    while i < fullBytes do
      n += java.lang.Integer.bitCount(byteAt(i))
      i += 1
    if tail > 0 then
      val mask = (1 << tail) - 1
      n += java.lang.Integer.bitCount(byteAt(i) & mask)
    n

  /** Return the first clear bit in [from, until), or -1 if none. */
  private def scanFirstClear(from: Int, until: Int): Int =
    if from >= until then return -1
    var bit = from

    // Step over to the next byte boundary one bit at a time.
    while bit < until && (bit & 7) != 0 do
      if !isSet(bit) then return bit
      bit += 1

    // Bulk scan one byte at a time. `byte == 0xff` means all eight bits are
    // allocated; skip the byte. Otherwise locate the first clear bit via
    // numberOfTrailingZeros on the inverted byte.
    while bit + 8 <= until do
      val b = byteAt(bit >>> 3)
      if b != 0xff then
        return bit + java.lang.Integer.numberOfTrailingZeros((~b) & 0xff)
      bit += 8

    // Trailing partial byte at the end of the range.
    while bit < until do
      if !isSet(bit) then return bit
      bit += 1

    -1

  /** Return the start of the first contiguous run of `n` clear bits in
    * [from, until), or -1 if none. */
  private def scanContiguousClear(from: Int, until: Int, n: Int): Int =
    var run = 0
    var runStart = from
    var bit = from
    while bit < until do
      if !isSet(bit) then
        if run == 0 then runStart = bit
        run += 1
        if run == n then return runStart
      else run = 0
      bit += 1
    -1
