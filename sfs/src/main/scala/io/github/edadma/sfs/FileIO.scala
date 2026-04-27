package io.github.edadma.sfs

import Constants.*

/** Byte-level file I/O on top of [[ExtentReader]] (reads) and
  * [[ExtentAllocator]] (writes).
  *
  * The unit of I/O at the API surface is bytes; the underlying extent
  * map operates in 4 KiB blocks. This module bridges the two: byte
  * offsets translate to (logical block, byte-in-block) pairs; partial
  * first/last blocks are handled with read-modify-write; sparse holes
  * are zeroed on read; writes past EOF grow the file (with an implicit
  * sparse hole if `offset > size`); writes that cover an existing
  * sparse block convert it to concrete by splitting the surrounding
  * sparse extent.
  *
  * Time fields (`mtimeSec/Nsec`, `ctimeSec/Nsec`) are bumped on writes
  * and on `truncateFile`. `atime` is left to the caller; the read path
  * doesn't mutate any state.
  *
  * Like [[ExtentAllocator]], this module is single-threaded. Journaling
  * happens in Phase 13 around the same primitives. */
object FileIO:

  // ---- read -----------------------------------------------------------

  /** Read up to `len` bytes from the file starting at byte `offset`.
    * Reads that overrun EOF are clamped: the returned array is shorter
    * than `len`. Sparse / uninitialized blocks read as zeros. */
  def readFile(
      ino: Inode,
      dev: BlockDevice,
      offset: Long,
      len: Int,
  ): Array[Byte] =
    require(offset >= 0L, s"offset must be non-negative, got $offset")
    require(len >= 0, s"len must be non-negative, got $len")
    if len == 0 || offset >= ino.size then return new Array[Byte](0)
    val effective = math.min(len.toLong, ino.size - offset).toInt
    val out = new Array[Byte](effective)
    val reader = new ExtentReader(dev, ino)
    val firstBlock = offset / BlockSize
    val lastBlock = (offset + effective - 1) / BlockSize
    val blockBuf = new Array[Byte](BlockSize)
    var b = firstBlock
    while b <= lastBlock do
      val blockBase = b * BlockSize
      val readStart = math.max(blockBase, offset)
      val readEnd = math.min(blockBase + BlockSize, offset + effective)
      val byteCount = (readEnd - readStart).toInt
      val srcInBlock = (readStart - blockBase).toInt
      val dstInOut = (readStart - offset).toInt
      reader.physicalBlock(b) match
        case BlockMapping.Concrete(p) =>
          dev.readBlock(p, blockBuf)
          System.arraycopy(blockBuf, srcInBlock, out, dstInOut, byteCount)
        case BlockMapping.Sparse | BlockMapping.Uninitialized =>
        // out is already zero-initialized; nothing to copy
        case BlockMapping.OutOfRange =>
          throw new SfsCorruptError(
            s"readFile: extent map shorter than ino.size at logical block $b",
          )
      b += 1
    out

  // ---- write ----------------------------------------------------------

  /** Write `bytes` into the file at byte `offset`. Returns the updated
    * inode with `size`, `blockCount`, `mtimeSec/Nsec`, and
    * `ctimeSec/Nsec` refreshed. If `offset > ino.size`, the gap is
    * filled with a sparse hole. Existing concrete blocks in the
    * affected range are read-modify-written. Existing sparse blocks
    * are converted to concrete (allocating fresh physical blocks and
    * splitting the surrounding sparse extent into sparse/concrete/sparse).
    * Existing uninitialized extents are not supported in Phase 8. */
  def writeFile(
      ino: Inode,
      dev: BlockDevice,
      bm: Bitmap,
      offset: Long,
      bytes: Array[Byte],
      timeSec: Int,
      timeNsec: Int,
  ): Inode =
    require(offset >= 0L, s"offset must be non-negative, got $offset")
    if bytes.length == 0 then return ino
    val totalLen = bytes.length
    val endByte = offset + totalLen
    val firstBlock = offset / BlockSize
    val lastBlock = (endByte - 1) / BlockSize

    var cur = ino
    var curBlocks = ExtentAllocator.totalBlockCount(cur, dev)

    // 1) Fill any gap between current extent map and the write start
    //    with a sparse hole. After this, every block in [0, firstBlock)
    //    is at least represented in the extent map.
    if firstBlock > curBlocks then
      val gap = firstBlock - curBlocks
      cur = ExtentAllocator.appendSparse(cur, dev, bm, toIntChecked(gap))
      curBlocks = firstBlock

    // 2) Write each affected block. Re-read-modify-write existing
    //    concrete blocks; convert sparse blocks via a split-and-replace
    //    of the surrounding extent; allocate fresh physical blocks for
    //    blocks past the current extent map end.
    val blockBuf = new Array[Byte](BlockSize)
    var b = firstBlock
    while b <= lastBlock do
      val blockBase = b * BlockSize
      val byteStart = (math.max(blockBase, offset) - blockBase).toInt
      val byteEnd = (math.min(blockBase + BlockSize, endByte) - blockBase).toInt
      val byteCount = byteEnd - byteStart
      val srcOff = (math.max(blockBase, offset) - offset).toInt

      if b < curBlocks then
        new ExtentReader(dev, cur).physicalBlock(b) match
          case BlockMapping.Concrete(phys) =>
            dev.readBlock(phys, blockBuf)
            System.arraycopy(bytes, srcOff, blockBuf, byteStart, byteCount)
            dev.writeBlock(phys, blockBuf)
          case BlockMapping.Sparse =>
            val newPhys = bm.allocate().getOrElse {
              throw new SfsCorruptError("writeFile: out of free blocks")
            }
            val rebuilt = splitSparseAt(
              ExtentAllocator.listExtents(cur, dev),
              b,
              newPhys,
            )
            cur = ExtentAllocator.replaceAllExtents(cur, dev, bm, rebuilt)
            // New block was just allocated; its on-disk content is
            // whatever residual stale bytes were there. Zero into the
            // local buffer so the unwritten parts read as zero.
            zeroBuffer(blockBuf)
            System.arraycopy(bytes, srcOff, blockBuf, byteStart, byteCount)
            dev.writeBlock(newPhys.toLong, blockBuf)
          case BlockMapping.Uninitialized =>
            throw new SfsCorruptError(
              "writeFile: uninitialized extents are not supported in Phase 8",
            )
          case BlockMapping.OutOfRange =>
            throw new SfsCorruptError(
              s"writeFile: extent map shorter than expected at block $b",
            )
      else
        cur = ExtentAllocator.append(cur, dev, bm, 1)
        curBlocks += 1
        val phys = new ExtentReader(dev, cur).physicalBlock(b) match
          case BlockMapping.Concrete(p) => p
          case other =>
            throw new SfsCorruptError(
              s"writeFile: just-appended block at $b not concrete (was $other)",
            )
        zeroBuffer(blockBuf)
        System.arraycopy(bytes, srcOff, blockBuf, byteStart, byteCount)
        dev.writeBlock(phys, blockBuf)
      b += 1

    // 3) Update size, blockCount, mtime, ctime.
    val newSize = math.max(cur.size, endByte)
    finishWrite(cur, dev, newSize, timeSec, timeNsec)

  // ---- truncate -------------------------------------------------------

  /** Truncate the file to exactly `newSize` bytes. Shrinking frees the
    * physical blocks past `newSize` and zeroes any tail bytes inside
    * the last partial block (so a future read of those bytes returns
    * 0 even if the underlying extent's start address survives).
    * Extending adds a sparse hole. Updates mtime/ctime. */
  def truncateFile(
      ino: Inode,
      dev: BlockDevice,
      bm: Bitmap,
      newSize: Long,
      timeSec: Int,
      timeNsec: Int,
  ): Inode =
    require(newSize >= 0L, s"newSize must be non-negative, got $newSize")
    if newSize == ino.size then return ino

    var cur = ino
    if newSize < ino.size then
      val newBlocks = ceilDiv(newSize, BlockSize.toLong)
      cur = ExtentAllocator.truncate(cur, dev, bm, newBlocks)
      // Zero any bytes inside the last partial block beyond newSize so
      // a subsequent read of [newSize, lastBlock_end) returns zeros even
      // if the extent's physical address still has stale bytes.
      if newSize > 0L && (newSize % BlockSize) != 0L then
        val lastBlock = (newSize - 1) / BlockSize
        val byteStart = (newSize - lastBlock * BlockSize).toInt
        new ExtentReader(dev, cur).physicalBlock(lastBlock) match
          case BlockMapping.Concrete(phys) =>
            val buf = new Array[Byte](BlockSize)
            dev.readBlock(phys, buf)
            var i = byteStart
            while i < BlockSize do
              buf(i) = 0.toByte
              i += 1
            dev.writeBlock(phys, buf)
          case _ => // sparse / uninit / oor: nothing to zero
    else
      val curBlocks = ExtentAllocator.totalBlockCount(cur, dev)
      val newBlocks = ceilDiv(newSize, BlockSize.toLong)
      if newBlocks > curBlocks then
        val gap = newBlocks - curBlocks
        cur = ExtentAllocator.appendSparse(cur, dev, bm, toIntChecked(gap))

    finishWrite(cur, dev, newSize, timeSec, timeNsec)

  // ---- internal helpers -----------------------------------------------

  /** Finalize an inode after a write- or truncate-shaped mutation:
    * recompute `blockCount` (in 512-byte units, capped at Int.MaxValue),
    * set `size`, and stamp mtime/ctime. */
  private def finishWrite(
      cur: Inode,
      dev: BlockDevice,
      newSize: Long,
      timeSec: Int,
      timeNsec: Int,
  ): Inode =
    val physBlocks = ExtentAllocator.physicalBlockCount(cur, dev)
    val blockCount = math.min(physBlocks * 8L, Int.MaxValue.toLong).toInt
    cur.copy(
      size = newSize,
      blockCount = blockCount,
      mtimeSec = timeSec,
      mtimeNsec = timeNsec,
      ctimeSec = timeSec,
      ctimeNsec = timeNsec,
    )

  /** Split the sparse extent that contains logical block `b`, replacing
    * it with up to three extents: an optional sparse prefix, a single
    * concrete extent at `newPhys`, and an optional sparse suffix.
    * Throws if `b` is not inside a sparse extent. */
  private def splitSparseAt(
      xs: Vector[Extent],
      b: Long,
      newPhys: Int,
  ): Vector[Extent] =
    var cum = 0L
    var i = 0
    while i < xs.length do
      val e = xs(i)
      val nextCum = cum + e.count.toLong
      if b >= cum && b < nextCum then
        if !e.sparse then
          throw new IllegalStateException(
            s"splitSparseAt: extent at logical $b is not sparse: $e",
          )
        val offsetInExt = (b - cum).toInt
        val suffixCount = e.count - offsetInExt - 1
        val builder = Vector.newBuilder[Extent]
        var j = 0
        while j < i do
          builder += xs(j)
          j += 1
        if offsetInExt > 0 then
          builder += Extent(0, offsetInExt, sparse = true)
        builder += Extent(start = newPhys, count = 1)
        if suffixCount > 0 then
          builder += Extent(0, suffixCount, sparse = true)
        var k = i + 1
        while k < xs.length do
          builder += xs(k)
          k += 1
        return builder.result()
      cum = nextCum
      i += 1
    throw new IllegalStateException(
      s"splitSparseAt: block $b past extent map (total = $cum)",
    )

  private def zeroBuffer(buf: Array[Byte]): Unit =
    var i = 0
    while i < buf.length do
      buf(i) = 0.toByte
      i += 1

  private def ceilDiv(a: Long, b: Long): Long = (a + b - 1) / b

  private def toIntChecked(n: Long): Int =
    if n < 0L || n > Int.MaxValue.toLong then
      throw new IllegalArgumentException(s"value $n does not fit in Int")
    n.toInt
