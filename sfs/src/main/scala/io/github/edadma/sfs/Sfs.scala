package io.github.edadma.sfs

import Constants.*

/** Top-level filesystem operations. At Phase 4 this is just `format` —
  * mount/unmount lands in Phase 5, file ops in later phases.
  */
object Sfs:

  /** Lay out an SFS volume on the device, writing all metadata regions
    * with the formats Phase 2 codecs produce.
    *
    * Steps (matching SPEC.md):
    *   1. Zero every metadata block (clean slate so trailing fields stay 0).
    *   2. Set bits 0..(dataStart-1) in the block bitmap (one bit per metadata
    *      block).
    *   3. Set bits 0, 1, 2 in the inode bitmap (null, bad-blocks, root).
    *   4. Allocate two consecutive data blocks for the root directory's
    *      root block + initial leaf, then write both.
    *   5. Write inode 1 (the bad-blocks file: regular, size 0).
    *   6. Write inode 2 (the root directory) pointing at the allocated extent.
    *   7. Flush the bitmaps; write the journal superblock.
    *   8. Write the filesystem superblock at block 0 and its backup at block 1
    *      with `fs_state = clean`.
    *
    * Returns the [[Layout]] so callers can immediately mount or inspect it.
    */
  def format(dev: BlockDevice, opts: FormatOptions = FormatOptions()): Layout =
    val total = dev.blockCount
    require(total > 0 && total <= Int.MaxValue, s"device size $total out of range")
    val layout = Layout.compute(total.toInt, opts.totalInodes, opts.journalBlocks)

    zeroMetadata(dev, layout)

    val blockBm = new Bitmap(dev, layout.blockBitmapStart, layout.blockBitmapLen, layout.totalBlocks)
    val inodeBm = new Bitmap(dev, layout.inodeBitmapStart, layout.inodeBitmapLen, layout.totalInodes)

    // Reserve every metadata block in the block bitmap up front; the data
    // region starts clear, ready for `allocate` calls below to carve it up.
    var i = 0
    while i < layout.dataStart do
      blockBm.set(i)
      i += 1

    inodeBm.set(InoNull)
    inodeBm.set(InoBadBlocks)
    inodeBm.set(InoRoot)

    // Two contiguous data blocks for the root dir (root block + initial leaf).
    val rootDirStart = blockBm.allocate().getOrElse(
      throw new IllegalStateException("no data blocks available for root directory"),
    )
    val leafBlock = blockBm.allocate().getOrElse(
      throw new IllegalStateException("no data blocks available for root directory leaf"),
    )
    require(
      leafBlock == rootDirStart + 1,
      s"format expected contiguous root-dir blocks, got $rootDirStart + $leafBlock",
    )

    writeRootDirBlocks(dev, rootDirStart.toLong, leafBlock.toLong)

    writeInode(dev, layout, InoBadBlocks, makeBadBlocksInode(opts.formatTime))
    writeInode(dev, layout, InoRoot, makeRootDirInode(rootDirStart, opts.formatTime))

    blockBm.flush()
    inodeBm.flush()

    writeJournalSuperblock(dev, layout, opts.uuid)

    writeSuperblock(
      dev,
      layout,
      opts,
      freeBlocks = blockBm.freeCount,
      freeInodes = inodeBm.freeCount,
    )

    dev.flush()
    layout

  // ---- metadata writers ------------------------------------------------

  private def zeroMetadata(dev: BlockDevice, layout: Layout): Unit =
    val zeros = new Array[Byte](BlockSize)
    var b = 0L
    val end = layout.dataStart.toLong
    while b < end do
      dev.writeBlock(b, zeros)
      b += 1L

  private def writeRootDirBlocks(dev: BlockDevice, rootAddr: Long, leafAddr: Long): Unit =
    val rootBuf = new Array[Byte](BlockSize)
    val rootBlock = DirRootBlock(
      dot = DirEntry(InoRoot, DirEntry.TypeDirectory, "."),
      dotdot = DirEntry(InoRoot, DirEntry.TypeDirectory, ".."),
      hashVersion = HashFnv1a,
      treeDepth = 0,
      flags = 0,
      // hash 0 covers the whole keyspace at depth 0; the leaf lives at
      // file-logical block 1 (root is file-logical block 0).
      indexEntries = IndexedSeq((0, 1)),
    )
    DirRootBlock.pack(rootBlock, InoRoot, rootBuf)
    dev.writeBlock(rootAddr, rootBuf)

    val leafBuf = new Array[Byte](BlockSize)
    DirLeafBlock.pack(
      Seq(DirEntry.tombstone(DirLeafBlock.UsableSize)),
      InoRoot,
      leafBuf,
    )
    dev.writeBlock(leafAddr, leafBuf)

  private def writeInode(dev: BlockDevice, layout: Layout, inodeNum: Int, ino: Inode): Unit =
    val (blk, off) = layout.inodeLocation(inodeNum)
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(blk, buf)
    Inode.pack(ino, buf, off)
    dev.writeBlock(blk, buf)

  private def writeJournalSuperblock(
      dev: BlockDevice,
      layout: Layout,
      fsUuid: IndexedSeq[Byte],
  ): Unit =
    val sb = JournalSuperblock(
      version = 1,
      // block_count is the number of journal blocks *excluding* the SB itself.
      blockCount = layout.journalLen - 1,
      head = 0,
      tail = 0,
      sequence = 0,
      fsUuid = fsUuid,
    )
    val buf = new Array[Byte](BlockSize)
    JournalSuperblock.pack(sb, buf, 0)
    dev.writeBlock(layout.journalStart.toLong, buf)

  private def writeSuperblock(
      dev: BlockDevice,
      layout: Layout,
      opts: FormatOptions,
      freeBlocks: Int,
      freeInodes: Int,
  ): Unit =
    val sb = Superblock(
      versionMajor = 1,
      versionMinor = 0,
      fsState = FsClean,
      totalBlocks = layout.totalBlocks,
      freeBlocks = freeBlocks,
      totalInodes = layout.totalInodes,
      freeInodes = freeInodes,
      blockBitmapStart = layout.blockBitmapStart,
      blockBitmapLen = layout.blockBitmapLen,
      inodeBitmapStart = layout.inodeBitmapStart,
      inodeBitmapLen = layout.inodeBitmapLen,
      inodeTableStart = layout.inodeTableStart,
      inodeTableLen = layout.inodeTableLen,
      journalStart = layout.journalStart,
      journalLen = layout.journalLen,
      dataStart = layout.dataStart,
      rootInode = InoRoot,
      hashAlgorithm = HashFnv1a,
      formatTime = opts.formatTime,
      lastMountTime = 0L,
      lastWriteTime = opts.formatTime,
      uuid = opts.uuid,
      volumeName = opts.volumeName,
    )
    val buf = new Array[Byte](BlockSize)
    Superblock.pack(sb, buf, 0)
    dev.writeBlock(0L, buf)
    dev.writeBlock(1L, buf) // backup at block 1

  // ---- inode prototypes ------------------------------------------------

  private def emptyExtentsExcept(first: Extent): IndexedSeq[Extent] =
    val xs = new Array[Extent](InlineExtents)
    xs(0) = first
    var i = 1
    while i < InlineExtents do
      xs(i) = Extent.Empty
      i += 1
    xs.toIndexedSeq

  private def makeBadBlocksInode(formatTime: Long): Inode =
    val t = formatTime.toInt
    Inode(
      mode = 0x81a4, // regular file, 0644
      linkCount = 1,
      uid = 0,
      gid = 0,
      flags = 0,
      size = 0L,
      blockCount = 0,
      generation = 1,
      atimeSec = t, atimeNsec = 0,
      mtimeSec = t, mtimeNsec = 0,
      ctimeSec = t, ctimeNsec = 0,
      crtimeSec = t, crtimeNsec = 0,
      body = InodeBody.EmptyExtents,
      indirect1 = 0,
      indirect2 = 0,
      indirect3 = 0,
      xattrBlock = 0,
    )

  private def makeRootDirInode(firstBlockAddr: Int, formatTime: Long): Inode =
    val t = formatTime.toInt
    Inode(
      mode = 0x41ed, // directory, 0755
      linkCount = 2, // `.` and `..` both reference self
      uid = 0,
      gid = 0,
      flags = 0,
      size = 2L * BlockSize,
      blockCount = (2 * BlockSize) / 512, // POSIX 512-byte units
      generation = 1,
      atimeSec = t, atimeNsec = 0,
      mtimeSec = t, mtimeNsec = 0,
      ctimeSec = t, ctimeNsec = 0,
      crtimeSec = t, crtimeNsec = 0,
      body = InodeBody.Extents(emptyExtentsExcept(Extent(start = firstBlockAddr, count = 2))),
      indirect1 = 0,
      indirect2 = 0,
      indirect3 = 0,
      xattrBlock = 0,
    )
