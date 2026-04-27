package io.github.edadma.sfs

import Constants.*

/** A mounted SFS volume. Owns the device, the parsed superblock, the
  * computed [[Layout]], and the loaded block + inode bitmaps for the
  * lifetime of the mount.
  *
  * Single-threaded API — calls must be serialized by the caller; the
  * filesystem itself does no locking. The mount changes the on-disk
  * superblock's `fs_state` from `clean` to `dirty`, and [[unmount]]
  * flips it back. A clean `unmount` is the only signal that lets the
  * next mount skip journal recovery.
  *
  * Until the journal lands in Phase 13, [[writeInode]] and bitmap
  * flushes go straight to the device. Their callers' API will not
  * change when the journal is wired in beneath them.
  */
final class Sfs private[sfs] (
    val device: BlockDevice,
    val layout: Layout,
    private var _sb: Superblock,
    val blockBitmap: Bitmap,
    val inodeBitmap: Bitmap,
):
  private var _mounted: Boolean = true

  def superblock: Superblock = _sb
  def isMounted: Boolean = _mounted

  /** Read inode `n` out of the inode table. */
  def readInode(n: Int): Inode =
    requireMounted()
    val (blk, off) = layout.inodeLocation(n)
    val buf = new Array[Byte](BlockSize)
    device.readBlock(blk, buf)
    Inode.unpack(buf, off)

  /** Write inode `n` into the inode table, preserving the other 15
    * inodes in the same 4 KiB block via read-modify-write. */
  def writeInode(n: Int, ino: Inode): Unit =
    requireMounted()
    val (blk, off) = layout.inodeLocation(n)
    val buf = new Array[Byte](BlockSize)
    device.readBlock(blk, buf)
    Inode.pack(ino, buf, off)
    device.writeBlock(blk, buf)

  /** Flush dirty bitmap blocks, mark the volume clean in the on-disk
    * superblock, and refuse further calls on this instance. */
  def unmount(): Unit =
    requireMounted()
    blockBitmap.flush()
    inodeBitmap.flush()
    _sb = _sb.copy(
      fsState = FsClean,
      freeBlocks = blockBitmap.freeCount,
      freeInodes = inodeBitmap.freeCount,
      lastWriteTime = Sfs.now(),
    )
    Sfs.writeSuperblockTo(device, _sb)
    device.flush()
    _mounted = false

  private def requireMounted(): Unit =
    if !_mounted then
      throw new IllegalStateException("filesystem is not mounted")

/** Static filesystem operations. `format` lays out a fresh volume;
  * `mount` opens an existing one for use.
  */
object Sfs:

  // ---- mount -----------------------------------------------------------

  /** Open a formatted volume for use. Reads the superblock (falling back
    * to the backup at block 1 on CRC failure), validates it, sets
    * `fs_state = dirty` on disk, and loads both bitmaps into memory.
    *
    * Until journal recovery lands in Phase 13, mounting a `dirty` volume
    * is rejected outright (the kernel would have to replay the journal
    * before any reads are safe). Mounting an `error` volume is also
    * rejected — those need fsck. */
  def mount(dev: BlockDevice): Sfs =
    val sb0 = readSuperblock(dev)
    sb0.fsState match
      case FsClean => () // ok
      case FsDirty =>
        throw new SfsCorruptError(
          "filesystem is dirty — journal recovery is not yet implemented (Phase 13)",
        )
      case FsError =>
        throw new SfsCorruptError("filesystem is in error state — run fsck")
      case other =>
        throw new SfsCorruptError(s"unknown fs_state $other")

    val layout = Layout.fromSuperblock(sb0)

    val blockBm = new Bitmap(dev, layout.blockBitmapStart, layout.blockBitmapLen, layout.totalBlocks)
    blockBm.load()
    val inodeBm = new Bitmap(dev, layout.inodeBitmapStart, layout.inodeBitmapLen, layout.totalInodes)
    inodeBm.load()

    val mountedSb = sb0.copy(fsState = FsDirty, lastMountTime = now())
    writeSuperblockTo(dev, mountedSb)
    dev.flush()

    new Sfs(dev, layout, mountedSb, blockBm, inodeBm)

  /** Try block 0 first; on CRC/magic failure fall back to the backup at
    * block 1. The backup write is part of every clean unmount, so it is
    * always at least as fresh as the moment of the last clean shutdown. */
  private def readSuperblock(dev: BlockDevice): Superblock =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(0L, buf)
    try Superblock.unpack(buf, 0)
    catch
      case _: SfsCorruptError =>
        dev.readBlock(1L, buf)
        Superblock.unpack(buf, 0)

  // ---- format ----------------------------------------------------------

  def format(dev: BlockDevice, opts: FormatOptions = FormatOptions()): Layout =
    val total = dev.blockCount
    require(total > 0 && total <= Int.MaxValue, s"device size $total out of range")
    val layout = Layout.compute(total.toInt, opts.totalInodes, opts.journalBlocks)

    zeroMetadata(dev, layout)

    val blockBm = new Bitmap(dev, layout.blockBitmapStart, layout.blockBitmapLen, layout.totalBlocks)
    val inodeBm = new Bitmap(dev, layout.inodeBitmapStart, layout.inodeBitmapLen, layout.totalInodes)

    var i = 0
    while i < layout.dataStart do
      blockBm.set(i)
      i += 1

    inodeBm.set(InoNull)
    inodeBm.set(InoBadBlocks)
    inodeBm.set(InoRoot)

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

    writeInodeRaw(dev, layout, InoBadBlocks, makeBadBlocksInode(opts.formatTime))
    writeInodeRaw(dev, layout, InoRoot, makeRootDirInode(rootDirStart, opts.formatTime))

    blockBm.flush()
    inodeBm.flush()

    writeJournalSuperblock(dev, layout, opts.uuid)

    val sb = makeSuperblock(layout, opts, blockBm.freeCount, inodeBm.freeCount)
    writeSuperblockTo(dev, sb)

    dev.flush()
    layout

  // ---- shared writers --------------------------------------------------

  /** Pack a [[Superblock]] and write it to both block 0 and the backup
    * at block 1. The atomic-ish ordering primary-then-backup is
    * intentional: if a power loss interrupts between the two writes,
    * the primary is the freshest version and the backup is at worst
    * stale by one update. */
  private[sfs] def writeSuperblockTo(dev: BlockDevice, sb: Superblock): Unit =
    val buf = new Array[Byte](BlockSize)
    Superblock.pack(sb, buf, 0)
    dev.writeBlock(0L, buf)
    dev.writeBlock(1L, buf)

  // ---- format helpers --------------------------------------------------

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

  private def writeInodeRaw(dev: BlockDevice, layout: Layout, inodeNum: Int, ino: Inode): Unit =
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
      blockCount = layout.journalLen - 1,
      head = 0,
      tail = 0,
      sequence = 0,
      fsUuid = fsUuid,
    )
    val buf = new Array[Byte](BlockSize)
    JournalSuperblock.pack(sb, buf, 0)
    dev.writeBlock(layout.journalStart.toLong, buf)

  private def makeSuperblock(
      layout: Layout,
      opts: FormatOptions,
      freeBlocks: Int,
      freeInodes: Int,
  ): Superblock =
    Superblock(
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
      mode = 0x81a4,
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
      mode = 0x41ed,
      linkCount = 2,
      uid = 0,
      gid = 0,
      flags = 0,
      size = 2L * BlockSize,
      blockCount = (2 * BlockSize) / 512,
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

  // ---- misc ------------------------------------------------------------

  private[sfs] def now(): Long = System.currentTimeMillis() / 1000L
