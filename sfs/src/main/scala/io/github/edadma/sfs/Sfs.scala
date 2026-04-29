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
  * Metadata writes flow through [[writeMetadataBlock]]. When the call
  * happens inside a [[withTransaction]] body the block is staged in
  * the active [[Transaction]]; outside one, it lands on the device
  * directly. The latter path is used during format, mount/unmount, and
  * direct test scaffolding — code under any user-visible operation
  * always runs inside a transaction.
  */
final class Sfs private[sfs] (
    val device: BlockDevice,
    val layout: Layout,
    private var _sb: Superblock,
    val blockBitmap: Bitmap,
    val inodeBitmap: Bitmap,
    val journal: Journal,
):
  private var _mounted: Boolean = true
  private var _currentTxn: Transaction | Null = null

  def superblock: Superblock = _sb
  def isMounted: Boolean = _mounted

  /** The transaction currently open on this Sfs, if any. Set by
    * [[withTransaction]] for the duration of the body. */
  def currentTxn: Transaction | Null = _currentTxn

  /** Begin a new journal transaction. The returned [[Transaction]] is
    * single-use — call `commit` or `abort` exactly once. Most callers
    * should use [[withTransaction]] instead. */
  def beginTxn(): Transaction =
    requireMounted()
    new Transaction(this)

  /** Run `body` inside a journal transaction. On normal return, the
    * dirty bitmap blocks are staged into the transaction and the txn
    * is committed; on a thrown exception the txn is aborted and the
    * exception propagates.
    *
    * Re-entrant: if `withTransaction` is called inside another
    * `withTransaction`, the inner call simply runs `body` against the
    * outer transaction without opening a nested one. This lets
    * higher-level public ops compose lower-level public ops (e.g.
    * `rename` calling `FileOps.unlink` to overwrite a non-directory
    * target) without each one starting its own commit. */
  def withTransaction[A](body: => A): A =
    requireMounted()
    if _currentTxn != null then body
    else
      val tx = new Transaction(this)
      _currentTxn = tx
      var committed = false
      try
        val r = body
        blockBitmap.stageInto(tx)
        inodeBitmap.stageInto(tx)
        tx.commit()
        committed = true
        r
      finally
        if !committed && tx.isOpen then tx.abort()
        _currentTxn = null

  /** Write a 4 KiB metadata block. If a transaction is open on this
    * Sfs (via [[withTransaction]]), the block is staged in the txn
    * and lands on disk through the journal commit + replay. Otherwise
    * the block is written straight to the device — used by `format`,
    * `mount`, `unmount`, and a handful of direct paths that operate
    * outside a transaction.
    *
    * Use [[BlockDevice.writeBlock]] directly only for *non-metadata*
    * writes (file data blocks under data=ordered) and for the
    * journal's own log + superblock writes. */
  private[sfs] def writeMetadataBlock(blockNum: Long, buf: Array[Byte]): Unit =
    if _currentTxn != null then _currentTxn.nn.writeMetadata(blockNum, buf)
    else device.writeBlock(blockNum, buf)

  /** Read a 4 KiB metadata block into `buf`. If a transaction is open
    * and has a staged copy of `blockNum`, that staged copy is returned
    * (read-your-writes within a txn). Otherwise the block is read off
    * disk via [[BlockDevice.readBlock]].
    *
    * Necessary because [[writeMetadataBlock]] only stages — it does
    * NOT touch the device until commit. Without this, code that
    * delete-then-insert into the same dir block would see the OLD
    * disk state on the second read, miss the just-staged change,
    * and silently corrupt the directory. */
  private[sfs] def readMetadataBlock(blockNum: Long, buf: Array[Byte]): Unit =
    val txn = _currentTxn
    if txn != null then
      txn.nn.peek(blockNum) match
        case Some(staged) => System.arraycopy(staged, 0, buf, 0, BlockSize)
        case None         => device.readBlock(blockNum, buf)
    else device.readBlock(blockNum, buf)

  /** A [[BlockDevice]] view of this Sfs that routes metadata reads
    * and writes through [[readMetadataBlock]] / [[writeMetadataBlock]].
    *
    * Used wherever code reads metadata that the active txn might have
    * staged (extent indirect blocks, directory blocks, inode table
    * blocks). Code that touches *non-metadata* (the journal log,
    * file data blocks under data=ordered) keeps using the raw
    * [[device]] field. */
  val metaDevice: BlockDevice = new BlockDevice:
    val blockCount: Long = Sfs.this.device.blockCount
    def readBlock(blockNum: Long, buf: Array[Byte]): Unit =
      Sfs.this.readMetadataBlock(blockNum, buf)
    def writeBlock(blockNum: Long, buf: Array[Byte]): Unit =
      Sfs.this.writeMetadataBlock(blockNum, buf)
    override def flush(): Unit = Sfs.this.device.flush()

  /** Read inode `n` out of the inode table. Goes through
    * [[readMetadataBlock]] so it sees any updates the active txn has
    * staged for that table block. */
  def readInode(n: Int): Inode =
    requireMounted()
    val (blk, off) = layout.inodeLocation(n)
    val buf = new Array[Byte](BlockSize)
    readMetadataBlock(blk, buf)
    Inode.unpack(buf, off)

  /** Write inode `n` into the inode table, preserving the other 15
    * inodes in the same 4 KiB block via read-modify-write. Routes
    * through [[writeMetadataBlock]] so it is journaled when called
    * inside [[withTransaction]]; uses [[readMetadataBlock]] for the
    * RMW base so other inode updates already staged in this txn
    * (which share the same 4 KiB table block) are not clobbered. */
  def writeInode(n: Int, ino: Inode): Unit =
    requireMounted()
    val (blk, off) = layout.inodeLocation(n)
    val buf = new Array[Byte](BlockSize)
    readMetadataBlock(blk, buf)
    Inode.pack(ino, buf, off)
    writeMetadataBlock(blk, buf)

  /** The current volume label. Convenience getter — same value as
    * `superblock.volumeName`. */
  def volumeName: String = _sb.volumeName

  /** The 16-byte volume UUID set at format time. Convenience getter —
    * same value as `superblock.uuid`. */
  def uuid: IndexedSeq[Byte] = _sb.uuid

  /** Rewrite the on-disk superblock with a new volume label. Goes
    * through a journal transaction so a crash mid-relabel is replayed
    * atomically on next mount. The new label must fit in the
    * [[Superblock.VolumeNameMax]]-byte usable region. */
  def relabel(newName: String): Unit =
    requireMounted()
    require(
      newName.getBytes(java.nio.charset.StandardCharsets.UTF_8).length <= Superblock.VolumeNameMax,
      s"newName too long for ${Superblock.VolumeNameMax}-byte usable region",
    )
    withTransaction {
      val updated = _sb.copy(volumeName = newName, lastWriteTime = Sfs.now())
      val buf = new Array[Byte](BlockSize)
      Superblock.pack(updated, buf, 0)
      writeMetadataBlock(0L, buf)
      writeMetadataBlock(1L, buf)
      _sb = updated
    }

  /** A read-only snapshot of filesystem capacity. `freeBlocks` and
    * `freeInodes` come from the live bitmaps, so they reflect any
    * allocations made since mount, not the stale on-disk SB values
    * that are only refreshed at unmount. */
  def statfs: StatfsInfo =
    requireMounted()
    StatfsInfo(
      blockSize = BlockSize,
      totalBlocks = layout.totalBlocks,
      freeBlocks = blockBitmap.freeCount,
      totalInodes = layout.totalInodes,
      freeInodes = inodeBitmap.freeCount,
    )

  /** Flush dirty bitmap blocks and journal state, mark the volume clean
    * in the on-disk superblock, and refuse further calls on this
    * instance.
    *
    * Bitmap and journal writes here are direct (not journaled) — the
    * superblock flip to `clean` is the outer commit, and any per-op
    * bitmap mutations have already been journaled by `withTransaction`.
    * The final `flush()` is a belt-and-suspenders catch for direct
    * mutations from tests. */
  def unmount(): Unit =
    requireMounted()
    blockBitmap.flush()
    inodeBitmap.flush()
    journal.flush()
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
    * to the backup at block 1 on CRC failure), validates it, runs
    * journal recovery if the volume is dirty, sets `fs_state = dirty`
    * on disk for the duration of the mount, and loads both bitmaps
    * into memory.
    *
    * A `dirty` volume gets [[Recovery.replay]] run against it — every
    * committed-but-not-yet-checkpointed transaction is re-applied to
    * its in-place fs_block locations. Replay is idempotent so it's
    * safe whether or not the in-place writes already happened.
    *
    * `error`-state volumes are rejected — those need fsck (Phase 16). */
  def mount(dev: BlockDevice): Sfs =
    val sb0 = readSuperblock(dev)
    sb0.fsState match
      case FsClean => () // ok
      case FsDirty => () // recover below
      case FsError =>
        throw new SfsCorruptError("filesystem is in error state — run fsck")
      case other =>
        throw new SfsCorruptError(s"unknown fs_state $other")

    val layout = Layout.fromSuperblock(sb0)

    val journal = Journal.load(dev, layout.journalStart.toLong, sb0.uuid)

    // Recovery runs *before* loading the bitmaps so any bitmap blocks
    // staged in committed-but-not-checkpointed transactions are
    // applied to disk first. The bitmap load below then sees the
    // canonical post-replay state.
    if sb0.fsState == FsDirty then
      val newHead = Recovery.replay(dev, journal)
      journal.replayHead(newHead)
      journal.flush()
      dev.flush()

    val blockBm = new Bitmap(dev, layout.blockBitmapStart, layout.blockBitmapLen, layout.totalBlocks)
    blockBm.load()
    val inodeBm = new Bitmap(dev, layout.inodeBitmapStart, layout.inodeBitmapLen, layout.totalInodes)
    inodeBm.load()

    val mountedSb = sb0.copy(fsState = FsDirty, lastMountTime = now())
    writeSuperblockTo(dev, mountedSb)
    dev.flush()

    val sfs = new Sfs(dev, layout, mountedSb, blockBm, inodeBm, journal)
    BadBlockOps.loadAtMount(sfs)
    sfs

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
    // The inode table needs more than zeros: every slot must have a
    // valid (zero) CRC over its 208-byte payload, so freshly allocated
    // inodes can be read back at generation = 0 before they're written
    // for the first time. Build one block's worth of empty inodes once,
    // then write it to every inode-table block.
    writeEmptyInodeTable(dev, layout)

  private def writeEmptyInodeTable(dev: BlockDevice, layout: Layout): Unit =
    val template = new Array[Byte](BlockSize)
    val empty = emptyInode()
    var off = 0
    while off < BlockSize do
      Inode.pack(empty, template, off)
      off += InodeSize
    var b = layout.inodeTableStart.toLong
    val end = b + layout.inodeTableLen.toLong
    while b < end do
      dev.writeBlock(b, template)
      b += 1L

  private def emptyInode(): Inode =
    Inode(
      mode = 0,
      linkCount = 0,
      uid = 0,
      gid = 0,
      flags = 0,
      size = 0L,
      blockCount = 0,
      generation = 0,
      atimeSec = 0, atimeNsec = 0,
      mtimeSec = 0, mtimeNsec = 0,
      ctimeSec = 0, ctimeNsec = 0,
      crtimeSec = 0, crtimeNsec = 0,
      body = InodeBody.EmptyExtents,
      indirect1 = 0,
      indirect2 = 0,
      indirect3 = 0,
      xattrBlock = 0,
    )

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
