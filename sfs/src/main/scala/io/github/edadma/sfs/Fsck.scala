package io.github.edadma.sfs

import Constants.*

/** A single inconsistency reported by [[Fsck.check]].
  *
  * Variants are added as later phase-16 chunks land:
  *
  *  - 16a (this file) — CRC / magic / owner-binding failures.
  *  - 16b — block- and inode-bitmap mismatches.
  *  - 16c — `link_count` mismatches and orphaned (allocated but
  *    unreferenced) inodes.
  */
enum FsckIssue:

  /** A superblock copy (block 0 = primary, block 1 = backup) failed
    * magic / `block_size` / CRC verification. The other copy may still
    * be intact — `Sfs.mount` falls back from primary to backup. */
  case SuperblockCorrupt(blockNum: Long, message: String)

  /** The journal superblock at `layout.journalStart` failed magic / CRC
    * verification. Until repaired, the journal can't be replayed. */
  case JournalSuperblockCorrupt(message: String)

  /** An inode whose bit is set in the inode bitmap could not be parsed —
    * its on-disk CRC doesn't match its payload, or its body field
    * decodes as an invalid type. `inodeNum` is the table slot. */
  case InodeCorrupt(inodeNum: Int, message: String)

  /** A directory data block reachable from `ownerInode` failed
    * [[DirTail.verify]] — bad magic, wrong owner inode, or CRC
    * mismatch. `physicalBlock` is the on-disk block address. */
  case DirBlockCorrupt(ownerInode: Int, physicalBlock: Long, message: String)

  /** An xattr block reachable from `ownerInode` failed
    * [[XattrBlock.unpack]] — bad magic, wrong owner inode, CRC
    * mismatch, or out-of-bounds entry header. */
  case XattrBlockCorrupt(ownerInode: Int, physicalBlock: Long, message: String)

/** Counters collected while walking the filesystem. Useful both for
  * sanity checks (compare against the superblock's free counts) and as
  * a smoke-test signal that fsck actually exercised the structures it
  * was meant to. */
final case class FsckStats(
    inodesChecked: Int,
    dirBlocksChecked: Int,
    xattrBlocksChecked: Int,
    extentsWalked: Int,
)

object FsckStats:
  val empty: FsckStats = FsckStats(0, 0, 0, 0)

/** Result of [[Fsck.check]]: every inconsistency found, plus walk
  * statistics. Ordering of `issues` is not stable across versions —
  * tests should match by content, not by index. */
final case class FsckReport(issues: Vector[FsckIssue], stats: FsckStats):
  def clean: Boolean = issues.isEmpty

/** Filesystem consistency checker.
  *
  * `check(sfs)` is read-only — it walks every reachable on-disk
  * structure and reports CRC / magic / owner-binding failures. Bitmap
  * reconciliation (chunk 16b), reference-graph + linkCount checks
  * (chunk 16c), and optional repair (chunk 16d) come in later
  * sub-chunks of phase 16.
  *
  * The walk reads through `sfs.device` directly (not `sfs.metaDevice`)
  * so it inspects the actual on-disk state, ignoring any txn-staged
  * writes the caller might have buffered. fsck on a *mounted* fs is
  * intended for offline checking only — the caller should ensure no
  * other writes are in flight. */
object Fsck:

  /** Run a read-only consistency check on `sfs`. */
  def check(sfs: Sfs): FsckReport =
    val issues = Vector.newBuilder[FsckIssue]
    issues ++= checkSuperblocks(sfs.device)
    issues ++= checkJournalSuperblock(sfs)
    val (inoIssues, parsed) = parseAllocatedInodes(sfs)
    issues ++= inoIssues
    val perInode = walkPerInode(sfs, parsed)
    issues ++= perInode.issues
    FsckReport(
      issues = issues.result(),
      stats = FsckStats(
        inodesChecked = parsed.size,
        dirBlocksChecked = perInode.dirBlocks,
        xattrBlocksChecked = perInode.xattrBlocks,
        extentsWalked = perInode.extents,
      ),
    )

  // ---- superblocks ----------------------------------------------------

  private def checkSuperblocks(dev: BlockDevice): Vector[FsckIssue] =
    val out = Vector.newBuilder[FsckIssue]
    val buf = new Array[Byte](BlockSize)
    var i = 0L
    while i < 2L do
      dev.readBlock(i, buf)
      try Superblock.unpack(buf, 0)
      catch case e: SfsCorruptError =>
        out += FsckIssue.SuperblockCorrupt(i, e.getMessage)
      i += 1L
    out.result()

  private def checkJournalSuperblock(sfs: Sfs): Vector[FsckIssue] =
    val buf = new Array[Byte](BlockSize)
    sfs.device.readBlock(sfs.layout.journalStart.toLong, buf)
    try
      JournalSuperblock.unpack(buf, 0)
      Vector.empty
    catch case e: SfsCorruptError =>
      Vector(FsckIssue.JournalSuperblockCorrupt(e.getMessage))

  // ---- inode table ----------------------------------------------------

  /** Walk the inode bitmap; for every bit that is set, try to read
    * and parse the inode at that table slot. Returns issues for any
    * unparseable allocated inode and a map of successfully parsed
    * inodes (inode number → parsed inode). */
  private def parseAllocatedInodes(sfs: Sfs): (Vector[FsckIssue], Map[Int, Inode]) =
    val issues = Vector.newBuilder[FsckIssue]
    val parsed = Map.newBuilder[Int, Inode]
    val total = sfs.layout.totalInodes
    var n = 0
    while n < total do
      if sfs.inodeBitmap.isSet(n) then
        try parsed += n -> readInodeFromDevice(sfs, n)
        catch case e: SfsCorruptError =>
          issues += FsckIssue.InodeCorrupt(n, e.getMessage)
      n += 1
    (issues.result(), parsed.result())

  /** Read inode `n` from the underlying device (bypasses
    * `sfs.metaDevice` so any txn-staged write is invisible — fsck
    * cares about on-disk truth only). */
  private def readInodeFromDevice(sfs: Sfs, n: Int): Inode =
    val (blk, off) = sfs.layout.inodeLocation(n)
    val buf = new Array[Byte](BlockSize)
    sfs.device.readBlock(blk, buf)
    Inode.unpack(buf, off)

  // ---- per-inode walk -------------------------------------------------

  /** Aggregate of per-inode walk results — returned as a small struct
    * to keep the call site readable. */
  private final case class PerInodeResult(
      issues: Vector[FsckIssue],
      dirBlocks: Int,
      xattrBlocks: Int,
      extents: Int,
  )

  private def walkPerInode(
      sfs: Sfs,
      parsed: Map[Int, Inode],
  ): PerInodeResult =
    val issues = Vector.newBuilder[FsckIssue]
    var dirBlocks = 0
    var xattrBlocks = 0
    var extents = 0
    val ordered = parsed.toIndexedSeq.sortBy(_._1)
    var i = 0
    while i < ordered.length do
      val (n, ino) = ordered(i)

      try extents += ExtentAllocator.listExtents(ino, sfs.device).length
      catch case e: SfsCorruptError =>
        issues += FsckIssue.InodeCorrupt(n, s"extent walk failed: ${e.getMessage}")

      if isDirectoryMode(ino.mode) then
        val dr = checkDirectoryBlocks(sfs, n, ino)
        issues ++= dr.issues
        dirBlocks += dr.checked

      if (ino.flags & InodeFlagHasXattr) != 0 && ino.xattrBlock != 0 then
        checkOneXattrBlock(sfs, n, ino) match
          case Some(issue) => issues += issue
          case None        => xattrBlocks += 1

      i += 1
    PerInodeResult(issues.result(), dirBlocks, xattrBlocks, extents)

  private def isDirectoryMode(mode: Int): Boolean =
    (mode & 0xf000) == 0x4000

  // ---- directory blocks -----------------------------------------------

  private final case class DirCheckResult(issues: Vector[FsckIssue], checked: Int)

  private def checkDirectoryBlocks(
      sfs: Sfs,
      inodeNum: Int,
      ino: Inode,
  ): DirCheckResult =
    val issues = Vector.newBuilder[FsckIssue]
    var checked = 0
    val totalLogical = ExtentAllocator.totalBlockCount(ino, sfs.device)
    val reader = new ExtentReader(sfs.device, ino)
    val buf = new Array[Byte](BlockSize)
    var logical = 0L
    while logical < totalLogical do
      reader.physicalBlock(logical) match
        case BlockMapping.Concrete(p) =>
          sfs.device.readBlock(p, buf)
          try
            DirTail.verify(buf, inodeNum)
            checked += 1
          catch case e: SfsCorruptError =>
            issues += FsckIssue.DirBlockCorrupt(inodeNum, p, e.getMessage)
        case _ => ()
      logical += 1L
    DirCheckResult(issues.result(), checked)

  // ---- xattr block ----------------------------------------------------

  private def checkOneXattrBlock(
      sfs: Sfs,
      inodeNum: Int,
      ino: Inode,
  ): Option[FsckIssue] =
    val buf = new Array[Byte](BlockSize)
    sfs.device.readBlock(ino.xattrBlock.toLong, buf)
    try
      XattrBlock.unpack(buf, inodeNum)
      None
    catch case e: SfsCorruptError =>
      Some(FsckIssue.XattrBlockCorrupt(inodeNum, ino.xattrBlock.toLong, e.getMessage))
