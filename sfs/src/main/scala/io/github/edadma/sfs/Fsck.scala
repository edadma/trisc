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

  /** A block is reachable from a live inode (or is in the metadata
    * region or the bad-blocks list) but the on-disk block bitmap has
    * its bit clear. The filesystem may hand the same block out to
    * another file. */
  case BlockBitmapMissingBit(blockNum: Int)

  /** A block has its on-disk block bitmap bit set but no live inode,
    * metadata region, or bad-blocks entry references it. The block is
    * "leaked" — permanently lost from the free pool. */
  case BlockBitmapLeakedBlock(blockNum: Int)

  /** Walking a directory's entries threw an [[SfsCorruptError]] —
    * usually because the directory's HTree structure (root, index, or
    * leaf) is corrupt past what `DirBlockCorrupt` already reports.
    * Without a successful walk, fsck cannot enumerate the directory's
    * children, which means orphans / link-count comparisons will be
    * incomplete for those subtrees. */
  case DirectoryWalkFailed(inodeNum: Int, message: String)

  /** A directory entry points at an inode whose bitmap bit is clear.
    * The reference is dangling — opening it would either find a stale
    * empty inode or be reused by a freshly-allocated unrelated file. */
  case InodeBitmapMissingBit(inodeNum: Int)

  /** An inode bit is set but the inode has `link_count == 0` and is
    * not referenced by any directory entry. The slot is wastefully
    * marked allocated. (Distinct from `OrphanedInode`, where the
    * inode IS in use according to its `link_count` but no dir entry
    * points at it.) */
  case InodeBitmapLeakedBit(inodeNum: Int)

  /** An inode's stored `link_count` doesn't match the number of
    * directory entries pointing at it. Includes `.`/`..` entries the
    * way POSIX does — for a directory with `n` subdirectories the
    * computed count is `2 + n`. Only emitted when `computed > 0`;
    * `computed == 0` with `stored > 0` is reported as
    * [[OrphanedInode]] instead. */
  case LinkCountMismatch(inodeNum: Int, stored: Int, computed: Int)

  /** An inode's bitmap bit is set and its `link_count > 0`, but no
    * directory entry references it. The inode's data is intact but
    * unreachable through the directory tree. Repair mode (chunk 16d)
    * can link these under `lost+found/`. */
  case OrphanedInode(inodeNum: Int, linkCount: Int)

/** Counters collected while walking the filesystem. Useful both for
  * sanity checks (compare against the superblock's free counts) and as
  * a smoke-test signal that fsck actually exercised the structures it
  * was meant to. */
final case class FsckStats(
    inodesChecked: Int,
    dirBlocksChecked: Int,
    xattrBlocksChecked: Int,
    extentsWalked: Int,
    claimedBlocks: Int,
)

object FsckStats:
  val empty: FsckStats = FsckStats(0, 0, 0, 0, 0)

/** Result of [[Fsck.check]]: every inconsistency found, plus walk
  * statistics. Ordering of `issues` is not stable across versions —
  * tests should match by content, not by index. */
final case class FsckReport(issues: Vector[FsckIssue], stats: FsckStats):
  def clean: Boolean = issues.isEmpty

/** Knobs for [[Fsck.repair]]. All on by default — toggle individual
  * fields to suppress a given fix. CRC corruptions (data loss) are
  * never auto-repaired. */
final case class FsckRepairOptions(
    fixLeakedBlocks: Boolean = true,
    fixLinkCounts: Boolean = true,
    rescueOrphans: Boolean = true,
    lostAndFoundName: String = "lost+found",
)

/** Summary of what [[Fsck.repair]] actually changed. `skipped` lists
  * the issues fsck did NOT fix (CRC corruptions, directory-walk
  * failures, etc.) — those need user attention. */
final case class FsckRepairReport(
    leakedBlocksCleared: Int,
    linkCountsRestored: Int,
    orphansLinked: Int,
    lostAndFoundCreated: Boolean,
    skipped: Vector[FsckIssue],
)

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
    val reach = walkReachability(sfs, parsed)
    issues ++= reach.walkIssues
    issues ++= reconcileBlockBitmap(sfs, reach.claimedBlocks)
    issues ++= reconcileInodes(sfs, parsed, reach.inodeReferences)
    FsckReport(
      issues = issues.result(),
      stats = FsckStats(
        inodesChecked = parsed.size,
        dirBlocksChecked = perInode.dirBlocks,
        xattrBlocksChecked = perInode.xattrBlocks,
        extentsWalked = perInode.extents,
        claimedBlocks = reach.claimedBlocks.size,
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

  // ---- reachability walk (16b foundation, 16c also feeds off this) ---

  /** Aggregate of the reachability walk:
    *
    *  - `claimedBlocks` — every physical block that fsck believes is in
    *    legitimate use: the metadata region, every block any parsed
    *    inode owns (extents, indirects, xattr), and every entry in the
    *    bad-blocks list.
    *  - `inodeReferences` — count of directory entries pointing at each
    *    inode. Used by chunk 16c (`link_count` reconciliation).
    *  - `walkIssues` — diagnostics for directories whose `HTree.list`
    *    couldn't complete; their referenced inodes won't appear in
    *    `inodeReferences`. */
  private final case class ReachabilityResult(
      claimedBlocks: Set[Int],
      inodeReferences: Map[Int, Int],
      walkIssues: Vector[FsckIssue],
  )

  private def walkReachability(
      sfs: Sfs,
      parsed: Map[Int, Inode],
  ): ReachabilityResult =
    val claimed = scala.collection.mutable.Set.empty[Int]
    val refs = scala.collection.mutable.Map.empty[Int, Int]
    val walkIssues = Vector.newBuilder[FsckIssue]

    // Metadata region — always claimed by the filesystem itself.
    var b = 0
    while b < sfs.layout.dataStart do
      claimed += b
      b += 1

    // Bad-blocks list — referenced by inode 1, but we want them to
    // appear as legitimately claimed even though no other inode owns
    // them.
    try
      val bads = BadBlockOps.list(sfs)
      var i = 0
      while i < bads.length do
        claimed += bads(i)
        i += 1
    catch case e: SfsCorruptError =>
      walkIssues += FsckIssue.InodeCorrupt(InoBadBlocks, s"bad-blocks list walk failed: ${e.getMessage}")

    // Per-inode block enumeration + per-directory entry walk.
    val ordered = parsed.toIndexedSeq.sortBy(_._1)
    var k = 0
    while k < ordered.length do
      val (n, ino) = ordered(k)
      try
        val blocks = enumerateInodeBlocks(ino, sfs.device)
        var j = 0
        while j < blocks.length do
          claimed += blocks(j)
          j += 1
      catch case e: SfsCorruptError =>
        walkIssues += FsckIssue.InodeCorrupt(n, s"block enumeration failed: ${e.getMessage}")

      if isDirectoryMode(ino.mode) then
        try
          val entries = HTree.list(ino, sfs.device, n)
          var j = 0
          while j < entries.length do
            val e = entries(j)
            // POSIX link-count semantics: every dir entry contributes,
            // including "." (self-link) and ".." (back-link). For a
            // directory N with k subdirectories, this gives expected
            // link_count = 2 + k (the "." in N, the entry in N's
            // parent, and one ".." per subdir).
            refs.updateWith(e.inode) { case Some(c) => Some(c + 1); case None => Some(1) }
            j += 1
        catch case e: SfsCorruptError =>
          walkIssues += FsckIssue.DirectoryWalkFailed(n, e.getMessage)

      k += 1

    ReachabilityResult(claimed.toSet, refs.toMap, walkIssues.result())

  /** Enumerate every physical block this inode lays claim to:
    *
    *  - Concrete and uninitialized extent blocks (sparse extents own
    *    no physical blocks).
    *  - The indirect tier blocks themselves (`indirect1`, `indirect2`
    *    plus its pointer block contents, `indirect3` plus its two
    *    levels of pointer blocks).
    *  - The xattr block, if `HAS_XATTR` is set.
    *
    * Reads through `dev` directly — never through any cache, so we
    * see actual on-disk state. */
  private def enumerateInodeBlocks(ino: Inode, dev: BlockDevice): Vector[Int] =
    val out = Vector.newBuilder[Int]

    // Extents (concrete and uninitialized — both own physical blocks).
    val xs = ExtentAllocator.listExtents(ino, dev)
    var i = 0
    while i < xs.length do
      val e = xs(i)
      if !e.sparse then
        var c = 0
        while c < e.count do
          out += e.start + c
          c += 1
      i += 1

    // Indirect tier blocks. ExtentAllocator.listExtents already walked
    // through the tier blocks, but those reads don't surface the tier
    // block addresses themselves — that's what we collect here.
    if (ino.flags & InodeFlagHasIndirect1) != 0 then out += ino.indirect1

    if (ino.flags & InodeFlagHasIndirect2) != 0 then
      out += ino.indirect2
      val ptrs = readPointerBlock(dev, ino.indirect2)
      var p = 0
      while p < ptrs.length && ptrs(p) != 0 do
        out += ptrs(p)
        p += 1

    if (ino.flags & InodeFlagHasIndirect3) != 0 then
      out += ino.indirect3
      val ptrs3 = readPointerBlock(dev, ino.indirect3)
      var p3 = 0
      while p3 < ptrs3.length && ptrs3(p3) != 0 do
        out += ptrs3(p3)
        val ptrs2 = readPointerBlock(dev, ptrs3(p3))
        var p2 = 0
        while p2 < ptrs2.length && ptrs2(p2) != 0 do
          out += ptrs2(p2)
          p2 += 1
        p3 += 1

    if (ino.flags & InodeFlagHasXattr) != 0 && ino.xattrBlock != 0 then
      out += ino.xattrBlock

    out.result()

  private def readPointerBlock(dev: BlockDevice, blockAddr: Int): IndexedSeq[Int] =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(blockAddr.toLong, buf)
    IndirectPointerBlock.unpack(buf, 0)

  // ---- block-bitmap reconciliation -----------------------------------

  /** For every block in `[0, totalBlocks)`, compare "is it in the
    * fsck-derived claimed set" against the on-disk block bitmap.
    * Reports a `BlockBitmapMissingBit` for claimed-but-clear and
    * `BlockBitmapLeakedBlock` for unclaimed-but-set. */
  private def reconcileBlockBitmap(
      sfs: Sfs,
      claimed: Set[Int],
  ): Vector[FsckIssue] =
    val out = Vector.newBuilder[FsckIssue]
    val total = sfs.layout.totalBlocks
    var b = 0
    while b < total do
      val isClaimed = claimed.contains(b)
      val isSet = sfs.blockBitmap.isSet(b)
      if isClaimed && !isSet then out += FsckIssue.BlockBitmapMissingBit(b)
      else if !isClaimed && isSet then out += FsckIssue.BlockBitmapLeakedBlock(b)
      b += 1
    out.result()

  // ---- inode reconciliation (16c) ------------------------------------

  /** Compare each inode's stored `link_count` against the count of
    * directory entries fsck found pointing at it (including `.` and
    * `..`), plus reconcile the inode bitmap with reachability.
    *
    * Reserved inodes [[InoNull]] and [[InoBadBlocks]] are skipped —
    * they are deliberately allocated without dir-tree references.
    * [[InoRoot]] is *not* skipped: its self-references through "."
    * and ".." (plus any subdirectory's "..") are counted by the
    * walker so the comparison is meaningful. */
  private def reconcileInodes(
      sfs: Sfs,
      parsed: Map[Int, Inode],
      refs: Map[Int, Int],
  ): Vector[FsckIssue] =
    val out = Vector.newBuilder[FsckIssue]
    val total = sfs.layout.totalInodes
    var n = 0
    while n < total do
      val isReserved = n == InoNull || n == InoBadBlocks
      val bit = sfs.inodeBitmap.isSet(n)
      val computed = refs.getOrElse(n, 0)
      val parsedIno = parsed.get(n)

      if !bit && computed > 0 then
        // A directory entry points at an inode the bitmap says is
        // free. The inode contents (if any) are stale.
        out += FsckIssue.InodeBitmapMissingBit(n)

      else if bit && !isReserved && parsedIno.isDefined then
        val ino = parsedIno.get
        val stored = ino.linkCount
        if stored == 0 && computed == 0 then
          // Allocated slot whose payload is empty and which nothing
          // points at — bitmap leak.
          out += FsckIssue.InodeBitmapLeakedBit(n)
        else if stored > 0 && computed == 0 then
          // The inode is in use according to its own link_count but
          // no dir entry points at it. Lost — repair mode would
          // re-link under lost+found.
          out += FsckIssue.OrphanedInode(n, stored)
        else if stored != computed then
          out += FsckIssue.LinkCountMismatch(n, stored = stored, computed = computed)

      n += 1
    out.result()

  // ---- repair (16d, optional) ----------------------------------------

  /** Apply a curated set of safe automated fixes to `sfs`.
    *
    * What gets repaired:
    *
    *  - [[FsckIssue.BlockBitmapLeakedBlock]] — clear the bit in the
    *    block bitmap so the allocator can reuse it.
    *  - [[FsckIssue.LinkCountMismatch]] — overwrite the inode's stored
    *    link_count with the value fsck computed from directory
    *    references.
    *  - [[FsckIssue.OrphanedInode]] — for non-directory orphans only,
    *    create a `lost+found` directory under root if needed and
    *    insert a "#N" entry pointing at the orphan, with link_count
    *    set to 1. Directory orphans are left in place (re-linking
    *    them safely needs a tree-cycle audit beyond 16d's scope).
    *
    * What does NOT get repaired:
    *
    *  - All `*Corrupt` variants (CRC / magic / owner mismatches) —
    *    those represent data loss; the user decides what to do.
    *  - `BlockBitmapMissingBit` / `InodeBitmapMissingBit` — silently
    *    setting these can mask deeper damage; report only.
    *  - `InodeBitmapLeakedBit` — same caveat (deferred to a future
    *    pass that can verify the slot is genuinely empty).
    *  - `DirectoryWalkFailed` — a partial walk leaves the orphan
    *    enumeration unsafe; reported, not repaired.
    *
    * All fixes land in a single [[Sfs.withTransaction]] so the volume
    * is atomically consistent across a crash mid-repair. */
  def repair(
      sfs: Sfs,
      timeSec: Int,
      timeNsec: Int,
      options: FsckRepairOptions = FsckRepairOptions(),
  ): FsckRepairReport =
    val report0 = check(sfs)

    var leakedBlocks = 0
    var linkCountFixes = 0
    var orphansLinked = 0
    var lostAndFoundCreated = false
    val skipped = Vector.newBuilder[FsckIssue]

    sfs.withTransaction {
      var i = 0
      while i < report0.issues.length do
        report0.issues(i) match
          case FsckIssue.BlockBitmapLeakedBlock(b) if options.fixLeakedBlocks =>
            sfs.blockBitmap.clear(b)
            leakedBlocks += 1

          case FsckIssue.LinkCountMismatch(n, _, computed) if options.fixLinkCounts =>
            val ino = sfs.readInode(n)
            sfs.writeInode(n, ino.copy(linkCount = computed, ctimeSec = timeSec, ctimeNsec = timeNsec))
            linkCountFixes += 1

          case other =>
            skipped += other
        i += 1

      // Orphan rescue runs after the simple per-issue fixes so that
      // any dir entries we add to lost+found don't perturb earlier
      // counts. We re-walk references because rescuing a non-dir
      // orphan adds one ref to it.
      if options.rescueOrphans then
        val orphans = report0.issues.collect {
          case FsckIssue.OrphanedInode(n, lc) => (n, lc)
        }
        if orphans.nonEmpty then
          val lf = ensureLostAndFound(sfs, options.lostAndFoundName, timeSec, timeNsec)
          lostAndFoundCreated = lf.created
          var j = 0
          while j < orphans.length do
            val (n, _) = orphans(j)
            val ino = sfs.readInode(n)
            if (ino.mode & 0xf000) == 0x4000 then
              // Directory orphan — out of scope for 16d.
              skipped += FsckIssue.OrphanedInode(n, ino.linkCount)
            else
              val lfIno = sfs.readInode(lf.inodeNum)
              val name = s"#$n"
              val updatedLfIno = HTree.insert(
                lfIno, sfs, lf.inodeNum,
                name, n, fileTypeFromMode(ino.mode),
              )
              sfs.writeInode(lf.inodeNum, updatedLfIno)
              // Set linkCount = 1 (this single lost+found entry).
              sfs.writeInode(n, ino.copy(
                linkCount = 1,
                ctimeSec = timeSec,
                ctimeNsec = timeNsec,
              ))
              orphansLinked += 1
            j += 1
    }

    FsckRepairReport(
      leakedBlocksCleared = leakedBlocks,
      linkCountsRestored = linkCountFixes,
      orphansLinked = orphansLinked,
      lostAndFoundCreated = lostAndFoundCreated,
      skipped = skipped.result(),
    )

  /** Look up `lostAndFoundName` under root. If it exists and is a
    * directory, return its inode number; otherwise create it via
    * `DirOps.mkdir` and return the new number. */
  private final case class LostFoundResult(inodeNum: Int, created: Boolean)

  private def ensureLostAndFound(
      sfs: Sfs,
      name: String,
      timeSec: Int,
      timeNsec: Int,
  ): LostFoundResult =
    val rootIno = sfs.readInode(InoRoot)
    HTree.lookup(rootIno, sfs.metaDevice, InoRoot, name) match
      case Some((existing, fileType)) =>
        if fileType != DirEntry.TypeDirectory then
          throw new SfsCorruptError(
            s"Fsck.repair: $name exists at root but is not a directory",
          )
        LostFoundResult(existing, created = false)
      case None =>
        val (newRoot, newIno) = DirOps.mkdir(
          rootIno, InoRoot, sfs, name,
          FileOps.ModeDirectory | 0x1c0, // owner-only rwx
          uid = 0, gid = 0,
          timeSec = timeSec, timeNsec = timeNsec,
        )
        sfs.writeInode(InoRoot, newRoot)
        LostFoundResult(newIno, created = true)

  /** Map a mode bits to the [[DirEntry]] file_type tag. Mirror of
    * [[FileOps.fileTypeFromMode]] which is private; small enough to
    * duplicate here rather than widen its visibility just for fsck. */
  private def fileTypeFromMode(mode: Int): Int =
    (mode & 0xf000) match
      case 0x8000 => DirEntry.TypeRegular
      case 0x4000 => DirEntry.TypeDirectory
      case 0xa000 => DirEntry.TypeSymlink
      case _      => DirEntry.TypeOther
