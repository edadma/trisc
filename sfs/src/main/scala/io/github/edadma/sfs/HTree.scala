package io.github.edadma.sfs

import Constants.*

/** Directory-level operations on an HTree directory. The directory file's
  * data blocks are organized as:
  *
  *   - Logical block 0: [[DirRootBlock]] (always; contains dot, dotdot,
  *     and the index_entries pointing into deeper blocks).
  *   - Logical blocks 1+: leaves (when `treeDepth = 0`) or interior
  *     index blocks alongside leaves (when `treeDepth = 1`).
  *
  * `treeDepth ≤ 1` is the entire HTree shape SFS specifies. At depth 0
  * the root's index_entries point directly at leaves; at depth 1 they
  * point at interior [[DirIndexBlock]]s, each of which points at
  * leaves. The tree grows on overflow:
  *
  *   - Leaf full → split the leaf and add a new pointer to its parent
  *     (root at depth 0, the relevant interior at depth 1).
  *   - Adding to root would exceed [[DirRootBlock.MaxIndexEntries]] at
  *     depth 0 → promote depth 0 → 1.
  *   - Interior full at depth 1 → split the interior and add a pointer
  *     to root.
  *   - Adding to root would exceed [[DirRootBlock.MaxIndexEntries]] at
  *     depth 1 → throw; `treeDepth = 2` is outside the spec.
  *
  * Every public mutation returns an updated [[Inode]] (possibly with
  * new blocks appended). Time stamps and link-count bookkeeping live
  * one layer up — Phase 10 owns those.
  */
object HTree:

  /** Ordering on hash values that treats them as unsigned 32-bit
    * integers. Used for sorting index_entries (which the lookup walker
    * also reads as unsigned) and for split-point selection. */
  private given unsignedHashOrdering: Ordering[Int] =
    (a, b) => java.lang.Integer.compareUnsigned(a, b)

  /** Where the index entry pointing at a particular leaf currently
    * lives. The leaf-split path uses this to know whether to update
    * the root's index_entries (depth 0) or an interior block (depth 1). */
  private sealed trait LeafParent
  private case object RootParent extends LeafParent
  private case class InteriorParent(blockNum: Int) extends LeafParent

  // ---- public API -----------------------------------------------------

  /** Initialize a fresh, empty directory: append two data blocks (root +
    * one empty leaf) to `ino` and write them. Returns the grown inode. */
  def initDirectory(
      ino: Inode,
      sfs: Sfs,
      ownerInode: Int,
      parentInode: Int,
  ): Inode =
    require(
      ExtentAllocator.totalBlockCount(ino, sfs.device) == 0L,
      "initDirectory: inode already has data blocks",
    )
    val grown = ExtentAllocator.append(ino, sfs, 2)

    val rootBuf = new Array[Byte](BlockSize)
    val root = DirRootBlock(
      dot = DirEntry(ownerInode, DirEntry.TypeDirectory, "."),
      dotdot = DirEntry(parentInode, DirEntry.TypeDirectory, ".."),
      hashVersion = HashFnv1a,
      treeDepth = 0,
      flags = 0,
      indexEntries = IndexedSeq((0, 1)),
    )
    DirRootBlock.pack(root, ownerInode, rootBuf)
    writeDirBlock(grown, sfs, 0L, rootBuf)

    val leafBuf = new Array[Byte](BlockSize)
    DirLeaf.initEmpty(leafBuf, ownerInode)
    writeDirBlock(grown, sfs, 1L, leafBuf)

    grown

  /** Look up a name in the directory. Returns `(childInode, fileType)`
    * if found; `None` otherwise. Resolves "." and ".." from the root. */
  def lookup(
      ino: Inode,
      dev: BlockDevice,
      ownerInode: Int,
      name: String,
  ): Option[(Int, Int)] =
    val rootBuf = readDirBlock(ino, dev, 0L)
    val root = DirRootBlock.unpack(rootBuf, ownerInode)
    if name == "." then return Some((root.dot.inode, root.dot.fileType))
    if name == ".." then return Some((root.dotdot.inode, root.dotdot.fileType))
    requireSupportedDepth(root)
    val (leafBlock, _) = findLeaf(ino, dev, ownerInode, root, Fnv1a.hash(name))
    val leafBuf = readDirBlock(ino, dev, leafBlock.toLong)
    DirTail.verify(leafBuf, ownerInode)
    DirLeaf.findByName(leafBuf, name).map { case (_, e) => (e.inode, e.fileType) }

  /** Insert a name → (inode, fileType) binding. Returns the (possibly
    * grown) inode. Splits the target leaf if it cannot accommodate the
    * new entry; promotes the directory from `treeDepth = 0` to
    * `treeDepth = 1` when a leaf split would overflow the root's
    * index_entries table. */
  def insert(
      ino: Inode,
      sfs: Sfs,
      ownerInode: Int,
      name: String,
      childInode: Int,
      fileType: Int,
  ): Inode =
    require(name != "." && name != "..", s"""HTree.insert: cannot insert "$name"""")
    val dev = sfs.metaDevice
    val rootBuf = readDirBlock(ino, dev, 0L)
    val root = DirRootBlock.unpack(rootBuf, ownerInode)
    requireSupportedDepth(root)
    val nameHash = Fnv1a.hash(name)
    val (leafBlock, parent) = findLeaf(ino, dev, ownerInode, root, nameHash)
    val leafBuf = readDirBlock(ino, dev, leafBlock.toLong)
    DirTail.verify(leafBuf, ownerInode)
    if DirLeaf.findByName(leafBuf, name).isDefined then
      throw new SfsExistsError(s"""HTree.insert: name "$name" already exists""")

    val entry = DirEntry(childInode, fileType, name)
    if DirLeaf.tryInsert(leafBuf, entry, ownerInode) then
      writeDirBlock(ino, sfs, leafBlock.toLong, leafBuf)
      ino
    else
      splitLeafAndRetry(
        ino, sfs, ownerInode, root, rootBuf,
        leafBlock, leafBuf, parent, entry,
      )

  /** Delete a name. Returns the (unchanged) inode. Throws
    * [[SfsNotFoundError]] if no such name exists. Refuses to delete
    * "." or "..". */
  def delete(
      ino: Inode,
      sfs: Sfs,
      ownerInode: Int,
      name: String,
  ): Inode =
    require(name != "." && name != "..", s"""HTree.delete: cannot delete "$name"""")
    val dev = sfs.metaDevice
    val rootBuf = readDirBlock(ino, dev, 0L)
    val root = DirRootBlock.unpack(rootBuf, ownerInode)
    requireSupportedDepth(root)
    val (leafBlock, _) = findLeaf(ino, dev, ownerInode, root, Fnv1a.hash(name))
    val leafBuf = readDirBlock(ino, dev, leafBlock.toLong)
    DirTail.verify(leafBuf, ownerInode)
    if !DirLeaf.delete(leafBuf, name, ownerInode) then
      throw new SfsNotFoundError(s"""HTree.delete: name "$name" not found""")
    writeDirBlock(ino, sfs, leafBlock.toLong, leafBuf)
    ino

  /** Variant that applies the relatime rule to the directory inode's
    * `atime` and persists it through `Sfs.withTransaction` if it
    * changed. Use from public list entry points; the `(ino, dev,
    * ownerInode)` primitive below is for fsck / DirOps internal walks
    * that must not touch metadata. */
  def list(
      ino: Inode,
      inoNum: Int,
      sfs: Sfs,
      nowSec: Int,
      nowNsec: Int,
  ): Vector[DirEntry] =
    val out = list(ino, sfs.device, inoNum)
    val updated = Atime.relatimeUpdate(ino, nowSec, nowNsec)
    if updated ne ino then
      sfs.withTransaction {
        sfs.writeInode(inoNum, updated)
      }
    out

  /** Return all live directory entries — dot, dotdot, and every live
    * entry in every leaf — in implementation-defined order. */
  def list(
      ino: Inode,
      dev: BlockDevice,
      ownerInode: Int,
  ): Vector[DirEntry] =
    val rootBuf = readDirBlock(ino, dev, 0L)
    val root = DirRootBlock.unpack(rootBuf, ownerInode)
    requireSupportedDepth(root)
    val out = Vector.newBuilder[DirEntry]
    out += root.dot
    out += root.dotdot
    val leafBlocks: Iterable[Int] =
      if root.treeDepth == 0 then
        liveIndexEntries(root).map(_._2)
      else
        for
          (_, interior) <- liveIndexEntries(root)
          (_, leaf) <- liveInteriorEntries(ino, dev, ownerInode, interior)
        yield leaf
    for leafBlock <- leafBlocks do
      val leafBuf = readDirBlock(ino, dev, leafBlock.toLong)
      DirTail.verify(leafBuf, ownerInode)
      for (_, e) <- DirLeaf.entries(leafBuf) do
        if e.inode != 0 then out += e
    out.result()

  // ---- traversal ------------------------------------------------------

  /** Walk root → optional interior → leaf and return both the leaf
    * logical block number and a tag identifying which parent block
    * holds the index entry pointing at that leaf. */
  private def findLeaf(
      ino: Inode,
      dev: BlockDevice,
      ownerInode: Int,
      root: DirRootBlock,
      h: Int,
  ): (Int, LeafParent) =
    val rootEntry = pickEntry(liveIndexEntries(root), h)
    if root.treeDepth == 0 then (rootEntry, RootParent)
    else
      val interiorEntries = liveInteriorEntries(ino, dev, ownerInode, rootEntry)
      val leaf = pickEntry(interiorEntries, h)
      (leaf, InteriorParent(rootEntry))

  private def liveInteriorEntries(
      ino: Inode,
      dev: BlockDevice,
      ownerInode: Int,
      interiorBlock: Int,
  ): IndexedSeq[(Int, Int)] =
    val buf = readDirBlock(ino, dev, interiorBlock.toLong)
    DirIndexBlock.unpack(buf, ownerInode).takeWhile(_._2 != 0)

  /** Pick the largest entry in a sorted index list whose hash is ≤ `h`,
    * returning its block field. */
  private def pickEntry(entries: IndexedSeq[(Int, Int)], h: Int): Int =
    if entries.isEmpty then
      throw new SfsCorruptError("HTree: directory has no index entries")
    var i = 0
    var lastBlock = entries.head._2
    while i < entries.length do
      val (hi, blk) = entries(i)
      if java.lang.Integer.compareUnsigned(hi, h) <= 0 then lastBlock = blk
      else return lastBlock
      i += 1
    lastBlock

  // ---- split + promote (chunks 9d–9e) ---------------------------------

  /** Split a full leaf, register the new sibling in its parent (root at
    * depth 0, interior block at depth 1), and re-issue the original
    * insert. Promotes the directory from depth 0 to depth 1 if a root
    * update would overflow [[DirRootBlock.MaxIndexEntries]]. Throws
    * [[SfsCorruptError]] if every entry in the source leaf shares one
    * hash (no clean partition), or if an interior block at depth 1
    * would overflow (Phase 9f territory). */
  private def splitLeafAndRetry(
      ino: Inode,
      sfs: Sfs,
      ownerInode: Int,
      root: DirRootBlock,
      rootBuf: Array[Byte],
      leafBlockNum: Int,
      leafBuf: Array[Byte],
      parent: LeafParent,
      newEntry: DirEntry,
  ): Inode =
    val dev = sfs.metaDevice
    val live = DirLeaf.entries(leafBuf).map(_._2).filter(_.inode != 0).toVector
    val withHash = live.map(e => (Fnv1a.hash(e.name), e)).sortBy(_._1)
    val cut = findSplitIndex(withHash.map(_._1)).getOrElse(
      throw new SfsCorruptError(
        "HTree.insert: cannot split leaf — every entry shares the same hash",
      ),
    )
    val medianHash = withHash(cut)._1

    val grown = ExtentAllocator.append(ino, sfs, 1)
    val newLeafLogicalBlock = (ExtentAllocator.totalBlockCount(grown, dev) - 1L).toInt

    val lowHalf = withHash.take(cut).map(_._2)
    val highHalf = withHash.drop(cut).map(_._2)
    repackLeaf(leafBuf, lowHalf, ownerInode)
    writeDirBlock(grown, sfs, leafBlockNum.toLong, leafBuf)

    val newLeafBuf = new Array[Byte](BlockSize)
    repackLeaf(newLeafBuf, highHalf, ownerInode)
    writeDirBlock(grown, sfs, newLeafLogicalBlock.toLong, newLeafBuf)

    val grown2 = parent match
      case RootParent =>
        addLeafEntryToRoot(grown, sfs, ownerInode, root, rootBuf, medianHash, newLeafLogicalBlock)
      case InteriorParent(interiorBlock) =>
        addLeafEntryToInterior(grown, sfs, ownerInode, interiorBlock, medianHash, newLeafLogicalBlock)

    insert(grown2, sfs, ownerInode, newEntry.name, newEntry.inode, newEntry.fileType)

  /** Either splice the new (hash, block) into root.indexEntries (if it
    * still fits), or promote the directory to depth 1 and place it in
    * the appropriate fresh interior block. Returns the (possibly
    * grown) inode. */
  private def addLeafEntryToRoot(
      ino: Inode,
      sfs: Sfs,
      ownerInode: Int,
      root: DirRootBlock,
      rootBuf: Array[Byte],
      medianHash: Int,
      newLeafBlock: Int,
  ): Inode =
    val merged =
      (liveIndexEntries(root) :+ ((medianHash, newLeafBlock))).sortBy(_._1)
    if merged.length <= DirRootBlock.MaxIndexEntries then
      val newRoot = root.copy(indexEntries = merged)
      DirRootBlock.pack(newRoot, ownerInode, rootBuf)
      writeDirBlock(ino, sfs, 0L, rootBuf)
      ino
    else
      promoteToDepth1(ino, sfs, ownerInode, root, rootBuf, merged)

  /** Promote a depth-0 directory to depth 1: spread the merged
    * index_entries (which would have overflowed the root) across two
    * fresh interior blocks, then rewrite the root with two pointers
    * (one per interior). */
  private def promoteToDepth1(
      ino: Inode,
      sfs: Sfs,
      ownerInode: Int,
      root: DirRootBlock,
      rootBuf: Array[Byte],
      merged: IndexedSeq[(Int, Int)],
  ): Inode =
    val dev = sfs.metaDevice
    val cut = findSplitIndex(merged.map(_._1)).getOrElse(
      throw new SfsCorruptError(
        "HTree.promoteToDepth1: cannot partition root index — every entry shares the same hash",
      ),
    )
    val left = merged.take(cut)
    val right = merged.drop(cut)
    require(
      left.length <= DirIndexBlock.Capacity && right.length <= DirIndexBlock.Capacity,
      s"promoteToDepth1: halves $left/$right exceed interior capacity ${DirIndexBlock.Capacity}",
    )

    val grown = ExtentAllocator.append(ino, sfs, 2)
    val total = ExtentAllocator.totalBlockCount(grown, dev).toInt
    val interiorA = total - 2
    val interiorB = total - 1

    val interiorABuf = new Array[Byte](BlockSize)
    DirIndexBlock.pack(left, ownerInode, interiorABuf)
    writeDirBlock(grown, sfs, interiorA.toLong, interiorABuf)

    val interiorBBuf = new Array[Byte](BlockSize)
    DirIndexBlock.pack(right, ownerInode, interiorBBuf)
    writeDirBlock(grown, sfs, interiorB.toLong, interiorBBuf)

    val newRoot = root.copy(
      treeDepth = 1,
      indexEntries = IndexedSeq(
        (left.head._1, interiorA),
        (right.head._1, interiorB),
      ),
    )
    DirRootBlock.pack(newRoot, ownerInode, rootBuf)
    writeDirBlock(grown, sfs, 0L, rootBuf)
    grown

  /** Add a new (hash, leafBlock) entry into an interior block at depth
    * 1. If the interior is already at capacity, splits it in two and
    * registers the new sibling in the root. Throws if the root would
    * overflow as a result (`treeDepth = 2` is not in the spec). */
  private def addLeafEntryToInterior(
      ino: Inode,
      sfs: Sfs,
      ownerInode: Int,
      interiorBlock: Int,
      medianHash: Int,
      newLeafBlock: Int,
  ): Inode =
    val buf = readDirBlock(ino, sfs.metaDevice, interiorBlock.toLong)
    val live = DirIndexBlock.unpack(buf, ownerInode).takeWhile(_._2 != 0)
    val merged = (live :+ ((medianHash, newLeafBlock))).sortBy(_._1)
    if merged.length <= DirIndexBlock.Capacity then
      DirIndexBlock.pack(merged, ownerInode, buf)
      writeDirBlock(ino, sfs, interiorBlock.toLong, buf)
      ino
    else
      splitInteriorAndAddToRoot(ino, sfs, ownerInode, interiorBlock, buf, merged)

  /** Split a full interior block into two siblings, repack each, and
    * splice a new (hash, sibling) entry into the root's index_entries.
    * The new sibling is allocated as a fresh logical block. Throws if
    * the root would overflow at depth 1 — that's `treeDepth = 2`,
    * which is not part of the SFS spec. */
  private def splitInteriorAndAddToRoot(
      ino: Inode,
      sfs: Sfs,
      ownerInode: Int,
      sourceInterior: Int,
      sourceBuf: Array[Byte],
      merged: IndexedSeq[(Int, Int)],
  ): Inode =
    val dev = sfs.metaDevice
    val cut = findSplitIndex(merged.map(_._1)).getOrElse(
      throw new SfsCorruptError(
        "HTree.insert: cannot split interior — every entry shares the same hash",
      ),
    )
    val left = merged.take(cut)
    val right = merged.drop(cut)

    val grown = ExtentAllocator.append(ino, sfs, 1)
    val newInterior = (ExtentAllocator.totalBlockCount(grown, dev) - 1L).toInt

    DirIndexBlock.pack(left, ownerInode, sourceBuf)
    writeDirBlock(grown, sfs, sourceInterior.toLong, sourceBuf)

    val newBuf = new Array[Byte](BlockSize)
    DirIndexBlock.pack(right, ownerInode, newBuf)
    writeDirBlock(grown, sfs, newInterior.toLong, newBuf)

    val rootBuf = readDirBlock(grown, dev, 0L)
    val root = DirRootBlock.unpack(rootBuf, ownerInode)
    val rootMerged =
      (liveIndexEntries(root) :+ ((right.head._1, newInterior))).sortBy(_._1)
    if rootMerged.length > DirRootBlock.MaxIndexEntries then
      throw new SfsCorruptError(
        "HTree.insert: root index_entries are full at depth 1 — " +
          "tree_depth = 2 is not part of the SFS spec",
      )
    val newRoot = root.copy(indexEntries = rootMerged)
    DirRootBlock.pack(newRoot, ownerInode, rootBuf)
    writeDirBlock(grown, sfs, 0L, rootBuf)
    grown

  /** Pack `entries` (with their `recLen` fields normalized to the
    * minimum) into `buf` from offset 0, donating any leftover bytes to
    * the trailing entry — or to a single tombstone if `entries` is
    * empty. Re-stamps the directory tail. */
  private def repackLeaf(
      buf: Array[Byte],
      entries: Seq[DirEntry],
      ownerInode: Int,
  ): Unit =
    if entries.isEmpty then
      DirLeaf.initEmpty(buf, ownerInode)
      return
    val normalized = entries.map(e => DirEntry(e.inode, e.fileType, e.name))
    val sumMin = normalized.foldLeft(0)(_ + _.recLen)
    require(
      sumMin <= DirLeaf.UsableSize,
      s"repackLeaf: entries' minRecLen sum $sumMin exceeds ${DirLeaf.UsableSize}",
    )
    val leftover = DirLeaf.UsableSize - sumMin
    var off = 0
    val n = normalized.length
    var i = 0
    while i < n do
      val e = normalized(i)
      if i == n - 1 && leftover > 0 then
        DirEntry.pack(e.copy(recLen = e.recLen + leftover), buf, off)
        off += e.recLen + leftover
      else
        DirEntry.pack(e, buf, off)
        off += e.recLen
      i += 1
    DirTail.pack(buf, ownerInode)

  /** Choose a cut index in `[1, n]` such that
    * `sortedHashes(j-1) != sortedHashes(j)` (strictly increasing
    * across the boundary), biased toward `n / 2`. Returns `None` if
    * every hash is identical (no clean partition exists). */
  private def findSplitIndex(sortedHashes: IndexedSeq[Int]): Option[Int] =
    val n = sortedHashes.length
    if n < 2 then return None
    val mid = n / 2
    var j = math.max(mid, 1)
    while j < n && sortedHashes(j) == sortedHashes(j - 1) do j += 1
    if j < n then Some(j)
    else
      var k = mid - 1
      while k > 0 && sortedHashes(k) == sortedHashes(k - 1) do k -= 1
      if k > 0 then Some(k) else None

  // ---- internal helpers -----------------------------------------------

  /** Filter the root's index_entries to just the populated leading
    * prefix (block != 0 means a real pointer). */
  private[sfs] def liveIndexEntries(root: DirRootBlock): IndexedSeq[(Int, Int)] =
    root.indexEntries.takeWhile(_._2 != 0)

  /** Depth-0 helper retained for tests: pick the leaf block from the
    * root's index_entries directly. Equivalent to `pickEntry` over
    * `liveIndexEntries(root)`. */
  private[sfs] def leafBlockFor(root: DirRootBlock, h: Int): Int =
    pickEntry(liveIndexEntries(root), h)

  private def requireSupportedDepth(root: DirRootBlock): Unit =
    if root.treeDepth > 1 then
      throw new SfsCorruptError(
        s"HTree: tree_depth ${root.treeDepth} not yet supported (chunks 9c–9e handle 0–1)",
      )

  private def readDirBlock(ino: Inode, dev: BlockDevice, logical: Long): Array[Byte] =
    val reader = new ExtentReader(dev, ino)
    reader.physicalBlock(logical) match
      case BlockMapping.Concrete(p) =>
        val buf = new Array[Byte](BlockSize)
        dev.readBlock(p, buf)
        buf
      case other =>
        throw new SfsCorruptError(
          s"HTree.readDirBlock: directory block $logical not concrete: $other",
        )

  private def writeDirBlock(
      ino: Inode,
      sfs: Sfs,
      logical: Long,
      buf: Array[Byte],
  ): Unit =
    // Use metaDevice so the ExtentReader sees any staged extent-map
    // changes made earlier in the same txn (e.g. by ExtentAllocator
    // when a directory has just grown into the indirect tier).
    val reader = new ExtentReader(sfs.metaDevice, ino)
    reader.physicalBlock(logical) match
      case BlockMapping.Concrete(p) =>
        sfs.writeMetadataBlock(p, buf)
      case other =>
        throw new SfsCorruptError(
          s"HTree.writeDirBlock: directory block $logical not concrete: $other",
        )
