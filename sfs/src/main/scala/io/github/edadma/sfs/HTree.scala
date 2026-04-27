package io.github.edadma.sfs

import Constants.*

/** Directory-level operations on an HTree directory. The directory file's
  * data blocks are organized as:
  *
  *   - Logical block 0: [[DirRootBlock]] (always; contains dot, dotdot, and
  *     the index_entries pointing into deeper blocks).
  *   - Logical blocks 1+: leaves (when `treeDepth = 0`) or interior index
  *     blocks (when `treeDepth >= 1`) followed by leaves.
  *
  * Phase 9c–9d support `treeDepth = 0` with arbitrarily many leaves
  * referenced directly from the root's index_entries; insert splits a
  * full leaf in two and adds a new index entry pointing at the new
  * sibling. Tree-depth promotion is Phase 9e; index splits are
  * Phase 9f.
  *
  * Every public mutation returns an updated [[Inode]] (possibly with new
  * blocks appended). Time stamps and link-count bookkeeping live one
  * layer up — Phase 10 owns those.
  */
object HTree:

  /** Ordering on hash values that treats them as unsigned 32-bit
    * integers. Used for sorting index_entries (which the lookup walker
    * also reads as unsigned) and for split-point selection. */
  private given unsignedHashOrdering: Ordering[Int] =
    (a, b) => java.lang.Integer.compareUnsigned(a, b)

  // ---- public API -----------------------------------------------------

  /** Initialize a fresh, empty directory: append two data blocks (root +
    * one empty leaf) to `ino` and write them. Returns the grown inode. */
  def initDirectory(
      ino: Inode,
      dev: BlockDevice,
      bm: Bitmap,
      ownerInode: Int,
      parentInode: Int,
  ): Inode =
    require(
      ExtentAllocator.totalBlockCount(ino, dev) == 0L,
      "initDirectory: inode already has data blocks",
    )
    val grown = ExtentAllocator.append(ino, dev, bm, 2)

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
    writeDirBlock(grown, dev, 0L, rootBuf)

    val leafBuf = new Array[Byte](BlockSize)
    DirLeaf.initEmpty(leafBuf, ownerInode)
    writeDirBlock(grown, dev, 1L, leafBuf)

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
    requireDepth0(root)
    val leafBlock = leafBlockFor(root, Fnv1a.hash(name))
    val leafBuf = readDirBlock(ino, dev, leafBlock.toLong)
    DirTail.verify(leafBuf, ownerInode)
    DirLeaf.findByName(leafBuf, name).map { case (_, e) => (e.inode, e.fileType) }

  /** Insert a name → (inode, fileType) binding. Returns the (possibly
    * grown) inode. Splits the target leaf in half if it cannot
    * accommodate the new entry. */
  def insert(
      ino: Inode,
      dev: BlockDevice,
      bm: Bitmap,
      ownerInode: Int,
      name: String,
      childInode: Int,
      fileType: Int,
  ): Inode =
    require(name != "." && name != "..", s"""HTree.insert: cannot insert "$name"""")
    val rootBuf = readDirBlock(ino, dev, 0L)
    val root = DirRootBlock.unpack(rootBuf, ownerInode)
    requireDepth0(root)
    val nameHash = Fnv1a.hash(name)
    val leafBlock = leafBlockFor(root, nameHash)
    val leafBuf = readDirBlock(ino, dev, leafBlock.toLong)
    DirTail.verify(leafBuf, ownerInode)
    if DirLeaf.findByName(leafBuf, name).isDefined then
      throw new SfsExistsError(s"""HTree.insert: name "$name" already exists""")

    val entry = DirEntry(childInode, fileType, name)
    if DirLeaf.tryInsert(leafBuf, entry, ownerInode) then
      writeDirBlock(ino, dev, leafBlock.toLong, leafBuf)
      ino
    else
      val grown = splitLeafAndRetry(
        ino, dev, bm, ownerInode, root, rootBuf,
        leafBlock, leafBuf, entry, nameHash,
      )
      grown

  /** Delete a name. Returns the (unchanged) inode. Throws
    * [[SfsNotFoundError]] if no such name exists. Refuses to delete
    * "." or "..". */
  def delete(
      ino: Inode,
      dev: BlockDevice,
      bm: Bitmap,
      ownerInode: Int,
      name: String,
  ): Inode =
    require(name != "." && name != "..", s"""HTree.delete: cannot delete "$name"""")
    val rootBuf = readDirBlock(ino, dev, 0L)
    val root = DirRootBlock.unpack(rootBuf, ownerInode)
    requireDepth0(root)
    val leafBlock = leafBlockFor(root, Fnv1a.hash(name))
    val leafBuf = readDirBlock(ino, dev, leafBlock.toLong)
    DirTail.verify(leafBuf, ownerInode)
    if !DirLeaf.delete(leafBuf, name, ownerInode) then
      throw new SfsNotFoundError(s"""HTree.delete: name "$name" not found""")
    writeDirBlock(ino, dev, leafBlock.toLong, leafBuf)
    ino

  /** Return all live directory entries — dot, dotdot, and every live
    * entry in every leaf — in implementation-defined order. */
  def list(
      ino: Inode,
      dev: BlockDevice,
      ownerInode: Int,
  ): Vector[DirEntry] =
    val rootBuf = readDirBlock(ino, dev, 0L)
    val root = DirRootBlock.unpack(rootBuf, ownerInode)
    requireDepth0(root)
    val out = Vector.newBuilder[DirEntry]
    out += root.dot
    out += root.dotdot
    for (_, leafBlock) <- liveIndexEntries(root) do
      val leafBuf = readDirBlock(ino, dev, leafBlock.toLong)
      DirTail.verify(leafBuf, ownerInode)
      for (_, e) <- DirLeaf.entries(leafBuf) do
        if e.inode != 0 then out += e
    out.result()

  // ---- split (chunk 9d) -----------------------------------------------

  /** Split a full leaf into two, register the new sibling in the root's
    * index_entries, and re-issue the original insert. Returns the
    * grown inode (one extra leaf block was appended). Throws
    * [[SfsCorruptError]] if the root's index table would overflow
    * (Phase 9e) or every entry hashes to the same value (no clean cut
    * point exists). */
  private def splitLeafAndRetry(
      ino: Inode,
      dev: BlockDevice,
      bm: Bitmap,
      ownerInode: Int,
      root: DirRootBlock,
      rootBuf: Array[Byte],
      leafBlockNum: Int,
      leafBuf: Array[Byte],
      newEntry: DirEntry,
      newEntryHash: Int,
  ): Inode =
    val live = DirLeaf.entries(leafBuf).map(_._2).filter(_.inode != 0).toVector
    val withHash = live.map(e => (Fnv1a.hash(e.name), e)).sortBy(_._1)
    val cut = findSplitIndex(withHash.map(_._1)).getOrElse(
      throw new SfsCorruptError(
        "HTree.insert: cannot split leaf — every entry shares the same hash",
      ),
    )
    val medianHash = withHash(cut)._1

    val grown = ExtentAllocator.append(ino, dev, bm, 1)
    val newLeafLogicalBlock = (ExtentAllocator.totalBlockCount(grown, dev) - 1L).toInt

    val lowHalf = withHash.take(cut).map(_._2)
    val highHalf = withHash.drop(cut).map(_._2)
    repackLeaf(leafBuf, lowHalf, ownerInode)
    writeDirBlock(grown, dev, leafBlockNum.toLong, leafBuf)

    val newLeafBuf = new Array[Byte](BlockSize)
    repackLeaf(newLeafBuf, highHalf, ownerInode)
    writeDirBlock(grown, dev, newLeafLogicalBlock.toLong, newLeafBuf)

    val updatedIndex =
      (liveIndexEntries(root) :+ ((medianHash, newLeafLogicalBlock))).sortBy(_._1)
    if updatedIndex.length > DirRootBlock.MaxIndexEntries then
      throw new SfsCorruptError(
        "HTree.insert: root index_entries are full — tree-depth promotion " +
          "(Phase 9e) is not yet implemented",
      )
    val newRoot = root.copy(indexEntries = updatedIndex)
    DirRootBlock.pack(newRoot, ownerInode, rootBuf)
    writeDirBlock(grown, dev, 0L, rootBuf)

    insert(grown, dev, bm, ownerInode, newEntry.name, newEntry.inode, newEntry.fileType)

  /** Pack `entries` (with their `recLen` fields normalized to the
    * minimum) into `buf` from offset 0, donating any leftover bytes to
    * a trailing tombstone. If `entries` is empty, the whole leaf
    * becomes one big tombstone. Re-stamps the directory tail. */
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

  /** For the depth-0 case: pick the single leaf block whose covering
    * index entry has the largest hash ≤ `h`. Index entries are kept
    * sorted by hash, with the first entry's hash being 0 to cover all
    * smaller-hash names. */
  private[sfs] def leafBlockFor(root: DirRootBlock, h: Int): Int =
    val live = liveIndexEntries(root)
    if live.isEmpty then
      throw new SfsCorruptError("HTree: directory has no leaf index entries")
    var i = 0
    var lastBlock = live.head._2
    while i < live.length do
      val (hi, blk) = live(i)
      if java.lang.Integer.compareUnsigned(hi, h) <= 0 then lastBlock = blk
      else return lastBlock
      i += 1
    lastBlock

  private def requireDepth0(root: DirRootBlock): Unit =
    if root.treeDepth != 0 then
      throw new SfsCorruptError(
        s"HTree: tree_depth ${root.treeDepth} not yet supported (chunk 9c handles only 0)",
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
      dev: BlockDevice,
      logical: Long,
      buf: Array[Byte],
  ): Unit =
    val reader = new ExtentReader(dev, ino)
    reader.physicalBlock(logical) match
      case BlockMapping.Concrete(p) =>
        dev.writeBlock(p, buf)
      case other =>
        throw new SfsCorruptError(
          s"HTree.writeDirBlock: directory block $logical not concrete: $other",
        )
