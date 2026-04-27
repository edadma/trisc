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
  * Phase 9c only supports `treeDepth = 0` with arbitrarily many leaves
  * referenced directly from the root's index_entries. Leaf splits are
  * Phase 9d; tree-depth promotion is Phase 9e; index splits are Phase 9f.
  *
  * Every public mutation returns an updated [[Inode]] (possibly with new
  * blocks appended). Time stamps and link-count bookkeeping live one
  * layer up — Phase 10 owns those.
  */
object HTree:

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
    * grown) inode. Throws [[SfsCorruptError]] if a leaf split is needed
    * (deferred to chunk 9d). */
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
    val leafBlock = leafBlockFor(root, Fnv1a.hash(name))
    val leafBuf = readDirBlock(ino, dev, leafBlock.toLong)
    DirTail.verify(leafBuf, ownerInode)
    if DirLeaf.findByName(leafBuf, name).isDefined then
      throw new SfsExistsError(s"""HTree.insert: name "$name" already exists""")

    val entry = DirEntry(childInode, fileType, name)
    if !DirLeaf.tryInsert(leafBuf, entry, ownerInode) then
      throw new SfsCorruptError(
        "HTree.insert: leaf is full — leaf split is not yet implemented (Phase 9d)",
      )
    writeDirBlock(ino, dev, leafBlock.toLong, leafBuf)
    ino

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
