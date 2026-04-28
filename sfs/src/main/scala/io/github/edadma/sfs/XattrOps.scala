package io.github.edadma.sfs

import Constants.*

/** Extended-attribute operations: a thin POSIX-shaped API
  * (`getxattr`, `setxattr`, `listxattr`, `removexattr`) layered over
  * [[XattrBlock]] and the journal.
  *
  * Storage policy: at most one xattr block per inode. The block is
  * allocated lazily on the first `setxattr` and freed on the
  * `removexattr` that empties it. While present, its address lives in
  * `Inode.xattrBlock` and `InodeFlagHasXattr` is set in `Inode.flags`.
  *
  * All mutating ops go through [[Sfs.withTransaction]] so the inode
  * write, the xattr block write, and any bitmap mutation all land
  * atomically.
  *
  * Read ops (`getxattr`, `listxattr`) read through `sfs.metaDevice` so
  * uncommitted writes within an open transaction are visible.
  */
object XattrOps:

  /** Replace flag for [[setxattr]]. `Create` fails if the name exists,
    * `Replace` fails if it doesn't, `CreateOrReplace` is unconditional. */
  enum SetMode:
    case CreateOrReplace, Create, Replace

  // ---- public API -----------------------------------------------------

  /** Look up `name` on `inodeNum`. Returns `None` when the inode has
    * no xattr block, or when the block has no entry with that name. */
  def get(sfs: Sfs, inodeNum: Int, name: String): Option[Array[Byte]] =
    require(name.nonEmpty, "xattr name must not be empty")
    val ino = sfs.readInode(inodeNum)
    if (ino.flags & InodeFlagHasXattr) == 0 then None
    else
      val entries = readXattrBlock(sfs, ino.xattrBlock, inodeNum)
      entries.find(_.name == name).map(_.value)

  /** Return every attribute name on `inodeNum`, in stored order.
    * Returns an empty sequence when there are none (or no xattr block
    * is allocated). */
  def list(sfs: Sfs, inodeNum: Int): IndexedSeq[String] =
    val ino = sfs.readInode(inodeNum)
    if (ino.flags & InodeFlagHasXattr) == 0 then IndexedSeq.empty
    else readXattrBlock(sfs, ino.xattrBlock, inodeNum).map(_.name)

  /** Set `name = value` on `inodeNum`. Allocates the xattr block on
    * first use and bumps the inode's `ctime`. Throws when the
    * resulting set wouldn't fit, or when `mode` is violated.
    *
    * On success, the inode is rewritten with the new flags +
    * `xattr_block` and the xattr block holds the updated entry list.
    * Returns the (in-memory) updated inode so the caller can chain. */
  def set(
      sfs: Sfs,
      inodeNum: Int,
      name: String,
      value: Array[Byte],
      timeSec: Int,
      timeNsec: Int,
      mode: SetMode = SetMode.CreateOrReplace,
  ): Inode = sfs.withTransaction {
    require(name.nonEmpty, "xattr name must not be empty")
    val ino = sfs.readInode(inodeNum)
    val current =
      if (ino.flags & InodeFlagHasXattr) == 0 then IndexedSeq.empty[XattrEntry]
      else readXattrBlock(sfs, ino.xattrBlock, inodeNum)

    val existingIdx = current.indexWhere(_.name == name)
    mode match
      case SetMode.Create if existingIdx >= 0 =>
        throw new SfsExistsError(s"""xattr "$name" already exists""")
      case SetMode.Replace if existingIdx < 0 =>
        throw new SfsNotFoundError(s"""xattr "$name" does not exist""")
      case _ => ()

    val newEntry = XattrEntry(name, value)
    val updated =
      if existingIdx >= 0 then current.updated(existingIdx, newEntry)
      else current :+ newEntry

    val total = updated.foldLeft(0)(_ + _.recLen)
    if total > XattrBlock.UsableSize then
      throw new SfsNoSpaceError(
        s"xattr set would consume $total bytes, exceeds usable ${XattrBlock.UsableSize}",
      )

    val (blockAddr, freshBlock) =
      if (ino.flags & InodeFlagHasXattr) != 0 then (ino.xattrBlock, false)
      else
        val a = sfs.blockBitmap.allocate().getOrElse(
          throw new SfsNoSpaceError("XattrOps.set: no free data block for xattr block"),
        )
        (a, true)

    writeXattrBlock(sfs, blockAddr.toLong, inodeNum, updated)

    val touched = ino.copy(
      flags = ino.flags | InodeFlagHasXattr,
      xattrBlock = blockAddr,
      ctimeSec = timeSec,
      ctimeNsec = timeNsec,
    )
    sfs.writeInode(inodeNum, touched)
    touched
  }

  /** Remove `name` from `inodeNum`. Throws [[SfsNotFoundError]] if it
    * isn't present. Frees the xattr block (and clears the flag +
    * pointer) when the last entry is removed. Bumps `ctime`. */
  def remove(
      sfs: Sfs,
      inodeNum: Int,
      name: String,
      timeSec: Int,
      timeNsec: Int,
  ): Inode = sfs.withTransaction {
    require(name.nonEmpty, "xattr name must not be empty")
    val ino = sfs.readInode(inodeNum)
    if (ino.flags & InodeFlagHasXattr) == 0 then
      throw new SfsNotFoundError(s"""xattr "$name" does not exist (no xattr block)""")
    val current = readXattrBlock(sfs, ino.xattrBlock, inodeNum)
    val idx = current.indexWhere(_.name == name)
    if idx < 0 then
      throw new SfsNotFoundError(s"""xattr "$name" does not exist""")
    val updated = current.patch(idx, Nil, 1)

    val touched =
      if updated.isEmpty then
        sfs.blockBitmap.clear(ino.xattrBlock)
        ino.copy(
          flags = ino.flags & ~InodeFlagHasXattr,
          xattrBlock = 0,
          ctimeSec = timeSec,
          ctimeNsec = timeNsec,
        )
      else
        writeXattrBlock(sfs, ino.xattrBlock.toLong, inodeNum, updated)
        ino.copy(ctimeSec = timeSec, ctimeNsec = timeNsec)

    sfs.writeInode(inodeNum, touched)
    touched
  }

  // ---- internals ------------------------------------------------------

  private def readXattrBlock(sfs: Sfs, blockAddr: Int, inodeNum: Int): IndexedSeq[XattrEntry] =
    require(blockAddr > 0, s"xattr block address $blockAddr is invalid")
    val buf = new Array[Byte](BlockSize)
    sfs.metaDevice.readBlock(blockAddr.toLong, buf)
    XattrBlock.unpack(buf, inodeNum)

  private def writeXattrBlock(
      sfs: Sfs,
      blockAddr: Long,
      inodeNum: Int,
      entries: Seq[XattrEntry],
  ): Unit =
    val buf = new Array[Byte](BlockSize)
    XattrBlock.pack(entries, inodeNum, buf)
    sfs.metaDevice.writeBlock(blockAddr, buf)
