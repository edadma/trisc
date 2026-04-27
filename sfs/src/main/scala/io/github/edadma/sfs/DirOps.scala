package io.github.edadma.sfs

import Constants.*

/** Directory-level operations: `mkdir`, `rmdir`, `readdir`, `rename`.
  *
  * Builds on [[HTree]] (entry layout) and the bitmaps. The
  * link-count bookkeeping for `..` is owned here:
  *
  *   - mkdir: child.linkCount = 2 ("." + entry-in-parent);
  *     parent.linkCount += 1 (".." back-pointer).
  *   - rmdir: child.linkCount = 0 → free; parent.linkCount -= 1.
  *
  * Times come in as parameters so the journal layer (Phase 13) can
  * stamp atomically. Like [[FileOps]], every mutation returns the
  * (possibly grown) parent inode; the caller persists it. */
object DirOps:

  // ---- mkdir ----------------------------------------------------------

  /** Create a new directory under `parent` named `name`. Allocates a
    * fresh inode, calls [[HTree.initDirectory]] to lay down its root +
    * leaf blocks, sets `link_count = 2`, and bumps the parent's
    * `link_count` for the new "..". Returns the updated parent inode
    * and the new directory's inode number. */
  def mkdir(
      parent: Inode,
      parentInodeNum: Int,
      sfs: Sfs,
      name: String,
      mode: Int,
      uid: Int,
      gid: Int,
      timeSec: Int,
      timeNsec: Int,
  ): (Inode, Int) =
    require(
      (mode & FileOps.ModeTypeMask) == 0 ||
        (mode & FileOps.ModeTypeMask) == FileOps.ModeDirectory,
      "DirOps.mkdir: mode must encode S_IFDIR (or no type bits, in which case S_IFDIR is added)",
    )
    val effectiveMode =
      if (mode & FileOps.ModeTypeMask) == 0 then mode | FileOps.ModeDirectory
      else mode

    val newInodeNum = sfs.inodeBitmap.allocate().getOrElse(
      throw new SfsNoSpaceError("DirOps.mkdir: out of free inodes"),
    )
    val previous = sfs.readInode(newInodeNum)

    // Skeleton inode — HTree.initDirectory will append two data blocks.
    val skeleton = Inode(
      mode = effectiveMode,
      linkCount = 2,
      uid = uid,
      gid = gid,
      flags = 0,
      size = 0L,
      blockCount = 0,
      generation = previous.generation + 1,
      atimeSec = timeSec, atimeNsec = timeNsec,
      mtimeSec = timeSec, mtimeNsec = timeNsec,
      ctimeSec = timeSec, ctimeNsec = timeNsec,
      crtimeSec = timeSec, crtimeNsec = timeNsec,
      body = InodeBody.EmptyExtents,
      indirect1 = 0, indirect2 = 0, indirect3 = 0, xattrBlock = 0,
    )
    val laidOut = HTree.initDirectory(
      skeleton, sfs.device, sfs.blockBitmap,
      ownerInode = newInodeNum, parentInode = parentInodeNum,
    )
    val sized = laidOut.copy(
      size = 2L * BlockSize,
      blockCount = (2 * BlockSize) / 512,
    )
    sfs.writeInode(newInodeNum, sized)

    val parentWithEntry = HTree.insert(
      parent, sfs.device, sfs.blockBitmap, parentInodeNum,
      name, newInodeNum, DirEntry.TypeDirectory,
    )
    val parentTouched = parentWithEntry.copy(
      linkCount = parentWithEntry.linkCount + 1,
      mtimeSec = timeSec, mtimeNsec = timeNsec,
      ctimeSec = timeSec, ctimeNsec = timeNsec,
    )
    (parentTouched, newInodeNum)

  // ---- rmdir ----------------------------------------------------------

  /** Remove an empty subdirectory. The target must contain only "." and
    * "..". On success: free the directory's data blocks, clear its
    * inode-bitmap bit, remove the entry from `parent`, and decrement
    * `parent.link_count` (the going-away ".."). */
  def rmdir(
      parent: Inode,
      parentInodeNum: Int,
      sfs: Sfs,
      name: String,
      timeSec: Int,
      timeNsec: Int,
  ): Inode =
    val (childNum, childType) = HTree
      .lookup(parent, sfs.device, parentInodeNum, name)
      .getOrElse(
        throw new SfsNotFoundError(s"""DirOps.rmdir: name "$name" not found"""),
      )
    if childType != DirEntry.TypeDirectory then
      throw new SfsNotDirectoryError(
        s"""DirOps.rmdir: "$name" is not a directory""",
      )
    if childNum == parentInodeNum then
      throw new IllegalArgumentException(
        s"""DirOps.rmdir: refusing to remove "$name" (would unlink ourselves)""",
      )
    val child = sfs.readInode(childNum)
    val nonSpecial = HTree.list(child, sfs.device, childNum)
      .count(e => e.name != "." && e.name != "..")
    if nonSpecial > 0 then
      throw new SfsNotEmptyError(
        s"""DirOps.rmdir: "$name" is not empty ($nonSpecial entries)""",
      )

    val drained = ExtentAllocator.truncate(child, sfs.device, sfs.blockBitmap, 0L)
    sfs.writeInode(
      childNum,
      drained.copy(
        linkCount = 0,
        size = 0L,
        blockCount = 0,
        ctimeSec = timeSec, ctimeNsec = timeNsec,
      ),
    )
    sfs.inodeBitmap.clear(childNum)

    val parentRemoved = HTree.delete(
      parent, sfs.device, sfs.blockBitmap, parentInodeNum, name,
    )
    parentRemoved.copy(
      linkCount = parentRemoved.linkCount - 1,
      mtimeSec = timeSec, mtimeNsec = timeNsec,
      ctimeSec = timeSec, ctimeNsec = timeNsec,
    )

  // ---- readdir --------------------------------------------------------

  /** Return every directory entry — including dot/dotdot — for the
    * given directory inode. Order is implementation-defined per POSIX
    * `getdents`; the current implementation walks index_entries
    * left-to-right and emits each leaf's live records in stored order. */
  def readdir(
      ino: Inode,
      dev: BlockDevice,
      ownerInode: Int,
  ): Vector[DirEntry] =
    HTree.list(ino, dev, ownerInode)

  // ---- rename ---------------------------------------------------------

  /** Atomic-ish rename of `(oldParent, oldName)` to `(newParent, newName)`.
    *
    * Rules (a non-journal best-effort approximation of POSIX):
    *
    *   - oldName must exist in oldParent — else [[SfsNotFoundError]].
    *   - If newName already exists in newParent and refers to the same
    *     inode as oldName, this is a no-op (returns both parents
    *     unchanged except for an mtime bump).
    *   - If newName exists and is a directory, we refuse — overwriting
    *     a directory requires checking it's empty + cross-tree work
    *     (Phase 13 journaling territory). Use [[rmdir]] first.
    *   - If newName exists and is a regular file (not a directory),
    *     it's silently unlinked (POSIX `rename(2)` semantics).
    *   - If oldName is a directory and is being moved across parents,
    *     update its `..` entry to point at newParent and adjust both
    *     parents' link_counts. Same-parent rename of a directory does
    *     NOT need link_count or `..` changes.
    *
    * Returns `(updatedOldParent, updatedNewParent)` — when both inputs
    * refer to the same parent, both returned values reflect every
    * mutation cumulatively (i.e. the second is the canonical one to
    * persist; the first is included for symmetry). */
  def rename(
      oldParent: Inode,
      oldParentInodeNum: Int,
      newParent: Inode,
      newParentInodeNum: Int,
      sfs: Sfs,
      oldName: String,
      newName: String,
      timeSec: Int,
      timeNsec: Int,
  ): (Inode, Inode) =
    val sameParent = oldParentInodeNum == newParentInodeNum

    val (oldChildNum, oldChildType) = HTree
      .lookup(oldParent, sfs.device, oldParentInodeNum, oldName)
      .getOrElse(
        throw new SfsNotFoundError(
          s"""DirOps.rename: source name "$oldName" not found""",
        ),
      )

    // No-op? Same inode same name same parent → just bump mtime.
    if sameParent && oldName == newName then
      val touched = oldParent.copy(
        mtimeSec = timeSec, mtimeNsec = timeNsec,
        ctimeSec = timeSec, ctimeNsec = timeNsec,
      )
      return (touched, touched)

    val newParentMaybeUnlinked =
      HTree.lookup(newParent, sfs.device, newParentInodeNum, newName) match
        case None => newParent
        case Some((existingNum, existingType)) =>
          if existingNum == oldChildNum then
            // Renaming over the same inode — drop the source name only.
            // Our delete-then-insert below will handle the rest.
            newParent
          else if existingType == DirEntry.TypeDirectory then
            throw new SfsIsDirectoryError(
              s"""DirOps.rename: target "$newName" is a directory""" +
                " — call rmdir first",
            )
          else if oldChildType == DirEntry.TypeDirectory then
            throw new SfsNotDirectoryError(
              s"""DirOps.rename: cannot rename directory "$oldName"""" +
                s""" over non-directory "$newName"""",
            )
          else
            // Overwrite: unlink the existing target. unlink takes care
            // of inode/block freeing and dir-entry removal.
            FileOps.unlink(newParent, newParentInodeNum, sfs, newName, timeSec, timeNsec)

    // Splice the new entry into newParent (or whichever parent that is).
    val newParentWithEntry = HTree.insert(
      newParentMaybeUnlinked, sfs.device, sfs.blockBitmap, newParentInodeNum,
      newName, oldChildNum, oldChildType,
    )

    // Now drop the old entry from oldParent (which may be the same
    // physical inode as newParentWithEntry if sameParent).
    val oldParentBase = if sameParent then newParentWithEntry else oldParent
    val oldParentRemoved = HTree.delete(
      oldParentBase, sfs.device, sfs.blockBitmap, oldParentInodeNum, oldName,
    )

    // Cross-parent move of a directory: fix the child's '..' and adjust
    // both parents' linkCounts. Same-parent moves leave both alone.
    val (finalOldParent, finalNewParent) =
      if !sameParent && oldChildType == DirEntry.TypeDirectory then
        repointChildDotDot(sfs, oldChildNum, newParentInodeNum, timeSec, timeNsec)
        val oldAdjusted = oldParentRemoved.copy(
          linkCount = oldParentRemoved.linkCount - 1,
        )
        val newAdjusted = newParentWithEntry.copy(
          linkCount = newParentWithEntry.linkCount + 1,
        )
        (oldAdjusted, newAdjusted)
      else if sameParent then
        // Single physical parent inode; the cumulative state lives in
        // oldParentRemoved (which was layered on top of
        // newParentWithEntry).
        (oldParentRemoved, oldParentRemoved)
      else
        (oldParentRemoved, newParentWithEntry)

    // Bump mtime/ctime on whichever parents were affected.
    val timestamped = (
      finalOldParent.copy(
        mtimeSec = timeSec, mtimeNsec = timeNsec,
        ctimeSec = timeSec, ctimeNsec = timeNsec,
      ),
      finalNewParent.copy(
        mtimeSec = timeSec, mtimeNsec = timeNsec,
        ctimeSec = timeSec, ctimeNsec = timeNsec,
      ),
    )
    timestamped

  /** Update a directory's `..` entry to point at a new parent.
    * Re-reads the child inode, rewrites the `..` entry's leaf in
    * place via DirLeaf.delete + DirLeaf.tryInsert (or directly
    * patching the root block, since `..` always lives there). */
  private def repointChildDotDot(
      sfs: Sfs,
      childInodeNum: Int,
      newParentInodeNum: Int,
      timeSec: Int,
      timeNsec: Int,
  ): Unit =
    val child = sfs.readInode(childInodeNum)
    val rootPhys = new ExtentReader(sfs.device, child).physicalBlock(0L) match
      case BlockMapping.Concrete(p) => p
      case other =>
        throw new SfsCorruptError(
          s"DirOps.rename: directory inode $childInodeNum has no concrete root block: $other",
        )
    val buf = new Array[Byte](BlockSize)
    sfs.device.readBlock(rootPhys, buf)
    val root = DirRootBlock.unpack(buf, childInodeNum)
    val newDotDot = DirEntry(newParentInodeNum, DirEntry.TypeDirectory, "..")
    DirRootBlock.pack(root.copy(dotdot = newDotDot), childInodeNum, buf)
    sfs.device.writeBlock(rootPhys, buf)
    sfs.writeInode(
      childInodeNum,
      child.copy(ctimeSec = timeSec, ctimeNsec = timeNsec),
    )
