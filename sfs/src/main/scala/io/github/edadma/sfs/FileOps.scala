package io.github.edadma.sfs

import Constants.*

/** File-level operations: allocating / freeing inodes, hard-linking,
  * unlinking, stat, and POSIX permission checks.
  *
  * These build on top of [[HTree]] (directory entries) and the
  * filesystem's inode + block bitmaps. They do *not* handle directory
  * creation — that's Phase 11's `mkdir`, which has to bootstrap
  * dot/dotdot via [[HTree.initDirectory]] and bump the parent's
  * link_count for the new "..".
  *
  * Every public operation that mutates a directory returns the
  * (possibly grown) parent inode; the caller is responsible for
  * persisting it via `sfs.writeInode(parentInodeNum, parent)` if
  * desired. Times are taken as parameters so the journal layer
  * (Phase 13) can stamp transactions atomically.
  */
object FileOps:

  // ---- type bits (subset of POSIX mode) -------------------------------

  /** S_IFREG. */
  val ModeRegular: Int = 0x8000

  /** S_IFDIR. */
  val ModeDirectory: Int = 0x4000

  /** S_IFLNK. */
  val ModeSymlink: Int = 0xa000

  /** Mask isolating the file-type bits in `mode`. */
  val ModeTypeMask: Int = 0xf000

  /** Mask isolating the permission bits in `mode` (octal 0o7777,
    * i.e. setuid/setgid/sticky + rwxrwxrwx). */
  val ModePermMask: Int = 0xfff

  /** Permission bits requested by an access check. */
  val AccessRead: Int = 4
  val AccessWrite: Int = 2
  val AccessExec: Int = 1

  // ---- create ---------------------------------------------------------

  /** Allocate a fresh regular-file (or symlink, or other-type) inode,
    * insert a directory entry for it under `parent`, and return the
    * (possibly grown) parent inode plus the new inode number.
    *
    * Rejects [[ModeDirectory]] — Phase 11's `mkdir` is the proper API
    * for directories. Rejects names that already exist (raises
    * [[SfsExistsError]] from the HTree layer).
    *
    * The new inode is written through to disk; the parent inode is
    * not — the caller decides when to persist via
    * `sfs.writeInode(parentInodeNum, parent)`. */
  def create(
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
      (mode & ModeTypeMask) != ModeDirectory,
      s"FileOps.create: cannot create a directory — use mkdir (Phase 11)",
    )
    val newInodeNum = sfs.inodeBitmap.allocate().getOrElse(
      throw new SfsNoSpaceError("FileOps.create: out of free inodes"),
    )
    val previous = sfs.readInode(newInodeNum)
    val fresh = freshInode(previous.generation + 1, mode, uid, gid, timeSec, timeNsec)
    sfs.writeInode(newInodeNum, fresh)

    val updatedParent = HTree.insert(
      parent, sfs.device, sfs.blockBitmap, parentInodeNum,
      name, newInodeNum, fileTypeFromMode(mode),
    )
    val touched = bumpMtimeCtime(updatedParent, timeSec, timeNsec)
    (touched, newInodeNum)

  // ---- unlink ---------------------------------------------------------

  /** Remove a name from `parent` and decrement the target inode's
    * `link_count`. When `link_count` reaches zero, free every data
    * block the file owned (via [[ExtentAllocator.truncate]] to size
    * zero) and clear the inode's bitmap bit so the number can be
    * reused. The target's on-disk inode bytes survive — only its
    * `link_count` reflects the freed state. (NFS-style generation
    * bumping happens on the next [[create]] that lands on this slot.)
    *
    * Refuses to unlink directories (use Phase 11's `rmdir`). Throws
    * [[SfsNotFoundError]] if no such name exists in `parent`. */
  def unlink(
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
        throw new SfsNotFoundError(s"""FileOps.unlink: name "$name" not found"""),
      )
    if childType == DirEntry.TypeDirectory then
      throw new SfsIsDirectoryError(
        s"""FileOps.unlink: "$name" is a directory — use rmdir""",
      )

    val target = sfs.readInode(childNum)
    val newLinkCount = target.linkCount - 1
    if newLinkCount > 0 then
      sfs.writeInode(childNum, target.copy(linkCount = newLinkCount, ctimeSec = timeSec, ctimeNsec = timeNsec))
    else
      val drained = ExtentAllocator.truncate(target, sfs.device, sfs.blockBitmap, 0L)
      sfs.writeInode(
        childNum,
        drained.copy(
          linkCount = 0,
          size = 0L,
          blockCount = 0,
          ctimeSec = timeSec,
          ctimeNsec = timeNsec,
        ),
      )
      sfs.inodeBitmap.clear(childNum)

    val updatedParent = HTree.delete(parent, sfs.device, sfs.blockBitmap, parentInodeNum, name)
    bumpMtimeCtime(updatedParent, timeSec, timeNsec)

  // ---- link -----------------------------------------------------------

  /** Add a hard link from `(parent, name)` to an existing non-directory
    * inode `targetInodeNum`. Bumps the target's `link_count` and
    * stamps its `ctime`. Refuses to hard-link directories (POSIX
    * forbids it; would require cycle detection at scale).
    *
    * Returns the (possibly grown) parent inode. The caller persists
    * it. Throws [[SfsExistsError]] (from HTree) if `name` is already
    * taken. */
  def link(
      parent: Inode,
      parentInodeNum: Int,
      sfs: Sfs,
      targetInodeNum: Int,
      name: String,
      timeSec: Int,
      timeNsec: Int,
  ): Inode =
    val target = sfs.readInode(targetInodeNum)
    if (target.mode & ModeTypeMask) == ModeDirectory then
      throw new SfsIsDirectoryError(
        "FileOps.link: hard-linking a directory is not supported",
      )
    val bumped = target.copy(
      linkCount = target.linkCount + 1,
      ctimeSec = timeSec,
      ctimeNsec = timeNsec,
    )
    sfs.writeInode(targetInodeNum, bumped)

    val updatedParent = HTree.insert(
      parent, sfs.device, sfs.blockBitmap, parentInodeNum,
      name, targetInodeNum, fileTypeFromMode(target.mode),
    )
    bumpMtimeCtime(updatedParent, timeSec, timeNsec)

  // ---- stat -----------------------------------------------------------

  /** Read an inode by number. */
  def stat(sfs: Sfs, inodeNum: Int): Inode = sfs.readInode(inodeNum)

  // ---- access check ---------------------------------------------------

  /** POSIX-style permission test: does `(uid, gid)` have the bits in
    * `want` (any combination of [[AccessRead]] / [[AccessWrite]] /
    * [[AccessExec]]) on `ino`? Root (`uid == 0`) is always allowed.
    *
    * Selection order matches POSIX: owner perms iff `uid` matches;
    * else group perms iff `gid` matches; else other perms. The chosen
    * triplet must contain every requested bit. */
  def canAccess(ino: Inode, uid: Int, gid: Int, want: Int): Boolean =
    if uid == 0 then return true
    val perm = ino.mode & ModePermMask
    val triplet =
      if uid == ino.uid then (perm >> 6) & 7
      else if gid == ino.gid then (perm >> 3) & 7
      else perm & 7
    (triplet & want) == want

  // ---- helpers --------------------------------------------------------

  /** Translate `mode`'s type bits into a [[DirEntry]] file_type tag. */
  def fileTypeFromMode(mode: Int): Int =
    (mode & ModeTypeMask) match
      case ModeRegular   => DirEntry.TypeRegular
      case ModeDirectory => DirEntry.TypeDirectory
      case ModeSymlink   => DirEntry.TypeSymlink
      case _             => DirEntry.TypeOther

  private def freshInode(
      generation: Int,
      mode: Int,
      uid: Int,
      gid: Int,
      timeSec: Int,
      timeNsec: Int,
  ): Inode =
    Inode(
      mode = mode,
      linkCount = 1,
      uid = uid,
      gid = gid,
      flags = 0,
      size = 0L,
      blockCount = 0,
      generation = generation,
      atimeSec = timeSec, atimeNsec = timeNsec,
      mtimeSec = timeSec, mtimeNsec = timeNsec,
      ctimeSec = timeSec, ctimeNsec = timeNsec,
      crtimeSec = timeSec, crtimeNsec = timeNsec,
      body = InodeBody.EmptyExtents,
      indirect1 = 0,
      indirect2 = 0,
      indirect3 = 0,
      xattrBlock = 0,
    )

  private def bumpMtimeCtime(ino: Inode, timeSec: Int, timeNsec: Int): Inode =
    ino.copy(
      mtimeSec = timeSec, mtimeNsec = timeNsec,
      ctimeSec = timeSec, ctimeNsec = timeNsec,
    )
