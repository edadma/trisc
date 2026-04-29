package io.github.edadma.sfs

import Constants.*
import java.nio.charset.StandardCharsets.UTF_8

/** Symlink operations: `symlink` (create) and `readlink` (read).
  *
  * SFS represents symlinks two ways depending on target length, both
  * teachable wins:
  *
  *   - **Inline** (target ≤ [[Constants.InlineSymlinkMax]] = 127 UTF-8
  *     bytes): the 128-byte inode union doubles as the symlink target
  *     storage, with the [[Constants.InodeFlagInlineSymlink]] flag set.
  *     No data block is allocated — the overwhelming majority of real
  *     symlinks (`/usr/bin → bin`, `libc.so.6 → libc.so.6.0.0`, etc.)
  *     fit comfortably here.
  *
  *   - **Extent** (128 .. [[Constants.PathMax]] − 1 = 4095 bytes): the
  *     flag is clear, the body is the standard inline-extents form, and
  *     the target is written byte-for-byte via [[FileIO.writeFile]].
  *
  * Path resolution and loop detection live one layer up; this module
  * only manipulates the on-disk symlink representation.
  */
object SymlinkOps:

  /** Maximum symlink target length per the spec (PATH_MAX − 1). */
  val MaxTargetBytes: Int = PathMax - 1

  /** Default permission bits assigned to fresh symlinks. POSIX
    * generally ignores symlink permissions on dereference; using
    * 0o777 matches widespread convention. */
  val DefaultPerms: Int = 0x1ff // 0o777

  /** Create a symlink under `parent` named `name` with the given
    * `target`. Returns the (possibly grown) parent inode and the new
    * symlink inode number.
    *
    * Rejects empty targets and targets longer than
    * [[MaxTargetBytes]]. Raises [[SfsExistsError]] if `name` already
    * exists in the parent. */
  def symlink(
      parent: Inode,
      parentInodeNum: Int,
      sfs: Sfs,
      name: String,
      target: String,
      uid: Int,
      gid: Int,
      timeSec: Int,
      timeNsec: Int,
      caller: Caller = Caller.Root,
  ): (Inode, Int) = sfs.withTransaction {
    Perms.requireAccess(caller, parent, FileOps.AccessWrite | FileOps.AccessExec, "SymlinkOps.symlink", s"parent inode #$parentInodeNum")
    val targetBytes = target.getBytes(UTF_8)
    require(
      targetBytes.length > 0,
      "SymlinkOps.symlink: target must be non-empty",
    )
    require(
      targetBytes.length <= MaxTargetBytes,
      s"SymlinkOps.symlink: target length ${targetBytes.length} exceeds " +
        s"PATH_MAX-1 = $MaxTargetBytes",
    )

    val newInodeNum = sfs.inodeBitmap.allocate().getOrElse(
      throw new SfsNoSpaceError("SymlinkOps.symlink: out of free inodes"),
    )
    val previous = sfs.readInode(newInodeNum)
    val baseMode = FileOps.ModeSymlink | DefaultPerms

    val finalInode =
      if targetBytes.length <= InlineSymlinkMax then
        // Fits inline — flag set, body holds the string.
        Inode(
          mode = baseMode,
          linkCount = 1,
          uid = uid,
          gid = gid,
          flags = InodeFlagInlineSymlink,
          size = targetBytes.length.toLong,
          blockCount = 0,
          generation = previous.generation + 1,
          atimeSec = timeSec, atimeNsec = timeNsec,
          mtimeSec = timeSec, mtimeNsec = timeNsec,
          ctimeSec = timeSec, ctimeNsec = timeNsec,
          crtimeSec = timeSec, crtimeNsec = timeNsec,
          body = InodeBody.InlineSymlink(target),
          indirect1 = 0, indirect2 = 0, indirect3 = 0, xattrBlock = 0,
        )
      else
        // Long target — allocate data blocks and write through FileIO.
        val skeleton = Inode(
          mode = baseMode,
          linkCount = 1,
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
        val written = FileIO.writeFile(
          skeleton, sfs,
          offset = 0L, bytes = targetBytes,
          timeSec = timeSec, timeNsec = timeNsec,
        )
        // FileIO stamps mtime/ctime too — preserve our crtime/atime.
        written.copy(
          atimeSec = timeSec, atimeNsec = timeNsec,
          crtimeSec = timeSec, crtimeNsec = timeNsec,
        )

    sfs.writeInode(newInodeNum, finalInode)

    val updatedParent = HTree.insert(
      parent, sfs, parentInodeNum,
      name, newInodeNum, DirEntry.TypeSymlink,
    )
    val touched = updatedParent.copy(
      mtimeSec = timeSec, mtimeNsec = timeNsec,
      ctimeSec = timeSec, ctimeNsec = timeNsec,
    )
    (touched, newInodeNum)
  }

  /** Read the target string of a symlink inode. The inode's mode must
    * encode S_IFLNK; otherwise raises [[SfsNotSymlinkError]]. */
  def readlink(ino: Inode, dev: BlockDevice): String =
    if (ino.mode & FileOps.ModeTypeMask) != FileOps.ModeSymlink then
      throw new SfsNotSymlinkError(
        s"SymlinkOps.readlink: inode is not a symlink (mode = 0x${ino.mode.toHexString})",
      )
    if (ino.flags & InodeFlagInlineSymlink) != 0 then
      ino.body match
        case InodeBody.InlineSymlink(target) => target
        case other =>
          throw new SfsCorruptError(
            s"SymlinkOps.readlink: INLINE_SYMLINK flag set but body is $other",
          )
    else
      val bytes = FileIO.readFile(ino, dev, offset = 0L, len = ino.size.toInt)
      new String(bytes, UTF_8)
