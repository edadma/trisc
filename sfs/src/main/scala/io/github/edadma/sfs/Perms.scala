package io.github.edadma.sfs

import FileOps.{AccessExec, AccessRead, AccessWrite, ModePermMask, canAccess}

/** Phase 17d permission helpers. `requireAccess` is the single entry
  * point every public SFS op uses to translate a [[Caller]] + an
  * [[Inode]] into either silent success or [[SfsPermissionError]].
  *
  * The actual bit-level decision is delegated to
  * [[FileOps.canAccess]]; this module only adds the throw-on-denial
  * wrapper plus convenience constants and the sticky-bit check used
  * by `unlink` / `rmdir` / `rename`.
  */
object Perms:

  /** POSIX `S_ISVTX` — sticky bit on a directory means: only the
    * file's owner (or the directory's owner, or root) may unlink /
    * rename entries within it. */
  val ModeSticky: Int = 0x200

  /** Throw [[SfsPermissionError]] iff `caller` lacks the bits in
    * `want` on `ino`. Root (`uid == 0`) always passes. The `op` and
    * `what` strings appear in the exception message to help the
    * caller diagnose which permission failed. */
  def requireAccess(
      caller: Caller,
      ino: Inode,
      want: Int,
      op: String,
      what: String,
  ): Unit =
    if !canAccess(ino, caller.uid, caller.gid, want) then
      throw new SfsPermissionError(
        s"$op: caller (uid=${caller.uid}, gid=${caller.gid}) lacks ${describe(want)} on $what",
      )

  /** Sticky-bit semantics for `unlink` / `rmdir` / `rename`: when the
    * parent directory has sticky set, only the *target file's owner*
    * (or root) may remove/rename it — even if the caller has full W+X
    * on the parent. This is the `/tmp` rule. */
  def requireStickyOk(
      caller: Caller,
      parentDir: Inode,
      target: Inode,
      op: String,
      what: String,
  ): Unit =
    if caller.isRoot then return
    if (parentDir.mode & ModeSticky) == 0 then return
    if caller.uid == target.uid then return
    if caller.uid == parentDir.uid then return
    throw new SfsPermissionError(
      s"$op: sticky bit set on parent and caller (uid=${caller.uid}) does not own $what (target uid=${target.uid})",
    )

  private def describe(want: Int): String =
    val bits = List(
      if (want & AccessRead) != 0 then "R" else "",
      if (want & AccessWrite) != 0 then "W" else "",
      if (want & AccessExec) != 0 then "X" else "",
    ).filter(_.nonEmpty)
    if bits.isEmpty then "(no perms)" else bits.mkString("+")
