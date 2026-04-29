package io.github.edadma.sfs

/** Identity of a caller invoking a public SFS operation, used by
  * Phase 17d's permission enforcement. The kernel (or test driver)
  * supplies a [[Caller]] for every op; the underlying inode's
  * `(uid, gid)` and permission bits are then consulted via
  * [[FileOps.canAccess]] / [[Perms.requireAccess]].
  *
  * Root (`uid == 0`) bypasses all permission checks. POSIX semantics:
  * the *effective* uid/gid is what matters for access decisions.
  */
final case class Caller(uid: Int, gid: Int):
  def isRoot: Boolean = uid == 0

object Caller:
  /** Convenience constant for tests and internal recovery code that
    * must always succeed regardless of inode permissions. */
  val Root: Caller = Caller(0, 0)
