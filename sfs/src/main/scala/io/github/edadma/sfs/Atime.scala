package io.github.edadma.sfs

/** The Linux-style **relatime** rule for access-time updates. Used by
  * read-path ops ([[FileIO.readFile]] for files, [[HTree.list]] for
  * directories) to refresh `atime` only when it would convey new
  * information.
  *
  * The rule, in plain terms: bump `atime` to "now" if and only if any
  * of:
  *   1. `atime < mtime` — the file has been modified since we last
  *      recorded a read; the existing atime no longer corresponds to
  *      the current data.
  *   2. `atime < ctime` — the inode metadata has changed since the
  *      last recorded read (e.g. permission flip, link count change).
  *   3. `now - atime >= 86400` — at least 24 hours have passed since
  *      the last bump; cap staleness so backup tools that key off
  *      atime still see useful values.
  *
  * Otherwise atime is left alone, sparing a metadata write per read.
  *
  * Symlinks intentionally do not get atime updates here; `readlink`
  * is a no-touch op per POSIX. */
object Atime:

  /** 24 hours in seconds — the relatime "max staleness" threshold. */
  val OneDaySec: Int = 86400

  /** Decide whether the inode's atime should be refreshed to
    * `(nowSec, nowNsec)` per the relatime rule, and if so return the
    * inode with `atimeSec/Nsec` updated. Otherwise return `ino` as-is.
    *
    * Caller is responsible for persisting the returned inode if it
    * differs from the input. */
  def relatimeUpdate(
      ino: Inode,
      nowSec: Int,
      nowNsec: Int,
  ): Inode =
    val should =
      ino.atimeSec < ino.mtimeSec ||
        ino.atimeSec < ino.ctimeSec ||
        (nowSec - ino.atimeSec) >= OneDaySec
    if should then ino.copy(atimeSec = nowSec, atimeNsec = nowNsec)
    else ino
