package io.github.edadma.sfs

/** Snapshot of filesystem-wide capacity and usage, returned by
  * [[Sfs.statfs]]. Modeled after the subset of POSIX `struct statvfs`
  * that SFS can answer cheaply: block size, block counts, and inode
  * counts.
  *
  * `freeBlocks` and `freeInodes` are read from the live in-memory
  * bitmaps, not the on-disk superblock fields — those are only refreshed
  * at unmount, so they would lag any in-flight allocations.
  */
final case class StatfsInfo(
    blockSize: Int,
    totalBlocks: Int,
    freeBlocks: Int,
    totalInodes: Int,
    freeInodes: Int,
):
  def usedBlocks: Int = totalBlocks - freeBlocks
  def usedInodes: Int = totalInodes - freeInodes
