package io.github.edadma.sfs

import Constants.*

/** Leaf block helpers — variable-length [[DirEntry]] records packed into
  * the first [[DirTail.UsableSize]] bytes of a 4 KiB block, followed by
  * the standard 12-byte [[DirTail]].
  *
  * The sequence of entries must cover *exactly* `UsableSize` bytes — every
  * leaf is either fully packed or has its last entry's `recLen` extended
  * to absorb the leftover space (the "trailing tombstone" trick from ext4).
  *
  * Phase 2 doesn't synthesize that tombstone for you: the caller passes in
  * the entries with the recLens it wants stored. The HTree code in Phase 9
  * will own the policy for filling, splitting, and tombstoning.
  */
object DirLeafBlock:

  /** Bytes available for entries (before the 12-byte tail). */
  val UsableSize: Int = DirTail.UsableSize

  /** Pack `entries` end-to-end starting at offset 0; their recLens must
    * sum to exactly `UsableSize`. Then stamp the directory tail. */
  def pack(entries: Seq[DirEntry], ownerInode: Int, buf: Array[Byte]): Unit =
    require(buf.length == BlockSize, s"directory block must be $BlockSize bytes")
    val total = entries.foldLeft(0)(_ + _.recLen)
    require(
      total == UsableSize,
      s"directory leaf entries must sum to $UsableSize bytes, got $total",
    )
    var off = 0
    for e <- entries do
      DirEntry.pack(e, buf, off)
      off += e.recLen
    DirTail.pack(buf, ownerInode)

  /** Verify the tail, then walk entries by recLen until the usable region
    * is exhausted. Tombstones (inode = 0) are returned as-is. */
  def unpack(buf: Array[Byte], expectedInode: Int): IndexedSeq[DirEntry] =
    require(buf.length == BlockSize, s"directory block must be $BlockSize bytes")
    DirTail.verify(buf, expectedInode)
    val out = IndexedSeq.newBuilder[DirEntry]
    var off = 0
    while off < UsableSize do
      val e = DirEntry.unpack(buf, off)
      if off + e.recLen > UsableSize then
        throw new SfsCorruptError(
          s"directory leaf entry at $off has recLen ${e.recLen} that overruns usable region",
        )
      out += e
      off += e.recLen
    out.result()
