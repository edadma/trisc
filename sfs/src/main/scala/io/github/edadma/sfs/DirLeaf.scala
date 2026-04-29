package io.github.edadma.sfs

import Constants.*

/** Operations on a single directory leaf block (the [[DirLeafBlock]]
  * codec is the byte-level layer; this object owns the *policy* for
  * insert / delete / iteration).
  *
  * Every leaf block is exactly `DirLeafBlock.UsableSize` bytes of
  * back-to-back [[DirEntry]] records followed by a 12-byte
  * [[DirTail]]. The sum of all `recLen` fields in the leaf is exactly
  * [[DirLeafBlock.UsableSize]] — there's no implicit "free space at
  * the end"; any space the caller hasn't claimed is donated to a
  * tombstone whose `inode == 0` and whose `recLen` covers the gap.
  *
  * Tombstones live in the entry stream like any other record: the
  * walker advances by `recLen` regardless of `inode`. Insertion can
  * shrink an existing entry's `recLen` (donating its trailing slack
  * to a new entry placed in the freed bytes) — this is the standard
  * ext-style space-reuse trick.
  *
  * Every public mutation re-stamps the tail (refreshes the CRC) so
  * the block is always self-consistent on return. Callers must
  * persist the buffer themselves.
  */
object DirLeaf:

  /** Bytes available for entries (before the 12-byte tail). */
  val UsableSize: Int = DirLeafBlock.UsableSize

  /** Build a fresh empty leaf — one giant tombstone covering all of
    * [[UsableSize]] — and stamp the tail. */
  def initEmpty(buf: Array[Byte], ownerInode: Int): Unit =
    require(buf.length == BlockSize, s"directory block must be $BlockSize bytes")
    DirEntry.pack(DirEntry.tombstone(UsableSize), buf, 0)
    DirTail.pack(buf, ownerInode)

  /** Walk every entry in the leaf (live + tombstones), in stored
    * order. Callers that want only live entries should filter
    * `e.inode != 0`. */
  def entries(buf: Array[Byte]): Vector[(Int, DirEntry)] =
    require(buf.length == BlockSize, s"directory block must be $BlockSize bytes")
    val out = Vector.newBuilder[(Int, DirEntry)]
    var off = 0
    while off < UsableSize do
      val e = DirEntry.unpack(buf, off)
      if off + e.recLen > UsableSize then
        throw new SfsCorruptError(
          s"directory leaf entry at $off has recLen ${e.recLen} that overruns usable region",
        )
      out += ((off, e))
      off += e.recLen
    out.result()

  /** Find a live entry by name. Returns `(offset, entry)` if present. */
  def findByName(buf: Array[Byte], name: String): Option[(Int, DirEntry)] =
    val target = name
    val xs = entries(buf)
    var i = 0
    while i < xs.length do
      val (off, e) = xs(i)
      if e.inode != 0 && e.name == target then return Some((off, e))
      i += 1
    None

  /** Try to insert `e` somewhere in `buf`. Returns `true` on success
    * (and re-stamps the tail); returns `false` if no record has
    * enough room — caller must split the leaf.
    *
    * Algorithm (first-fit, never moves existing entries):
    *
    *   - If the current record is a *tombstone* with `recLen >= e.recLen`,
    *     replace it: write `e` at the same offset, taking the tombstone's
    *     entire `recLen`. Future inserts can shrink this record again.
    *   - If the current record is *live* with
    *     `slack = recLen - minRecLen(nameLen) >= e.recLen`, shrink it to
    *     its minimum, then place the new entry immediately after with a
    *     `recLen` covering the whole donated slack.
    *
    * In both cases the new entry's `recLen` may exceed
    * `minRecLen(e.nameLen)`; the excess is its own donatable slack. The
    * sum of every record's `recLen` is invariantly [[UsableSize]]. */
  def tryInsert(buf: Array[Byte], e: DirEntry, ownerInode: Int): Boolean =
    val xs = entries(buf)
    val needed = e.recLen
    var i = 0
    while i < xs.length do
      val (off, cur) = xs(i)
      if cur.inode == 0 then
        if cur.recLen >= needed then
          DirEntry.pack(e.copy(recLen = cur.recLen), buf, off)
          DirTail.pack(buf, ownerInode)
          return true
      else
        val keep = DirEntry.minRecLen(cur.nameLen)
        val slack = cur.recLen - keep
        if slack >= needed then
          DirEntry.pack(cur.copy(recLen = keep), buf, off)
          DirEntry.pack(e.copy(recLen = slack), buf, off + keep)
          DirTail.pack(buf, ownerInode)
          return true
      i += 1
    false

  /** Tombstone a live entry by name. The entry's bytes stay in the
    * stream — only `inode`, `nameLen`, `fileType`, and the name bytes
    * are zeroed. `recLen` is preserved so subsequent inserts can
    * reuse the space. Returns `true` if the entry existed and was
    * tombstoned, `false` if no live entry of that name was found.
    *
    * Re-stamps the tail on success. */
  def delete(buf: Array[Byte], name: String, ownerInode: Int): Boolean =
    findByName(buf, name) match
      case Some((off, e)) =>
        DirEntry.pack(DirEntry.tombstone(e.recLen), buf, off)
        DirTail.pack(buf, ownerInode)
        true
      case None => false
