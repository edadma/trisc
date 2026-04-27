package io.github.edadma.sfs

import Constants.*

/** Root block of an HTree directory (always block 0 of the directory file).
  *
  * Layout:
  * {{{
  *   0       12    "."   directory entry         (rec_len = 12)
  *   12      12    ".."  directory entry         (rec_len = 12)
  *   24      4     reserved                      (zero)
  *   28      1     hash_version                  (0 = FNV-1a)
  *   29      1     info_length                   (always 8)
  *   30      1     tree_depth                    (0 = leaves only, 1 = one index level)
  *   31      1     flags
  *   32      8×n   index_entries[]               (hash, block)
  *   …
  *   4084    12    DirTail
  * }}}
  *
  * The dot/dotdot entries are kept in the root rather than a leaf so that
  * `getdents` on a freshly-formatted directory has no work to do beyond
  * reading the root block.
  */
final case class DirRootBlock(
    dot: DirEntry,
    dotdot: DirEntry,
    hashVersion: Int,
    treeDepth: Int,
    flags: Int,
    indexEntries: IndexedSeq[(Int, Int)],
):
  require(dot.name == ".", s"""dot entry name must be ".", got "${dot.name}"""")
  require(dot.recLen == 12, s"dot entry must be exactly 12 bytes, got ${dot.recLen}")
  require(dotdot.name == "..", s"""dotdot entry name must be "..", got "${dotdot.name}"""")
  require(dotdot.recLen == 12, s"dotdot entry must be exactly 12 bytes, got ${dotdot.recLen}")
  require(hashVersion >= 0 && hashVersion <= 0xff, s"hashVersion $hashVersion out of byte range")
  require(treeDepth >= 0 && treeDepth <= 0xff, s"treeDepth $treeDepth out of byte range")
  require(flags >= 0 && flags <= 0xff, s"flags $flags out of byte range")
  require(
    indexEntries.length <= DirRootBlock.MaxIndexEntries,
    s"too many index entries (${indexEntries.length}) for root block " +
      s"(capacity ${DirRootBlock.MaxIndexEntries})",
  )

object DirRootBlock:

  /** info_length is fixed at 8 by the spec (header bytes 24..31 inclusive). */
  val InfoLength: Int = 8

  /** Combined size of dot + dotdot. */
  val DotDotSize: Int = 24

  /** Offset where index entries begin. */
  val IndexEntriesOff: Int = 32

  /** Bytes available for index entries. */
  val IndexCapacityBytes: Int = DirTail.UsableSize - IndexEntriesOff

  /** Max number of index entries in a root block: floor(IndexCapacityBytes / 8). */
  val MaxIndexEntries: Int = IndexCapacityBytes / 8

  def pack(b: DirRootBlock, ownerInode: Int, buf: Array[Byte]): Unit =
    require(buf.length == BlockSize, s"directory block must be $BlockSize bytes")
    DirEntry.pack(b.dot, buf, 0)
    DirEntry.pack(b.dotdot, buf, 12)
    Le.putU32(buf, 24, 0) // reserved
    Le.putU8(buf, 28, b.hashVersion)
    Le.putU8(buf, 29, InfoLength)
    Le.putU8(buf, 30, b.treeDepth)
    Le.putU8(buf, 31, b.flags)
    var i = 0
    while i < b.indexEntries.length do
      val (h, blk) = b.indexEntries(i)
      Le.putU32(buf, IndexEntriesOff + i * 8, h)
      Le.putU32(buf, IndexEntriesOff + i * 8 + 4, blk)
      i += 1
    val tailFromEntries = IndexEntriesOff + i * 8
    Le.zero(buf, tailFromEntries, DirTail.UsableSize - tailFromEntries)
    DirTail.pack(buf, ownerInode)

  /** Decode a root block. The `indexEntries` field is returned at full
    * capacity, including any trailing zero-block slots. */
  def unpack(buf: Array[Byte], expectedInode: Int): DirRootBlock =
    require(buf.length == BlockSize, s"directory block must be $BlockSize bytes")
    DirTail.verify(buf, expectedInode)
    val dot = DirEntry.unpack(buf, 0)
    if dot.recLen != 12 then
      throw new SfsCorruptError(
        s"directory root: dot entry recLen must be 12, got ${dot.recLen}",
      )
    val dotdot = DirEntry.unpack(buf, 12)
    if dotdot.recLen != 12 then
      throw new SfsCorruptError(
        s"directory root: dotdot entry recLen must be 12, got ${dotdot.recLen}",
      )
    val hashVersion = Le.u8(buf, 28)
    val infoLength = Le.u8(buf, 29)
    if infoLength != InfoLength then
      throw new SfsCorruptError(
        s"directory root: info_length must be $InfoLength, got $infoLength",
      )
    val treeDepth = Le.u8(buf, 30)
    val flags = Le.u8(buf, 31)
    val entries = new Array[(Int, Int)](MaxIndexEntries)
    var i = 0
    while i < MaxIndexEntries do
      val h = Le.u32(buf, IndexEntriesOff + i * 8)
      val blk = Le.u32(buf, IndexEntriesOff + i * 8 + 4)
      entries(i) = (h, blk)
      i += 1
    DirRootBlock(dot, dotdot, hashVersion, treeDepth, flags, entries.toIndexedSeq)
