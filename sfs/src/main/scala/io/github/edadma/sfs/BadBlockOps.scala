package io.github.edadma.sfs

import Constants.*

/** Bad-block tracking. A small persistent record of disk blocks the
  * filesystem must never hand out from the allocator.
  *
  * **On-disk form.** Inode 1 ([[Constants.InoBadBlocks]]) is a regular
  * file whose content is a packed array of little-endian u32 block
  * addresses. The file's `size` therefore equals `4 * count`. Adding a
  * block appends 4 bytes (rewriting / extending the inode body and
  * extents through [[FileIO.writeFile]]); removing a block (not yet
  * supported) would rewrite the whole file.
  *
  * **Runtime model.** [[Sfs.mount]] calls [[loadAtMount]] which reads
  * the file once and `set`s every listed block in [[Sfs.blockBitmap]].
  * From that point on the regular [[Bitmap]] allocator naturally skips
  * those bits — there is no separate Set-based filter on the hot
  * `allocate` path. New marks via [[mark]] update both the file *and*
  * the bitmap inside a single [[Sfs.withTransaction]] so the two stay
  * consistent across crash + recovery.
  *
  * **No migration of in-use data.** Marking a block bad does not move
  * the file currently using it. If the block is already concrete in
  * some file's extent map, that file's reads will return what the
  * underlying device returns (which is presumably garbage — the
  * point of marking the block bad is that the user knows it can't
  * be trusted). A future fsck phase can flag the inconsistency. */
object BadBlockOps:

  /** Walk inode 1's file content, return every recorded bad-block
    * address. Empty result for a freshly formatted volume. Reads
    * through `sfs.metaDevice` so an active txn's staged writes are
    * visible. */
  def list(sfs: Sfs, caller: Caller = Caller.Root): IndexedSeq[Int] =
    if !caller.isRoot then
      throw new SfsPermissionError(s"BadBlockOps.list: requires root (caller uid=${caller.uid})")
    val ino = sfs.readInode(InoBadBlocks)
    if ino.size == 0L then IndexedSeq.empty
    else
      val len = ino.size
      require(
        len <= Int.MaxValue.toLong && (len % 4L) == 0L,
        s"bad-blocks file size $len is not a positive multiple of 4",
      )
      val intLen = len.toInt
      val data = FileIO.readFile(ino, sfs.metaDevice, 0L, intLen)
      val n = intLen / 4
      val out = new Array[Int](n)
      var i = 0
      while i < n do
        out(i) = Le.u32(data, i * 4)
        i += 1
      out.toIndexedSeq

  /** Append `blockAddr` to inode 1's file (so it survives unmount) and
    * `set` its bit in the in-memory + on-disk block bitmap. Idempotent:
    * if `blockAddr` is already marked, this is a no-op (no second copy
    * is appended).
    *
    * Throws if `blockAddr` is outside `[dataStart, totalBlocks)` —
    * marking a metadata block (superblock, bitmap, inode table,
    * journal) would make the filesystem unmountable; the caller has
    * to take the volume out first.
    *
    * Returns the (in-memory) updated bad-blocks inode. */
  def mark(
      sfs: Sfs,
      blockAddr: Int,
      timeSec: Int,
      timeNsec: Int,
      caller: Caller = Caller.Root,
  ): Inode = sfs.withTransaction {
    if !caller.isRoot then
      throw new SfsPermissionError(s"BadBlockOps.mark: requires root (caller uid=${caller.uid})")
    val layout = sfs.layout
    require(
      blockAddr >= layout.dataStart && blockAddr < layout.totalBlocks,
      s"refusing to mark $blockAddr bad: outside data region [${layout.dataStart}, ${layout.totalBlocks})",
    )

    val existing = list(sfs)
    if existing.contains(blockAddr) then sfs.readInode(InoBadBlocks)
    else
      val ino = sfs.readInode(InoBadBlocks)
      val buf = new Array[Byte](4)
      Le.putU32(buf, 0, blockAddr)
      val grown = FileIO.writeFile(ino, sfs, ino.size, buf, timeSec, timeNsec)
      sfs.writeInode(InoBadBlocks, grown)
      sfs.blockBitmap.set(blockAddr)
      grown
  }

  // ---- mount-time integration -----------------------------------------

  /** Called by [[Sfs.mount]] after the bitmaps are loaded but before
    * the volume is handed back. For each block in the bad-blocks file,
    * make sure its bitmap bit is `set` so the allocator never returns
    * it. Idempotent (safe across remounts).
    *
    * No journal writes happen here; we only mutate the in-memory
    * bitmap cache. The on-disk bitmap will absorb these bits on the
    * next mutation that goes through `withTransaction.stageInto`, or
    * on the next clean unmount via `Bitmap.flush`. */
  private[sfs] def loadAtMount(sfs: Sfs): Unit =
    val bb = list(sfs)
    var i = 0
    while i < bb.length do
      val b = bb(i)
      if b >= sfs.layout.dataStart && b < sfs.layout.totalBlocks then
        sfs.blockBitmap.set(b)
      i += 1
