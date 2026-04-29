package io.github.edadma.sfs

import Constants.*

/** The 12-byte tail every directory block (root, index, leaf) carries.
  *
  * Layout (relative to block end):
  * {{{
  *   -12   4   magic       0x53465344 ("SFSD")
  *   -8    4   inode_num   owning directory inode
  *   -4    4   crc32       over bytes 0 .. (BlockSize - 4), i.e. everything
  *                          before the crc32 field itself
  * }}}
  *
  * Including the owning inode in the checksum prevents block-swap attacks:
  * a structurally valid directory block from a *different* directory will
  * fail this check, so an attacker can't substitute one for another and
  * have it pass authentication.
  */
object DirTail:

  /** Tail size in bytes. */
  val Size: Int = 12

  /** Usable bytes before the tail. */
  val UsableSize: Int = BlockSize - Size

  /** Stamp the magic + inode_num at `BlockSize-12`, recompute CRC over the
    * first `BlockSize-4` bytes, and write it at `BlockSize-4`. The block is
    * mutated in place; pre-existing content of the tail region is overwritten.
    */
  def pack(buf: Array[Byte], ownerInode: Int): Unit =
    require(buf.length == BlockSize, s"directory block must be $BlockSize bytes")
    val magicOff = BlockSize - 12
    val inoOff = BlockSize - 8
    val crcOff = BlockSize - 4
    Le.putU32(buf, magicOff, MagicDirTail)
    Le.putU32(buf, inoOff, ownerInode)
    Le.putU32(buf, crcOff, 0) // zero before computing
    val crc = Crc32.compute(buf, 0, BlockSize - 4)
    Le.putU32(buf, crcOff, crc)

  /** Verify magic, owner inode, and CRC. Throws [[SfsCorruptError]] on
    * mismatch. Returns nothing on success. */
  def verify(buf: Array[Byte], expectedInode: Int): Unit =
    require(buf.length == BlockSize, s"directory block must be $BlockSize bytes")
    val magicOff = BlockSize - 12
    val inoOff = BlockSize - 8
    val crcOff = BlockSize - 4
    val gotMagic = Le.u32(buf, magicOff)
    if gotMagic != MagicDirTail then
      throw new SfsCorruptError(
        f"directory tail magic mismatch: expected 0x$MagicDirTail%08x, got 0x$gotMagic%08x",
      )
    val gotInode = Le.u32(buf, inoOff)
    if gotInode != expectedInode then
      throw new SfsCorruptError(
        s"directory tail inode mismatch: expected $expectedInode, got $gotInode",
      )
    val storedCrc = Le.u32(buf, crcOff)
    Le.putU32(buf, crcOff, 0)
    val computedCrc = Crc32.compute(buf, 0, BlockSize - 4)
    Le.putU32(buf, crcOff, storedCrc) // restore caller's view of the buffer
    if storedCrc != computedCrc then
      throw new SfsCorruptError(
        f"directory tail CRC mismatch: stored 0x$storedCrc%08x, computed 0x$computedCrc%08x",
      )
