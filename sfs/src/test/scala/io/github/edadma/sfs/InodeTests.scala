package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class InodeTests extends AnyFreeSpec with Matchers:

  private def baseFile(body: InodeBody = InodeBody.EmptyExtents): Inode =
    Inode(
      mode = 0x81a4, // regular file, 0644
      linkCount = 1,
      uid = 1000,
      gid = 1000,
      flags = 0,
      size = 0L,
      blockCount = 0,
      generation = 1,
      atimeSec = 0,
      atimeNsec = 0,
      mtimeSec = 0,
      mtimeNsec = 0,
      ctimeSec = 0,
      ctimeNsec = 0,
      crtimeSec = 0,
      crtimeNsec = 0,
      body = body,
      indirect1 = 0,
      indirect2 = 0,
      indirect3 = 0,
      xattrBlock = 0,
    )

  "round-trips a basic file inode with empty extents" in {
    val ino = baseFile()
    val buf = new Array[Byte](Inode.Size)
    Inode.pack(ino, buf, 0)
    val got = Inode.unpack(buf, 0)
    got.mode shouldBe 0x81a4
    got.linkCount shouldBe 1
    got.uid shouldBe 1000
    got.gid shouldBe 1000
    got.body shouldBe InodeBody.EmptyExtents
  }

  "round-trips an inode with non-trivial inline extents" in {
    val xs = (0 until InlineExtents).map(i => Extent(start = i + 1, count = i + 1))
    val ino = baseFile(InodeBody.Extents(xs)).copy(size = 999L, blockCount = 8)
    val buf = new Array[Byte](Inode.Size)
    Inode.pack(ino, buf, 0)
    val got = Inode.unpack(buf, 0)
    got.body shouldBe InodeBody.Extents(xs)
    got.size shouldBe 999L
    got.blockCount shouldBe 8
  }

  "round-trips an inline symlink" in {
    val ino = baseFile()
      .copy(
        mode = 0xa1ff, // symlink, 0777
        flags = InodeFlagInlineSymlink,
        body = InodeBody.InlineSymlink("/etc/passwd"),
        size = 11L,
      )
    val buf = new Array[Byte](Inode.Size)
    Inode.pack(ino, buf, 0)
    val got = Inode.unpack(buf, 0)
    got.body shouldBe InodeBody.InlineSymlink("/etc/passwd")
    (got.flags & InodeFlagInlineSymlink) should not be 0
  }

  "rejects flags + body mismatch (Extents body with INLINE_SYMLINK set)" in {
    an[IllegalArgumentException] should be thrownBy baseFile().copy(flags = InodeFlagInlineSymlink)
  }

  "rejects flags + body mismatch (InlineSymlink body without INLINE_SYMLINK set)" in {
    an[IllegalArgumentException] should be thrownBy
      baseFile().copy(body = InodeBody.InlineSymlink("/x"))
  }

  "rejects an over-long inline symlink target" in {
    an[IllegalArgumentException] should be thrownBy
      baseFile().copy(
        flags = InodeFlagInlineSymlink,
        body = InodeBody.InlineSymlink("a" * 128),
      )
  }

  "stores CRC at offset 208 over bytes 0..207" in {
    val buf = new Array[Byte](Inode.Size)
    Inode.pack(baseFile(), buf, 0)
    val storedCrc = Le.u32(buf, Inode.CrcOff)
    val tmp = buf.clone()
    Le.putU32(tmp, Inode.CrcOff, 0)
    val recomputed = Crc32.compute(tmp, 0, Inode.CrcCoverage)
    storedCrc shouldBe recomputed
  }

  "rejects a bad CRC" in {
    val buf = new Array[Byte](Inode.Size)
    Inode.pack(baseFile(), buf, 0)
    buf(0) = (buf(0) ^ 0xff).toByte
    a[SfsCorruptError] should be thrownBy Inode.unpack(buf, 0)
  }

  "indirect pointers and xattr_block round-trip at the right offsets" in {
    val ino = baseFile().copy(indirect1 = 100, indirect2 = 200, indirect3 = 300, xattrBlock = 400)
    val buf = new Array[Byte](Inode.Size)
    Inode.pack(ino, buf, 0)
    Le.u32(buf, 192) shouldBe 100
    Le.u32(buf, 196) shouldBe 200
    Le.u32(buf, 200) shouldBe 300
    Le.u32(buf, 204) shouldBe 400
    val got = Inode.unpack(buf, 0)
    got.indirect1 shouldBe 100
    got.indirect2 shouldBe 200
    got.indirect3 shouldBe 300
    got.xattrBlock shouldBe 400
  }

  "size is little-endian 64-bit at offset 16" in {
    val ino = baseFile().copy(size = 0x1122334455667788L)
    val buf = new Array[Byte](Inode.Size)
    Inode.pack(ino, buf, 0)
    Le.u64(buf, 16) shouldBe 0x1122334455667788L
  }

  "trailing reserved bytes (212..255) are zeroed" in {
    val buf = Array.fill[Byte](Inode.Size)(0xff.toByte)
    Inode.pack(baseFile(), buf, 0)
    for i <- 212 until Inode.Size do buf(i) shouldBe 0.toByte
  }
