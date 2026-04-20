package io.github.edadma.trisc

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Paths}

/** Create a boot info image for aarch64 QEMU boot module isolation.
  *
  * Reads pre-built .bin program/server files from /tmp/slix-aarch64/bin/
  * (raw binaries linked at 0x60000000 — see oskit/arch/aarch64/prog.ld)
  * and packs them into a SLIX boot info image.
  *
  * Layout (matches MakeX86BootInfoMain so RS-side parsing can be shared):
  *   +0: magic "SLIX" (4 bytes)
  *   +4: module_count (4 bytes LE)
  *   +8: per module (24 bytes each):
  *     +0: name (8 bytes, NUL-padded)
  *     +8: offset from image start (8 bytes LE i64)
  *    +16: size (8 bytes LE i64)
  *   Blob data: page-aligned after header
  *
  * The kernel loads the image at a known physical address (0x44000000
  * on QEMU virt) via `-device loader,file=bootinfo.img,addr=0x44000000`,
  * then converts offsets to absolute physical addresses.
  *
  * Pass module names as args; each must exist as
  * /tmp/slix-aarch64/bin/<name>.bin.
  *
  * Run: sbt "triscCliJVM/runMain io.github.edadma.trisc.MakeAarch64BootInfoMain test_cross_as"
  */
object MakeAarch64BootInfoMain:
  def main(args: Array[String]): Unit =
    if args.isEmpty then
      System.err.println("usage: MakeAarch64BootInfoMain <module> [<module> ...]")
      System.exit(1)

    val binDir  = Paths.get("/tmp/slix-aarch64/bin")
    val outPath = Paths.get("/tmp/slix-aarch64/bootinfo.img")

    val modules: Seq[(String, Array[Byte])] = args.toSeq.map { name =>
      val path = binDir.resolve(s"$name.bin")
      if !Files.exists(path) then
        System.err.println(s"error: $path not found")
        System.exit(1)
      val data = Files.readAllBytes(path)
      System.err.println(f"  $name: ${data.length}%d bytes")
      name -> data
    }

    val headerSize = 8 + modules.length * 24
    val firstBlobOffset = (headerSize + 0xFFF) & ~0xFFF // page-align

    var blobOffset = firstBlobOffset
    val entries = modules.map { case (name, blob) =>
      val offset = blobOffset
      blobOffset = ((offset + blob.length) + 0xFFF) & ~0xFFF
      (name, offset, blob.length, blob)
    }

    val totalSize = blobOffset
    val buf = ByteBuffer.allocate(totalSize)
    buf.order(ByteOrder.LITTLE_ENDIAN)

    buf.put(Array[Byte](0x53, 0x4C, 0x49, 0x58)) // "SLIX"
    buf.putInt(modules.length)

    for (name, offset, size, _) <- entries do
      val nameBytes = name.getBytes("ASCII")
      val padded = new Array[Byte](8)
      System.arraycopy(nameBytes, 0, padded, 0, math.min(nameBytes.length, 8))
      buf.put(padded)
      buf.putLong(offset.toLong)
      buf.putLong(size.toLong)

    for (_, offset, _, blob) <- entries do
      buf.position(offset)
      buf.put(blob)

    Files.write(outPath, buf.array())
    System.err.println(s"wrote $outPath ($totalSize bytes, ${modules.length} modules)")
