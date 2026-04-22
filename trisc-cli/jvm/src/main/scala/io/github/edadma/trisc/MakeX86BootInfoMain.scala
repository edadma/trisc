package io.github.edadma.trisc

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Paths, Path}

/** Create a boot info image for x86_64 QEMU boot module isolation.
  *
  * Reads pre-built server ELFs from /tmp/slix-x86_64/servers/ and packs
  * them into a SLIX boot info image matching the format used by
  * TriscCli.writeBootInfo.
  *
  * Layout:
  *   +0: magic "SLIX" (4 bytes)
  *   +4: module_count (4 bytes, LE)
  *   +8: per module (24 bytes each):
  *     +0: name (8 bytes, NUL-padded)
  *     +8: offset from image start (8 bytes, LE i64)
  *    +16: size (8 bytes, LE i64)
  *   Blob data: page-aligned after header
  *
  * Addresses are stored as offsets from image start. RS adds the
  * multiboot module base address at runtime.
  *
  * Run: sbt "triscCliJVM/runMain io.github.edadma.trisc.MakeX86BootInfoMain"
  */
object MakeX86BootInfoMain:
  // Boot order must match RS expectations
  private val serverNames = Seq("rs", "disk", "tfs", "tty", "pm", "vfs", "ds", "nic", "inet", "init")

  def main(args: Array[String]): Unit =
    val srvDir = Paths.get("/tmp/slix-x86_64/servers")
    val outPath = Paths.get("/tmp/slix-x86_64/bootinfo.img")

    // Read server ELFs in boot order
    val modules: Seq[(String, Array[Byte])] = serverNames.flatMap { name =>
      val path = srvDir.resolve(name)
      if Files.exists(path) then
        val data = Files.readAllBytes(path)
        System.err.println(f"  $name: ${data.length}%d bytes")
        Some(name -> data)
      else
        System.err.println(s"  WARNING: $name not found, skipping")
        None
    }

    if modules.isEmpty then
      System.err.println("error: no server binaries found in /tmp/slix-x86_64/servers/")
      System.exit(1)

    // Compute layout
    val headerSize = 8 + modules.length * 24
    val firstBlobOffset = (headerSize + 0xFFF) & ~0xFFF // page-align

    // Compute blob offsets
    var blobOffset = firstBlobOffset
    val entries = modules.map { case (name, blob) =>
      val offset = blobOffset
      blobOffset = ((offset + blob.length) + 0xFFF) & ~0xFFF
      (name, offset, blob.length, blob)
    }

    val totalSize = blobOffset
    val buf = ByteBuffer.allocate(totalSize)
    buf.order(ByteOrder.LITTLE_ENDIAN)

    // Magic "SLIX"
    buf.put(Array[Byte](0x53, 0x4C, 0x49, 0x58))
    // Module count
    buf.putInt(modules.length)

    // Module entries
    for (name, offset, size, _) <- entries do
      // Name (8 bytes, NUL-padded)
      val nameBytes = name.getBytes("ASCII")
      val padded = new Array[Byte](8)
      System.arraycopy(nameBytes, 0, padded, 0, math.min(nameBytes.length, 8))
      buf.put(padded)
      // Offset from image start (i64 LE)
      buf.putLong(offset.toLong)
      // Size (i64 LE)
      buf.putLong(size.toLong)

    // Blob data
    for (_, offset, _, blob) <- entries do
      buf.position(offset)
      buf.put(blob)

    Files.write(outPath, buf.array())
    System.err.println(s"wrote $outPath ($totalSize bytes, ${modules.length} modules)")
