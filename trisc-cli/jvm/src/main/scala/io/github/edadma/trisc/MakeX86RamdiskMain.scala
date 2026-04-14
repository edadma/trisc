package io.github.edadma.trisc

import java.nio.file.{Files, Paths}

// Create a TFS ramdisk image for x86_64 QEMU.
// Writes /tmp/slix-x86_64/ramdisk.img with basic filesystem structure.
// Run: sbt "triscCliJVM/runMain io.github.edadma.trisc.MakeX86RamdiskMain"
object MakeX86RamdiskMain:
  def main(args: Array[String]): Unit =
    val outDir = Paths.get("/tmp/slix-x86_64")
    Files.createDirectories(outDir)
    val outPath = outDir.resolve("ramdisk.img")

    val prefill =
      """/dev dir
        |/dev/tty0 char 0 0
        |/dev/null char 0 1
        |/bin dir
        |/etc dir
        |/etc/ttytab file "tty0 nsh"
        |/root dir
        |/tmp dir""".stripMargin

    val disk = TFS.format(
      blockSize = 4096,
      totalBlocks = 256,   // 1 MB
      maxInodes = 128,
      prefill = prefill,
    )

    Files.write(outPath, disk)
    System.err.println(s"wrote $outPath (${disk.length} bytes)")
