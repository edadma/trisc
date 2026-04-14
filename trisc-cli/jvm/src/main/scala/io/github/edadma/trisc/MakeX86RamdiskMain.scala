package io.github.edadma.trisc

import java.nio.file.{Files, Paths, Path}

// Create a TFS ramdisk image for x86_64 QEMU.
// Includes any .trb binaries found in /tmp/slix-x86_64/bin/ as /bin/<name>.
// Run: sbt "triscCliJVM/runMain io.github.edadma.trisc.MakeX86RamdiskMain"
object MakeX86RamdiskMain:
  def main(args: Array[String]): Unit =
    val outDir = Paths.get("/tmp/slix-x86_64")
    Files.createDirectories(outDir)
    val outPath = outDir.resolve("ramdisk.img")
    val binDir = outDir.resolve("bin")

    // Collect pre-built TRB binaries
    val files: Map[String, Array[Byte]] =
      if Files.isDirectory(binDir) then
        import scala.jdk.CollectionConverters.*
        Files.list(binDir).iterator().asScala
          .filter(_.toString.endsWith(".trb"))
          .map { p =>
            val name = p.getFileName.toString.stripSuffix(".trb")
            val path = s"/bin/$name"
            System.err.println(s"  including $path (${Files.size(p)} bytes)")
            path -> Files.readAllBytes(p)
          }
          .toMap
      else Map.empty

    val basePrefill =
      """/dev dir
        |/dev/tty0 char 0 0
        |/dev/null char 0 1
        |/bin dir
        |/etc dir
        |/etc/ttytab file "tty0 nsh"
        |/root dir
        |/tmp dir""".stripMargin

    // Add file entries for each binary (TFS.format needs "path file" lines)
    val fileLines = files.keys.map(path => s"$path file").mkString("\n")
    val prefill = if fileLines.nonEmpty then s"$basePrefill\n$fileLines" else basePrefill

    val disk = TFS.format(
      blockSize = 4096,
      totalBlocks = 256,   // 1 MB
      maxInodes = 128,
      prefill = prefill,
      files = files,
    )

    Files.write(outPath, disk)
    System.err.println(s"wrote $outPath (${disk.length} bytes, ${files.size} binaries)")
