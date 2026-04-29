package io.github.edadma.trisc

import java.nio.file.{Files, Paths, Path}

// Create a TFS ramdisk image for aarch64 QEMU.
// Mirrors MakeX86RamdiskMain: includes ELF binaries found in
// /tmp/slix-aarch64/bin/ as /bin/<name>. Binaries are detected
// by ELF magic (\x7fELF), not file extension, so the flat `.bin`
// copies produced by build_prog.sh are ignored (they don't start
// with the ELF magic). The resulting image is loaded at the
// hardcoded physical address 0x50000000 via QEMU's `-device loader`
// at run time — see board/virt/build.sh and run-test.sh.
// Run: sbt "triscCliJVM/runMain io.github.edadma.trisc.MakeAarch64RamdiskMain"
object MakeAarch64RamdiskMain:
  private def isELF(path: Path): Boolean =
    val bytes = Files.readAllBytes(path)
    bytes.length >= 4 && bytes(0) == 0x7f && bytes(1) == 'E' && bytes(2) == 'L' && bytes(3) == 'F'

  def main(args: Array[String]): Unit =
    val outDir = Paths.get("/tmp/slix-aarch64")
    Files.createDirectories(outDir)
    val outPath = outDir.resolve("ramdisk.img")
    val binDir = outDir.resolve("bin")

    val files: Map[String, Array[Byte]] =
      if Files.isDirectory(binDir) then
        import scala.jdk.CollectionConverters.*
        Files.list(binDir).iterator().asScala
          .filter(p => !p.getFileName.toString.contains(".") && isELF(p))
          .map { p =>
            val name = p.getFileName.toString
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
        |/etc/ttytab file "tty0 login"
        |/etc/passwd file "root:x:0:0:root:/root:/nsh\ned:x:1000:1000:ed:/home/ed:/nsh"
        |/etc/shadow file "root:slix:3b1b8291c0bdb62febcd914f45884bca403ae1c42a4bb1c41755881f3886d158\ned:slix:c638d5b6e91f70b96934aac8d7be42363ce4ea5927f9a9bbbe2d64a8b51926b5"
        |/etc/hosts file "127.0.0.1 localhost\n::1 localhost\n"
        |/root dir
        |/home dir
        |/home/ed dir
        |/tmp dir""".stripMargin

    val fileLines = files.keys.map(path => s"$path file").mkString("\n")
    val prefill = if fileLines.nonEmpty then s"$basePrefill\n$fileLines" else basePrefill

    val disk = TFS.format(
      blockSize = 4096,
      totalBlocks = 32768, // 128 MB — well under TFS' ~256 MB cap (65535 × 4096) and gives plenty of room for the musl-test corpus on both arches
      maxInodes = 1024,
      prefill = prefill,
      files = files,
    )

    Files.write(outPath, disk)
    System.err.println(s"wrote $outPath (${disk.length} bytes, ${files.size} binaries)")
