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

    val binFiles: Map[String, Array[Byte]] =
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

    // Phase 4 chunk 5 fixture: a tiny synthesized tar at /test.tar
    // exercised by slix/test/untar.c. Bytes come from the shared
    // TestTar object so x86 and aarch64 ramdisks contain bit-identical
    // archives.
    val tarBytes = TestTar.bytes
    System.err.println(s"  including /test.tar (${tarBytes.length} bytes, synthesized)")

    // Phase 1 chunk 9 fixture: ship the dynamic linker (ld-musl-aarch64.so.1)
    // for PT_INTERP-driven exec(). musl's libc.so IS the dynamic linker —
    // it's renamed at install time to /lib/ld-musl-<arch>.so.1. We do the
    // same here at ramdisk-pack time, sourcing from slix/build-musl/lib/libc.so
    // (produced by slix/build-musl.sh with --enable-shared). If the file
    // isn't present, the ramdisk just won't include it; dynamically-linked
    // tests fail but static binaries are unaffected.
    val ldMuslPath = Paths.get("slix/build-musl/lib/libc.so")
    val ldMuslBytes: Option[Array[Byte]] =
      if Files.exists(ldMuslPath) then
        val bs = Files.readAllBytes(ldMuslPath)
        System.err.println(s"  including /lib/ld-musl-aarch64.so.1 (${bs.length} bytes, from $ldMuslPath)")
        Some(bs)
      else
        System.err.println(s"  WARN: $ldMuslPath not found — /lib/ld-musl-aarch64.so.1 omitted")
        None

    // Phase 1 chunk-9/10 layout. /lib carries only ld-musl-<arch>.so.1
    // because that path is baked into every PIE binary's PT_INTERP at
    // link time (-dynamic-linker /lib/ld-musl-aarch64.so.1). Shared
    // objects (libc.so + libm.so) live in /usr/lib per FHS and are
    // resolved by SONAME via ld-musl's default search path
    // (/lib:/usr/local/lib:/usr/lib). musl unifies the math impls
    // into libc.so, so libm.so is a byte-identical copy whose SONAME
    // is "libc.so" — ld-musl dedupes by SONAME at runtime, so a
    // dhello-style link against -lm doesn't double-load libc.
    val files: Map[String, Array[Byte]] =
      binFiles
        + ("/test.tar" -> tarBytes)
        ++ ldMuslBytes.map("/lib/ld-musl-aarch64.so.1" -> _).toMap
        ++ ldMuslBytes.map("/usr/lib/libc.so" -> _).toMap
        ++ ldMuslBytes.map("/usr/lib/libm.so" -> _).toMap

    val basePrefill =
      """/dev dir
        |/dev/tty0 char 0 0
        |/dev/null char 0 1
        |/bin dir
        |/lib dir
        |/usr dir
        |/usr/lib dir
        |/etc dir
        |/etc/ttytab file "tty0 login"
        |/etc/passwd file "root:x:0:0:root:/root:/nsh\ned:x:1000:1000:ed:/home/ed:/nsh"
        |/etc/shadow file "root:slix:3b1b8291c0bdb62febcd914f45884bca403ae1c42a4bb1c41755881f3886d158\ned:slix:c638d5b6e91f70b96934aac8d7be42363ce4ea5927f9a9bbbe2d64a8b51926b5"
        |/etc/hosts file "127.0.0.1 localhost\n::1 localhost\n"
        |/etc/pipetest.sh file "/bin/busybox cat /etc/passwd | /bin/busybox grep root | /bin/busybox wc -l\n"
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
