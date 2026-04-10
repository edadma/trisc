package io.github.edadma.trisc

import java.nio.file.{Files, Paths}

// Rewrite trisc-cli/shared/src/main/resources/ramdisk/bin/*.trb from oskit sources (for classpath
// / JAR builds where repo paths are unavailable). Not required to run the demo from a checkout:
// RamdiskBinPrograms compiles oskit/bin/*.lsysl first when those files exist.
// Run from the trisc repo root: sbt "triscCliJVM/runMain io.github.edadma.trisc.RegenRamdiskBinMain"
object RegenRamdiskBinMain:
  def main(args: Array[String]): Unit =
    val root = Paths.get(args.headOption.getOrElse(".")).toAbsolutePath.normalize
    val outDir = root.resolve("trisc-cli/shared/src/main/resources/ramdisk/bin")
    Files.createDirectories(outDir)
    RamdiskBinPrograms.compileAllEmbeddedBinaries().foreach { case (abs, bytes) =>
      val short = abs.stripPrefix("/bin/")
      val path = outDir.resolve(s"$short.trb")
      Files.write(path, bytes)
      System.err.println(s"wrote $path (${bytes.length} bytes)")
    }
