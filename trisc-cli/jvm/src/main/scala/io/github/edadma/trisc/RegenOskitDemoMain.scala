package io.github.edadma.trisc

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

// Rebuild OS demo TOFs under /tmp (login shell demo and desktop GUI demo).
// Ramdisk /bin utilities (hello, echo, cat) are built from oskit/bin when you run the CLI from the
// repo; embedded .trb in the JAR is only a fallback.
// Run from the trisc repo root:
//   sbt "triscCliJVM/runMain io.github.edadma.trisc.RegenOskitDemoMain"
//   sbt "triscCliJVM/runMain io.github.edadma.trisc.RegenOskitDemoMain login"
//   sbt "triscCliJVM/runMain io.github.edadma.trisc.RegenOskitDemoMain desktop"
// First argument: login | desktop | all (default: all).
// Then run, e.g.: sbt "triscCliJVM/run run --gui /tmp/os-login.tof"
object RegenOskitDemoMain:
  private val utf8 = StandardCharsets.UTF_8

  def main(args: Array[String]): Unit =
    val mode =
      args.headOption
        .filter(m => m == "login" || m == "desktop" || m == "all")
        .getOrElse("all")

    def writeTof(path: java.nio.file.Path, tof: TOF): Unit =
      val text = tof.serialize
      Files.write(path, text.getBytes(utf8))
      System.err.println(s"wrote $path (${text.length} chars)")

    val outLogin = Paths.get("/tmp/os-login.tof")
    val outDesktop = Paths.get("/tmp/os-desktop.tof")

    if mode == "login" || mode == "all" then writeTof(outLogin, OskitDemoBuilder.buildLoginTof())
    if mode == "desktop" || mode == "all" then writeTof(outDesktop, OskitDemoBuilder.buildDesktopTof())
