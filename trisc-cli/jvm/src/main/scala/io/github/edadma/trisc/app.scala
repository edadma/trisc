package io.github.edadma.trisc

import java.nio.file.{Files, Paths}

def app(args: Config): Unit =
  val input =
    args match
      case Config(None) => scala.io.Source.stdin.mkString
      case Config(Some(file)) =>
        val path = file.toPath.normalize.toAbsolutePath

        if !Files.exists(path) then problem(s"file '$path' not found")
        else if !Files.isReadable(path) then problem(s"file '$path' is not readable")
        else if !Files.isRegularFile(path) then problem(s"file '$path' is not a file")
        else new String(Files.readAllBytes(file.toPath))

//  val segs = assemble(input, orgs = Map("bss" -> 0x1000))
