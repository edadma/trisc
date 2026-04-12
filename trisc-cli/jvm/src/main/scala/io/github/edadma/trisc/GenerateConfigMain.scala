package io.github.edadma.trisc

import java.nio.file.{Files, Paths}
import scala.io.Source

/** Build tool: reads `sysl.toml` at the repo root and emits the generated
  * sysl config file at the path derived from the `[config].module` setting.
  *
  * Usage:
  * {{{
  *   sbt "triscCliJVM/runMain io.github.edadma.trisc.GenerateConfigMain"
  * }}}
  *
  * Optional argument: path to a TOML file (default: `sysl.toml`).
  */
object GenerateConfigMain:

  def main(args: Array[String]): Unit =
    val tomlPath = args.headOption.getOrElse("sysl.toml")
    val tomlFile = Paths.get(tomlPath)

    if !Files.exists(tomlFile) then
      System.err.println(s"error: $tomlPath not found")
      sys.exit(1)

    val source = Source.fromFile(tomlFile.toFile).mkString
    val result =
      try SyslBuildConfig.generate(source)
      catch
        case e: SyslBuildConfig.Error =>
          System.err.println(s"error: ${e.getMessage}")
          sys.exit(1)

    val outputFile = Paths.get(result.path)
    val parent = outputFile.getParent
    if parent != null then Files.createDirectories(parent)
    Files.writeString(outputFile, result.content)

    println(s"wrote ${result.content.length} bytes to ${result.path}")
