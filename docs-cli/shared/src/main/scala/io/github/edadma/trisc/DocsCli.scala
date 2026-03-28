package io.github.edadma.trisc

import scopt.OParser

sealed trait DocsCommand
case class TangleCommand(
    inputs: Seq[String] = Seq.empty,
    output: Option[String] = None,
) extends DocsCommand
case class WeaveCommand(
    inputs: Seq[String] = Seq.empty,
    output: Option[String] = None,
    channel: String = "dev",
    mode: String = "literate", // literate, prose, api
) extends DocsCommand

case class DocsConfig(
    command: DocsCommand = TangleCommand(),
)

object DocsCli:
  private val builder = OParser.builder[DocsConfig]

  private val cliParser =
    import builder.*
    OParser.sequence(
      programName("docs"),
      head("docs", "0.1.0"),
      cmd("tangle")
        .text("Extract code from .lsysl files")
        .action((_, c) => c.copy(command = TangleCommand()))
        .children(
          opt[String]('o', "output")
            .text("Output file or directory")
            .action((v, c) =>
              c.copy(command = c.command match
                case tc: TangleCommand => tc.copy(output = Some(v))
                case other             => other
              )
            ),
          arg[String]("<source>...")
            .unbounded()
            .text("Literate source files (.lsysl) or a directory")
            .action((v, c) =>
              c.copy(command = c.command match
                case tc: TangleCommand => tc.copy(inputs = tc.inputs :+ v)
                case other             => other
              )
            ),
        ),
      cmd("weave")
        .text("Generate documentation from .lsysl files")
        .action((_, c) => c.copy(command = WeaveCommand()))
        .children(
          opt[String]('o', "output")
            .text("Output file or directory")
            .action((v, c) =>
              c.copy(command = c.command match
                case wc: WeaveCommand => wc.copy(output = Some(v))
                case other            => other
              )
            ),
          opt[String]("channel")
            .text("Channel: code, dev (default), website, api, reference")
            .action((v, c) =>
              c.copy(command = c.command match
                case wc: WeaveCommand => wc.copy(channel = v)
                case other            => other
              )
            ),
          opt[String]("mode")
            .text("Output mode: literate (default), prose, api")
            .validate(v =>
              if Seq("literate", "prose", "api").contains(v) then builder.success
              else builder.failure(s"Unknown mode: $v (expected literate, prose, api)")
            )
            .action((v, c) =>
              c.copy(command = c.command match
                case wc: WeaveCommand => wc.copy(mode = v)
                case other            => other
              )
            ),
          arg[String]("<source>...")
            .unbounded()
            .text("Literate source files (.lsysl) or a directory")
            .action((v, c) =>
              c.copy(command = c.command match
                case wc: WeaveCommand => wc.copy(inputs = wc.inputs :+ v)
                case other            => other
              )
            ),
        ),
      checkConfig(c =>
        c.command match
          case TangleCommand(inputs, _) if inputs.isEmpty =>
            builder.failure("No input files specified")
          case WeaveCommand(inputs, _, _, _) if inputs.isEmpty =>
            builder.failure("No input files specified")
          case _ => builder.success
      ),
    )

  def parse(args: Seq[String]): Option[DocsConfig] =
    OParser.parse(cliParser, args, DocsConfig())

  private case class CliError(msg: String) extends RuntimeException(msg)

  private def fail(msg: String): Nothing =
    System.err.println(msg)
    throw CliError(msg)

  def execute(config: DocsConfig): Unit =
    try
      config.command match
        case cmd: TangleCommand => executeTangle(cmd)
        case cmd: WeaveCommand  => executeWeave(cmd)
    catch case CliError(_) => ()

  private def executeTangle(cmd: TangleCommand): Unit =
    val sources = resolveSources(cmd.inputs)
    val parser = new LiterateParser

    for (name, source) <- sources do
      val doc = parser.parse(source)
      val code = LiterateRenderer.tangle(doc)
      val outFile = outputPath(cmd.output, name, ".sysl", sources.size)
      io.writeFile(outFile, code)
      System.err.println(s"  $name -> $outFile")

  private def executeWeave(cmd: WeaveCommand): Unit =
    val sources = resolveSources(cmd.inputs)
    val parser = new LiterateParser
    val channel = ChannelConfig.defaults.getOrElse(cmd.channel, ChannelConfig(cmd.channel, Set(cmd.channel)))

    for (name, source) <- sources do
      val doc = parser.parse(source)
      val rendered = cmd.mode match
        case "literate" => LiterateRenderer.renderLiterate(doc, channel)
        case "prose"    => LiterateRenderer.renderProse(doc, channel)
        case "api"      => LiterateRenderer.renderApi(doc)
        case _          => fail(s"Unknown mode: ${cmd.mode}")

      val ext = cmd.mode match
        case "api" => ".api.md"
        case _     => ".md"

      val outFile = outputPath(cmd.output, name, ext, sources.size)
      io.writeFile(outFile, rendered)
      System.err.println(s"  $name -> $outFile")

  private def io: FileOps = FileOps.instance

  private def resolveSources(inputs: Seq[String]): Map[String, String] =
    if inputs.size == 1 && io.isDirectory(inputs.head) then
      val files = io.listFiles(inputs.head).filter(io.fileName(_).endsWith(".lsysl"))
      if files.isEmpty then
        fail(s"error: no .lsysl files in directory: ${inputs.head}")
      files.map(f => (io.fileName(f).stripSuffix(".lsysl"), io.readFile(f))).toMap
    else
      inputs.map { path =>
        if !io.exists(path) then
          fail(s"error: file not found: $path")
        (io.fileName(path).stripSuffix(".lsysl"), io.readFile(path))
      }.toMap

  private def outputPath(output: Option[String], name: String, ext: String, sourceCount: Int): String =
    output match
      case Some(out) if sourceCount == 1 => out
      case Some(out) =>
        if !io.exists(out) then io.mkdirs(out)
        io.joinPath(out, name + ext)
      case None => name + ext
