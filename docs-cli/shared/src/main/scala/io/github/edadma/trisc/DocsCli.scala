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
        .text("Generate highlighted HTML from .lsysl files")
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
          case WeaveCommand(inputs, _) if inputs.isEmpty =>
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

    for (name, source) <- sources do
      val html = LiterateRenderer.renderHTML(source)
      val outFile = outputPath(cmd.output, name, ".html", sources.size)
      io.writeFile(outFile, html)
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
