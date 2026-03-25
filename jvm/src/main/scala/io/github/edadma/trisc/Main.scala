package io.github.edadma.trisc

import java.io.File

case class Config(
    input: Option[File] = None,
)

@main def run(args: String*): Unit =
  import scopt.OParser

  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("trisc"),
      head("TRISC", "v0.0.1"),
//      arg[File]("<input file>")
//        .optional()
//        .action((x, c) => c.copy(input = Some(x)))
//        .text("input file (may default to standard input depending on context)"),
      opt[Unit]("s")
        .action((_, c) => { println("s"); null }),
      help("help").text("prints this usage text"),
      cmd("assemble").action((_, c) => { println("assemble"); null }),
      cmd("emulate").action((_, c) => { println("emulate"); null }),
//      cmd("as")
//        .action((_, c) => c.copy(mode = "update"))
//        .text("update is a command.")
//        .children(
//          opt[Unit]("not-keepalive")
//            .abbr("nk")
//            .action((_, c) => c.copy(keepalive = false))
//            .text("disable keepalive"),
//          opt[Boolean]("xyz")
//            .action((x, c) => c.copy(xyz = x))
//            .text("xyz is a boolean property"),
//          opt[Unit]("debug-update")
//            .hidden()
//            .action((_, c) => c.copy(debug = true))
//            .text("this option is hidden in the usage text"),
//          checkConfig(c =>
//            if (c.keepalive && c.xyz) failure("xyz cannot keep alive")
//            else success,
//          ),
//        ),
//      opt[Unit]('m', "multi")
//        .action((_, c) => c.copy(multi = true))
//        .text("multi"),
//      opt[String]('o', "output")
//        .valueName("<output file>")
//        .action((x, c) => c.copy(output = x))
//        .text("output file (defaults to <input>.<type>)"),
//      opt[String]('p', "paper")
//        .valueName("<a4 | letter>")
//        .action((x, c) => c.copy(paper = x))
//        .validate({
//          case "a4" | "letter" => success
//          case _               => failure("only 'a4' or 'letter' are allowed as paper types")
//        })
//        .text("paper size (defaults to letter)"),
//      opt[String]('r', "resolution")
//        .valueName("<sd | hd | fhd>")
//        .action((x, c) => c.copy(resolution = x))
//        .validate({
//          case "sd" | "hd" | "fhd" => success
//          case _                   => failure("only 'sd' | 'hd' | 'fhd' are allowed as resolutions")
//        })
//        .text("resolution (defaults to hd)"),
//      opt[Int]('s', "size")
//        .valueName("<inches>")
//        .action((x, c) => c.copy(size = x))
//        .validate(s =>
//          if 0 < s then success
//          else failure("only positive values are allowed as screen sizes"),
//        )
//        .text("screen size in inches for PNG file output (defaults to 13)"),
//      opt[String]('t', "type")
//        .valueName("<pdf | png>")
//        .action((x, c) => c.copy(typ = x))
//        .validate({
//          case "png" | "pdf" => success
//          case _             => failure("only 'png' or 'pdf' are allowed as output file types")
//        })
//        .text("output file type (defaults to pdf, or png for multi mode)"),
//      opt[Unit]('u', "usfx")
//        .action((_, c) => c.copy(usfx = true))
//        .text("USFX"),
      version("version").text("prints the current version"),
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(c) => app(c)
    case _       =>
  }
