package io.github.edadma.trisc

object Main:
  def main(args: Array[String]): Unit =
    SyslCli.parse(args.toSeq) match
      case Some(config) => SyslCli.execute(config)
      case None         => // scopt already printed usage/error
