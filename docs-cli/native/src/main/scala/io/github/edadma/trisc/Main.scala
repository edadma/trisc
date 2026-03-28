package io.github.edadma.trisc

object Main:
  def main(args: Array[String]): Unit =
    FileOps.instance = NativeFileOps
    DocsCli.parse(args.toSeq) match
      case Some(config) => DocsCli.execute(config)
      case None         => // scopt already printed usage/error
