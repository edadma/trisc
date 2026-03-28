package io.github.edadma.trisc

@main def run(args: String*): Unit =
  FileOps.instance = JvmFileOps
  DocsCli.parse(args) match
    case Some(config) => DocsCli.execute(config)
    case None         => // scopt already printed usage/error
