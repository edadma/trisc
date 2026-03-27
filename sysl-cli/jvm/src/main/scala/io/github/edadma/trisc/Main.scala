package io.github.edadma.trisc

@main def run(args: String*): Unit =
  SyslCli.parse(args) match
    case Some(config) => SyslCli.execute(config)
    case None         => // scopt already printed usage/error
