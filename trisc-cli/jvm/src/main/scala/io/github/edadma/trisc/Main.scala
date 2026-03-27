package io.github.edadma.trisc

@main def run(args: String*): Unit =
  TriscCli.parse(args) match
    case Some(config) => TriscCli.execute(config)
    case None         => // scopt already printed usage/error
