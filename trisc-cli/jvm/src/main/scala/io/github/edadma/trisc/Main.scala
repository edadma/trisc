package io.github.edadma.trisc

@main def run(args: String*): Unit =
  TriscCli.guiLauncher = Some(EmulatorGui.launch)
  TriscCli.parse(args) match
    case Some(config) => TriscCli.execute(config)
    case None         => // scopt already printed usage/error
