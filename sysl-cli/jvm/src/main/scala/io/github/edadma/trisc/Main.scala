package io.github.edadma.trisc

@main def run(args: String*): Unit =
  FileOps.instance = JvmFileOps
  GitFetcherProvider.instance = Some(JvmGitFetcher.default)
  SyslCli.parse(args) match
    case Some(config) => SyslCli.execute(config)
    case None         => // scopt already printed usage/error
