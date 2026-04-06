package io.github.edadma.trisc

class SyslLibSignalTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/signal/signal" -> readSysl("posix/signal/signal.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.signal.*
       |$main
       |""".stripMargin)

  "SIGHUP is 1" in { evalWith("main() -> int = SIGHUP") shouldBe 1 }
  "SIGINT is 2" in { evalWith("main() -> int = SIGINT") shouldBe 2 }
  "SIGQUIT is 3" in { evalWith("main() -> int = SIGQUIT") shouldBe 3 }
  "SIGILL is 4" in { evalWith("main() -> int = SIGILL") shouldBe 4 }
  "SIGABRT is 6" in { evalWith("main() -> int = SIGABRT") shouldBe 6 }
  "SIGKILL is 9" in { evalWith("main() -> int = SIGKILL") shouldBe 9 }
  "SIGSEGV is 11" in { evalWith("main() -> int = SIGSEGV") shouldBe 11 }
  "SIGTERM is 15" in { evalWith("main() -> int = SIGTERM") shouldBe 15 }
  "SIGCHLD is 17" in { evalWith("main() -> int = SIGCHLD") shouldBe 17 }
  "SIGSTOP is 19" in { evalWith("main() -> int = SIGSTOP") shouldBe 19 }
  "SIGTSTP is 20" in { evalWith("main() -> int = SIGTSTP") shouldBe 20 }
  "SIG_DFL is 0" in { evalWith("main() -> int = SIG_DFL") shouldBe 0 }
  "SIG_IGN is 1" in { evalWith("main() -> int = SIG_IGN") shouldBe 1 }
  "SIG_ERR is -1" in { evalWith("main() -> int = SIG_ERR") shouldBe -1 }
}
