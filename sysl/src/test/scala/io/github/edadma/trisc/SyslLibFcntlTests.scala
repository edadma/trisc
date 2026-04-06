package io.github.edadma.trisc

class SyslLibFcntlTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/fcntl/fcntl" -> readSysl("posix/fcntl/fcntl.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.fcntl.*
       |$main
       |""".stripMargin)

  // Access modes
  "O_RDONLY is 0" in { evalWith("main() -> int = O_RDONLY") shouldBe 0 }
  "O_WRONLY is 1" in { evalWith("main() -> int = O_WRONLY") shouldBe 1 }
  "O_RDWR is 2" in { evalWith("main() -> int = O_RDWR") shouldBe 2 }

  // Creation flags
  "O_CREAT is 0x40" in { evalWith("main() -> int = O_CREAT") shouldBe 0x40 }
  "O_EXCL is 0x80" in { evalWith("main() -> int = O_EXCL") shouldBe 0x80 }
  "O_TRUNC is 0x200" in { evalWith("main() -> int = O_TRUNC") shouldBe 0x200 }
  "O_APPEND is 0x400" in { evalWith("main() -> int = O_APPEND") shouldBe 0x400 }

  // Seek
  "SEEK_SET is 0" in { evalWith("main() -> int = SEEK_SET") shouldBe 0 }
  "SEEK_CUR is 1" in { evalWith("main() -> int = SEEK_CUR") shouldBe 1 }
  "SEEK_END is 2" in { evalWith("main() -> int = SEEK_END") shouldBe 2 }

  // Flags can be combined
  "O_WRONLY | O_CREAT | O_TRUNC" in {
    evalWith("main() -> int = O_WRONLY | O_CREAT | O_TRUNC") shouldBe (1 | 0x40 | 0x200)
  }
}
