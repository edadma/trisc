package io.github.edadma.trisc

class SyslLibCtypeTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/ctype/ctype" -> readSysl("posix/ctype/ctype.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.ctype.*
       |$main
       |""".stripMargin)

  // ===== isalpha =====

  "isalpha uppercase" in { evalWith("main() -> int = isalpha(65)") shouldBe 1 }  // 'A'
  "isalpha lowercase" in { evalWith("main() -> int = isalpha(122)") shouldBe 1 }  // 'z'
  "isalpha digit" in { evalWith("main() -> int = isalpha(48)") shouldBe 0 }  // '0'
  "isalpha space" in { evalWith("main() -> int = isalpha(32)") shouldBe 0 }

  // ===== isdigit =====

  "isdigit 0" in { evalWith("main() -> int = isdigit(48)") shouldBe 1 }
  "isdigit 9" in { evalWith("main() -> int = isdigit(57)") shouldBe 1 }
  "isdigit letter" in { evalWith("main() -> int = isdigit(65)") shouldBe 0 }

  // ===== isalnum =====

  "isalnum letter" in { evalWith("main() -> int = isalnum(65)") shouldBe 1 }
  "isalnum digit" in { evalWith("main() -> int = isalnum(48)") shouldBe 1 }
  "isalnum punct" in { evalWith("main() -> int = isalnum(33)") shouldBe 0 }  // '!'

  // ===== isspace =====

  "isspace space" in { evalWith("main() -> int = isspace(32)") shouldBe 1 }
  "isspace tab" in { evalWith("main() -> int = isspace(9)") shouldBe 1 }
  "isspace newline" in { evalWith("main() -> int = isspace(10)") shouldBe 1 }
  "isspace cr" in { evalWith("main() -> int = isspace(13)") shouldBe 1 }
  "isspace vt" in { evalWith("main() -> int = isspace(11)") shouldBe 1 }
  "isspace ff" in { evalWith("main() -> int = isspace(12)") shouldBe 1 }
  "isspace letter" in { evalWith("main() -> int = isspace(65)") shouldBe 0 }

  // ===== isupper / islower =====

  "isupper A" in { evalWith("main() -> int = isupper(65)") shouldBe 1 }
  "isupper a" in { evalWith("main() -> int = isupper(97)") shouldBe 0 }
  "islower a" in { evalWith("main() -> int = islower(97)") shouldBe 1 }
  "islower A" in { evalWith("main() -> int = islower(65)") shouldBe 0 }

  // ===== isprint / isgraph =====

  "isprint space" in { evalWith("main() -> int = isprint(32)") shouldBe 1 }
  "isprint tilde" in { evalWith("main() -> int = isprint(126)") shouldBe 1 }
  "isprint control" in { evalWith("main() -> int = isprint(0)") shouldBe 0 }
  "isprint DEL" in { evalWith("main() -> int = isprint(127)") shouldBe 0 }
  "isgraph letter" in { evalWith("main() -> int = isgraph(65)") shouldBe 1 }
  "isgraph space" in { evalWith("main() -> int = isgraph(32)") shouldBe 0 }

  // ===== ispunct =====

  "ispunct !" in { evalWith("main() -> int = ispunct(33)") shouldBe 1 }
  "ispunct @" in { evalWith("main() -> int = ispunct(64)") shouldBe 1 }
  "ispunct letter" in { evalWith("main() -> int = ispunct(65)") shouldBe 0 }
  "ispunct digit" in { evalWith("main() -> int = ispunct(48)") shouldBe 0 }
  "ispunct space" in { evalWith("main() -> int = ispunct(32)") shouldBe 0 }

  // ===== iscntrl =====

  "iscntrl NUL" in { evalWith("main() -> int = iscntrl(0)") shouldBe 1 }
  "iscntrl BEL" in { evalWith("main() -> int = iscntrl(7)") shouldBe 1 }
  "iscntrl DEL" in { evalWith("main() -> int = iscntrl(127)") shouldBe 1 }
  "iscntrl space" in { evalWith("main() -> int = iscntrl(32)") shouldBe 0 }

  // ===== isxdigit =====

  "isxdigit 0" in { evalWith("main() -> int = isxdigit(48)") shouldBe 1 }
  "isxdigit a" in { evalWith("main() -> int = isxdigit(97)") shouldBe 1 }
  "isxdigit F" in { evalWith("main() -> int = isxdigit(70)") shouldBe 1 }
  "isxdigit g" in { evalWith("main() -> int = isxdigit(103)") shouldBe 0 }

  // ===== isblank =====

  "isblank space" in { evalWith("main() -> int = isblank(32)") shouldBe 1 }
  "isblank tab" in { evalWith("main() -> int = isblank(9)") shouldBe 1 }
  "isblank newline" in { evalWith("main() -> int = isblank(10)") shouldBe 0 }

  // ===== tolower / toupper =====

  "tolower A" in { evalWith("main() -> int = tolower(65)") shouldBe 97 }
  "tolower z" in { evalWith("main() -> int = tolower(122)") shouldBe 122 }
  "tolower digit" in { evalWith("main() -> int = tolower(48)") shouldBe 48 }
  "toupper a" in { evalWith("main() -> int = toupper(97)") shouldBe 65 }
  "toupper Z" in { evalWith("main() -> int = toupper(90)") shouldBe 90 }
  "toupper digit" in { evalWith("main() -> int = toupper(48)") shouldBe 48 }
}
