package io.github.edadma.trisc

class SyslLibErrnoTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/errno/errno" -> readSysl("posix/errno/errno.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.errno.*
       |$main
       |""".stripMargin)

  // ===== errno variable =====

  "errno initially zero" in {
    evalWith("main() -> int = get_errno()") shouldBe 0
  }

  "errno can be set and read" in {
    evalWith(
      """main() -> int
        |    set_errno(EINVAL)
        |    get_errno()
        |""".stripMargin) shouldBe 22
  }

  "errno can be reset" in {
    evalWith(
      """main() -> int
        |    set_errno(ENOMEM)
        |    set_errno(0)
        |    get_errno()
        |""".stripMargin) shouldBe 0
  }

  // ===== constants =====

  "EPERM is 1" in { evalWith("main() -> int = EPERM") shouldBe 1 }
  "ENOENT is 2" in { evalWith("main() -> int = ENOENT") shouldBe 2 }
  "EIO is 5" in { evalWith("main() -> int = EIO") shouldBe 5 }
  "EBADF is 9" in { evalWith("main() -> int = EBADF") shouldBe 9 }
  "EAGAIN is 11" in { evalWith("main() -> int = EAGAIN") shouldBe 11 }
  "ENOMEM is 12" in { evalWith("main() -> int = ENOMEM") shouldBe 12 }
  "EACCES is 13" in { evalWith("main() -> int = EACCES") shouldBe 13 }
  "EINVAL is 22" in { evalWith("main() -> int = EINVAL") shouldBe 22 }
  "ENOSYS is 38" in { evalWith("main() -> int = ENOSYS") shouldBe 38 }
  "ERANGE is 34" in { evalWith("main() -> int = ERANGE") shouldBe 34 }
  "EDOM is 33" in { evalWith("main() -> int = EDOM") shouldBe 33 }
  "ENOTEMPTY is 39" in { evalWith("main() -> int = ENOTEMPTY") shouldBe 39 }
  "EWOULDBLOCK equals EAGAIN" in { evalWith("main() -> int = if EWOULDBLOCK == EAGAIN then 1 else 0") shouldBe 1 }
}
