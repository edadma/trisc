package io.github.edadma.trisc

class SyslLibLimitsTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/limits/limits" -> readSysl("posix/limits/limits.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.limits.*
       |$main
       |""".stripMargin)

  "I8_MIN" in { evalWith("main() -> int = I8_MIN") shouldBe -128 }
  "I8_MAX" in { evalWith("main() -> int = I8_MAX") shouldBe 127 }
  "U8_MAX" in { evalWith("main() -> int = U8_MAX") shouldBe 255 }
  "BYTE_MAX" in { evalWith("main() -> int = BYTE_MAX") shouldBe 255 }
  "I16_MIN" in { evalWith("main() -> int = I16_MIN") shouldBe -32768 }
  "I16_MAX" in { evalWith("main() -> int = I16_MAX") shouldBe 32767 }
  "U16_MAX" in { evalWith("main() -> int = U16_MAX") shouldBe 65535 }
  "INT_MIN" in { evalWith("main() -> i64 = i64(INT_MIN)") shouldBe -2147483648L }
  "INT_MAX" in { evalWith("main() -> int = INT_MAX") shouldBe 2147483647 }
  "I64_MAX" in { evalWith("main() -> i64 = I64_MAX") shouldBe 9223372036854775807L }
  "I64_MIN is negative" in { evalWith("main() -> int = if I64_MIN < 0i64 then 1 else 0") shouldBe 1 }
  "PATH_MAX" in { evalWith("main() -> int = PATH_MAX") shouldBe 4096 }
  "NAME_MAX" in { evalWith("main() -> int = NAME_MAX") shouldBe 255 }
}
