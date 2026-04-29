package io.github.edadma.trisc

class SyslLibMathExtTests extends SyslTestHelpers {

  private val testFileOps: FileOps = new FileOps:
    def readFile(path: String): String = scala.io.Source.fromFile(path).mkString
    def writeFile(path: String, content: String): Unit = java.nio.file.Files.writeString(java.nio.file.Paths.get(path), content)
    def exists(path: String): Boolean = java.io.File(path).exists()
    def isDirectory(path: String): Boolean = java.io.File(path).isDirectory
    def listFiles(path: String): Seq[String] = java.io.File(path).listFiles().map(_.getPath).toSeq
    def fileName(path: String): String = java.io.File(path).getName
    def mkdirs(path: String): Unit = java.io.File(path).mkdirs()
    def joinPath(dir: String, name: String): String = java.nio.file.Paths.get(dir, name).toString

  private val simpleTangler: String => String = raw =>
    raw.linesIterator
      .filter(_.startsWith("    "))
      .map(_.drop(4))
      .mkString("\n")

  val libs: Map[String, String] = Map(
    "posix/math/math" -> readSysl("posix/math/math.sysl"),
  )

  private def evalWith(main: String): Long =
    val sources = libs + ("test" -> s"import posix.math.*\n$main\n")
    val driver = new SyslDriver(Some(testFileOps), List("."), tangler = Some(simpleTangler))
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged)

  // ===== isnan =====

  "isnan on normal" in { evalWith("main() -> int = isnan(1.0)") shouldBe 0 }
  "isnan on zero" in { evalWith("main() -> int = isnan(0.0)") shouldBe 0 }
  "isnan on NaN" in { evalWith("main() -> int = isnan(0.0 / 0.0)") shouldBe 1 }

  // ===== isinf =====

  "isinf on normal" in { evalWith("main() -> int = isinf(1.0)") shouldBe 0 }
  "isinf on zero" in { evalWith("main() -> int = isinf(0.0)") shouldBe 0 }
  "isinf on positive inf" in { evalWith("main() -> int = isinf(1.0e308 * 10.0)") shouldBe 1 }
  "isinf on negative inf" in { evalWith("main() -> int = isinf(-1.0e308 * 10.0)") shouldBe 1 }

  // ===== isfinite =====

  "isfinite on normal" in { evalWith("main() -> int = isfinite(42.0)") shouldBe 1 }
  "isfinite on zero" in { evalWith("main() -> int = isfinite(0.0)") shouldBe 1 }
  "isfinite on inf" in { evalWith("main() -> int = isfinite(1.0e308 * 10.0)") shouldBe 0 }
  "isfinite on NaN" in { evalWith("main() -> int = isfinite(0.0 / 0.0)") shouldBe 0 }

  // ===== signbit =====

  "signbit positive" in { evalWith("main() -> int = signbit(1.0)") shouldBe 0 }
  "signbit negative" in { evalWith("main() -> int = signbit(-1.0)") shouldBe 1 }
  "signbit zero" in { evalWith("main() -> int = signbit(0.0)") shouldBe 0 }

  // ===== constants =====

  "E is approximately 2.718" in {
    evalWith(
      """main() -> int
        |    if E > 2.718 && E < 2.719 then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "HUGE_VAL is large" in { evalWith("main() -> int = if HUGE_VAL > 1.0e307 then 1 else 0") shouldBe 1 }
}
