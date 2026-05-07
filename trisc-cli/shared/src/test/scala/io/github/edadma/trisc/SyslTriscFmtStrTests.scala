package io.github.edadma.trisc

/** Audit item #14 (Tier 3): TRISC backend was missing a `case TFmtStr` arm
 *  in `SyslTriscCodegen.genExpr` and a `__str_fmt_i64` runtime helper, so
 *  every f-string interpolation requiring formatting (`f"{n}%x"`,
 *  `f"{n}%05d"`, etc.) crashed at compile time with `RuntimeException:
 *  codegen: unhandled expression type: TFmtStr`. The fix at sysl@<TBD>
 *  ports SVM's `__svm_str_fmt_i64` (~150 lines of asm — see
 *  `SVMRuntime.scala:604–890`) to TRISC asm and routes integer-verb format
 *  strings through it.
 *
 *  Tests mirror SyslSVMFmtStrTests so the same Sysl source exercises both
 *  backends and any divergence shows up immediately. The `len(s)` /
 *  `i64(s[idx])` probes keep assertions numeric (no string equality
 *  needed at the test layer).
 *
 *  Note: `f"{n}%d"` (no flags) is rewritten to plain `TStr(n)` by the
 *  analyzer (see `SyslAnalyzerExpressions.scala:1893`) — that path was
 *  already handled. Only fmt-specs with a width / pad / sign flag, or a
 *  non-`d` verb, actually exercise the new TFmtStr branch.
 */
class SyslTriscFmtStrTests extends SyslCodegenHelpers {

  // Allocator infrastructure (same pattern as SyslCodegenStringInterpolationTests).
  // f-string lowering uses malloc, so a real heap is required.
  private val allocSource = scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString
  private val stringSource = scala.io.Source.fromFile("posix/string/string.sysl").mkString
  private val ctypeSource = scala.io.Source.fromFile("posix/ctype/ctype.sysl").mkString

  private def sbrkModule(heapSize: Int = 16384): String =
    s"""module posix.unistd
       |
       |var _heap: [$heapSize]i8
       |var _brk: *i8 = *i8(0)
       |var _brk_initialized = false
       |
       |sbrk(increment: int) -> *i8
       |    if !_brk_initialized
       |        _brk = &_heap
       |        _brk_initialized = true
       |
       |    if increment == 0 then return _brk
       |
       |    val old_brk = _brk
       |    val new_brk = old_brk + increment
       |
       |    if i64(new_brk) > i64(&_heap + $heapSize) then return *i8(-1)
       |
       |    _brk = new_brk
       |    old_brk
       |""".stripMargin

  private def allocSources(mainSource: String): Map[String, String] =
    Map(
      "posix/unistd/sbrk" -> sbrkModule(),
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "posix/stdlib/alloc" -> allocSource,
      "main" -> mainSource,
    )

  /** Compile + run a Sysl program with the standard posix/stdlib allocator wired in. */
  private def runWithAlloc(source: String): Long =
    compileMultiAndRun(allocSources(s"import posix.stdlib.*\n\n$source"))

  "f-string %x lowercase length" in {
    runWithAlloc(
      """main() -> i64
        |    val n = 255
        |    val s = f"$n%x"
        |    len(s)
        |""".stripMargin) shouldBe 2
  }

  "f-string %x lowercase first byte" in {
    // 255 = 0xff → "ff", first byte 'f' = 102
    runWithAlloc(
      """main() -> i64
        |    val n = 255
        |    val s = f"$n%x"
        |    i64(s[0])
        |""".stripMargin) shouldBe 102
  }

  "f-string %X uppercase first byte" in {
    // 255 = 0xff → "FF", first byte 'F' = 70
    runWithAlloc(
      """main() -> i64
        |    val n = 255
        |    val s = f"$n%X"
        |    i64(s[0])
        |""".stripMargin) shouldBe 70
  }

  "f-string %o length" in {
    // 8 → "10" in octal, length 2
    runWithAlloc(
      """main() -> i64
        |    val n = 8
        |    val s = f"$n%o"
        |    len(s)
        |""".stripMargin) shouldBe 2
  }

  "f-string %b length" in {
    // 5 → "101" in binary, length 3
    runWithAlloc(
      """main() -> i64
        |    val n = 5
        |    val s = f"$n%b"
        |    len(s)
        |""".stripMargin) shouldBe 3
  }

  "f-string %b first digit" in {
    // 5 → "101", first byte '1' = 49
    runWithAlloc(
      """main() -> i64
        |    val n = 5
        |    val s = f"$n%b"
        |    i64(s[0])
        |""".stripMargin) shouldBe 49
  }

  "f-string %05d zero-pad length" in {
    // 42 → "00042" with %05d, length 5
    runWithAlloc(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%05d"
        |    len(s)
        |""".stripMargin) shouldBe 5
  }

  "f-string %05d leading char is '0'" in {
    runWithAlloc(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%05d"
        |    i64(s[0])
        |""".stripMargin) shouldBe 48
  }

  "f-string %05d trailing chars are digits" in {
    // 42 → "00042"; s[3]='4'=52, s[4]='2'=50
    runWithAlloc(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%05d"
        |    i64(s[3])
        |""".stripMargin) shouldBe 52
  }

  "f-string %5d space-pad first byte" in {
    // %5d on 42 → "   42", first byte ' ' = 32
    runWithAlloc(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%5d"
        |    i64(s[0])
        |""".stripMargin) shouldBe 32
  }

  "f-string %-5d left-align last byte" in {
    // %-5d on 42 → "42   ", last byte ' ' = 32
    runWithAlloc(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%-5d"
        |    i64(s[4])
        |""".stripMargin) shouldBe 32
  }

  "f-string %-5d left-align first byte is digit" in {
    // %-5d on 42 → "42   ", first byte '4' = 52
    runWithAlloc(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%-5d"
        |    i64(s[0])
        |""".stripMargin) shouldBe 52
  }

  "f-string %+d showSign positive" in {
    // %+d on 42 → "+42", first byte '+' = 43
    runWithAlloc(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%+d"
        |    i64(s[0])
        |""".stripMargin) shouldBe 43
  }

  "f-string %+d showSign negative still has minus" in {
    runWithAlloc(
      """main() -> i64
        |    val n = -7
        |    val s = f"$n%+d"
        |    i64(s[0])
        |""".stripMargin) shouldBe 45
  }

  "f-string showSign overall length" in {
    // %+d on 42 → "+42", length 3
    runWithAlloc(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%+d"
        |    len(s)
        |""".stripMargin) shouldBe 3
  }

  // Note on no direct-helper probe: SVM ships `__svm_str_fmt_i64` in its
  // global runtime (linked unconditionally), so SyslSVMFmtStrTests can call
  // it via a bare `extern` declaration. TRISC emits `__str_fmt_i64` per-
  // program, gated on `needsStrFmtI64` being set during codegen — declaring
  // `extern __str_fmt_i64` without a real `f"..."` somewhere doesn't trigger
  // the gate, so there's no equivalent direct probe possible without
  // unconditionally emitting the helper into every TRISC binary. The
  // f-string lowering tests above cover the same surface.
}
