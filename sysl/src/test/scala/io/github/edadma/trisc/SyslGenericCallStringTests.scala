package io.github.edadma.trisc

class SyslGenericCallStringTests extends SyslTestHelpers {

  // ===== `string` as a type argument inside [...] =====
  //
  // The parser had two unrelated rules consuming `string`:
  //   - the legitimate string-builder call form `string(arg, ...)` (commits to `(`)
  //   - the scalar-type-keyword fallback used inside `[T]` (`int`, `byte`, `bool`, …)
  //
  // The fallback list was missing `string`, so `Parser[string]` saw the call rule
  // win, fail at the `]` (where it wanted `(`), and surface the most-progressed
  // error — `'(' expected but ']' found`. Every other primitive (`int`, `byte`,
  // `bool`, `unit`, …) flowed through the fallback cleanly. Adding `string` to
  // the fallback list makes the four shapes parse uniformly while leaving the
  // legitimate `string(arg, ...)` call form untouched (it's tried first and
  // succeeds whenever a `(` actually follows).

  "Parser[string]((closure)) parses" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |make(s: string) -> Parser[string] =
        |    Parser[string]((x: int) -> s)
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  "Parser[int], Parser[byte], Parser[bool], Parser[string] all parse uniformly" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |mi() -> Parser[int]    = Parser[int]((x: int) -> x)
        |mb() -> Parser[byte]   = Parser[byte]((x: int) -> 0u8)
        |mz() -> Parser[bool]   = Parser[bool]((x: int) -> true)
        |ms() -> Parser[string] = Parser[string]((x: int) -> "hi")
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  "Parser[string] callable round-trips a literal" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |make() -> Parser[string] = Parser[string]((x: int) -> "hi")
        |
        |main() -> int
        |    val p = make()
        |    val s = p(0)
        |    len(s)
        |""".stripMargin) shouldBe 2
  }

  // ===== `string` in nested positions inside [...] =====

  "Parser[[]string] (slice of string as type arg) parses" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |make() -> Parser[[]string] = Parser[[]string]((x: int) -> ["a", "b", "c"])
        |
        |main() -> int
        |    val p = make()
        |    len(p(0))
        |""".stripMargin) shouldBe 3
  }

  // ===== Regression: `string(...)` call form still parses =====

  "regression: string(bytes) byte-to-string conversion still parses" in {
    // The legitimate `string(arg)` call form: convert a `[]byte` slice to a
    // string. The parser rule that owns this (`"string" ~> "(" ~> ...`) is
    // tried before the new keyword fallback, so it still wins whenever a `(`
    // actually follows.
    eval(
      """main() -> int
        |    val bytes: [3]byte = [104u8, 105u8, 33u8]
        |    val s = string(bytes[:])
        |    len(s)
        |""".stripMargin) shouldBe 3
  }
}
