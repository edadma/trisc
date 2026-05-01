package io.github.edadma.trisc

class SyslLineContinuationTests extends SyslTestHelpers {

  // ===== Trailing-operator line continuation =====
  //
  // Outside any paren/bracket/brace pair, a token that looks like a binary
  // operator at the end of a line should suppress the implicit Newline so the
  // RHS can live on the next indented line. This mirrors what `(` `[` `{`
  // already do via `lineJoining`, but driven by the *trailing* token instead
  // of an enclosing pair.
  //
  // Excluded: `=`, `->`, `=>`, `++`, `--` — these legitimately end a statement
  // (`++`/`--`) or drive their own indented-block parser construct.

  // ----- Built-in binary operators continue across newline+indent -----

  "trailing `+` continues the expression on the next indented line" in {
    eval(
      """main() -> int
        |    val x = 7 +
        |        3
        |    x
        |""".stripMargin) shouldBe 10
  }

  "trailing `<<` continues the expression" in {
    eval(
      """main() -> int
        |    val x = 1 <<
        |        4
        |    x
        |""".stripMargin) shouldBe 16
  }

  "trailing `&&` continues the expression" in {
    eval(
      """main() -> int = if true &&
        |    true then 42 else 0
        |""".stripMargin) shouldBe 42
  }

  // ----- User-defined infix operators continue across newline+indent -----

  "trailing user-defined `^^` continues onto the next indented line" in {
    eval(
      """trait Map[A, B, R]
        |    #operator("^^")
        |    pmap(a: A, b: B) -> R
        |
        |impl Map[int, int, int]
        |    pmap(a: int, b: int) -> int = a * b
        |
        |main() -> int
        |    val r = 7 ^^
        |        2
        |    r
        |""".stripMargin) shouldBe 14
  }

  "trailing user-defined `~>` continues" in {
    eval(
      """struct Pair
        |    a: int
        |    b: int
        |
        |trait SeqR[L, R, P]
        |    #operator("~>")
        |    seqr(l: L, r: R) -> P
        |
        |impl SeqR[int, int, Pair]
        |    seqr(l: int, r: int) -> Pair = Pair(l, r)
        |
        |main() -> int
        |    val p = 7 ~>
        |        35
        |    p.a + p.b
        |""".stripMargin) shouldBe 42
  }

  "trailing user-defined `<~` continues" in {
    eval(
      """trait SeqL[L, R, P]
        |    #operator("<~")
        |    seql(l: L, r: R) -> L
        |
        |impl SeqL[int, int, int]
        |    seql(l: int, r: int) -> int = l
        |
        |main() -> int
        |    val n = 9 <~
        |        99
        |    n
        |""".stripMargin) shouldBe 9
  }

  "trailing user-defined `|` continues (parsyl-style alternation)" in {
    eval(
      """trait Or[A, B, R]
        |    #operator("|")
        |    or(a: A, b: B) -> R
        |
        |impl Or[int, int, int]
        |    or(a: int, b: int) -> int = a + b
        |
        |main() -> int
        |    val r = 40 |
        |        2
        |    r
        |""".stripMargin) shouldBe 42
  }

  // ----- Compound-assignment operators continue across newline+indent -----

  "trailing `+=` continues onto an indented RHS" in {
    eval(
      """main() -> int
        |    var x = 10
        |    x +=
        |        32
        |    x
        |""".stripMargin) shouldBe 42
  }

  // ----- Excluded tokens still drive their own indented-block constructs -----

  "regression: trailing `=` still introduces an indented val RHS block (multi-line val)" in {
    // The lexer must NOT treat `=` as a continuation token, otherwise this
    // form (handled by `valRhs`'s `Newline ~> Indent ~> tupleExpr <~ Dedent`
    // alternative) breaks.
    eval(
      """main() -> int
        |    val v: int =
        |        100 + 23
        |    v - 81
        |""".stripMargin) shouldBe 42
  }

  "regression: trailing `=` still introduces an indented function body" in {
    // Function body uses `bodyExprOrBlock` which expects `Newline ~> Indent ~>
    // ... <~ Dedent`. If `=` were a continuation token, the lexer would skip
    // the newline and the multi-statement block form wouldn't match.
    eval(
      """f() -> int =
        |    var x = 5
        |    var y = 6
        |    x * y + 12
        |
        |main() -> int = f()
        |""".stripMargin) shouldBe 42
  }

  "regression: trailing `->` still introduces a function block body" in {
    eval(
      """f() -> int
        |    var x = 7
        |    x * 6
        |
        |main() -> int = f()
        |""".stripMargin) shouldBe 42
  }

  "regression: trailing `->` in match arm still emits indent for the arm body" in {
    // Match arms use `->` to introduce an inline body or an indented block.
    // The block-trigger machinery already special-cases `->` here; make sure
    // it stays special-cased and isn't accidentally treated as a generic
    // continuation token.
    eval(
      """main() -> int
        |    var r = 0
        |    1 match
        |        1 ->
        |            r = 42
        |        else -> r = 0
        |    r
        |""".stripMargin) shouldBe 42
  }

  // ----- Pre-existing paren-pair line-joining still works (no regression) -----

  "regression: trailing `+` inside parens continues (paren-pair join — pre-existing)" in {
    eval(
      """main() -> int
        |    val x = (7 +
        |        3)
        |    x
        |""".stripMargin) shouldBe 10
  }

  "regression: trailing `,` inside parens still joins (pre-existing)" in {
    eval(
      """add3(a: int, b: int, c: int) -> int = a + b + c
        |
        |main() -> int = add3(10,
        |    20,
        |    12)
        |""".stripMargin) shouldBe 42
  }

  // ----- Statement-terminating tokens (++ / --) are NOT continuations -----

  // ----- `*` is excluded from continuation (import-glob context) -----

  "regression: `import x.*` glob still parses (trailing `*` is NOT a continuation)" in {
    // `*` is the import-glob marker. If it were treated as a generic
    // continuation operator, `import std.foo.*` would silently glue the next
    // top-level decl onto the import line and break parsing. Use a real
    // stdlib import here to pin the behavior end-to-end.
    val libs = Map(
      "lib/k" -> """module lib.k
                   |
                   |val K: int = 42
                   |""".stripMargin
    )
    runWithLibs(
      libs,
      """import lib.k.*
        |
        |main() -> int = K
        |""".stripMargin,
    )._1 shouldBe 42
  }

  "regression: `var.field++` ends the statement (not treated as continuation)" in {
    // If `++` were a continuation token, the lexer would swallow the newline
    // after `x++` and the next line `x` would attach to the postfix-increment
    // statement, breaking it.
    eval(
      """main() -> int
        |    var x = 41
        |    x++
        |    x
        |""".stripMargin) shouldBe 42
  }
}
