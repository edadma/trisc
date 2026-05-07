package io.github.edadma.trisc

/** Audit item #29 (Tier 4): the analyzer rejects mixed signed/unsigned arithmetic
 *  and comparisons with a hard error. The previous wording was:
 *
 *      cannot mix signed and unsigned in +: int + u32
 *      cannot compare signed and unsigned: int == u32
 *
 *  …which says *what's wrong* but not *what to do about it*. The improved
 *  diagnostic also lists both cast directions (`u32(...)` to make both
 *  unsigned, `i32(...)` to make both signed) so the user can pick whichever
 *  matches their intent without having to consult the reference.
 *
 *  These tests pin the new wording — both directions of the operand pair
 *  (signed-on-left vs unsigned-on-left), and across the three banned cases:
 *  arithmetic (+ - * /), bitwise/shift, and relational comparison.
 *
 *  Note: signed 32-bit prints as `int` (not `i32`) per `SyslType.toString`,
 *  so the hint says `int(...)` — both `int(x)` and `i32(x)` are valid casts.
 */
class SyslSignednessMismatchMsgTests extends SyslTestHelpers {

  // ===== Arithmetic =====

  "int + u32 → cast hint names both directions" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    val a: int = 1
          |    val b: u32 = 2u32
          |    a + b
          |""".stripMargin)
    }
    val m = thrown.getMessage
    m should include("cannot mix signed and unsigned in +")
    m should include("u32(...)")
    m should include("int(...)")
  }

  "u32 - int → hint also fires when unsigned is on the left" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    val a: u32 = 5u32
          |    val b: int = 1
          |    a - b
          |""".stripMargin)
    }
    val m = thrown.getMessage
    m should include("cannot mix signed and unsigned in -")
    m should include("u32(...)")
    m should include("int(...)")
  }

  "i64 * u64 → both 64-bit unsigned/signed handled" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    val a: i64 = 2i64
          |    val b: u64 = 3u64
          |    val c: i64 = i64(a * b)
          |    int(c)
          |""".stripMargin)
    }
    val m = thrown.getMessage
    m should include("cannot mix signed and unsigned in *")
    m should include("u64(...)")
    m should include("i64(...)")
  }

  // ===== Bitwise / shift =====

  "int | u32 → bitwise also gets the hint" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    val a: int = 1
          |    val b: u32 = 2u32
          |    int(a | b)
          |""".stripMargin)
    }
    val m = thrown.getMessage
    m should include("cannot mix signed and unsigned in |")
    m should include("u32(...)")
    m should include("int(...)")
  }

  "u32 << int → shift with sign mismatch surfaces hint" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    val a: u32 = 1u32
          |    val n: int = 4
          |    int(a << n)
          |""".stripMargin)
    }
    val m = thrown.getMessage
    m should include("cannot mix signed and unsigned in <<")
    m should include("u32(...)")
    m should include("int(...)")
  }

  // ===== Relational comparison =====

  "int == u32 → compare diagnostic uses 'compare' wording" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    val a: int = 1
          |    val b: u32 = 1u32
          |    if a == b then 1 else 0
          |""".stripMargin)
    }
    val m = thrown.getMessage
    m should include("cannot compare signed and unsigned")
    m should include("u32(...)")
    m should include("int(...)")
  }

  "u64 < i64 → compare hint with 64-bit operands" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    val a: u64 = 1u64
          |    val b: i64 = 2i64
          |    if a < b then 1 else 0
          |""".stripMargin)
    }
    val m = thrown.getMessage
    m should include("cannot compare signed and unsigned")
    m should include("u64(...)")
    m should include("i64(...)")
  }

  // ===== Cast suggestions are syntactically valid =====
  //
  // The hint says `u32(...)` — verify that pattern actually compiles when
  // the user follows the suggestion.

  "u32(rhs) fix actually compiles" in {
    eval(
      """main() -> int
        |    val a: int = 1
        |    val b: u32 = 2u32
        |    int(u32(a) + b)
        |""".stripMargin) shouldBe 3
  }

  "int(rhs) fix actually compiles" in {
    eval(
      """main() -> int
        |    val a: int = 1
        |    val b: u32 = 2u32
        |    a + int(b)
        |""".stripMargin) shouldBe 3
  }
}
