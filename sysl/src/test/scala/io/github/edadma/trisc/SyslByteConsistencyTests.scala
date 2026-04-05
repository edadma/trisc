package io.github.edadma.trisc

import SyslType.*

// Regression tests for byte/i8/u8 consistency.
//
// Source-level `byte` is unsigned (U8 = UIntType(8)). Previously, several
// places confused `byte` with signed i8:
//   1. IntType(8).toString printed "byte" — but "byte" means U8 at the source
//      level, so this produced misleading error messages like
//      "cannot compare signed and unsigned: byte == u8" where both sides
//      looked unsigned to the user.
//   2. String indexing returned I8 (signed) — but sysl-reference documents
//      string as {ptr: *u8, len: i64}, so s[i] should be U8.
//   3. fromPrefix("byte") returned I8 for symmetry with (1), but the
//      source-level analyzer maps "byte" -> U8, so the two round-trip
//      directions disagreed.
//
class SyslByteConsistencyTests extends SyslTestHelpers {

  "IntType(8).toString is 'i8', not 'byte'" in {
    IntType(8).toString shouldBe "i8"
  }

  "UIntType(8).toString is 'u8'" in {
    UIntType(8).toString shouldBe "u8"
  }

  "source-level `byte` and `u8` are the same type" in {
    Byte shouldBe U8
  }

  "fromPrefix('byte') returns U8 (matches source-level analyzer)" in {
    SyslType.fromPrefix("byte") shouldBe U8
  }

  // End-to-end: byte() cast and s[i] must be comparable without type errors.
  // This was the originally-reported bug: the comparison produced
  // "cannot compare signed and unsigned: byte == u8" where both sides
  // already looked unsigned to the user.

  "byte(ch) and s[i] compare without signed/unsigned error" in {
    eval(
      """main() -> int
        |    val s = "1"
        |    if byte('1') == s[0] then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "byte(ch) and s[i] inequality" in {
    eval(
      """main() -> int
        |    val s = "1"
        |    if byte('2') != s[0] then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "string indexing result assigns to byte without cast" in {
    eval(
      """main() -> int
        |    val s = "A"
        |    var b: byte = s[0]
        |    int(b)
        |""".stripMargin) shouldBe 65
  }

  "string indexing result assigns to u8 without cast" in {
    eval(
      """main() -> int
        |    val s = "A"
        |    var b: u8 = s[0]
        |    int(b)
        |""".stripMargin) shouldBe 65
  }

}
