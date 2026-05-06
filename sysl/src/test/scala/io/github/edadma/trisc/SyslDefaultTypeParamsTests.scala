package io.github.edadma.trisc

/** Phase B — default type parameters: `[T = Default]`.
 *
 *  At declaration time, any type parameter (on a trait, struct, enum, alias, or
 *  free function) may carry a default type expression: `[T = int]`,
 *  `[T: Ord = i64]` (bound first, default last). At instantiation time, missing
 *  trailing type slots fill from defaults; explicit args still win, and
 *  inference still wins where it succeeds.
 *
 *  Defaults resolve under the partially-built type env, so later parameters can
 *  refer to earlier ones: `[I, O = I]`.
 */
class SyslDefaultTypeParamsTests extends SyslTestHelpers {

  // ===== Generic functions =====

  "generic fn default fires when no inference target" in {
    eval(
      """make[T = i64]() -> i64 = 7i64
        |main() -> int = i32(make())
        |""".stripMargin) shouldBe 7
  }

  "explicit type arg overrides the default on a generic fn" in {
    eval(
      """make[T = i64](x: T) -> T = x
        |main() -> int = make[int](42)
        |""".stripMargin) shouldBe 42
  }

  "inference still wins over a generic-fn default" in {
    eval(
      """make[T = i64](x: T) -> T = x
        |main() -> int = i32(make(5i64) + 37i64)
        |""".stripMargin) shouldBe 42
  }

  "default referencing earlier type param" in {
    eval(
      """pick[I, O = I](x: I) -> O = x
        |main() -> int = pick(42)
        |""".stripMargin) shouldBe 42
  }

  // ===== Generic structs =====

  "struct with default type param fills when omitted" in {
    eval(
      """struct Box[T = int]
        |    v: T
        |
        |main() -> int
        |    var b = Box(42)
        |    b.v
        |""".stripMargin) shouldBe 42
  }

  "explicit type arg overrides struct default" in {
    eval(
      """struct Box[T = i64]
        |    v: T
        |
        |main() -> int
        |    var b = Box[int](42)
        |    b.v
        |""".stripMargin) shouldBe 42
  }

  "struct partial defaults: trailing slot fills" in {
    eval(
      """struct Pair[A, B = int]
        |    a: A
        |    b: B
        |
        |main() -> int
        |    var p = Pair[i64](30i64, 12)
        |    i32(p.a) + p.b
        |""".stripMargin) shouldBe 42
  }

  // ===== Generic enums =====

  "enum with default type param: default variant constructed" in {
    eval(
      """enum Result[T, E = int]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |main() -> int
        |    val r: Result[int] = Err(42)
        |    r match
        |        Ok(v) -> v
        |        Err(e) -> e
        |""".stripMargin) shouldBe 42
  }

  // ===== Generic type aliases =====

  "type alias with default type param fills when omitted" in {
    eval(
      """struct Box[T]
        |    v: T
        |
        |type IntBox[T = int] = Box[T]
        |
        |main() -> int
        |    var b: IntBox = Box(42)
        |    b.v
        |""".stripMargin) shouldBe 42
  }

  // ===== Cross-module SMETA round-trip (v16) =====

  "generic struct with default rides through SMETA" in {
    val libs = Map(
      "deflib/d" ->
        """module deflib
          |
          |struct Box[T = int]
          |    v: T
          |""".stripMargin,
    )
    evalWithLibs(libs,
      """import deflib.*
        |
        |main() -> int
        |    var b = Box(42)
        |    b.v
        |""".stripMargin) shouldBe 42
  }

  "generic fn with default rides through SMETA" in {
    val libs = Map(
      "deflib/d" ->
        """module deflib
          |
          |make[T = i64]() -> i64 = 7i64
          |""".stripMargin,
    )
    evalWithLibs(libs,
      """import deflib.*
        |
        |main() -> int = i32(make())
        |""".stripMargin) shouldBe 7
  }
}
