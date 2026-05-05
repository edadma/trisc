package io.github.edadma.trisc

/** Phase B.2 — type-parameter defaults that reference an associated-type
 *  projection of an earlier (bounded) parameter.
 *
 *  `pull[I: Reader, T = I::Token](i: I) -> T = ...`
 *
 *  When `pull(int_input)` is instantiated, `I` is bound to `int`, and `T`
 *  must default to whatever `int`'s `Reader` impl declares as `Token`.
 *  This requires `assocBindingsEnv` to be active during default resolution.
 */
class SyslDefaultRefAssocTests extends SyslTestHelpers {

  "default referencing assoc type of earlier bounded param" in {
    eval(
      """trait Reader[I]
        |    type Token
        |    head(i: I) -> Self::Token
        |
        |impl Reader[int]
        |    type Token = i64
        |    head(i: int) -> i64 = i64(i) * 2i64
        |
        |pull[I: Reader, T = I::Token](i: I) -> T = Reader.head(i)
        |
        |main() -> int = i32(pull(21))
        |""".stripMargin) shouldBe 42
  }
}
