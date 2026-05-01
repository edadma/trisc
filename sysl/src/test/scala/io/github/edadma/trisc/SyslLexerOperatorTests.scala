package io.github.edadma.trisc

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Focused tests for the operator-char muncher in `SyslLexical`.
 *  Verifies that the greedy lexer produces the right token sequence for
 *  all the edge cases that the parser depends on (built-in operator
 *  doubling, prefix `*`/`&` context-sensitivity, multi-char user ops). */
class SyslLexerOperatorTests extends AnyWordSpec with Matchers {

  private def opTokens(src: String): List[String] =
    val l = new SyslLexical
    l.scan(src)
      .filter(t => t.isInstanceOf[l.Keyword] || t.isInstanceOf[l.Identifier] || t.isInstanceOf[l.NumericLit])
      .map(_.chars)
      .toList

  "single-char operators" should {
    "lex `+`, `-`, `*`, `/`, `%` standalone" in {
      opTokens("a + b - c * d / e % f") shouldBe
        List("a", "+", "b", "-", "c", "*", "d", "/", "e", "%", "f")
    }
  }

  "built-in multi-char operators (greedy munch)" should {
    "lex `==`, `!=`, `<=`, `>=`" in {
      opTokens("a == b != c <= d >= e") shouldBe
        List("a", "==", "b", "!=", "c", "<=", "d", ">=", "e")
    }
    "lex `<<`, `>>`, `&&`, `||`" in {
      opTokens("a << b >> c && d || e") shouldBe
        List("a", "<<", "b", ">>", "c", "&&", "d", "||", "e")
    }
    "lex compound assignments `+=` `-=` `*=` `/=` `%=` `&=` `|=` `^=` `<<=` `>>=`" in {
      // Each compound op is its own munched token, with operands intact
      // around it. (Ten ops, lexed independently — no false longer-match.)
      opTokens("a += 1") shouldBe List("a", "+=", "1")
      opTokens("a -= 1") shouldBe List("a", "-=", "1")
      opTokens("a *= 1") shouldBe List("a", "*=", "1")
      opTokens("a /= 1") shouldBe List("a", "/=", "1")
      opTokens("a %= 1") shouldBe List("a", "%=", "1")
      opTokens("a &= 1") shouldBe List("a", "&=", "1")
      opTokens("a |= 1") shouldBe List("a", "|=", "1")
      opTokens("a ^= 1") shouldBe List("a", "^=", "1")
      opTokens("a <<= 1") shouldBe List("a", "<<=", "1")
      opTokens("a >>= 1") shouldBe List("a", ">>=", "1")
    }
    "lex arrows `->` and `=>`" in {
      opTokens("a -> b => c") shouldBe List("a", "->", "b", "=>", "c")
    }
    "lex postfix `++` and `--`" in {
      opTokens("a++ b--") shouldBe List("a", "++", "b", "--")
    }
  }

  "prefix `*` context-sensitivity" should {
    "split `**T` into two `*` tokens for ptr-to-ptr type syntax" in {
      opTokens("**T") shouldBe List("*", "*", "T")
    }
    "split `***T` into three `*` tokens" in {
      opTokens("***T") shouldBe List("*", "*", "*", "T")
    }
    "lex `*=` as a single compound-assign token" in {
      opTokens("x *= y") shouldBe List("x", "*=", "y")
    }
    "split `*=*p` into `*=` then `*` then `p`" in {
      opTokens("x*=*p") shouldBe List("x", "*=", "*", "p")
    }
    "split `*++p` (deref pre-inc) into `*` then `++` then `p`" in {
      opTokens("*++p") shouldBe List("*", "++", "p")
    }
    "split `*--p` (deref pre-dec) into `*` then `--` then `p`" in {
      opTokens("*--p") shouldBe List("*", "--", "p")
    }
    "split `*&a` (deref addr-of) into `*` then `&` then `a`" in {
      opTokens("*&a") shouldBe List("*", "&", "a")
    }
  }

  "prefix `&` context-sensitivity" should {
    "lex `&&` as one token (logical-and)" in {
      opTokens("a && b") shouldBe List("a", "&&", "b")
    }
    "lex `&=` as one token (compound and-assign)" in {
      opTokens("a &= b") shouldBe List("a", "&=", "b")
    }
    "split `&*p` (addr of deref) into `&` then `*` then `p`" in {
      opTokens("&*p") shouldBe List("&", "*", "p")
    }
    "split `&-x` (addr of negate) into `&` then `-` then `x`" in {
      opTokens("&-x") shouldBe List("&", "-", "x")
    }
  }

  "user-definable operator symbols" should {
    "lex `<>` as one token" in {
      opTokens("a <> b") shouldBe List("a", "<>", "b")
    }
    "lex `>>>` as one token" in {
      opTokens("a >>> b") shouldBe List("a", ">>>", "b")
    }
    "lex `|>` as one token" in {
      opTokens("a |> b") shouldBe List("a", "|>", "b")
    }
    "lex `<*>` as one token" in {
      opTokens("a <*> b") shouldBe List("a", "<*>", "b")
    }
    "lex `*>` as one token (allowed; no conflict with prefix-`*`)" in {
      opTokens("a *> b") shouldBe List("a", "*>", "b")
    }
    "lex `<*` as one token" in {
      opTokens("a <* b") shouldBe List("a", "<*", "b")
    }
    "lex `~~` as one token" in {
      opTokens("a ~~ b") shouldBe List("a", "~~", "b")
    }
    "lex `<=>` as one token (spaceship comparison)" in {
      opTokens("a <=> b") shouldBe List("a", "<=>", "b")
    }
    "lex `===` and `!==` as one token each" in {
      opTokens("a === b !== c") shouldBe List("a", "===", "b", "!==", "c")
    }
  }

  "comments take precedence over operator munching" should {
    "stop muncher before `//` line comment" in {
      // `+//comment` should be `+` then comment-eaten
      opTokens("a + // not part of op\nb") shouldBe List("a", "+", "b")
    }
    "stop muncher before `/*` block comment" in {
      opTokens("a +/*c*/ b") shouldBe List("a", "+", "b")
    }
    "lex `/` standalone when not starting a comment" in {
      opTokens("a / b") shouldBe List("a", "/", "b")
    }
  }
}
