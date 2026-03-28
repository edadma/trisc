package io.github.edadma.trisc

import io.github.edadma.indentation.IndentationLexical
import scala.util.parsing.input.CharSequenceReader.EofCh

class SyslLexical extends IndentationLexical(
  newlineBeforeIndent = true,
  newlineAfterDedent = true,
  startLineJoining = List("(", "["),
  endLineJoining = List(")", "]"),
  lineComment = "//",
  blockCommentStart = "/*",
  blockCommentEnd = "*/",
) {
  reserved ++= List(
    "if", "then", "elif", "else", "while", "do", "for", "break", "continue", "return",
    "import", "private", "var", "val", "struct", "sizeof", "asm", "extern",
    "func",
    "int", "char", "byte", "bool", "void", "string",
    "i8", "i16", "i32", "i64", "double", "f64",
    "true", "false",
    "end",
  )

  delimiters ++= List(
    "(", ")", "[", "]",
    "++", "--",
    "+", "-", "*", "/", "%",
    "<<", ">>",
    "==", "!=", "<=", ">=", "<", ">",
    "&&", "||", "!",
    "&", "|", "^", "~",
    "=", "+=", "-=", "*=", "/=", "%=",
    "&=", "|=", "^=", "<<=", ">>=",
    "->",
    ",", ":", ";", ".",
  )

  private def hexDigit = elem("hex digit", c => c.isDigit || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F')

  private def exponent: Parser[List[Char]] =
    (elem('e') | elem('E')) ~ opt(elem('+') | elem('-')) ~ rep1(digit) ^^ {
      case e ~ sign ~ digits => e :: sign.toList ::: digits
    }

  override def token: Parser[Token] =
    // Float literal: digits.digits[e[+-]digits] or digits e[+-]digits
    rep1(digit) ~ '.' ~ rep1(digit) ~ opt(exponent) ^^ {
      case intPart ~ dot ~ fracPart ~ exp =>
        NumericLit((intPart ::: dot :: fracPart ::: exp.getOrElse(Nil)).mkString)
    } |
    rep1(digit) ~ exponent ^^ {
      case intPart ~ exp => NumericLit((intPart ::: exp).mkString)
    } |
    '0' ~> (elem('x') | elem('X')) ~> rep1(hexDigit) ^^ { digits =>
      NumericLit(java.lang.Long.parseLong(digits.mkString, 16).toString)
    } | super.token
}
