package io.github.edadma.trisc

import io.github.edadma.indentation.IndentationLexical
import scala.util.parsing.input.CharSequenceReader.EofCh

class SyslLexical extends IndentationLexical(
  newlineBeforeIndent = true,
  newlineAfterDedent = true,
  startLineJoining = List("(", "[", "{"),
  endLineJoining = List(")", "]", "}"),
  lineComment = "//",
  blockCommentStart = "/*",
  blockCommentEnd = "*/",
) {
  reserved ++= List(
    "if", "then", "elif", "else", "while", "do", "for", "break", "continue", "return", "defer", "match",
    "import", "module", "private", "var", "val", "struct", "enum", "type", "sizeof", "asm", "extern", "endif", "new",
    "func",
    "int", "char", "byte", "bool", "void", "string",
    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "double", "f64",
    "true", "false",
    "end",
  )

  delimiters ++= List(
    "(", ")", "[", "]", "{", "}",
    "++", "--",
    "+", "-", "*", "/", "%",
    "<<", ">>",
    "==", "!=", "<=", ">=", "<", ">",
    "&&", "||", "!",
    "&", "|", "^", "~",
    "=", "+=", "-=", "*=", "/=", "%=",
    "&=", "|=", "^=", "<<=", ">>=",
    "->", "=>",
    ",", ":", ";", ".", "#",
  )

  private def hexDigit = elem("hex digit", c => c.isDigit || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F')

  private def exponent: Parser[List[Char]] =
    (elem('e') | elem('E')) ~ opt(elem('+') | elem('-')) ~ rep1(digit) ^^ {
      case e ~ sign ~ digits => e :: sign.toList ::: digits
    }

  // Integer type suffix: i8, i16, i32, i64, u8, u16, u32, u64
  private def typeSuffix: Parser[String] =
    (elem('i') | elem('u')) ~ rep1(digit) ^^ {
      case sign ~ digits => (sign :: digits).mkString
    }

  private def escapeChar: Parser[Char] =
    '\\' ~> (
      elem('n') ^^^ '\n' |
      elem('t') ^^^ '\t' |
      elem('r') ^^^ '\r' |
      elem('0') ^^^ '\u0000' |
      elem('\\') ^^^ '\\' |
      elem('\'') ^^^ '\'' |
      elem('"') ^^^ '"'
    )

  override def token: Parser[Token] =
    // Character literal: 'x' or '\n' — emitted as NumericLit with :char suffix
    '\'' ~> (escapeChar | chrExcept('\'', '\n', EofCh)) <~ '\'' ^^ { c =>
      NumericLit(s"${c.toLong}:char")
    } |
    // Float literal: digits.digits[e[+-]digits] or digits e[+-]digits
    rep1(digit) ~ '.' ~ rep1(digit) ~ opt(exponent) ^^ {
      case intPart ~ dot ~ fracPart ~ exp =>
        NumericLit((intPart ::: dot :: fracPart ::: exp.getOrElse(Nil)).mkString)
    } |
    rep1(digit) ~ exponent ^^ {
      case intPart ~ exp => NumericLit((intPart ::: exp).mkString)
    } |
    // Hex literal with optional type suffix: 0xFF, 0xFFu8
    '0' ~> (elem('x') | elem('X')) ~> rep1(hexDigit) ~ opt(typeSuffix) ^^ {
      case digits ~ suffix =>
        val value = java.lang.Long.parseLong(digits.mkString, 16).toString
        suffix match
          case Some(s) => NumericLit(s"$value:$s")
          case None => NumericLit(value)
    } |
    // Decimal literal with optional type suffix: 100, 100u32
    rep1(digit) ~ opt(typeSuffix) ^^ {
      case digits ~ suffix =>
        val value = digits.mkString
        suffix match
          case Some(s) => NumericLit(s"$value:$s")
          case None => NumericLit(value)
    } |
    // String literal with escape processing: "hello\nworld"
    '"' ~> rep(escapeChar | chrExcept('"', '\n', EofCh)) <~ '"' ^^ { chars =>
      StringLit(chars.mkString)
    } | super.token
}
