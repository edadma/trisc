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
    "if", "then", "elif", "else", "while", "do", "for", "in", "downTo", "step", "break", "continue", "return", "defer", "match", "is", "_",
    "import", "module", "private", "var", "val", "def", "struct", "enum", "trait", "impl", "type", "interface", "sizeof", "asm", "extern", "endif", "new",
    "int", "char", "byte", "bool", "unit", "string",
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
    ",", ":", ";", "..<", "..", ".", "#", "?",
  )

  private def hexDigit = elem("hex digit", c => c.isDigit || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F')

  // Digits with optional `_` separators between (e.g. 1_000_000). Underscore must be followed by a digit.
  private def digits1: Parser[List[Char]] =
    digit ~ rep(rep(elem('_')) ~> digit) ^^ { case first ~ rest => first :: rest }

  private def hexDigits1: Parser[List[Char]] =
    hexDigit ~ rep(rep(elem('_')) ~> hexDigit) ^^ { case first ~ rest => first :: rest }

  private def exponent: Parser[List[Char]] =
    (elem('e') | elem('E')) ~ opt(elem('+') | elem('-')) ~ digits1 ^^ {
      case e ~ sign ~ digits => e :: sign.toList ::: digits
    }

  // Integer type suffix: i8, i16, i32, i64, u8, u16, u32, u64
  private def typeSuffix: Parser[String] =
    (elem('i') | elem('u')) ~ rep1(digit) ^^ {
      case sign ~ digits => (sign :: digits).mkString
    }

  private def escapeHexDigit: Parser[Char] =
    elem("hex digit", c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))

  private def escapeChar: Parser[Char] =
    '\\' ~> (
      elem('n') ^^^ '\n' |
      elem('t') ^^^ '\t' |
      elem('r') ^^^ '\r' |
      elem('0') ^^^ '\u0000' |
      elem('\\') ^^^ '\\' |
      elem('\'') ^^^ '\'' |
      elem('"') ^^^ '"' |
      elem('x') ~> escapeHexDigit ~ escapeHexDigit ^^ { case hi ~ lo =>
        Integer.parseInt(s"$hi$lo", 16).toChar
      }
    )

  // Interpolated string: s"..." — uses "s:" prefix in token value to mark it
  // Custom parser to avoid consuming 's' when not followed by '"'
  private def interpStringLit: Parser[Token] =
    Parser { in =>
      if in.first == 's' && !in.rest.atEnd && in.rest.first == '"' then
        val bodyParser = rep(escapeChar | chrExcept('"', '\n', EofCh)) <~ '"'
        bodyParser(in.rest.rest) match // skip 's' and opening '"'
          case Success(chars, next) => Success(StringLit("s:" + chars.mkString), next)
          case ns: NoSuccess => ns
      else
        Failure("not an interpolated string", in)
    }

  // Formatted string: f"..." — uses "f:" prefix in token value
  private def fmtStringLit: Parser[Token] =
    Parser { in =>
      if in.first == 'f' && !in.rest.atEnd && in.rest.first == '"' then
        val bodyParser = rep(escapeChar | chrExcept('"', '\n', EofCh)) <~ '"'
        bodyParser(in.rest.rest) match
          case Success(chars, next) => Success(StringLit("f:" + chars.mkString), next)
          case ns: NoSuccess => ns
      else
        Failure("not a formatted string", in)
    }

  override def token: Parser[Token] =
    // Interpolated/formatted string literals — must come before identifiers
    interpStringLit |
    fmtStringLit |
    // Character literal: 'x' or '\n' — emitted as NumericLit with :char suffix
    '\'' ~> (escapeChar | chrExcept('\'', '\n', EofCh)) <~ '\'' ^^ { c =>
      NumericLit(s"${c.toLong}:char")
    } |
    // Float literal: digits.digits[e[+-]digits] or digits e[+-]digits
    digits1 ~ '.' ~ digits1 ~ opt(exponent) ^^ {
      case intPart ~ dot ~ fracPart ~ exp =>
        NumericLit((intPart ::: dot :: fracPart ::: exp.getOrElse(Nil)).mkString)
    } |
    digits1 ~ exponent ^^ {
      case intPart ~ exp => NumericLit((intPart ::: exp).mkString)
    } |
    // Hex literal with optional type suffix: 0xFF, 0xFFu8, 0xFF_FF
    '0' ~> (elem('x') | elem('X')) ~> hexDigits1 ~ opt(typeSuffix) ^^ {
      case digits ~ suffix =>
        val value = java.lang.Long.parseUnsignedLong(digits.mkString, 16).toString
        suffix match
          case Some(s) => NumericLit(s"$value:$s")
          case None => NumericLit(value)
    } |
    // Decimal literal with optional type suffix: 100, 100u32, 1_000_000
    digits1 ~ opt(typeSuffix) ^^ {
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
