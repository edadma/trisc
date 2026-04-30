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
    "if", "then", "elif", "else", "while", "do", "for", "loop", "in", "downTo", "step", "break", "continue", "return", "defer", "match", "is", "_",
    "import", "module", "private", "volatile", "var", "val", "const", "def", "struct", "enum", "trait", "impl", "type", "interface", "sizeof", "asm", "extern", "endif", "new", "within", "where", "require", "ensure", "not", "null", "invariant", "variant", "assume", "static_assert",
    "int", "uint", "long", "ulong", "short", "ushort", "char", "byte", "bool", "unit", "string",
    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "float", "f32", "double", "f64",
    "true", "false",
    "end",
  )

  // Operator-char sequences (composed of + - * / % < > = ! & | ^ ~) are
  // tokenized greedily by `operatorMuncher` below into a single Keyword
  // token, but each built-in operator string must still be registered
  // here — scala-parser-combinators 2.4 enforces a parser-literal
  // whitelist against `delimiters`/`reserved`. So the listing of
  // operator strings here is now a *vocabulary registration* for the
  // parser, not a tokenization rule.
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
    ",", "::", ":", ";", "..<", "..", ".", "#", "?",
  )

  // Operator characters — any greedy sequence of these forms one
  // OPERATOR token (Keyword). Built-in operators (+ - * / % == != < <= > >=
  // && || << >> & | ^ ~ ! and the assignment compounds += -= ... ->) and
  // user-defined operators (e.g. <> >>> |> ~~) all funnel through this
  // path; the parser/analyzer then dispatches by symbol. See
  // project_sysl_operator_overload_design.md for the full design.
  private val opChars: Set[Char] =
    Set('+', '-', '*', '/', '%', '<', '>', '=', '!', '&', '|', '^', '~')

  // True if `r` starts a comment (`//` or `/*`). Comments take precedence
  // over operator munching: a `/` that begins a comment must NOT be eaten
  // as part of an operator token.
  private def startsComment(r: scala.util.parsing.input.Reader[Char]): Boolean =
    !r.atEnd && r.first == '/' && !r.rest.atEnd &&
      (r.rest.first == '/' || r.rest.first == '*')

  // Decide whether the muncher should stop *before* appending `c` to `buf`.
  // This is the only place where `*` and `&` get context-sensitive — both
  // act as prefix unary operators (deref / addr-of) and as type sigils, so
  // they MUST lex as single-char tokens unless paired with `=` (or, for
  // `&`, with another `&`). Without this rule, `**T`, `*&a`, `*++p`,
  // `*=*p`, etc. would all be miscategorized.
  //
  //   - `*` mid-token: always stop (preserves `**T`, `*=*p`, etc.).
  //   - `&` mid-token: stop unless prev char is `&` (allows `&&`).
  //   - After a `*`: only `=` may follow (allows `*=`).
  //   - After a `&`: only `&` (for `&&`) or `=` (for `&=`).
  //
  // No other rules — `+ - / % < > = ! | ^ ~` chain freely so `<<=`, `>>=`,
  // `==`, `!=`, `++`, `--`, `||`, `->`, `=>`, `..` etc. munch as today.
  private def shouldStop(buf: StringBuilder, c: Char): Boolean =
    if buf.isEmpty then false
    else if c == '*' then true
    else if c == '&' then buf.last != '&'
    else if buf.last == '*' then c != '='
    else if buf.last == '&' then c != '=' && c != '&'
    else false

  private def operatorMuncher: Parser[Token] =
    Parser { in =>
      if in.atEnd || !opChars(in.first) || startsComment(in) then
        Failure("not an operator", in)
      else
        val buf = new StringBuilder
        var cur: scala.util.parsing.input.Reader[Char] = in
        var stop = false
        while !stop && !cur.atEnd && opChars(cur.first) && !startsComment(cur) do
          if shouldStop(buf, cur.first) then
            stop = true
          else
            buf.append(cur.first)
            cur = cur.rest
        Success(Keyword(buf.toString), cur)
    }

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
    } |
    operatorMuncher |
    super.token
}
