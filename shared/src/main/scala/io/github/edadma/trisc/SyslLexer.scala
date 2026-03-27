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
    "import", "private", "var",
    "func",
    "int", "char", "byte", "bool", "void",
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
    ",", ":", ";",
  )

  private def hexDigit = elem("hex digit", c => c.isDigit || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F')

  override def token: Parser[Token] =
    '0' ~> (elem('x') | elem('X')) ~> rep1(hexDigit) ^^ { digits =>
      NumericLit(java.lang.Long.parseLong(digits.mkString, 16).toString)
    } | super.token
}
