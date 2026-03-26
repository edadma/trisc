package io.github.edadma.trisc

import io.github.edadma.indentation.IndentationLexical

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
    "if", "then", "else", "while", "for", "return",
    "int", "char", "void",
    "true", "false",
    "end",
  )

  delimiters ++= List(
    "(", ")", "[", "]",
    "+", "-", "*", "/", "%",
    "==", "!=", "<=", ">=", "<", ">",
    "&&", "||", "!",
    "=", "->",
    ",", ":", "&",
  )
}
