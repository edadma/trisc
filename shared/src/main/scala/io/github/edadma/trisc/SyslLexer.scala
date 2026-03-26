package io.github.edadma.trisc

import io.github.edadma.gramma.*

object SyslLexer extends StdLexer:
  override protected def indentSensitive: Boolean = true

  delimiters ++= List(
    "(", ")", "[", "]",
    "+", "-", "*", "/", "%",
    "==", "!=", "<=", ">=", "<", ">",
    "&&", "||", "!",
    "=", "->",
    ",", ":", "&",
  )

  reserved ++= List(
    "if", "then", "else", "while", "for", "return",
    "int", "char", "void",
    "true", "false",
    "var",
  )

  override protected def skip(using ctx: LexCtx): Unit =
    skipWhitespace("//", "/*", "*/", false)
