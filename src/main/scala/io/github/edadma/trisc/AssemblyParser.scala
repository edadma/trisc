package io.github.edadma.trisc

import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharSequenceReader

object AssemblyParser extends StandardTokenParsers with PackratParsers with ImplicitConversions:
  override val lexical = new AssemblyLexer

  def parseExpr(input: String): Expr =
    phrase(expression)(new lexical.Scanner(new PackratReader(new CharSequenceReader(input)))) match {
      case Success(ast, _) => ast
      case e: NoSuccess    => sys.error(s"parse error: $e")
    }

  lexical.reserved ++= ("""
                          |equate
                          |segment
                          |r0
                          |r1
                          |r2
                          |r3
                          |r4
                          |r5
                          |r6
                          |r7
                          |sp
                          |""".trim.stripMargin split "\\s+")
  lexical.delimiters ++= ("+ - * / % ( )" split ' ')

  type P[+T] = PackratParser[T]

//  lazy val assembly: P[Seq[LineAST]] = rep1(declaration)

  lazy val register: P[RegisterExpr] =
    "r0" ^^^ RegisterExpr(0)
      | "r1" ^^^ RegisterExpr(1)
      | "r2" ^^^ RegisterExpr(2)
      | "r3" ^^^ RegisterExpr(3)
      | "r4" ^^^ RegisterExpr(4)
      | "r5" ^^^ RegisterExpr(5)
      | "r6" ^^^ RegisterExpr(6)
      | "r7" ^^^ RegisterExpr(7)

  lazy val literal: P[LiteralExpr] = numericLit ^^ LiteralExpr.apply

  lazy val expression: P[Expr] =
    register
      | literal
