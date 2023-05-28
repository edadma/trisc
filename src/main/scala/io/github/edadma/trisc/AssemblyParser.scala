package io.github.edadma.trisc

import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharSequenceReader

object AssemblyParser extends StandardTokenParsers with PackratParsers with ImplicitConversions:
  override val lexical = new AssemblyLexer

  def parseExpression(input: String): ExprAST =
    phrase(expression)(new lexical.Scanner(new PackratReader(new CharSequenceReader(input)))) match {
      case Success(ast, _) => ast
      case e: NoSuccess    => sys.error(s"parse error: $e")
    }

  def parseAssembly(input: String): Seq[LineAST] =
    phrase(assembly)(new lexical.Scanner(new PackratReader(new CharSequenceReader(input)))) match {
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
                          |ldi
                          |addi
                          |stb
                          |sti
                          |bls
                          |beq
                          |brk
                          |""".trim.stripMargin split "\\s+")
  lexical.delimiters ++= ("+ - * / % ( ) : , = \n" split ' ')

  type P[+T] = PackratParser[T]

  lazy val nl: P[_] = rep("\n")

  lazy val assembly: P[Seq[LineAST]] = nl ~> repsep(line, nl) <~ nl ^^ (_.flatten)

  lazy val register: P[RegisterExprAST] =
    "r0" ^^^ RegisterExprAST(0)
      | "r1" ^^^ RegisterExprAST(1)
      | "r2" ^^^ RegisterExprAST(2)
      | "r3" ^^^ RegisterExprAST(3)
      | "r4" ^^^ RegisterExprAST(4)
      | "r5" ^^^ RegisterExprAST(5)
      | "r6" ^^^ RegisterExprAST(6)
      | "r7" ^^^ RegisterExprAST(7)

  lazy val literal: P[LiteralExprAST] = numericLit ^^ LiteralExprAST.apply

  lazy val string: P[StringExprAST] = stringLit ^^ StringExprAST.apply

  lazy val reference: P[ReferenceExprAST] = ident ^^ ReferenceExprAST.apply

  lazy val expression: P[ExprAST] = positioned(
    register
      | literal
      | string
      | reference,
  )

  lazy val label: P[LabelLineAST] = ident <~ opt(":") ^^ LabelLineAST.apply

  lazy val segment: P[SegmentLineAST] = "segment" ~> ident ^^ SegmentLineAST.apply

  lazy val equate: P[EquateLineAST] = ident ~> ("equate" | "=") ~ expression ^^ EquateLineAST.apply

  lazy val simpleLine: P[LineAST] =
    label
      | segment
      | equate
      | instruction

  lazy val line: P[Seq[LineAST]] =
    simpleLine ^^ (Seq(_))
      | label ~ instruction ^^ { case l ~ i => Seq(l, i) }

  lazy val mnemonics: P[String] =
    "ldi" | "addi" | "stb" | "sti" | "bls" | "beq" | "brk"

  lazy val instruction: P[InstructionLineAST] =
    mnemonics ~ repsep(expression, ",") ^^ { case m ~ es =>
      InstructionLineAST(m, es)
    }
