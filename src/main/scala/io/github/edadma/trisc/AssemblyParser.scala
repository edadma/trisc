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
                          |equ
                          |segment
                          |include
                          |db
                          |dd
                          |dl
                          |ds
                          |dw
                          |r0
                          |r1
                          |r2
                          |r3
                          |r4
                          |r5
                          |r6
                          |r7
                          |sp
                          |add
                          |addi
                          |and
                          |beq
                          |bls
                          |bra
                          |div
                          |gpsr
                          |halt
                          |jalr
                          |ld
                          |ldb
                          |ldd
                          |ldi
                          |lds
                          |ldw
                          |movi
                          |mul
                          |or
                          |resb
                          |resd
                          |resl
                          |ress
                          |resw
                          |rem
                          |rte
                          |sli
                          |spsr
                          |st
                          |stb
                          |std
                          |sti
                          |sts
                          |stw
                          |sub
                          |trap
                          |xor
                          |""".trim.stripMargin split "\\s+")
  lexical.delimiters ++= ("+ - * / % ( ) : , = . \n" split ' ')

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

  lazy val literal: P[ExprAST] = numericLit ^^ { n =>
    if n.startsWith("0x") then LongExprAST(java.lang.Long.parseLong(n.drop(2), 16))
    else if n.contains('.') || n.contains('e') || n.contains('E') then DoubleExprAST(n.toDouble)
    else LongExprAST(n.toLong)
  }

  lazy val string: P[StringExprAST] = stringLit ^^ StringExprAST.apply

  lazy val reference: P[ReferenceExprAST] = ident ^^ ReferenceExprAST.apply

  lazy val localReference: P[LocalExprAST] = "." ~> ident ^^ (l => LocalExprAST(l, null))

  lazy val primary: P[ExprAST] = positioned(
    register
      | literal
      | string
      | reference
      | localReference,
  )

  lazy val expression: P[ExprAST] = positioned(
    "-" ~ primary ^^ UnaryExprAST.apply
      | primary,
  )

  lazy val label: P[LabelLineAST] = ident <~ opt(":") ^^ LabelLineAST.apply

  lazy val local: P[LocalLineAST] = "." ~> ident <~ opt(":") ^^ LocalLineAST.apply

  lazy val segment: P[SegmentLineAST] = "segment" ~> ident ^^ SegmentLineAST.apply

  lazy val equate: P[EquateLineAST] = ident ~ (("equ" | "=") ~> expression) ^^ EquateLineAST.apply

  lazy val include: P[IncludeLineAST] = "include" ~> stringLit ^^ IncludeLineAST.apply

  lazy val data: P[DataLineAST] = ("db" | "ds" | "dw" | "dl" | "dd") ~ repsep(expression, ",") ^^ {
    case "db" ~ d => DataLineAST(1, d)
    case "ds" ~ d => DataLineAST(2, d)
    case "dw" ~ d => DataLineAST(4, d)
    case "dl" ~ d => DataLineAST(8, d)
    case "dd" ~ d => DataLineAST(0, d)
  }

  lazy val reserve: P[ReserveLineAST] = ("resb" | "ress" | "resw" | "resl" | "resd") ~ expression ^^ {
    case "resb" ~ n => ReserveLineAST(1, n)
    case "ress" ~ n => ReserveLineAST(2, n)
    case "resw" ~ n => ReserveLineAST(4, n)
    case "resl" ~ n => ReserveLineAST(8, n)
    case "resd" ~ n => ReserveLineAST(0, n)
  }

  lazy val simpleLine: P[LineAST] = positioned(
    segment
      | equate
      | label
      | local
      | include
      | instruction
      | data
      | reserve,
  )

  lazy val line: P[Seq[LineAST]] =
    simpleLine ^^ (Seq(_))
      | label ~ instruction ^^ { case l ~ i => Seq(l, i) }

  lazy val mnemonics: P[String] =
    "add" | "addi" | "and" | "beq" | "bls" | "bra" | "div" | "gpsr" | "halt" | "jalr" | "ld" | "ldb" | "ldd" | "ldi" | "lds" | "ldw" | "movi" | "mul" | "or" | "rem" | "rte" | "sli" | "spsr" | "st" | "stb" | "std" | "sti" | "sts" | "stw" | "sub" | "trap" | "xor"

  lazy val instruction: P[InstructionLineAST] =
    mnemonics ~ repsep(expression, ",") ^^ { case m ~ es =>
      InstructionLineAST(m, es)
    }
