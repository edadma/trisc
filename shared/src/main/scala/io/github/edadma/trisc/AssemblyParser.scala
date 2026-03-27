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
                          |align
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
                          |adc
                          |add
                          |addi
                          |and
                          |asr
                          |auipc
                          |bclr
                          |beq
                          |bge
                          |bgeu
                          |bgt
                          |bgu
                          |ble
                          |bleu
                          |bls
                          |blu
                          |bne
                          |bra
                          |bset
                          |btst
                          |chk
                          |cli
                          |clz
                          |cnt
                          |ctz
                          |cvt
                          |div
                          |divu
                          |entry
                          |extern
                          |fabs
                          |fence
                          |fadd
                          |fdiv
                          |fint
                          |finv
                          |fmul
                          |fneg
                          |fpow
                          |fslt
                          |fsqrt
                          |fsub
                          |global
                          |gpsr
                          |gusp
                          |halt
                          |jalr
                          |ld
                          |ldb
                          |ldd
                          |ldi
                          |lds
                          |ll
                          |ldw
                          |lsl
                          |lsr
                          |mov
                          |movi
                          |mul
                          |mulu
                          |neg
                          |nop
                          |not
                          |or
                          |popb
                          |popd
                          |popr
                          |pops
                          |popw
                          |pshb
                          |pshd
                          |pshr
                          |pshs
                          |pshw
                          |resb
                          |resd
                          |resl
                          |ress
                          |resw
                          |rem
                          |remu
                          |ret
                          |rev
                          |rol
                          |ror
                          |rte
                          |sbc
                          |sc
                          |seb
                          |ses
                          |sew
                          |sext
                          |sli
                          |slt
                          |sltu
                          |spsr
                          |st
                          |stb
                          |std
                          |sti
                          |sts
                          |stw
                          |sub
                          |susp
                          |swsp
                          |trap
                          |trapv
                          |wfi
                          |xor
                          |zeb
                          |zes
                          |zew
                          |""".trim.stripMargin.split("\\s+"))
  lexical.delimiters ++= ("+ - * / % ( ) : , = . \n" split ' ')

  type P[+T] = PackratParser[T]

  lazy val nl: P[?] = rep("\n")

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

  lazy val alignDir: P[AlignLineAST] = "align" ~> numericLit ^^ { n =>
    val a = if n.startsWith("0x") then Integer.parseInt(n.drop(2), 16) else n.toInt
    AlignLineAST(a)
  }

  lazy val entryDecl: P[EntryLineAST] = "entry" ~> ident ^^ EntryLineAST.apply

  lazy val externDecl: P[ExternLineAST] = "extern" ~> ident ^^ ExternLineAST.apply

  lazy val typeInfoToken: P[String] = ident | numericLit
  lazy val commaField: P[Seq[String]] = "," ~> rep1(typeInfoToken)

  lazy val globalDecl: P[GlobalLineAST] =
    "global" ~> ident ~ opt("," ~> ident ~ rep(commaField)) ^^ {
      case name ~ None => GlobalLineAST(name, SymbolType.Func)
      case name ~ Some(typStr ~ fields) =>
        val symType = typStr match
          case "func"  => SymbolType.Func
          case "data"  => SymbolType.Data
          case "const" => SymbolType.Const
          case other   => sys.error(s"unknown symbol type '$other' (expected func, data, or const)")
        val isHex = (s: String) =>
          val raw = if s.startsWith("0x") || s.startsWith("0X") then s.drop(2) else s
          raw.nonEmpty && raw.forall(c => c.isDigit || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F')
        val parseNum = (s: String) => if s.startsWith("0x") then java.lang.Long.parseLong(s.drop(2), 16) else s.toLong
        val (symSize, typeInfo) = fields match
          case Nil => (None, None)
          case List(tokens) =>
            if symType == SymbolType.Data && tokens.size == 1 && isHex(tokens.head) then
              (Some(parseNum(tokens.head)), None)
            else
              (None, Some(tokens.mkString(" ")))
          case List(sizeTokens, tiTokens) =>
            (Some(parseNum(sizeTokens.head)), Some(tiTokens.mkString(" ")))
          case _ => sys.error("too many fields in global directive")
        GlobalLineAST(name, symType, symSize, typeInfo)
    }

  lazy val data: P[DataLineAST] = ("db" | "ds" | "dw" | "dl" | "dd") ~ repsep(expression, ",") ^^ {
    case "db" ~ d => DataLineAST(1, d)
    case "ds" ~ d => DataLineAST(2, d)
    case "dw" ~ d => DataLineAST(4, d)
    case "dl" ~ d => DataLineAST(8, d)
    case "dd" ~ d => DataLineAST(0, d)
    case other ~ _ => sys.error(s"unexpected data directive: $other")
  }

  lazy val reserve: P[ReserveLineAST] = ("resb" | "ress" | "resw" | "resl" | "resd") ~ expression ^^ {
    case "resb" ~ n => ReserveLineAST(1, n)
    case "ress" ~ n => ReserveLineAST(2, n)
    case "resw" ~ n => ReserveLineAST(4, n)
    case "resl" ~ n => ReserveLineAST(8, n)
    case "resd" ~ n => ReserveLineAST(0, n)
    case other ~ _ => sys.error(s"unexpected reserve directive: $other")
  }

  lazy val comment: P[CommentLineAST] = accept("comment", {
    case lexical.StringLit(s) if s.startsWith("#") => CommentLineAST(s.drop(1).trim)
  })

  lazy val simpleLine: P[LineAST] = positioned(
    comment
      | segment
      | alignDir
      | entryDecl
      | externDecl
      | globalDecl
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
    "adc" | "add" | "addi" | "and" | "asr" | "auipc" | "bclr" | "beq" | "bge" | "bgeu" | "bgt" | "bgu" | "ble" | "bleu" | "bls" | "blu" | "bne" | "bra" | "bset" | "btst" | "chk" | "cli" | "clz" | "cnt" | "ctz" | "cvt" | "div" | "divu" | "fabs" | "fadd" | "fdiv" | "fence" | "fint" | "finv" | "fmul" | "fneg" | "fpow" | "fslt" | "fsqrt" | "fsub" | "gpsr" | "gusp" | "halt" | "jalr" | "ld" | "ldb" | "ldd" | "ldi" | "lds" | "ldw" | "ll" | "lsl" | "lsr" | "mov" | "movi" | "mul" | "mulu" | "neg" | "nop" | "not" | "or" | "popb" | "popd" | "popr" | "pops" | "popw" | "pshb" | "pshd" | "pshr" | "pshs" | "pshw" | "rem" | "remu" | "ret" | "rev" | "rol" | "ror" | "rte" | "sbc" | "sc" | "seb" | "ses" | "sew" | "sext" | "sli" | "slt" | "sltu" | "spsr" | "st" | "stb" | "std" | "sti" | "sts" | "stw" | "sub" | "susp" | "swsp" | "trap" | "trapv" | "wfi" | "xor" | "zeb" | "zes" | "zew"

  lazy val instruction: P[InstructionLineAST] =
    mnemonics ~ repsep(expression, ",") ^^ { case m ~ es =>
      InstructionLineAST(m, es)
    }
