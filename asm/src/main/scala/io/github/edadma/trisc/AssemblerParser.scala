package io.github.edadma.trisc

import scala.util.parsing.combinator.RegexParsers

object AssemblerParser extends RegexParsers:
  // Skip spaces, tabs, and ; comments, but NOT newlines (they delimit lines)
  override val whiteSpace = "([ \t]|;[^\n]*)+".r

  def parseExpression(input: String): ExprAST =
    parseAll(expression, input) match
      case Success(ast, _) => ast
      case e: NoSuccess    => sys.error(s"parse error: $e")

  def parseAssembly(input: String): Seq[LineAST] =
    parseAll(assembly, input) match
      case Success(ast, _) => ast
      case e: NoSuccess    => sys.error(s"parse error: $e")

  // Newlines (one or more)
  private val nl: Parser[Any] = rep1("\n")
  private val optNl: Parser[Any] = rep("\n")

  private lazy val assembly: Parser[Seq[LineAST]] = optNl ~> repsep(line, nl) <~ optNl ^^ (_.flatten)

  // --- Tokens ---

  private val ident: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r

  private val hexLit: Parser[String] = "0[xX][0-9a-fA-F]+".r

  private val floatLit: Parser[String] = "[0-9]+\\.[0-9]+([eE][+-]?[0-9]+)?".r | "[0-9]+[eE][+-]?[0-9]+".r

  private val intLit: Parser[String] = "[0-9]+".r

  private def escape: Parser[Char] =
    "\\" ~> (
      "n" ^^^ '\n'
      | "r" ^^^ '\r'
      | "t" ^^^ '\t'
      | "b" ^^^ '\b'
      | "f" ^^^ '\f'
      | "0" ^^^ '\u0000'
      | "\\" ^^^ '\\'
      | "'" ^^^ '\''
      | "\"" ^^^ '"'
      | "x" ~> "[0-9a-fA-F]{2}".r ^^ (s => Integer.parseInt(s, 16).toChar)
      | "u" ~> "[0-9a-fA-F]{4}".r ^^ (s => Integer.parseInt(s, 16).toChar)
    )

  private val stringLit: Parser[String] =
    "\"" ~> rep(escape | "[^\"\\\\\\n]".r ^^ (_.charAt(0))) <~ "\"" ^^ (_.mkString)

  private val charLit: Parser[Long] =
    "'" ~> (escape | "[^'\\\\\\n]".r ^^ (_.charAt(0))) <~ "'" ^^ (_.toLong)

  // --- Registers ---

  private val register: Parser[RegisterExprAST] =
    "\\br[0-7]\\b".r ^^ (s => RegisterExprAST(s.charAt(1) - '0'))
    | kw("sp") ^^^ RegisterExprAST(7)

  // --- Expressions ---

  private lazy val primary: Parser[ExprAST] =
    register
    | hexLit ^^ (s => LongExprAST(java.lang.Long.parseLong(s.drop(2), 16)))
    | floatLit ^^ (s => DoubleExprAST(s.toDouble))
    | charLit ^^ (n => LongExprAST(n))
    | intLit ^^ (s => LongExprAST(s.toLong))
    | stringLit ^^ StringExprAST.apply
    | "." ~> ident ^^ (l => LocalExprAST(l, null))
    | ident ^^ ReferenceExprAST.apply
    | "(" ~> expression <~ ")"

  private lazy val unary: Parser[ExprAST] =
    "-" ~> primary ^^ (e => UnaryExprAST("-", e))
    | primary

  private lazy val expression: Parser[ExprAST] = unary

  // --- Mnemonics ---

  private val mnemonicSet = Set(
    "adc", "add", "addi", "and", "asr", "auipc",
    "bclr", "beq", "bge", "bgeu", "bgs", "bgu", "ble", "bleu", "bls", "blu", "bne", "bra", "bset", "btst",
    "chk", "cli", "clz", "cnt", "ctz", "cvt",
    "div", "divu",
    "exg",
    "f32tof64", "f64tof32", "fabs", "fadd", "fdiv", "fence", "fint", "fmul", "fneg", "fseq", "fslt", "fsqrt", "fsub",
    "gasid", "gfault", "gfcause", "gpsr", "gptbr", "gusp",
    "halt",
    "jalr",
    "ld", "ldb", "ldc", "ldd", "ldi", "lds", "ldw", "ll", "lsl", "lsr",
    "max", "min", "mov", "movi", "mul", "mulh", "mulhsu", "mulhu",
    "neg", "nop", "not",
    "or",
    "popb", "popd", "popr", "pops", "popw", "pshb", "pshd", "pshr", "pshs", "pshw",
    "cas", "rem", "remu", "ret", "rev", "rol", "ror", "rte",
    "sasid", "sbc", "sc", "seb", "ses", "sew", "sext", "sli", "slt", "sltu", "spsr", "sptbr",
    "st", "stb", "std", "sti", "sts", "stw", "sub", "susp", "swsp",
    "tlbi", "tlbia", "trap", "trapv", "tsr",
    "wfi",
    "xor",
    "zeb", "zes", "zew",
  )

  private val mnemonic: Parser[String] = ident.filter(mnemonicSet.contains) withFailureMessage "expected mnemonic"

  private val instruction: Parser[InstructionLineAST] =
    mnemonic ~ repsep(expression, ",") ^^ { case m ~ es => InstructionLineAST(m, es) }

  // --- Directives ---

  private val directiveSet = Set("align", "db", "dd", "dl", "ds", "dw", "entry", "equ", "extern", "global", "include", "rb", "rd", "rl", "rs", "rw", "segment")

  private val segment: Parser[SegmentLineAST] = kw("segment") ~> ident ^^ SegmentLineAST.apply

  private val equate: Parser[EquateLineAST] = (ident <~ (kw("equ") | "=")) ~ expression ^^ { case n ~ e => EquateLineAST(n, e) }

  private val include: Parser[IncludeLineAST] = kw("include") ~> stringLit ^^ IncludeLineAST.apply

  private val alignDir: Parser[AlignLineAST] = kw("align") ~> (hexLit | intLit) ^^ { n =>
    val a = if n.startsWith("0x") || n.startsWith("0X") then Integer.parseInt(n.drop(2), 16) else n.toInt
    AlignLineAST(a)
  }

  private val entryDecl: Parser[EntryLineAST] = kw("entry") ~> ident ^^ EntryLineAST.apply

  private val externDecl: Parser[ExternLineAST] = kw("extern") ~> ident ^^ ExternLineAST.apply

  private val typeInfoToken: Parser[String] = ident | hexLit | intLit
  private val commaField: Parser[Seq[String]] = "," ~> rep1(typeInfoToken)

  private val globalDecl: Parser[GlobalLineAST] =
    kw("global") ~> ident ~ opt("," ~> ident ~ rep(commaField)) ^^ {
      case name ~ None => GlobalLineAST(name, SymbolType.Func)
      case name ~ Some(typStr ~ fields) =>
        val symType = typStr match
          case "func"  => SymbolType.Func
          case "data"  => SymbolType.Data
          case "const" => SymbolType.Const
          case other   => sys.error(s"unknown symbol type '$other' (expected func, data, or const)")
        val isNumeric = (s: String) =>
          if s.startsWith("0x") || s.startsWith("0X") then
            val raw = s.drop(2)
            raw.nonEmpty && raw.forall(c => c.isDigit || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F')
          else s.nonEmpty && s.forall(_.isDigit)
        val parseNum = (s: String) => if s.startsWith("0x") then java.lang.Long.parseLong(s.drop(2), 16) else s.toLong
        val (symSize, typeInfo) = fields match
          case Nil => (None, None)
          case List(tokens) =>
            if symType == SymbolType.Data && tokens.size == 1 && isNumeric(tokens.head) then
              (Some(parseNum(tokens.head)), None)
            else
              (None, Some(tokens.mkString(" ")))
          case List(sizeTokens, tiTokens) =>
            (Some(parseNum(sizeTokens.head)), Some(tiTokens.mkString(" ")))
          case _ => sys.error("too many fields in global directive")
        GlobalLineAST(name, symType, symSize, typeInfo)
    }

  private def kw(s: String): Parser[String] = s"\\b$s\\b".r

  private val data: Parser[DataLineAST] =
    (kw("db") | kw("ds") | kw("dw") | kw("dl") | kw("dd")) ~ repsep(expression, ",") ^^ {
      case "db" ~ d => DataLineAST(1, d)
      case "ds" ~ d => DataLineAST(2, d)
      case "dw" ~ d => DataLineAST(4, d)
      case "dl" ~ d => DataLineAST(8, d)
      case "dd" ~ d => DataLineAST(0, d)
      case other ~ _ => sys.error(s"unexpected data directive: $other")
    }

  private val reserve: Parser[ReserveLineAST] =
    (kw("rb") | kw("rs") | kw("rw") | kw("rl") | kw("rd")) ~ expression ^^ {
      case "rb" ~ n => ReserveLineAST(1, n)
      case "rs" ~ n => ReserveLineAST(2, n)
      case "rw" ~ n => ReserveLineAST(4, n)
      case "rl" ~ n => ReserveLineAST(8, n)
      case "rd" ~ n => ReserveLineAST(0, n)
      case other ~ _ => sys.error(s"unexpected reserve directive: $other")
    }

  // --- Comments ---

  private val comment: Parser[CommentLineAST] = "#[^\n]*".r ^^ (s => CommentLineAST(s.drop(1).trim))

  // --- Labels ---

  // A colon-labeled ident is always a label (even if it matches a mnemonic)
  private val colonLabel: Parser[LabelLineAST] = ident <~ ":" ^^ LabelLineAST.apply
  // A bare ident is a label only if it's not a mnemonic or directive
  private val bareLabel: Parser[LabelLineAST] = ident.filter(s => !mnemonicSet.contains(s) && !directiveSet.contains(s)) ^^ LabelLineAST.apply
  private val label: Parser[LabelLineAST] = colonLabel | bareLabel

  private val local: Parser[LocalLineAST] = "." ~> ident <~ opt(":") ^^ LocalLineAST.apply

  // --- Lines ---

  private lazy val directive: Parser[LineAST] =
    comment | segment | alignDir | entryDecl | externDecl | globalDecl | include | data | reserve

  // Order matters:
  // 1. colonLabel first — `add:` is always a label
  // 2. directives and instructions before bare labels
  // 3. bare labels last (only non-mnemonic/directive identifiers)
  private lazy val simpleLine: Parser[LineAST] =
    directive | colonLabel | equate | instruction | local | bareLabel

  private lazy val labeledLine: Parser[LineAST] = instruction | directive

  private lazy val line: Parser[Seq[LineAST]] =
    // Label (with colon) followed by instruction/directive on same line
    (ident <~ ":") ~ labeledLine ^^ { case l ~ i => Seq(LabelLineAST(l), i) }
    // Non-mnemonic/directive ident (no colon) followed by instruction/directive on same line
    | (ident.filter(s => !mnemonicSet.contains(s) && !directiveSet.contains(s))) ~ labeledLine ^^ { case l ~ i => Seq(LabelLineAST(l), i) }
    | simpleLine ^^ (Seq(_))
    | success(Seq.empty)
