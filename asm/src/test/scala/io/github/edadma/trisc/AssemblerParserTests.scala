package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AssemblerParserTests extends AnyFreeSpec with Matchers {

  def parse(s: String): Seq[LineAST] = AssemblerParser.parseAssembly(s)
  def parseExpr(s: String): ExprAST = AssemblerParser.parseExpression(s)

  // ===== Word boundaries =====

  "identifier starting with mnemonic prefix parses as label" in {
    val result = parse("addr\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "addr"
  }

  "identifier starting with db parses as label" in {
    val result = parse("dbl\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "dbl"
  }

  "identifier starting with res parses as label" in {
    val result = parse("reset\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "reset"
  }

  "identifier starting with st parses as label" in {
    val result = parse("store\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "store"
  }

  "identifier starting with entry parses as label" in {
    val result = parse("entry_point\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "entry_point"
  }

  "identifier starting with global parses as label" in {
    val result = parse("global_var\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "global_var"
  }

  "identifier starting with align parses as label" in {
    val result = parse("alignment\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "alignment"
  }

  "identifier starting with segment parses as label" in {
    val result = parse("segments\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "segments"
  }

  "identifier starting with include parses as label" in {
    val result = parse("includes\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "includes"
  }

  "identifier starting with extern parses as label" in {
    val result = parse("external\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "external"
  }

  "identifier starting with equ parses as label" in {
    val result = parse("equals\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "equals"
  }

  "double as label name" in {
    val result = parse("double\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "double"
  }

  "int as label name" in {
    val result = parse("int\n  halt\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "int"
  }

  // ===== Label/instruction ambiguity =====

  "mnemonic with colon is label" in {
    val result = parse("halt:\n  ldi r1, 1\n")
    result.head shouldBe a[LabelLineAST]
    result.head.asInstanceOf[LabelLineAST].name shouldBe "halt"
  }

  "mnemonic without colon and no operands is instruction" in {
    val result = parse("halt\n")
    result.head shouldBe a[InstructionLineAST]
    result.head.asInstanceOf[InstructionLineAST].mnemonic shouldBe "halt"
  }

  "mnemonic with colon followed by instruction" in {
    val result = parse("loop: bra loop\n")
    result should have size 2
    result(0) shouldBe LabelLineAST("loop")
    result(1) shouldBe a[InstructionLineAST]
    result(1).asInstanceOf[InstructionLineAST].mnemonic shouldBe "bra"
  }

  "non-mnemonic label without colon followed by instruction" in {
    val result = parse("myloop bra myloop\n")
    result should have size 2
    result(0) shouldBe LabelLineAST("myloop")
    result(1) shouldBe a[InstructionLineAST]
  }

  "non-mnemonic label without colon followed by directive" in {
    val result = parse("data db 0x42\n")
    result should have size 2
    result(0) shouldBe LabelLineAST("data")
    result(1) shouldBe a[DataLineAST]
  }

  "label with colon followed by directive" in {
    val result = parse("mydata: dw 0x1234\n")
    result should have size 2
    result(0) shouldBe LabelLineAST("mydata")
    result(1) shouldBe a[DataLineAST]
  }

  "label alone on line" in {
    val result = parse("start\n  halt\n")
    result(0) shouldBe LabelLineAST("start")
  }

  "label with colon alone on line" in {
    val result = parse("start:\nhalt\n")
    result(0) shouldBe LabelLineAST("start")
  }

  // ===== Local labels =====

  "local label parses" in {
    val result = parse(".loop\n  halt\n")
    result.head shouldBe a[LocalLineAST]
    result.head.asInstanceOf[LocalLineAST].name shouldBe "loop"
  }

  "local label with colon parses" in {
    val result = parse(".done:\n  halt\n")
    result.head shouldBe a[LocalLineAST]
    result.head.asInstanceOf[LocalLineAST].name shouldBe "done"
  }

  "local reference in operand" in {
    val result = parse("bra .loop\n")
    val insn = result.head.asInstanceOf[InstructionLineAST]
    insn.operands.head shouldBe a[LocalExprAST]
    insn.operands.head.asInstanceOf[LocalExprAST].local shouldBe "loop"
  }

  // ===== String and char literals =====

  "simple string literal" in {
    val result = parse("db \"hello\"\n")
    val data = result.head.asInstanceOf[DataLineAST]
    data.data.head.asInstanceOf[StringExprAST].s shouldBe "hello"
  }

  "string with newline escape" in {
    val result = parse("db \"line1\\nline2\"\n")
    val data = result.head.asInstanceOf[DataLineAST]
    data.data.head.asInstanceOf[StringExprAST].s shouldBe "line1\nline2"
  }

  "string with tab escape" in {
    val result = parse("db \"col1\\tcol2\"\n")
    val data = result.head.asInstanceOf[DataLineAST]
    data.data.head.asInstanceOf[StringExprAST].s shouldBe "col1\tcol2"
  }

  "string with backslash escape" in {
    val result = parse("db \"a\\\\b\"\n")
    val data = result.head.asInstanceOf[DataLineAST]
    data.data.head.asInstanceOf[StringExprAST].s shouldBe "a\\b"
  }

  "string with hex escape" in {
    val result = parse("db \"\\x41\"\n")
    val data = result.head.asInstanceOf[DataLineAST]
    data.data.head.asInstanceOf[StringExprAST].s shouldBe "A"
  }

  "string with unicode escape" in {
    val result = parse("db \"\\u0041\"\n")
    val data = result.head.asInstanceOf[DataLineAST]
    data.data.head.asInstanceOf[StringExprAST].s shouldBe "A"
  }

  "string with null escape" in {
    val result = parse("db \"hello\\0\"\n")
    val data = result.head.asInstanceOf[DataLineAST]
    data.data.head.asInstanceOf[StringExprAST].s shouldBe "hello\u0000"
  }

  "char literal simple" in {
    val result = parse("ldi r1, 'A'\n")
    val insn = result.head.asInstanceOf[InstructionLineAST]
    insn.operands(1).asInstanceOf[LongExprAST].n shouldBe 65
  }

  "char literal newline escape" in {
    val result = parse("ldi r1, '\\n'\n")
    val insn = result.head.asInstanceOf[InstructionLineAST]
    insn.operands(1).asInstanceOf[LongExprAST].n shouldBe 10
  }

  "char literal backslash escape" in {
    val result = parse("ldi r1, '\\\\'\n")
    val insn = result.head.asInstanceOf[InstructionLineAST]
    insn.operands(1).asInstanceOf[LongExprAST].n shouldBe 92
  }

  "char literal single quote escape" in {
    val result = parse("ldi r1, '\\''\n")
    val insn = result.head.asInstanceOf[InstructionLineAST]
    insn.operands(1).asInstanceOf[LongExprAST].n shouldBe 39
  }

  // ===== Numeric literals =====

  "decimal integer" in {
    parseExpr("42").asInstanceOf[LongExprAST].n shouldBe 42
  }

  "hex integer" in {
    parseExpr("0xFF").asInstanceOf[LongExprAST].n shouldBe 255
  }

  "hex integer uppercase X" in {
    parseExpr("0XFF").asInstanceOf[LongExprAST].n shouldBe 255
  }

  "float literal" in {
    parseExpr("3.14").asInstanceOf[DoubleExprAST].n shouldBe 3.14
  }

  "float with exponent" in {
    parseExpr("1e10").asInstanceOf[DoubleExprAST].n shouldBe 1e10
  }

  "float with decimal and exponent" in {
    parseExpr("1.5e2").asInstanceOf[DoubleExprAST].n shouldBe 150.0
  }

  "float with negative exponent" in {
    parseExpr("1.5e-2").asInstanceOf[DoubleExprAST].n shouldBe 0.015
  }

  "zero" in {
    parseExpr("0").asInstanceOf[LongExprAST].n shouldBe 0
  }

  // ===== Comments =====

  "hash comment standalone" in {
    val result = parse("# this is a comment\nhalt\n")
    result(0) shouldBe a[CommentLineAST]
    result(0).asInstanceOf[CommentLineAST].text shouldBe "this is a comment"
  }

  "semicolon inline comment after instruction" in {
    val result = parse("ldi r1, 42 ; load 42\nhalt\n")
    result(0) shouldBe a[InstructionLineAST]
    result(0).asInstanceOf[InstructionLineAST].mnemonic shouldBe "ldi"
  }

  "semicolon inline comment after label" in {
    val result = parse("start: ; entry point\nhalt\n")
    result(0) shouldBe a[LabelLineAST]
  }

  "hash and semicolon coexist" in {
    val result = parse("# line comment\nldi r1, 42 ; inline\nhalt\n")
    result(0) shouldBe a[CommentLineAST]
    result(1) shouldBe a[InstructionLineAST]
  }

  // ===== Blank lines and whitespace =====

  "blank lines between instructions" in {
    val result = parse("ldi r1, 1\n\n\nldi r2, 2\nhalt\n")
    val insns = result.collect { case i: InstructionLineAST => i }
    insns should have size 3
  }

  "trailing newlines" in {
    val result = parse("halt\n\n\n")
    result.collect { case i: InstructionLineAST => i } should have size 1
  }

  "leading newlines" in {
    val result = parse("\n\nhalt\n")
    result.collect { case i: InstructionLineAST => i } should have size 1
  }

  // ===== Registers =====

  "all registers parse" in {
    for i <- 0 to 7 do
      val result = parse(s"ldi r$i, 0\n")
      val insn = result.head.asInstanceOf[InstructionLineAST]
      insn.operands.head.asInstanceOf[RegisterExprAST].reg shouldBe i
  }

  "sp is r7" in {
    val result = parse("ldi sp, 0\n")
    val insn = result.head.asInstanceOf[InstructionLineAST]
    insn.operands.head.asInstanceOf[RegisterExprAST].reg shouldBe 7
  }

  "identifier starting with r0 is not register" in {
    val result = parse("r0data\nhalt\n")
    result(0) shouldBe a[LabelLineAST]
    result(0).asInstanceOf[LabelLineAST].name shouldBe "r0data"
  }

  // ===== Expressions =====

  "negative operand" in {
    val result = parse("addi r1, r2, -5\n")
    val insn = result.head.asInstanceOf[InstructionLineAST]
    insn.operands(2) shouldBe a[UnaryExprAST]
    val unary = insn.operands(2).asInstanceOf[UnaryExprAST]
    unary.op shouldBe "-"
    unary.expr.asInstanceOf[LongExprAST].n shouldBe 5
  }

  "reference as operand" in {
    val result = parse("bra target\n")
    val insn = result.head.asInstanceOf[InstructionLineAST]
    insn.operands.head.asInstanceOf[ReferenceExprAST].ref shouldBe "target"
  }

  "string with zero in db" in {
    val result = parse("db \"hello\", 0\n")
    val data = result.head.asInstanceOf[DataLineAST]
    data.data should have size 2
    data.data(0).asInstanceOf[StringExprAST].s shouldBe "hello"
    data.data(1).asInstanceOf[LongExprAST].n shouldBe 0
  }

  // ===== Directives =====

  "db directive" in {
    val result = parse("db 1, 2, 3\n")
    val data = result.head.asInstanceOf[DataLineAST]
    data.width shouldBe 1
    data.data should have size 3
  }

  "ds directive" in {
    val result = parse("ds 0x1234\n")
    result.head.asInstanceOf[DataLineAST].width shouldBe 2
  }

  "dw directive" in {
    val result = parse("dw 0x12345678\n")
    result.head.asInstanceOf[DataLineAST].width shouldBe 4
  }

  "dl directive" in {
    val result = parse("dl 0\n")
    result.head.asInstanceOf[DataLineAST].width shouldBe 8
  }

  "dd directive with float" in {
    val result = parse("dd 3.14\n")
    val data = result.head.asInstanceOf[DataLineAST]
    data.width shouldBe 0
    data.data.head.asInstanceOf[DoubleExprAST].n shouldBe 3.14
  }

  "rb directive" in {
    val result = parse("rb 100\n")
    val res = result.head.asInstanceOf[ReserveLineAST]
    res.width shouldBe 1
    res.n.asInstanceOf[LongExprAST].n shouldBe 100
  }

  "align directive decimal" in {
    val result = parse("align 8\n")
    result.head.asInstanceOf[AlignLineAST].alignment shouldBe 8
  }

  "align directive hex" in {
    val result = parse("align 0x10\n")
    result.head.asInstanceOf[AlignLineAST].alignment shouldBe 16
  }

  "segment directive" in {
    val result = parse("segment code\n")
    result.head.asInstanceOf[SegmentLineAST].name shouldBe "code"
  }

  "entry directive" in {
    val result = parse("entry main\n")
    result.head.asInstanceOf[EntryLineAST].name shouldBe "main"
  }

  "extern directive" in {
    val result = parse("extern printf\n")
    result.head.asInstanceOf[ExternLineAST].name shouldBe "printf"
  }

  "equate with equals" in {
    val result = parse("STDOUT = 0xFF8\n")
    val eq = result.head.asInstanceOf[EquateLineAST]
    eq.name shouldBe "STDOUT"
    eq.expr.asInstanceOf[LongExprAST].n shouldBe 0xFF8
  }

  "equate with equ" in {
    val result = parse("SIZE equ 100\n")
    val eq = result.head.asInstanceOf[EquateLineAST]
    eq.name shouldBe "SIZE"
  }

  "include directive" in {
    val result = parse("include \"header.asm\"\n")
    result.head.asInstanceOf[IncludeLineAST].path shouldBe "header.asm"
  }

  "global with func type" in {
    val result = parse("global main, func\n")
    val g = result.head.asInstanceOf[GlobalLineAST]
    g.name shouldBe "main"
    g.typ shouldBe SymbolType.Func
  }

  "global with data type and size" in {
    val result = parse("global buffer, data, 100\n")
    val g = result.head.asInstanceOf[GlobalLineAST]
    g.name shouldBe "buffer"
    g.typ shouldBe SymbolType.Data
    g.size shouldBe Some(100)
  }

  // ===== Full programs =====

  "minimal program" in {
    val result = parse("halt\n")
    result should have size 1
    result.head.asInstanceOf[InstructionLineAST].mnemonic shouldBe "halt"
  }

  "program with vectors" in {
    val result = parse(
      """dd 0xFF0
        |dd 160
        |rb 144
        |ldi r1, 42
        |halt
        |""".stripMargin)
    val insns = result.collect { case i: InstructionLineAST => i }
    insns.map(_.mnemonic) shouldBe Seq("ldi", "halt")
  }

  "program with labels and branches" in {
    val result = parse(
      """start
        |  ldi r1, 0
        |loop
        |  addi r1, r1, 1
        |  ldi r2, 10
        |  bls r2, r1, done
        |  bra loop
        |done
        |  halt
        |""".stripMargin)
    val labels = result.collect { case l: LabelLineAST => l.name }
    labels shouldBe Seq("start", "loop", "done")
  }

  "program with equate and reference" in {
    val result = parse(
      """STDOUT = 0xFF8
        |  movi r1, STDOUT
        |  halt
        |""".stripMargin)
    result(0) shouldBe a[EquateLineAST]
    val insn = result(1).asInstanceOf[InstructionLineAST]
    insn.operands(1).asInstanceOf[ReferenceExprAST].ref shouldBe "STDOUT"
  }

  "label on same line as db with string and zero" in {
    val result = parse("msg db \"hello\", 0\n")
    result should have size 2
    result(0) shouldBe LabelLineAST("msg")
    result(1) shouldBe a[DataLineAST]
    val data = result(1).asInstanceOf[DataLineAST]
    data.data should have size 2
  }

  // Audit item #33: large i64 literals must round-trip through the parser.
  // Surfaced when std/math/bits TRISC-codegen output references Long.MinValue
  // (= -9223372036854775808). Magnitude 9223372036854775808 overflows Long
  // when parsed unsigned; we use BigInt + two's-complement truncation.

  "intLit Long.MaxValue parses as positive long" in {
    parseExpr("9223372036854775807") shouldBe LongExprAST(Long.MaxValue)
  }

  "intLit Long.MinValue magnitude wraps via two's-complement" in {
    // The bare magnitude (no minus sign) wraps to Long.MinValue. The compiler
    // only emits this in `-9223372036854775808` form; the unary `-` is then a
    // no-op on Long.MinValue (since `-Long.MinValue == Long.MinValue` in i64).
    parseExpr("9223372036854775808") shouldBe LongExprAST(Long.MinValue)
  }

  "intLit beyond Long.MinValue magnitude errors" in {
    an[Exception] shouldBe thrownBy(parseExpr("9223372036854775809"))
  }

  "negative Long.MinValue parses correctly" in {
    parseExpr("-9223372036854775808") shouldBe UnaryExprAST("-", LongExprAST(Long.MinValue))
  }
}
