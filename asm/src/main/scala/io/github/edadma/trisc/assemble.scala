package io.github.edadma.trisc

import pprint.pprintln

import scala.annotation.tailrec
import scala.collection.{mutable, immutable}
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Positional

def assemble(src: String, stacked: Boolean = true, orgs: Map[String, Long] = Map(), addresses: Int = 2, relocatable: Boolean = false): TOF =
  class Pass1(val name: String):
    var org: Long = 0
    var size: Long = 0
    val symbols: ArrayBuffer[String] = new ArrayBuffer
    var last: Option[String] = None

    override def toString: String =
      s"org: ${org.toHexString}; size: ${size.toHexString}; symbols: [${symbols mkString ", "}]"

  trait Symbol:
    val name: String

  case class EquateSymbol(name: String, value: ExprAST) extends Symbol
  case class LabelSymbol(name: String, var value: Long, sym: Positional, var referenced: Boolean = false) extends Symbol
  case class ExternSymbol(name: String) extends Symbol

  val lines = AssemblyParser.parseAssembly(src)
  val symbols = new mutable.LinkedHashMap[String, Symbol]
  val segments = new mutable.LinkedHashMap[String, Pass1]
  var segment = Pass1("_default_")
  val builder = TOF.builder
  val globals = new mutable.LinkedHashMap[String, GlobalLineAST]
  val declaredExterns = new mutable.LinkedHashSet[String]
  val referencedExterns = new mutable.LinkedHashSet[String]
  var entryPoint: Option[String] = None

  def addSymbol(sym: Positional, name: String): Unit =
    if symbols contains name then problem(sym, s"duplicate symbol: '$name'")
    symbols(name) = LabelSymbol(name, segment.size, sym)
    segment.symbols += name

  def locals(expr: ExprAST): Unit =
    expr match
      case l @ LocalExprAST(local, _) =>
        l.ref = s"${segment.last getOrElse problem(expr, "no preceding label")}.$local"
      case BinaryExprAST(l, _, r) =>
        locals(l)
        locals(r)
      case _ =>

  def fold(e: ExprAST, absolute: Boolean = false, immediate: Boolean = false, references: Boolean = true): ExprAST =
    e match
      case lit: (LongExprAST | DoubleExprAST) => lit
      case reg: RegisterExprAST               => reg
      case StringExprAST(s) if immediate =>
        if s.isEmpty || s.length > 1 then problem(e, "expected a single character")

        val v = LongExprAST(s.codePointAt(0))

        v.setPos(e.pos)
        v
      case s: StringExprAST => s
      case ReferenceExprAST(ref) =>
        if !references then problem(e, s"references not allowed here")

        symbols.get(ref) match
          case Some(_: ExternSymbol) =>
            referencedExterns += ref
            e // return unresolved — caller handles relocation
          case None =>
            if relocatable then e // return unresolved — caller handles relocation
            else problem(e, s"unrecognized symbol '$ref'")
          case Some(l @ LabelSymbol(_, value, _, _)) =>
            l.referenced = true
            if relocatable && absolute then e // defer absolute references to linker
            else LongExprAST(if absolute then value else value - (builder.length + 2 + builder.org))
          case Some(EquateSymbol(_, value)) => fold(value, absolute, immediate)
          case Some(s) => problem(e, s"unexpected symbol type for '$ref': $s")
      case LocalExprAST(_, ref) =>
        symbols.get(ref) match
          case None => problem(e, s"unrecognized symbol '$ref'")
          case Some(l @ LabelSymbol(_, value, _, _)) =>
            l.referenced = true
            LongExprAST(if absolute then value else value - (builder.length + 2 + builder.org))
          case Some(s) => problem(e, s"unexpected symbol type for '$ref': $s")
      case UnaryExprAST("-", expr) =>
        fold(expr, absolute, immediate) match
          case LongExprAST(n)   => LongExprAST(-n)
          case DoubleExprAST(n) => DoubleExprAST(-n)
          case e                => e

  segments("_default_") = segment

  // Pass 1: size calculation and symbol registration
  lines foreach {
    case SegmentLineAST(name) =>
      segments get name match
        case None =>
          segment = Pass1(name)
          segments(name) = segment
        case Some(s) => segment = s
    case label @ LabelLineAST(name) =>
      addSymbol(label, name)
      segment.last = Some(name)
    case local @ LocalLineAST(name) =>
      addSymbol(local, s"${segment.last getOrElse problem(local, "no preceding label")}.$name")
    case equate @ EquateLineAST(name, expr) =>
      if symbols contains name then problem(equate, s"duplicate definition of '$name'")
      symbols(name) = EquateSymbol(name, expr)
    case e @ EntryLineAST(name) =>
      if entryPoint.isDefined then problem(e, "duplicate entry directive")
      entryPoint = Some(name)
    case ext @ ExternLineAST(name) =>
      if symbols contains name then problem(ext, s"duplicate symbol: '$name'")
      symbols(name) = ExternSymbol(name)
      declaredExterns += name
    case g @ GlobalLineAST(name, typ, size, typeInfo) =>
      globals(name) = g
    case CommentLineAST(_) => // pass 1: skip comments
    case AlignLineAST(alignment) =>
      val pad = ((alignment - (segment.size % alignment)) % alignment).toInt
      segment.size += pad
    case DataLineAST(width, Nil) => segment.size += (if width == 0 then 8 else width)
    case DataLineAST(width, data) =>
      val startingSize = segment.size

      for d <- data do
        locals(d)
        segment.size +=
          (d match
            case StringExprAST(s) => (s.getBytes(scala.io.Codec.UTF8.charSet).length + 1) & 0xfffffffe
            case _                => if width == 0 then 8 else width
          )

      if (segment.size - startingSize) % 2 == 1 then segment.size += 1
    case ReserveLineAST(width, n) =>
      val startingSize = segment.size

      fold(n, absolute = true) match
        case LongExprAST(count) if 0 < count && count <= 10 * 1024 * 1024 =>
          segment.size += count.toInt * (if width == 0 then 8 else width)
        case _ => problem(n, s"must be a positive integer up to 10 meg")

      if (segment.size - startingSize) % 2 == 1 then segment.size += 1
    case InstructionLineAST(mnemonic, operands) =>
      segment.size += (
        mnemonic match
          case "movi"                                          => addresses * 2
          case "bne" | "bge" | "bgeu" | "ble" | "bleu"        => 4 // inverted branch + bra
          case _                                               => 2
      )
      operands foreach locals
  }

  // Validate globals reference existing labels
  for (name, g) <- globals do
    symbols.get(name) match
      case Some(_: LabelSymbol) => // ok
      case _                    => problem(g, s"global '$name' does not refer to a defined label")

  // Set entry point on builder
  for name <- entryPoint do
    builder.setEntry(name)

  def relocate(seg: Pass1, org: Long): Unit =
    seg.symbols foreach (n => symbols(n).asInstanceOf[LabelSymbol].value += org)
    seg.org = org

  var base = 0L

  for (n, s) <- segments do
    orgs get n match
      case None =>
        if stacked then
          relocate(s, base)
          base += s.size
      case Some(o) =>
        relocate(s, o)
        if stacked then base = o + s.size

  def addInstruction(pieces: (Int, Int)*): Unit =
    var inst = 0
    var shift = 16

    for (w, v) <- pieces do
      val mask = (1 << w) - 1

      shift -= w
      inst |= (v & mask) << shift

    builder += (inst >> 8).toByte
    builder += inst.toByte

  def emitMoviReloc(reg: Int, symbolName: String): Unit =
    val offset = builder.length
    val relocType = addresses match
      case 2 => RelocType.MOVI2
      case 3 => RelocType.MOVI3
      case 4 => RelocType.MOVI4
      case _ => sys.error(s"unsupported address size $addresses for relocation")
    builder.addExtern(symbolName)
    builder.addReloc(relocType, offset, symbolName)
    // emit zero placeholders
    for _ <- 0 until addresses do
      addInstruction(3 -> 7, 3 -> reg, 2 -> 0, 8 -> 0)

  def emitAbs32Reloc(symbolName: String): Unit =
    val offset = builder.length
    builder.addExtern(symbolName)
    builder.addReloc(RelocType.ABS32, offset, symbolName)
    // emit zero placeholder
    builder += 0.toByte
    builder += 0.toByte
    builder += 0.toByte
    builder += 0.toByte

  def emitAbs64Reloc(symbolName: String): Unit =
    val offset = builder.length
    builder.addExtern(symbolName)
    builder.addReloc(RelocType.ABS64, offset, symbolName)
    // emit zero placeholder
    for _ <- 0 until 8 do builder += 0.toByte

  builder.segment("_default_", segments("_default_").org)

  // Emit symbols for a segment: globals, relocatable auto-exports, and entry point
  val emittedSymbols = new mutable.LinkedHashSet[String]

  def emitSymbolsForSegment(segName: String): Unit =
    val seg = segments(segName)
    val org = seg.org

    if globals.nonEmpty then
      for (gname, g) <- globals if seg.symbols.contains(gname) do
        symbols.get(gname) match
          case Some(LabelSymbol(_, value, _, _)) =>
            builder.addSymbol(gname, value - org, g.typ, g.size, g.typeInfo)
            emittedSymbols += gname
          case _ =>
    else if relocatable then
      for symName <- seg.symbols do
        symbols.get(symName) match
          case Some(LabelSymbol(n, value, _, _)) if !n.contains('.') =>
            builder.addSymbol(n, value - org, SymbolType.Func)
            emittedSymbols += n
          case _ =>
    // Always emit entry point symbol if it belongs to this segment
    for ep <- entryPoint if !emittedSymbols.contains(ep) && seg.symbols.contains(ep) do
      symbols.get(ep) match
        case Some(LabelSymbol(_, value, _, _)) =>
          builder.addSymbol(ep, value - org, SymbolType.Func)
          emittedSymbols += ep
        case _ =>

  emitSymbolsForSegment("_default_")

  // Pass 2: code generation
  lines foreach {
    case SegmentLineAST(name) =>
      builder.segment(name, segments(name).org)
      emitSymbolsForSegment(name)
    case LabelLineAST(_)         =>
    case LocalLineAST(_)         =>
    case EquateLineAST(_, _)     =>
    case EntryLineAST(_)             =>
    case ExternLineAST(_)            =>
    case GlobalLineAST(_, _, _, _)  =>
    case CommentLineAST(text)       => builder.addComment(text)
    case AlignLineAST(alignment) =>
      val pad = ((alignment - (builder.length % alignment)) % alignment).toInt
      for _ <- 0 until pad do builder += 0.toByte
    case DataLineAST(width, Nil) => builder ++= (if width == 0 then Seq.fill(8)(0) else Seq.fill(width)(0))
    case DataLineAST(width, data) =>
      val startingLength = builder.length

      for d <- data do
        fold(d, absolute = true) match
          case ReferenceExprAST(ref) if (relocatable || declaredExterns.contains(ref)) && width == 4 =>
            emitAbs32Reloc(ref)
          case ReferenceExprAST(ref) if (relocatable || declaredExterns.contains(ref)) && width == 8 =>
            emitAbs64Reloc(ref)
          case StringExprAST(s) =>
            val bytes = s.getBytes(scala.io.Codec.UTF8.charSet)

            builder ++= immutable.ArraySeq.unsafeWrapArray(bytes)

            if bytes.length % 2 == 1 then builder += 0
          case value =>
            width match
              case 1 =>
                value match
                  case _: DoubleExprAST                => problem(d, "expected an int value, found float")
                  case LongExprAST(v) if v.isValidByte => builder += v.toByte
                  case _                               => problem(d, "expected a byte value, out of range")
              case 2 =>
                value match
                  case _: DoubleExprAST => problem(d, "expected an int value, found float")
                  case LongExprAST(v) if v.isValidShort =>
                    builder += (v >> 8).toByte
                    builder += v.toByte
                  case _ => problem(d, "expected a short value, out of range")
              case 4 =>
                value match
                  case _: DoubleExprAST => problem(d, "expected an int value, found float")
                  case ReferenceExprAST(ref) if relocatable =>
                    builder.addReloc(RelocType.ABS32, builder.length, ref)
                    builder.addExtern(ref)
                    for _ <- 0 until 4 do builder += 0.toByte
                  case LongExprAST(v) if v.isValidInt =>
                    builder += (v >> 24).toByte
                    builder += (v >> 16).toByte
                    builder += (v >> 8).toByte
                    builder += v.toByte
                  case _ => problem(d, "expected a short value, out of range")
              case 8 =>
                value match
                  case _: DoubleExprAST => problem(d, "expected an int value, found float")
                  case ReferenceExprAST(ref) if relocatable =>
                    builder.addReloc(RelocType.ABS64, builder.length, ref)
                    builder.addExtern(ref)
                    for _ <- 0 until 8 do builder += 0.toByte
                  case LongExprAST(v) =>
                    builder += (v >> 56).toByte
                    builder += (v >> 48).toByte
                    builder += (v >> 40).toByte
                    builder += (v >> 32).toByte
                    builder += (v >> 24).toByte
                    builder += (v >> 16).toByte
                    builder += (v >> 8).toByte
                    builder += v.toByte
              case 0 =>
                value match
                  case ReferenceExprAST(ref) if relocatable =>
                    builder.addReloc(RelocType.ABS64, builder.length, ref)
                    builder.addExtern(ref)
                    for _ <- 0 until 8 do builder += 0.toByte
                  case _ =>
                    val v =
                      value match
                        case DoubleExprAST(d) => java.lang.Double.doubleToLongBits(d)
                        case LongExprAST(l)   => l

                    builder += (v >> 56).toByte
                    builder += (v >> 48).toByte
                    builder += (v >> 40).toByte
                    builder += (v >> 32).toByte
                    builder += (v >> 24).toByte
                    builder += (v >> 16).toByte
                    builder += (v >> 8).toByte
                    builder += v.toByte

      if (builder.length - startingLength) % 2 == 1 then builder += 0
    case ReserveLineAST(width, n) =>
      fold(n, absolute = true) match
        case LongExprAST(count) if 0 < count && count <= 10 * 1024 * 1024 =>
          val size = count * (if width == 0 then 8 else width)
          val align = size % 2

          builder.addRes((size + align).toInt)
        case _ => problem(n, s"must be a positive integer up to 10 meg")
    case InstructionLineAST(mnemonic @ ("auipc"), Seq(o1, o2)) =>
      val reg =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val imm =
        fold(o2, absolute = true, immediate = true) match
          case _: DoubleExprAST                        => problem(o2, "immediate must be integral")
          case LongExprAST(n) if -128 <= n && n <= 255 => n.toInt
          case _: LongExprAST                          => problem(o2, "immediate must be a byte value")

      addInstruction(3 -> 7, 3 -> reg, 2 -> 1, 8 -> imm)
    case InstructionLineAST(mnemonic @ ("ldi" | "sli" | "sti"), Seq(o1, o2)) =>
      val opcode =
        mnemonic match
          case "ldi" => 0
          case "sli" => 2
          case "sti" => 3
      val reg =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val imm =
        fold(o2, absolute = true, immediate = true) match
          case _: DoubleExprAST                        => problem(o2, "immediate must be integral")
          case LongExprAST(n) if -128 <= n && n <= 255 => n.toInt
          case _: LongExprAST                          => problem(o2, "immediate must be a byte value")

      addInstruction(3 -> 7, 3 -> reg, 2 -> opcode, 8 -> imm)
    case InstructionLineAST(mnemonic @ ("trap"), Seq(o1)) =>
      val imm =
        fold(o1, immediate = true) match
          case _: DoubleExprAST                   => problem(o1, "immediate must be integral")
          case LongExprAST(n) if 0 <= n && n <= 7 => n.toInt
          case _: LongExprAST                     => problem(o1, "immediate must be between 0 and 7")

      addInstruction(3 -> 7, 3 -> 0, 3 -> 0, 4 -> 3, 3 -> imm)
    case InstructionLineAST(mnemonic @ ("addi"), Seq(o1, o2, o3)) =>
      val opcode =
        mnemonic match
          case "addi" => 5
      val reg1 =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val reg2 =
        fold(o2) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o2, "expected register as second operand")
      val imm =
        fold(o3, immediate = true) match
          case _: DoubleExprAST                      => problem(o3, "immediate must be integral")
          case LongExprAST(n) if -64 <= n && n <= 63 => n.toInt
          case _: LongExprAST                        => problem(o3, "immediate must be a signed 7-bit value")

      addInstruction(3 -> opcode, 3 -> reg1, 3 -> reg2, 7 -> imm)
    case InstructionLineAST(mnemonic @ ("beq" | "blu" | "bls"), Seq(o1, o2, o3)) =>
      val opcode =
        mnemonic match
          case "beq" => 2
          case "blu" => 3
          case "bls" => 4
      val reg1 =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val reg2 =
        fold(o2) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o2, "expected register as second operand")
      val imm =
        fold(o3, immediate = true) match
          case _: DoubleExprAST                                      => problem(o3, "immediate must be integral")
          case LongExprAST(n) if -128 <= n && n <= 126 && n % 2 == 0 => n.toInt
          case _: LongExprAST => problem(o3, "immediate must be an even signed 8-bit value")

      addInstruction(3 -> opcode, 3 -> reg1, 3 -> reg2, 7 -> imm / 2)
    case InstructionLineAST(
          mnemonic @ ("ldb" | "stb" | "lds" | "sts" | "ldw" | "stw" | "ldd" | "std" | "add" | "sub" | "mul" | "div" |
          "rem" | "and" | "or" | "xor" | "asr" | "lsr" | "lsl" | "slt" | "sltu" | "adc" | "sbc" | "mulu" | "divu" |
          "remu" | "fslt" | "fadd" | "fsub" | "fmul" | "fdiv" | "fseq"),
          Seq(o1, o2, o3),
        ) =>
      val (prefix, opcode) =
        mnemonic match
          case "ldb"  => (0, 0)
          case "stb"  => (0, 1)
          case "lds"  => (0, 2)
          case "sts"  => (0, 3)
          case "ldw"  => (0, 4)
          case "stw"  => (0, 5)
          case "ldd"  => (0, 6)
          case "std"  => (0, 7)
          case "add"  => (0, 8)
          case "sub"  => (0, 9)
          case "mul"  => (0, 10)
          case "div"  => (0, 11)
          case "rem"  => (0, 12)
          case "and"  => (0, 13)
          case "or"   => (0, 14)
          case "xor"  => (0, 15)
          case "asr"  => (1, 0)
          case "lsr"  => (1, 1)
          case "lsl"  => (1, 2)
          case "slt"  => (1, 3)
          case "sltu" => (1, 4)
          case "adc"  => (1, 5)
          case "sbc"  => (1, 6)
          case "mulu" => (1, 7)
          case "divu" => (1, 8)
          case "remu" => (1, 9)
          case "fslt" => (1, 10)
          case "fadd" => (1, 11)
          case "fsub" => (1, 12)
          case "fmul" => (1, 13)
          case "fdiv" => (1, 14)
          case "fseq" => (1, 15)
      val reg1 =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val reg2 =
        fold(o2) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o2, "expected register as second operand")
      val reg3 =
        fold(o3) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as third operand")

      addInstruction(3 -> prefix, 3 -> reg1, 3 -> reg2, 3 -> reg3, 4 -> opcode)
    case InstructionLineAST(mnemonic @ ("jalr"), Seq(o1, o2)) =>
      val opcode =
        mnemonic match
          case "jalr" => 0
      val reg1 =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val reg2 =
        fold(o2) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o2, "expected register as second operand")

      addInstruction(3 -> 6, 3 -> reg1, 3 -> reg2, 2 -> 0, 5 -> opcode)
    case InstructionLineAST(mnemonic @ ("ld" | "st"), Seq(o1, o2, o3)) =>
      val opcode =
        mnemonic match
          case "ld" => 2
          case "st" => 3
      val reg1 =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val reg2 =
        fold(o2) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o2, "expected register as second operand")
      val imm =
        fold(o3, absolute = true, immediate = true) match
          case _: DoubleExprAST                                  => problem(o3, "immediate must be integral")
          case LongExprAST(n) if 0 <= n && n <= 62 && n % 2 == 0 => n.toInt
          case _: LongExprAST => problem(o3, "immediate must be an even non-negative value between 0 and 62")

      addInstruction(3 -> 6, 3 -> reg1, 3 -> reg2, 2 -> opcode, 5 -> imm / 2)
    case InstructionLineAST(mnemonic @ ("rte" | "fence" | "wfi" | "trapv" | "cli" | "sti" | "swsp"), Nil) =>
      val opcode =
        mnemonic match
          case "rte"   => 10
          case "fence" => 11
          case "wfi"   => 12
          case "trapv" => 15
          case "cli"   => 18
          case "sti"   => 19
          case "swsp"  => 20

      addInstruction(3 -> 7, 3 -> 0, 3 -> 0, 7 -> opcode)
    case InstructionLineAST(mnemonic @ ("spsr" | "gpsr" | "gusp" | "susp" | "tsr"), Seq(o)) =>
      val opcode =
        mnemonic match
          case "spsr" => 8
          case "gusp" => 13
          case "susp" => 14
          case "gpsr" => 9
          case "tsr"  => 21
      val reg =
        fold(o) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o, "expected register as first operand")

      addInstruction(3 -> 7, 3 -> 0, 3 -> reg, 7 -> opcode)
    case InstructionLineAST(mnemonic @ ("zeb" | "zes" | "zew" | "seb" | "ses" | "sew" | "neg" | "not" | "cvt" | "fneg" | "finv" | "fint" | "fsqrt" | "fabs" | "ll" | "sc" | "clz" | "ctz" | "chk" | "btst" | "bset" | "bclr" | "rol" | "ror" | "cnt" | "rev" | "sext" | "mov" | "min" | "max" | "exg"), Seq(o1, o2)) =>
      val opcode =
        mnemonic match
          case "zeb"   => 1
          case "zes"   => 2
          case "zew"   => 3
          case "seb"   => 4
          case "ses"   => 5
          case "sew"   => 6
          case "neg"   => 7
          case "not"   => 8
          case "cvt"   => 9
          case "fneg"  => 10
          case "finv"  => 11
          case "fint"  => 12
          case "fsqrt" => 13
          case "fabs"  => 14
          case "ll"    => 15
          case "sc"    => 16
          case "clz"   => 17
          case "ctz"   => 18
          case "chk"   => 19
          case "btst"  => 20
          case "bset"  => 21
          case "bclr"  => 22
          case "rol"   => 23
          case "ror"   => 24
          case "cnt"   => 25
          case "rev"   => 26
          case "sext"  => 27
          case "mov"   => 28
          case "min"   => 29
          case "max"   => 30
          case "exg"   => 31
      val reg1 =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val reg2 =
        fold(o2) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o2, "expected register as second operand")

      addInstruction(3 -> 6, 3 -> reg1, 3 -> reg2, 2 -> 0, 5 -> opcode)
    case InstructionLineAST(mnemonic @ ("fpow"), Seq(o1, o2)) =>
      val opcode =
        mnemonic match
          case "fpow" => 0
      val reg1 =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val reg2 =
        fold(o2) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o2, "expected register as second operand")

      addInstruction(3 -> 6, 3 -> reg1, 3 -> reg2, 2 -> 1, 5 -> opcode)
    case InstructionLineAST(mnemonic @ ("pshr" | "popr"), Seq(o)) =>
      val opcode = mnemonic match
        case "pshr" => 16
        case "popr" => 17
      val reg =
        fold(o) match
          case RegisterExprAST(reg) if reg >= 1 && reg <= 7 => reg
          case RegisterExprAST(_) => problem(o, "register must be r1-r7")
          case _ => problem(o, "expected register as operand")

      addInstruction(3 -> 7, 3 -> 0, 3 -> reg, 7 -> opcode)
    case InstructionLineAST(mnemonic @ ("pshb" | "popb" | "pshs" | "pops" | "pshw" | "popw" | "pshd" | "popd"), Seq(o)) =>
      val opcode = mnemonic match
        case "pshb" => 0
        case "popb" => 1
        case "pshs" => 2
        case "pops" => 3
        case "pshw" => 4
        case "popw" => 5
        case "pshd" => 6
        case "popd" => 7
      val reg =
        fold(o) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o, "expected register as operand")

      addInstruction(3 -> 7, 3 -> 0, 3 -> reg, 7 -> opcode)
    case InstructionLineAST("halt", Nil) => addInstruction(3 -> 6, 3 -> 0, 3 -> 0, 2 -> 0, 5 -> 0) // jalr 0,0
    case InstructionLineAST("bra", Seq(o)) =>
      val imm =
        fold(o, immediate = true) match
          case _: DoubleExprAST                                      => problem(o, "immediate must be integral")
          case LongExprAST(n) if -128 <= n && n <= 126 && n % 2 == 0 => n.toInt
          case _: LongExprAST => problem(o, "immediate must be an even signed 8-bit value")

      addInstruction(3 -> 2, 3 -> 0, 3 -> 0, 7 -> imm / 2) // beq r0, r0, imm
    case InstructionLineAST("movi", Seq(o1, o2)) =>
      val reg =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      fold(o2, absolute = true, immediate = true) match
        case ReferenceExprAST(ref) if relocatable || declaredExterns.contains(ref) =>
          emitMoviReloc(reg, ref)
        case result =>
          val imm = result match
            case _: DoubleExprAST                            => problem(o2, "immediate must be integral")
            case LongExprAST(n) if 0 <= n && n <= 0x7fffffff => n.toInt
            case _: LongExprAST                              => problem(o2, "immediate out of range")

          addresses match
            case 1 => addInstruction(3 -> 7, 3 -> reg, 2 -> 0, 8 -> (imm & 0xff))
            case 2 =>
              addInstruction(3 -> 7, 3 -> reg, 2 -> 0, 8 -> ((imm >> 8) & 0xff))
              addInstruction(3 -> 7, 3 -> reg, 2 -> 2, 8 -> (imm & 0xff))
            case 3 =>
              addInstruction(3 -> 7, 3 -> reg, 2 -> 0, 8 -> ((imm >> 16) & 0xff))
              addInstruction(3 -> 7, 3 -> reg, 2 -> 2, 8 -> ((imm >> 8) & 0xff))
              addInstruction(3 -> 7, 3 -> reg, 2 -> 2, 8 -> (imm & 0xff))
            case 4 =>
              addInstruction(3 -> 7, 3 -> reg, 2 -> 0, 8 -> ((imm >> 24) & 0xff))
              addInstruction(3 -> 7, 3 -> reg, 2 -> 2, 8 -> ((imm >> 16) & 0xff))
              addInstruction(3 -> 7, 3 -> reg, 2 -> 2, 8 -> ((imm >> 8) & 0xff))
              addInstruction(3 -> 7, 3 -> reg, 2 -> 2, 8 -> (imm & 0xff))
    case InstructionLineAST("nop", Nil) => addInstruction(3 -> 5, 3 -> 0, 3 -> 0, 7 -> 0) // addi r0, r0, 0
    case InstructionLineAST("ret", Nil) =>
      addInstruction(3 -> 6, 3 -> 0, 3 -> 7, 2 -> 0, 5 -> 0) // jalr r0, r7
    case InstructionLineAST(mnemonic @ ("bgt" | "bgu"), Seq(o1, o2, o3)) =>
      val baseOpcode = mnemonic match
        case "bgt" => 4
        case "bgu" => 3
      val reg1 =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val reg2 =
        fold(o2) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o2, "expected register as second operand")
      val imm =
        fold(o3, immediate = true) match
          case _: DoubleExprAST                                      => problem(o3, "immediate must be integral")
          case LongExprAST(n) if -128 <= n && n <= 126 && n % 2 == 0 => n.toInt
          case _: LongExprAST => problem(o3, "immediate must be an even signed 8-bit value")

      addInstruction(3 -> baseOpcode, 3 -> reg2, 3 -> reg1, 7 -> imm / 2)
    case InstructionLineAST(mnemonic @ ("bne" | "bge" | "bgeu" | "ble" | "bleu"), Seq(o1, o2, o3)) =>
      val (baseOpcode, swap) = mnemonic match
        case "bne"  => (2, false)
        case "bge"  => (4, false)
        case "bgeu" => (3, false)
        case "ble"  => (4, true)
        case "bleu" => (3, true)
      val reg1 =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val reg2 =
        fold(o2) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o2, "expected register as second operand")
      val imm =
        fold(o3, immediate = true) match
          case _: DoubleExprAST                                      => problem(o3, "immediate must be integral")
          case LongExprAST(n) if -128 <= n && n <= 126 && n % 2 == 0 => (n - 2).toInt
          case _: LongExprAST => problem(o3, "immediate must be an even signed 8-bit value")

      val (r1, r2) = if swap then (reg2, reg1) else (reg1, reg2)
      addInstruction(3 -> baseOpcode, 3 -> r1, 3 -> r2, 7 -> 1)
      addInstruction(3 -> 2, 3 -> 0, 3 -> 0, 7 -> imm / 2)
  }

  // Warnings and validation
  if !relocatable then
    symbols.values foreach {
      case LabelSymbol(name, _, sym, false) => warning(sym, s"Warning: label '$name' never referenced")
      case _                                =>
    }

  for name <- declaredExterns do
    if !referencedExterns.contains(name) then
      println(s"Warning: extern '$name' declared but never referenced")

  builder.tof
