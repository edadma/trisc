package io.github.edadma.trisc

import scala.collection.{mutable, immutable}
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Positional

def svmAssemble(src: String, stacked: Boolean = true, orgs: Map[String, Long] = Map(), relocatable: Boolean = false): TOF =
  class Pass1(val name: String):
    var org: Long = 0
    var size: Long = 0
    val symbols: ArrayBuffer[String] = new ArrayBuffer
    var last: Option[String] = None

  trait Symbol:
    val name: String

  case class EquateSymbol(name: String, value: ExprAST) extends Symbol
  case class LabelSymbol(name: String, var value: Long, sym: Positional, var referenced: Boolean = false) extends Symbol
  case class ExternSymbol(name: String) extends Symbol

  val lines = SVMAssemblerParser.parseAssembly(src)
  val symbols = new mutable.LinkedHashMap[String, Symbol]
  val segments = new mutable.LinkedHashMap[String, Pass1]
  var segment = Pass1("_default_")
  val builder = TOF.builder
  val globals = new mutable.LinkedHashMap[String, GlobalLineAST]
  val declaredExterns = new mutable.LinkedHashSet[String]
  val referencedExterns = new mutable.LinkedHashSet[String]
  var entryPoint: Option[String] = None
  val symbolSegment = new mutable.LinkedHashMap[String, String]

  segments("_default_") = segment

  def problem(pos: Positional, msg: String): Nothing = sys.error(s"$msg at ${pos.pos}")
  def warning(pos: Positional, msg: String): Unit = println(s"$msg at ${pos.pos}")

  def addSymbol(sym: Positional, name: String): Unit =
    if symbols contains name then problem(sym, s"duplicate symbol: '$name'")
    symbols(name) = LabelSymbol(name, segment.size, sym)
    segment.symbols += name
    symbolSegment(name) = segment.name

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
            e
          case None =>
            if relocatable then e
            else problem(e, s"unrecognized symbol '$ref'")
          case Some(l @ LabelSymbol(_, value, _, _)) =>
            l.referenced = true
            if relocatable && absolute then e
            else LongExprAST(if absolute then value else value - builder.length)
          case Some(EquateSymbol(_, value)) => fold(value, absolute, immediate)
          case Some(s) => problem(e, s"unexpected symbol type for '$ref': $s")
      case LocalExprAST(_, ref) =>
        symbols.get(ref) match
          case None => problem(e, s"unrecognized symbol '$ref'")
          case Some(l @ LabelSymbol(_, value, _, _)) =>
            l.referenced = true
            LongExprAST(if absolute then value else value - builder.length)
          case Some(s) => problem(e, s"unexpected symbol type for '$ref': $s")
      case UnaryExprAST("-", expr) =>
        fold(expr, absolute, immediate) match
          case LongExprAST(n)   => LongExprAST(-n)
          case DoubleExprAST(n) => DoubleExprAST(-n)
          case e                => e
      case _ => e

  // --- Instruction size calculation ---
  // SVM instructions: opcode (1 byte) + optional operand
  def instrSize(mnemonic: String, operands: Seq[ExprAST]): Int = mnemonic match
    // Zero-operand (1 byte)
    case "nop" | "drop" | "dup" | "swap" | "over" | "rot" | "nrot" | "nip" | "tuck" |
         "drop2" | "dup2" | "swap2" | "over2" | "depth" |
         "push_0" | "push_1" | "push_2" | "push_m1" |
         "add" | "sub" | "mul" | "div" | "mod" | "divmod" | "divu" | "modu" |
         "neg" | "abs" | "inc" | "dec" |
         "and" | "or" | "xor" | "not" | "shl" | "shr" | "sar" |
         "clz" | "ctz" | "popcnt" | "rotl" | "rotr" | "bswap" |
         "eq" | "neq" | "lt" | "gt" | "le" | "ge" |
         "ltu" | "gtu" | "leu" | "geu" |
         "eqz" | "nez" | "ltz" | "gtz" | "lez" | "gez" |
         "load8" | "load8s" | "load16" | "load16s" | "load32" | "load32s" | "load64" |
         "store8" | "store16" | "store32" | "store64" |
         "ret" | "callr" | "tailr" | "halt" |
         "r_push" | "r_pop" | "r_peek" |
         "fadd" | "fsub" | "fmul" | "fdiv" | "fmod" | "fneg" | "fabs" | "fsqrt" |
         "ffloor" | "fceil" | "fround" | "ftrunc" | "fmin" | "fmax" |
         "feq" | "fneq" | "flt" | "fgt" | "fle" | "fge" |
         "f2i" | "i2f" | "f2u" | "u2f" |
         "push_f0" | "push_f1" | "f64tof32" |
         "dup_add" | "dup_mul" | "over_add" | "over_sub" |
         "dup_load64" |
         "breakpoint" => 1
    // opcode + u8 (2 bytes)
    case "push_i8" | "push_u8" | "frame" | "local_get" | "local_set" | "local_tee" |
         "trap" |
         "add_imm8" | "sub_imm8" | "mul_imm8" |
         "local_get_add" | "local_get_sub" | "local_get_eqz" |
         "push_i8_add" | "push_i8_load64" => 2
    // opcode + i16 (3 bytes)
    case "push_i16" | "jump" | "jumpz" | "jumpnz" |
         "eqz_jumpz" | "eqz_jumpnz" | "inc_jumpnz" | "dec_jumpnz" |
         "drop_jump" => 3
    // opcode + u8 + i16 (4 bytes)
    case "local_get_jumpz" | "local_get_jumpnz" => 4
    // opcode + i32 (5 bytes)
    case "push_i32" | "call" | "tail" | "jump_wide" => 5
    // opcode + i64 (9 bytes)
    case "push_i64" | "call_abs" => 9
    case _ => sys.error(s"unknown SVM mnemonic '$mnemonic'")

  // --- Opcode table ---
  val opcodeMap: Map[String, Int] = Map(
    "nop" -> 0x00, "drop" -> 0x01, "dup" -> 0x02, "swap" -> 0x03,
    "over" -> 0x04, "rot" -> 0x05, "nrot" -> 0x06, "nip" -> 0x07,
    "tuck" -> 0x08, "drop2" -> 0x09, "dup2" -> 0x0A, "swap2" -> 0x0B,
    "over2" -> 0x0C, "depth" -> 0x0D,
    "push_0" -> 0x10, "push_1" -> 0x11, "push_2" -> 0x12, "push_m1" -> 0x13,
    "push_i8" -> 0x14, "push_u8" -> 0x15, "push_i16" -> 0x16,
    "push_i32" -> 0x17, "push_i64" -> 0x18,
    "add" -> 0x20, "sub" -> 0x21, "mul" -> 0x22, "div" -> 0x23,
    "mod" -> 0x24, "divmod" -> 0x25, "divu" -> 0x26, "modu" -> 0x27,
    "neg" -> 0x28, "abs" -> 0x29, "inc" -> 0x2A, "dec" -> 0x2B,
    "and" -> 0x30, "or" -> 0x31, "xor" -> 0x32, "not" -> 0x33,
    "shl" -> 0x34, "shr" -> 0x35, "sar" -> 0x36, "clz" -> 0x37,
    "ctz" -> 0x38, "popcnt" -> 0x39, "rotl" -> 0x3A, "rotr" -> 0x3B,
    "bswap" -> 0x3C,
    "eq" -> 0x40, "neq" -> 0x41, "lt" -> 0x42, "gt" -> 0x43,
    "le" -> 0x44, "ge" -> 0x45, "ltu" -> 0x46, "gtu" -> 0x47,
    "leu" -> 0x48, "geu" -> 0x49, "eqz" -> 0x4A, "nez" -> 0x4B,
    "ltz" -> 0x4C, "gtz" -> 0x4D, "lez" -> 0x4E, "gez" -> 0x4F,
    "load8" -> 0x50, "load8s" -> 0x51, "load16" -> 0x52, "load16s" -> 0x53,
    "load32" -> 0x54, "load32s" -> 0x55, "load64" -> 0x56,
    "store8" -> 0x58, "store16" -> 0x59, "store32" -> 0x5A, "store64" -> 0x5B,
    "jump" -> 0x60, "jumpz" -> 0x61, "jumpnz" -> 0x62, "call" -> 0x63,
    "ret" -> 0x64, "tail" -> 0x65, "callr" -> 0x66, "tailr" -> 0x67,
    "jump_wide" -> 0x68, "call_abs" -> 0x69, "trap" -> 0x6A, "halt" -> 0x6B,
    "frame" -> 0x70, "local_get" -> 0x71, "local_set" -> 0x72, "local_tee" -> 0x73,
    "r_push" -> 0x80, "r_pop" -> 0x81, "r_peek" -> 0x82,
    "fadd" -> 0x90, "fsub" -> 0x91, "fmul" -> 0x92, "fdiv" -> 0x93,
    "fmod" -> 0x94, "fneg" -> 0x95, "fabs" -> 0x96, "fsqrt" -> 0x97,
    "ffloor" -> 0x98, "fceil" -> 0x99, "fround" -> 0x9A, "ftrunc" -> 0x9B,
    "fmin" -> 0x9C, "fmax" -> 0x9D,
    "feq" -> 0x9E, "fneq" -> 0x9F, "flt" -> 0xA0, "fgt" -> 0xA1,
    "fle" -> 0xA2, "fge" -> 0xA3,
    "f2i" -> 0xA4, "i2f" -> 0xA5, "f2u" -> 0xA6, "u2f" -> 0xA7,
    "push_f0" -> 0xA8, "push_f1" -> 0xA9, "f64tof32" -> 0xAA,
    "dup_add" -> 0xB0, "dup_mul" -> 0xB1, "over_add" -> 0xB2, "over_sub" -> 0xB3,
    "add_imm8" -> 0xB4, "sub_imm8" -> 0xB5, "mul_imm8" -> 0xB6,
    "eqz_jumpz" -> 0xB7, "eqz_jumpnz" -> 0xB8, "inc_jumpnz" -> 0xB9, "dec_jumpnz" -> 0xBA,
    "local_get_add" -> 0xBB, "local_get_sub" -> 0xBC, "local_get_eqz" -> 0xBD,
    "local_get_jumpz" -> 0xBE, "local_get_jumpnz" -> 0xBF,
    "dup_load64" -> 0xC0, "drop_jump" -> 0xC1,
    "push_i8_add" -> 0xC2, "push_i8_load64" -> 0xC3,
    "breakpoint" -> 0xFF,
  )

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
    case CommentLineAST(_) =>
    case AlignLineAST(alignment) =>
      val pad = ((alignment - (segment.size % alignment)) % alignment).toInt
      if pad > 0 then
        for name <- segment.symbols if symbols(name).isInstanceOf[LabelSymbol] && symbols(name).asInstanceOf[LabelSymbol].value == segment.size do
          symbols(name).asInstanceOf[LabelSymbol].value += pad
        segment.size += pad
    case DataLineAST(width, Nil) =>
      val ew = if width == 0 then 8 else width
      segment.size += ew
    case DataLineAST(width, data) =>
      val ew = if width == 0 then 8 else width
      for d <- data do
        locals(d)
        segment.size += (d match
          case StringExprAST(s) => s.getBytes(scala.io.Codec.UTF8.charSet).length + 1
          case _                => ew
        )
    case ReserveLineAST(width, n) =>
      val ew = if width == 0 then 8 else width
      fold(n, absolute = true) match
        case LongExprAST(count) if 0 < count && count <= 10 * 1024 * 1024 =>
          segment.size += count.toInt * ew
        case _ => problem(n, s"must be a positive integer up to 10 meg")
    case InstructionLineAST(mnemonic, operands) =>
      operands foreach locals
      // For relocatable mode, call/tail always expand to CALL_ABS (9 bytes) and
      // push_i32 with symbol refs expands to push_i64 (9 bytes) because fold()
      // defers all absolute references in relocatable mode.
      val sz = if relocatable then
        def hasSymbolRef: Boolean = operands.headOption match
          case Some(ReferenceExprAST(ref)) => !symbols.get(ref).exists(_.isInstanceOf[EquateSymbol])
          case Some(LocalExprAST(_, ref)) if ref != null => true
          case _ => false
        mnemonic match
          case "call" | "tail" if hasSymbolRef => 9 // CALL_ABS
          case "push_i32" if hasSymbolRef => 9 // promoted to push_i64 + reloc
          case _ => instrSize(mnemonic, operands)
      else instrSize(mnemonic, operands)
      segment.size += sz
    case IncludeLineAST(_) =>
  }

  // Validate globals reference existing labels
  for (name, g) <- globals do
    symbols.get(name) match
      case Some(_: LabelSymbol) =>
      case _                    => problem(g, s"global '$name' does not refer to a defined label")

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

  // Emit symbols for a segment
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
    for ep <- entryPoint if !emittedSymbols.contains(ep) && seg.symbols.contains(ep) do
      symbols.get(ep) match
        case Some(LabelSymbol(_, value, _, _)) =>
          builder.addSymbol(ep, value - org, SymbolType.Func)
          emittedSymbols += ep
        case _ =>

  builder.segment("_default_", segments("_default_").org, explicitOrg = orgs.contains("_default_"))
  emitSymbolsForSegment("_default_")

  // --- Emit helpers ---
  def emitByte(b: Int): Unit = builder += b.toByte

  def emitI16(v: Int): Unit =
    builder += (v >> 8).toByte
    builder += v.toByte

  def emitI32(v: Int): Unit =
    builder += (v >> 24).toByte
    builder += (v >> 16).toByte
    builder += (v >> 8).toByte
    builder += v.toByte

  def emitI64(v: Long): Unit =
    for shift <- (56 to 0 by -8) do builder += ((v >> shift) & 0xff).toByte

  def emitAbs64Reloc(symbolName: String): Unit =
    val offset = builder.length
    builder.addExtern(symbolName)
    builder.addReloc(RelocType.ABS64, offset, symbolName)
    for _ <- 0 until 8 do builder += 0.toByte

  // Resolve a branch/call operand to a relative offset from the byte AFTER the instruction.
  // Returns None if the operand is a relocatable extern (caller should emit CALL_ABS with relocation).
  def resolveBranchRel(operand: ExprAST, instrEnd: Long): Option[Long] =
    fold(operand, absolute = true) match
      case LongExprAST(target) => Some(target - instrEnd)
      case ReferenceExprAST(ref) if relocatable => None // caller handles via CALL_ABS relocation
      case _ => problem(operand, "expected label or constant for branch target")

  def emitCallAbsReloc(symbolName: String): Unit =
    emitByte(0x69) // CALL_ABS opcode
    emitAbs64Reloc(symbolName)

  // Pass 2: code generation
  lines foreach {
    case SegmentLineAST(name) =>
      builder.segment(name, segments(name).org, explicitOrg = orgs.contains(name))
      emitSymbolsForSegment(name)
    case LabelLineAST(_) | LocalLineAST(_) | EquateLineAST(_, _) | EntryLineAST(_) |
         ExternLineAST(_) | GlobalLineAST(_, _, _, _) | IncludeLineAST(_) =>
    case CommentLineAST(text) => builder.addComment(text)
    case AlignLineAST(alignment) =>
      val pad = ((alignment - (builder.length % alignment)) % alignment).toInt
      for _ <- 0 until pad do builder += 0.toByte
    case DataLineAST(width, Nil) =>
      val ew = if width == 0 then 8 else width
      builder ++= Seq.fill(ew)(0)
    case DataLineAST(width, data) =>
      val ew = if width == 0 then 8 else width
      for d <- data do
        fold(d, absolute = true) match
          case ReferenceExprAST(ref) if relocatable && width == 8 => emitAbs64Reloc(ref)
          case StringExprAST(s) =>
            val bytes = s.getBytes(scala.io.Codec.UTF8.charSet)
            builder ++= immutable.ArraySeq.unsafeWrapArray(bytes)
            builder += 0 // null terminator
          case value =>
            width match
              case 1 =>
                value match
                  case LongExprAST(v) if -128 <= v && v <= 255 => builder += v.toByte
                  case _ => problem(d, "expected a byte value")
              case 2 =>
                value match
                  case LongExprAST(v) if -32768 <= v && v <= 0xffffL =>
                    builder += (v >> 8).toByte
                    builder += v.toByte
                  case _ => problem(d, "expected a short value")
              case 4 =>
                value match
                  case LongExprAST(v) if -2147483648L <= v && v <= 0xffffffffL =>
                    emitI32(v.toInt)
                  case _ => problem(d, "expected a word value")
              case 8 | 0 =>
                value match
                  case DoubleExprAST(f) => emitI64(java.lang.Double.doubleToLongBits(f))
                  case LongExprAST(v) => emitI64(v)
                  case _ => problem(d, "expected a 64-bit value")

    case ReserveLineAST(width, n) =>
      val ew = if width == 0 then 8 else width
      fold(n, absolute = true) match
        case LongExprAST(count) if 0 < count && count <= 10 * 1024 * 1024 =>
          builder.addRes((count * ew).toInt)
        case _ => problem(n, s"must be a positive integer up to 10 meg")

    case inst @ InstructionLineAST(mnemonic, operands) =>
      val opcode = opcodeMap.getOrElse(mnemonic, problem(inst, s"unknown mnemonic '$mnemonic'"))
      val size = instrSize(mnemonic, operands)

      mnemonic match
        // --- Zero-operand instructions ---
        case m if size == 1 && operands.isEmpty =>
          emitByte(opcode)

        // --- opcode + u8 operand ---
        case "push_i8" | "push_u8" | "frame" | "local_get" | "local_set" | "local_tee" |
             "add_imm8" | "sub_imm8" | "mul_imm8" |
             "local_get_add" | "local_get_sub" | "local_get_eqz" |
             "push_i8_add" | "push_i8_load64" =>
          if operands.size != 1 then problem(inst, s"$mnemonic requires 1 operand")
          val imm = fold(operands.head, absolute = true, immediate = true) match
            case LongExprAST(n) if -128 <= n && n <= 255 => n.toInt
            case _ => problem(operands.head, "expected byte value")
          emitByte(opcode)
          emitByte(imm)

        case "trap" =>
          if operands.size != 1 then problem(inst, "trap requires 1 operand")
          val imm = fold(operands.head, absolute = true, immediate = true) match
            case LongExprAST(n) if 0 <= n && n <= 255 => n.toInt
            case _ => problem(operands.head, "expected unsigned byte value")
          emitByte(opcode)
          emitByte(imm)

        // --- opcode + i16 relative branch ---
        case "jump" | "jumpz" | "jumpnz" | "eqz_jumpz" | "eqz_jumpnz" |
             "inc_jumpnz" | "dec_jumpnz" | "drop_jump" =>
          if operands.size != 1 then problem(inst, s"$mnemonic requires 1 operand")
          val instrEnd = builder.length + builder.org + size
          resolveBranchRel(operands.head, instrEnd) match
            case Some(offset) =>
              if offset < -32768 || offset > 32767 then problem(operands.head, "branch offset out of 16-bit range")
              emitByte(opcode)
              emitI16(offset.toInt)
            case None =>
              problem(operands.head, "relocatable branch targets not supported for 16-bit jumps")

        // --- opcode + i16 literal ---
        case "push_i16" =>
          if operands.size != 1 then problem(inst, "push_i16 requires 1 operand")
          val imm = fold(operands.head, absolute = true, immediate = true) match
            case LongExprAST(n) if -32768 <= n && n <= 65535 => n.toInt
            case _ => problem(operands.head, "expected 16-bit value")
          emitByte(opcode)
          emitI16(imm)

        // --- opcode + u8 + i16 (local_get_jumpz, local_get_jumpnz) ---
        case "local_get_jumpz" | "local_get_jumpnz" =>
          if operands.size != 2 then problem(inst, s"$mnemonic requires 2 operands")
          val idx = fold(operands.head, absolute = true, immediate = true) match
            case LongExprAST(n) if 0 <= n && n <= 255 => n.toInt
            case _ => problem(operands.head, "expected local index (0-255)")
          val instrEnd = builder.length + builder.org + size
          resolveBranchRel(operands(1), instrEnd) match
            case Some(offset) =>
              if offset < -32768 || offset > 32767 then problem(operands(1), "branch offset out of 16-bit range")
              emitByte(opcode)
              emitByte(idx)
              emitI16(offset.toInt)
            case None =>
              problem(operands(1), "relocatable branch targets not supported for local_get_jump")

        // --- opcode + i32 relative (call, tail, jump_wide) ---
        case "call" | "tail" | "jump_wide" =>
          if operands.size != 1 then problem(inst, s"$mnemonic requires 1 operand")
          val instrEnd = builder.length + builder.org + size
          resolveBranchRel(operands.head, instrEnd) match
            case Some(offset) =>
              emitByte(opcode)
              emitI32(offset.toInt)
            case None =>
              // Extern/relocatable — emit CALL_ABS with ABS64 relocation
              val ref = operands.head match
                case ReferenceExprAST(name) => name
                case _ => problem(operands.head, "expected symbol name for relocatable call")
              emitCallAbsReloc(ref)

        // --- opcode + i32 literal ---
        case "push_i32" =>
          if operands.size != 1 then problem(inst, "push_i32 requires 1 operand")
          fold(operands.head, absolute = true, immediate = true) match
            case LongExprAST(n) =>
              emitByte(opcode)
              emitI32(n.toInt)
            case ReferenceExprAST(ref) if relocatable =>
              // Promote to push_i64 with relocation
              emitByte(0x18) // PUSH_i64 opcode
              emitAbs64Reloc(ref)
            case _ => problem(operands.head, "expected 32-bit value")

        // --- opcode + i64 ---
        case "push_i64" =>
          if operands.size != 1 then problem(inst, "push_i64 requires 1 operand")
          fold(operands.head, absolute = true, immediate = true) match
            case LongExprAST(n) =>
              emitByte(opcode)
              emitI64(n)
            case DoubleExprAST(d) =>
              emitByte(opcode)
              emitI64(java.lang.Double.doubleToLongBits(d))
            case ReferenceExprAST(ref) if relocatable =>
              emitByte(opcode)
              emitAbs64Reloc(ref)
            case _ => problem(operands.head, "expected 64-bit value")

        // --- opcode + abs64 (call_abs) ---
        case "call_abs" =>
          if operands.size != 1 then problem(inst, "call_abs requires 1 operand")
          val imm = fold(operands.head, absolute = true, immediate = true) match
            case LongExprAST(n) => n
            case _ => problem(operands.head, "expected 64-bit address")
          emitByte(opcode)
          emitI64(imm)

        case _ =>
          problem(inst, s"unhandled instruction: $mnemonic")
  }

  // Warnings
  if !relocatable then
    symbols.values foreach {
      case LabelSymbol(name, _, sym, false) if !globals.contains(name) && !entryPoint.contains(name) =>
        warning(sym, s"Warning: label '$name' never referenced")
      case _ =>
    }

  for name <- declaredExterns do
    if !referencedExterns.contains(name) then
      println(s"Warning: extern '$name' declared but never referenced")

  builder.tof
