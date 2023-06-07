package io.github.edadma.trisc

import pprint.pprintln

import scala.annotation.tailrec
import scala.collection.{mutable, immutable}
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Positional

def assemble(src: String, stacked: Boolean = true, orgs: Map[String, Long] = Map(), addresses: Int = 2): TOF =
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

        symbols get ref match
          case None => problem(e, s"unrecognized symbol '$ref'")
          case Some(l @ LabelSymbol(_, value, _, _)) =>
            l.referenced = true
            LongExprAST(if absolute then value else value - (builder.length + 2 + builder.org))
          case Some(EquateSymbol(_, value)) => fold(value, absolute, immediate)
      case LocalExprAST(_, ref) =>
        symbols get ref match
          case None => problem(e, s"unrecognized symbol '$ref'")
          case Some(l @ LabelSymbol(_, value, _, _)) =>
            l.referenced = true
            LongExprAST(if absolute then value else value - (builder.length + 2 + builder.org))
      case UnaryExprAST("-", expr) =>
        fold(expr, absolute, immediate) match
          case LongExprAST(n)   => LongExprAST(-n)
          case DoubleExprAST(n) => DoubleExprAST(-n)
          case e                => e

  segments("_default_") = segment

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
          case "movi" => addresses * 2
          case _      => 2
      )
      operands foreach locals
  }

//    pprintln(equates)

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

  builder.segment("_default_", segments("_default_").org)

  lines foreach {
    case SegmentLineAST(name)    => builder.segment(name, segments(name).org)
    case LabelLineAST(_)         =>
    case LocalLineAST(_)         =>
    case EquateLineAST(_, _)     =>
    case DataLineAST(width, Nil) => builder ++= (if width == 0 then Seq.fill(8)(0) else Seq.fill(width)(0))
    case DataLineAST(width, data) =>
      val startingLength = builder.length

      for d <- data do
        fold(d, absolute = true) match
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
                  case LongExprAST(v) if v.isValidInt =>
                    builder += (v >> 24).toByte
                    builder += (v >> 16).toByte
                    builder += (v >> 8).toByte
                    builder += v.toByte
                  case _ => problem(d, "expected a short value, out of range")
              case 8 =>
                value match
                  case _: DoubleExprAST => problem(d, "expected an int value, found float")
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

      addInstruction(3 -> 6, 3 -> 0, 3 -> 0, 2 -> 1, 5 -> imm)
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
          "rem" | "and" | "or" | "xor"),
          Seq(o1, o2, o3),
        ) =>
      val opcode =
        mnemonic match
          case "ldb" => 0
          case "stb" => 1
          case "lds" => 2
          case "sts" => 3
          case "ldw" => 4
          case "stw" => 5
          case "ldd" => 6
          case "std" => 7
          case "add" => 8
          case "sub" => 9
          case "mul" => 10
          case "div" => 11
          case "rem" => 12
          case "and" => 13
          case "or"  => 14
          case "xor" => 15
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

      addInstruction(3 -> 0, 3 -> reg1, 3 -> reg2, 3 -> reg3, 4 -> opcode)
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
    case InstructionLineAST(mnemonic @ ("rte"), Nil) =>
      val opcode =
        mnemonic match
          case "rte" => 10

      addInstruction(3 -> 7, 3 -> 0, 3 -> 0, 7 -> opcode)
    case InstructionLineAST(mnemonic @ ("spsr" | "gpsr"), Seq(o)) =>
      val opcode =
        mnemonic match
          case "spsr" => 8
          case "gpsr" => 9
      val reg =
        fold(o) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o, "expected register as first operand")

      addInstruction(3 -> 7, 3 -> 0, 3 -> reg, 7 -> opcode)
    case InstructionLineAST("halt", Nil) => addInstruction(3 -> 6, 3 -> 0, 3 -> 0, 2 -> 0, 5 -> 0) // jalr 0,0
    case InstructionLineAST("bra", Seq(o)) =>
      val imm =
        fold(o, immediate = true) match
          case _: DoubleExprAST                                      => problem(o, "immediate must be integral")
          case LongExprAST(n) if -128 <= n && n <= 126 && n % 2 == 0 => n.toInt
          case _: LongExprAST => problem(o, "immediate must ben even signed 8-bit value")

      addInstruction(3 -> 2, 3 -> 0, 3 -> 0, 7 -> imm / 2) // beq r0, r0, imm
    case InstructionLineAST("movi", Seq(o1, o2)) =>
      val reg =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val imm =
        fold(o2, absolute = true, immediate = true) match
          case _: DoubleExprAST                            => problem(o2, "immediate must be integral")
          case LongExprAST(n) if 0 <= n && n <= 0x7fffffff => n.toInt
          case _: LongExprAST                              => problem(o2, "immediate must be a byte value")

      addresses match
        case 1 => addInstruction(3 -> 7, 3 -> reg, 2 -> 0, 8 -> (imm & 0xff)) // ldi r(reg), <imm
        case 2 =>
          addInstruction(3 -> 7, 3 -> reg, 2 -> 0, 8 -> ((imm >> 8) & 0xff)) // ldi r(reg), >imm
          addInstruction(3 -> 7, 3 -> reg, 2 -> 2, 8 -> (imm & 0xff)) // sli r(reg), <imm
        case 3 =>
          addInstruction(3 -> 7, 3 -> reg, 2 -> 0, 8 -> ((imm >> 16) & 0xff)) // ldi r(reg), >>imm
          addInstruction(3 -> 7, 3 -> reg, 2 -> 2, 8 -> ((imm >> 8) & 0xff)) // sli r(reg), >imm
          addInstruction(3 -> 7, 3 -> reg, 2 -> 2, 8 -> (imm & 0xff)) // sli r(reg), <imm
        case 4 =>
          addInstruction(3 -> 7, 3 -> reg, 2 -> 0, 8 -> ((imm >> 24) & 0xff)) // ldi r(reg), >>imm
          addInstruction(3 -> 7, 3 -> reg, 2 -> 2, 8 -> ((imm >> 16) & 0xff)) // sli r(reg), >>imm
          addInstruction(3 -> 7, 3 -> reg, 2 -> 2, 8 -> ((imm >> 8) & 0xff)) // sli r(reg), >imm
          addInstruction(3 -> 7, 3 -> reg, 2 -> 2, 8 -> (imm & 0xff)) // sli r(reg), <imm
    case InstructionLineAST("nop", Nil) => addInstruction(3 -> 5, 3 -> 0, 3 -> 0, 7 -> 0) // addi r0, r0, 0
    case InstructionLineAST("mov", Seq(o1, o2)) =>
      val reg1 =
        fold(o1) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o1, "expected register as first operand")
      val reg2 =
        fold(o2) match
          case RegisterExprAST(reg) => reg
          case _                    => problem(o2, "expected register as second operand")

      addInstruction(3 -> 5, 3 -> reg1, 3 -> reg2, 7 -> 0) // addi r(reg1), r(reg2), 0
  }

  symbols.values foreach {
    case LabelSymbol(name, _, sym, false) => warning(sym, s"Warning: label '$name' never referenced")
    case _                                =>
  }

//  pprintln(segments)
//  symbols.values foreach {
//    case LabelSymbol(n, v, _, _) => println((n, v.toHexString))
//    case _                       =>
//  }
//  println(segments("_default_"))

//    segments foreach ((name, seg) => println((name, seg.code map (b => (b & 0xff).toHexString))))

  builder.tof
