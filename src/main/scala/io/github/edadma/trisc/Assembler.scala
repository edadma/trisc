package io.github.edadma.trisc

import pprint.pprintln

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait Chunk
case class DataChunk(data: Seq[Byte]) extends Chunk

case class Segment(org: Long, chunks: Seq[Chunk])

class Assembler(stacked: Boolean = false):
  private case class Pass1Segment(
      var size: Long = 0,
      var symbols: mutable.HashMap[String, Long] = new mutable.HashMap,
      code: ArrayBuffer[Byte] = new ArrayBuffer,
  )

  def assemble(src: String): Unit /*Seq[Segment]*/ =
    val lines = AssemblyParser.parseAssembly(src)
    val equates = new mutable.HashMap[String, ExprAST]
    val segments = new mutable.LinkedHashMap[String, Pass1Segment]
    var segment = Pass1Segment()

    @tailrec
    def fold(e: ExprAST): ExprAST =
      e match
        case lit: LiteralExprAST  => lit
        case reg: RegisterExprAST => reg
        case ReferenceExprAST(ref) =>
          equates get ref match
            case None       => problem(e, s"unrecognized equate '$ref'")
            case Some(expr) => fold(expr)

    def eval(e: ExprAST): Number =
      e match
        case LiteralExprAST(n) => n
        case ReferenceExprAST(ref) =>
          equates get ref match
            case None       => problem(e, s"unrecognized equate '$ref'")
            case Some(expr) => eval(expr)

    def addInstruction(pieces: (Int, Int)*): Unit =
      var inst = 0
      var shift = 16

      for (w, v) <- pieces do
        shift -= w
        inst |= v << shift

      segment.code += (inst >> 8).toByte
      segment.code += inst.toByte

    segments("_default_") = segment

    lines foreach {
      case SegmentLineAST(name) =>
        segments get name match
          case None =>
            segment = Pass1Segment()
            segments(name) = segment
          case Some(s) => segment = s
      case label @ LabelLineAST(name) =>
        if segments.exists((_, s) => s.symbols contains name) then problem(label, s"duplicate label '$name'")
        if equates contains name then problem(label, s"equate '$name' already defined")
        segment.symbols(name) = segment.size
      case equate @ EquateLineAST(name, expr) =>
        if equates contains name then problem(equate, s"duplicate equate '$name'")
        if segments.exists((_, s) => s.symbols contains name) then problem(equate, s"label '$name' already defined")
        equates(name) = expr
      case DataLineAST(width, data) =>
        for d <- data do
          segment.size +=
            (d match
              case StringExprAST(s) => s.length
              case _                => width
            )
      case _: InstructionLineAST => segment.size += 2
    }

    pprintln(equates)

    if stacked then
      var start: Long = segments.values.head.size

      for s <- segments.values.tail do
        s.symbols = s.symbols map ((name, offset) => name -> (start + offset))
        start += s.size

    lines foreach {
      case SegmentLineAST(name) => segment = segments(name)
      case LabelLineAST(_)      =>
      case EquateLineAST(_, _)  =>
      case DataLineAST(width, data) =>
        for d <- data do
          width match
            case 1 =>
              eval(d) match
                case _: java.lang.Double => problem(d, "expected a byte value, found float")
                case v: java.lang.Long if v.asInstanceOf[Long].isValidByte => segment.code += v.toByte
                case _ => problem(d, "expected a byte value, out of range")
            case 2 =>
              eval(d) match
                case _: java.lang.Double => problem(d, "expected a short value, found float")
                case v: java.lang.Long if v.asInstanceOf[Long].isValidShort =>
                  segment.code += (v >> 8).toByte
                  segment.code += v.toByte
                case _ => problem(d, "expected a short value, out of range")
            case 4 =>
              eval(d) match
                case _: java.lang.Double => problem(d, "expected an int value, found float")
                case v: java.lang.Long if v.asInstanceOf[Long].isValidInt =>
                  segment.code += (v >> 24).toByte
                  segment.code += (v >> 16).toByte
                  segment.code += (v >> 8).toByte
                  segment.code += v.toByte
                case _ => problem(d, "expected a short value, out of range")
            case 8 =>
              eval(d) match
                case _: Double => problem(d, "expected an int value, found float")
                case v: Long =>
                  segment.code += (v >> 56).toByte
                  segment.code += (v >> 48).toByte
                  segment.code += (v >> 40).toByte
                  segment.code += (v >> 32).toByte
                  segment.code += (v >> 24).toByte
                  segment.code += (v >> 16).toByte
                  segment.code += (v >> 8).toByte
                  segment.code += v.toByte
            case 0 =>
              val v =
                eval(d) match
                  case d: Double => java.lang.Double.doubleToLongBits(d)
                  case l: Long   => java.lang.Double.doubleToLongBits(l)

              segment.code += (v >> 56).toByte
              segment.code += (v >> 48).toByte
              segment.code += (v >> 40).toByte
              segment.code += (v >> 32).toByte
              segment.code += (v >> 24).toByte
              segment.code += (v >> 16).toByte
              segment.code += (v >> 8).toByte
              segment.code += v.toByte
      case InstructionLineAST(mnemonic @ "ldi", Seq(o1, o2)) =>
        val opcode =
          mnemonic match
            case "ldi" => 0
        val reg =
          fold(o1) match
            case RegisterExprAST(reg) => reg
            case _                    => problem(o1, "expected register as first operand")
        val imm =
          fold(o2) match
            case LiteralExprAST(_: Double)                 => problem(o2, "immediate must be integral")
            case LiteralExprAST(n: Long) if !n.isValidByte => problem(o2, "immediate must be a byte value")
            case LiteralExprAST(n: Long)                   => n.toInt

        addInstruction(3 -> 7, 3 -> reg, 2 -> opcode, 8 -> imm)
      case InstructionLineAST(mnemonic @ ("beq" | "blu" | "bls" | "addi"), Seq(o1, o2, o3)) =>
        val opcode =
          mnemonic match
            case "beq"  => 2
            case "blu"  => 3
            case "bls"  => 4
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
          fold(o3) match
            case LiteralExprAST(_: Double)                        => problem(o2, "immediate must be integral")
            case LiteralExprAST(n: Long) if -128 <= n && n <= 127 => n.toInt
            case _: LiteralExprAST                                => problem(o2, "immediate must be a byte value")

        addInstruction(3 -> opcode, 3 -> reg1, 3 -> reg2, 7 -> imm)
    }

    pprintln(segments)

    segments foreach ((name, seg) => println((name, seg.code map (b => (b & 0xff).toHexString))))
