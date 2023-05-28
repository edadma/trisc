package io.github.edadma.trisc

import pprint.pprintln

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait Chunk
case class DataChunk(data: Seq[Byte]) extends Chunk

case class Segment(org: Long, chunks: Seq[Chunk])

class Assembler(stacked: Boolean = false):
  private case class Pass1Segment(
      var size: Long = 0,
      symbols: mutable.HashMap[String, Long] = new mutable.HashMap,
      code: ArrayBuffer[Byte] = new ArrayBuffer,
  )

  def assemble(src: String): Unit /*Seq[Segment]*/ =
    val lines = AssemblyParser.parseAssembly(src)
    val equates = new mutable.HashMap[String, ExprAST]
    val segments = new mutable.LinkedHashMap[String, Pass1Segment]
    var segment = Pass1Segment()

    def add(pieces: (Int, Int)*): Unit =
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
        if segments.exists((_, s) => s.symbols contains name) then problem(label.pos, s"duplicate label '$name'")
        if equates contains name then problem(label.pos, s"equate '$name' already defined")
        segment.symbols(name) = segment.size
      case equate @ EquateLineAST(name, expr) =>
        if equates contains name then problem(equate.pos, s"duplicate equate '$name'")
        if segments.exists((_, s) => s.symbols contains name) then problem(equate.pos, s"label '$name' already defined")
        equates(name) = expr
      case _: InstructionLineAST => segment.size += 2
    }

    pprintln(equates)
    pprintln(segments)

    lines foreach {
      case SegmentLineAST(name) => segment = segments(name)
      case LabelLineAST(_)      =>
      case EquateLineAST(_, _)  =>
      case InstructionLineAST(mnemonic @ "ldi", Seq(RegisterExprAST(reg), LiteralExprAST(n))) =>
        val opcode =
          mnemonic match
            case "ldi" => 0
        add(3 -> 0xe, 3 -> reg, 2 -> opcode, 8 -> n)
    }
