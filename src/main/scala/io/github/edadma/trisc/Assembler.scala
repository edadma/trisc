package io.github.edadma.trisc

import scala.collection.mutable

trait Chunk
case class DataChunk(data: Seq[Byte]) extends Chunk

case class Segment(org: Long, chunks: Seq[Chunk])

class Assembler(stacked: Boolean = false):
  private class Pass1Segment:
    var size: Long = 0
    val symbols = new mutable.HashMap[String, Long]

  def assemble(code: String): Unit /*Seq[Segment]*/ =
    val lines = AssemblyParser.parseAssembly(code)
    val equates = new mutable.HashMap[String, ExprAST]
    val segments = new mutable.LinkedHashMap[String, Pass1Segment]
    var segment = new Pass1Segment

    segments("_default_") = segment

    lines foreach {
      case SegmentLineAST(name) =>
        segments get name match
          case None =>
            segment = new Pass1Segment
            segments(name) = segment
          case Some(s) => segment = s
      case label @ LabelLineAST(name) =>
        if segments.exists((_, s) => s.symbols contains name) then problem(label.pos, s"duplicate label '$name'")
        if equates contains name then problem(label.pos, s"equate '$name' already defined")
        segment.symbols(name) = segment.size
      case equate @ EquateLineAST(name, expr) =>
        if equates contains name then problem(equate.pos, s"duplicate equate '$name'")
        if segments.exists((_, s) => s.symbols contains name) then problem(equate.pos, s"label '$name' already defined")
    }
