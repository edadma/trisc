package io.github.edadma.trisc

import io.github.edadma.char_reader.CharReader

trait Assembler:
  val pos: CharReader

case class Label(pos: CharReader, name: String) extends Assembler
case class Instruction(pos: CharReader, mnemonic: String, operands: Expression)
case class Segment(pos: CharReader, name: String) extends Assembler
case class Equate(pos: CharReader, name: String, expr: Expression) extends Assembler

trait Expression extends Assembler

case class Register(pos: CharReader, reg: Int) extends Expression
case class Literal(pos: CharReader, n: Number) extends Expression
case class Reference(pos: CharReader, ref: String) extends Expression
case class Binary(pos: CharReader, left: Expression, op: String, right: Expression) extends Expression
case class Indirect(pos: CharReader, expr: Expression) extends Expression

object Assembler:
  def parse(input: scala.io.Source): Seq[Assembler] = null
