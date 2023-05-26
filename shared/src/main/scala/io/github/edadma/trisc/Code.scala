package io.github.edadma.trisc

import scala.util.parsing.input.Positional

case class Code(lines: Seq[Line])

trait Line extends Positional

case class Label(name: String) extends Line
case class Instruction(mnemonic: String, operands: Expr) extends Line
case class Segment(name: String) extends Line
case class Equate(name: String, expr: Expr) extends Line

trait Expr extends Positional

case class Register(reg: Int) extends Expr
case class Literal(n: Number) extends Expr
case class Reference(ref: String) extends Expr
case class Binary(left: Expr, op: String, right: Expr) extends Expr
case class Indirect(expr: Expr) extends Expr
