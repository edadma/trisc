package io.github.edadma.trisc

import scala.util.parsing.input.Positional

case class AssemblyAST(lines: Seq[Line])

trait Line extends Positional

case class LabelLine(name: String) extends Line
case class InstructionLine(mnemonic: String, operands: Expr) extends Line
case class SegmentLine(name: String) extends Line
case class EquateLine(name: String, expr: Expr) extends Line

trait Expr extends Positional

case class RegisterExpr(reg: Int) extends Expr
case class LiteralExpr(n: String) extends Expr
case class ReferenceExpr(ref: String) extends Expr
case class StringExpr(ref: String) extends Expr
case class BinaryExpr(left: Expr, op: String, right: Expr) extends Expr
case class IndirectExpr(expr: Expr) extends Expr

case class BinaryExpression(left: Expr, op: String, right: Expr) extends Expr
