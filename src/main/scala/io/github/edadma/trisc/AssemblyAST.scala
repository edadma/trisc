package io.github.edadma.trisc

import scala.util.parsing.input.Positional

trait LineAST extends Positional

case class LabelLineAST(name: String) extends LineAST
case class InstructionLineAST(mnemonic: String, operands: Seq[ExprAST]) extends LineAST
case class SegmentLineAST(name: String) extends LineAST
case class EquateLineAST(name: String, expr: ExprAST) extends LineAST
case class IncludeLineAST(path: String) extends LineAST
case class DataLineAST(width: Int, data: Seq[ExprAST]) extends LineAST

trait ExprAST extends Positional

case class RegisterExprAST(reg: Int) extends ExprAST
case class LongExprAST(n: Long) extends ExprAST
case class DoubleExprAST(n: Double) extends ExprAST
case class ReferenceExprAST(ref: String) extends ExprAST
case class StringExprAST(s: String) extends ExprAST
case class BinaryExprAST(left: ExprAST, op: String, right: ExprAST) extends ExprAST
case class UnaryExprAST(op: String, expr: ExprAST) extends ExprAST
