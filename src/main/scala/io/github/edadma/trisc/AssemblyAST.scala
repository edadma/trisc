package io.github.edadma.trisc

import scala.util.parsing.input.Positional

trait LineAST extends Positional

case class LabelLineAST(name: String) extends LineAST
case class InstructionLineAST(mnemonic: String, operands: Seq[ExprAST]) extends LineAST
case class SegmentLineAST(name: String) extends LineAST
case class EquateLineAST(name: String, expr: ExprAST) extends LineAST
case class IncludeLineAST(path: String) extends LineAST

trait ExprAST extends Positional

case class RegisterExprAST(reg: Int) extends ExprAST
case class LiteralExprAST(n: Number) extends ExprAST
case class ReferenceExprAST(ref: String) extends ExprAST
case class StringExprAST(ref: String) extends ExprAST
case class BinaryExprAST(left: ExprAST, op: String, right: ExprAST) extends ExprAST
