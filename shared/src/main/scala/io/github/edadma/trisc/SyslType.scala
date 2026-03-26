package io.github.edadma.trisc

enum SyslType:
  case IntType
  case CharType
  case ByteType
  case BoolType
  case VoidType
  case PtrType(pointee: SyslType)
  case ArrayType(elem: SyslType, size: Int)

  def isNumeric: Boolean = this match
    case IntType | CharType | ByteType => true
    case _ => false

  def isIntegral: Boolean = isNumeric

  def isBoolOrNumeric: Boolean = this match
    case IntType | CharType | ByteType | BoolType => true
    case _ => false

  def isPointerLike: Boolean = this match
    case PtrType(_) | ArrayType(_, _) => true
    case _ => false

  override def toString: String = this match
    case IntType => "int"
    case CharType => "char"
    case ByteType => "byte"
    case BoolType => "bool"
    case VoidType => "void"
    case PtrType(t) => s"*$t"
    case ArrayType(t, n) => s"[$n]$t"
