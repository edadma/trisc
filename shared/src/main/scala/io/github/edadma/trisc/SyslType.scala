package io.github.edadma.trisc

enum SyslType:
  case IntType
  case CharType
  case ByteType
  case BoolType
  case VoidType
  case PtrType(pointee: SyslType)
  case ArrayType(elem: SyslType, size: Int)
  case FuncType(params: List[SyslType], returnType: SyslType)

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
    case FuncType(params, ret) => s"func(${params.mkString(",")}) -> $ret"

  def toPrefix: String = this match
    case IntType        => "int"
    case CharType       => "char"
    case ByteType       => "byte"
    case BoolType       => "bool"
    case VoidType       => "void"
    case PtrType(t)     => s"ptr ${t.toPrefix}"
    case ArrayType(t, n) => s"arr $n ${t.toPrefix}"
    case FuncType(params, ret) => s"func ${params.size} ${params.map(_.toPrefix).mkString(" ")}${if params.nonEmpty then " " else ""}${ret.toPrefix}"

object SyslType:
  def fromPrefix(s: String): SyslType =
    val tokens = s.split("\\s+").iterator
    parseType(tokens)

  def parseType(tokens: Iterator[String]): SyslType =
    tokens.next() match
      case "int"  => IntType
      case "char" => CharType
      case "byte" => ByteType
      case "bool" => BoolType
      case "void" => VoidType
      case "ptr"  => PtrType(parseType(tokens))
      case "arr" =>
        val size = tokens.next().toInt
        ArrayType(parseType(tokens), size)
      case "func" =>
        val nparams = tokens.next().toInt
        val params = (1 to nparams).map(_ => parseType(tokens)).toList
        val ret = parseType(tokens)
        FuncType(params, ret)
      case other => throw IllegalArgumentException(s"unknown type token: '$other'")

  def funcSigToPrefix(params: List[SyslType], ret: SyslType): String =
    s"${params.size} ${params.map(_.toPrefix).mkString(" ")}${if params.nonEmpty then " " else ""}${ret.toPrefix}"

  def funcSigFromPrefix(s: String): (List[SyslType], SyslType) =
    val tokens = s.split("\\s+").iterator
    val nparams = tokens.next().toInt
    val params = (1 to nparams).map(_ => parseType(tokens)).toList
    val ret = parseType(tokens)
    (params, ret)
