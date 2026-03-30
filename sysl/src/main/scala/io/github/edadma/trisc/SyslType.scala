package io.github.edadma.trisc

enum SyslType:
  case IntType(width: Int)   // i8, i16, i32, i64
  case UIntType(width: Int)  // u8, u16, u32, u64
  case BoolType
  case VoidType
  case PtrType(pointee: SyslType)
  case ArrayType(elem: SyslType, size: Int)
  case FuncType(params: List[SyslType], returnType: SyslType)
  case StructType(name: String, fields: List[(String, SyslType)])
  case DoubleType
  case StringType
  case SliceType(elem: SyslType)

  def isNumeric: Boolean = this match
    case _: IntType | _: UIntType => true
    case DoubleType => true
    case _ => false

  def isIntegral: Boolean = this match
    case _: IntType | _: UIntType => true
    case _ => false

  def isSigned: Boolean = this match
    case _: IntType => true
    case _ => false

  def isUnsigned: Boolean = this match
    case _: UIntType => true
    case _ => false

  def isBoolOrNumeric: Boolean = this match
    case _: IntType | _: UIntType | BoolType | DoubleType => true
    case _ => false

  def isPointerLike: Boolean = this match
    case PtrType(_) | ArrayType(_, _) | SliceType(_) => true
    case _ => false

  // Size in bytes
  def sizeOf: Long = this match
    case IntType(w) => w / 8
    case UIntType(w) => w / 8
    case BoolType => 1
    case VoidType => 0
    case PtrType(_) => 8
    case FuncType(_, _) => 8
    case ArrayType(elem, size) => elem.sizeOf * size
    case DoubleType => 8
    case StringType => 12        // ptr(8) + len(4)
    case SliceType(_) => 16      // ptr(8) + len(4) + cap(4)
    case st @ StructType(_, fields) =>
      var offset = 0L
      for (_, typ) <- fields do
        val align = typ.alignOf
        offset = ((offset + align - 1) / align) * align
        offset += typ.sizeOf
      // Pad to struct alignment for array stride
      val structAlign = st.alignOf
      ((offset + structAlign - 1) / structAlign) * structAlign

  def alignOf: Long = this match
    case IntType(w) => (w / 8).toLong.min(8)
    case UIntType(w) => (w / 8).toLong.min(8)
    case BoolType => 1
    case VoidType => 1
    case PtrType(_) => 8
    case FuncType(_, _) => 8
    case ArrayType(elem, _) => elem.alignOf
    case DoubleType => 8
    case StringType => 8
    case SliceType(_) => 8
    case StructType(_, fields) => if fields.isEmpty then 1 else fields.map(_._2.alignOf).max

  // Width in bits (for integer types)
  def bitWidth: Int = this match
    case IntType(w) => w
    case UIntType(w) => w
    case DoubleType => 64
    case BoolType => 8
    case PtrType(_) => 64
    case _ => 64

  override def toString: String = this match
    case IntType(8) => "byte"
    case IntType(16) => "i16"
    case IntType(32) => "int"
    case IntType(64) => "i64"
    case IntType(w) => s"i$w"
    case UIntType(8) => "u8"
    case UIntType(16) => "u16"
    case UIntType(32) => "u32"
    case UIntType(64) => "u64"
    case UIntType(w) => s"u$w"
    case DoubleType => "f64"
    case BoolType => "bool"
    case VoidType => "void"
    case PtrType(t) => s"*$t"
    case ArrayType(t, n) => s"[$n]$t"
    case FuncType(params, ret) => s"func(${params.mkString(",")}) -> $ret"
    case StructType(name, _) => name
    case StringType => "string"
    case SliceType(t) => s"[]$t"

  def toPrefix: String = this match
    case IntType(w) => s"i$w"
    case UIntType(w) => s"u$w"
    case DoubleType => "f64"
    case BoolType => "bool"
    case VoidType => "void"
    case PtrType(t) => s"ptr ${t.toPrefix}"
    case ArrayType(t, n) => s"arr $n ${t.toPrefix}"
    case FuncType(params, ret) => s"func ${params.size} ${params.map(_.toPrefix).mkString(" ")}${if params.nonEmpty then " " else ""}${ret.toPrefix}"
    case StringType => "string"
    case SliceType(t) => s"slice ${t.toPrefix}"
    case StructType(name, fields) => s"struct $name ${fields.size} ${fields.map((n, t) => s"$n ${t.toPrefix}").mkString(" ")}"

object SyslType:
  // Canonical type aliases — signed
  val I8: IntType = IntType(8)
  val I16: IntType = IntType(16)
  val I32: IntType = IntType(32)
  val I64: IntType = IntType(64)

  // Canonical type aliases — unsigned
  val U8: UIntType = UIntType(8)
  val U16: UIntType = UIntType(16)
  val U32: UIntType = UIntType(32)
  val U64: UIntType = UIntType(64)

  // Source-level aliases
  val Byte: IntType = I8
  val Char: UIntType = U32
  val Int: IntType = I32
  val Double: DoubleType.type = DoubleType

  def fromPrefix(s: String): SyslType =
    val tokens = s.split("\\s+").iterator
    parseType(tokens)

  def parseType(tokens: Iterator[String]): SyslType =
    tokens.next() match
      case "bool" => BoolType
      case "void" => VoidType
      case s if s.startsWith("u") && s.drop(1).forall(_.isDigit) =>
        UIntType(s.drop(1).toInt)
      case s if s.startsWith("i") && s.drop(1).forall(_.isDigit) =>
        IntType(s.drop(1).toInt)
      case "f64" | "double" => DoubleType
      // Legacy prefix names for backward compatibility
      case "int"  => I32
      case "char" => U32
      case "byte" => I8
      case "string" => StringType
      case "ptr"  => PtrType(parseType(tokens))
      case "slice" => SliceType(parseType(tokens))
      case "arr" =>
        val size = tokens.next().toInt
        ArrayType(parseType(tokens), size)
      case "func" =>
        val nparams = tokens.next().toInt
        val params = (1 to nparams).map(_ => parseType(tokens)).toList
        val ret = parseType(tokens)
        FuncType(params, ret)
      case "struct" =>
        val name = tokens.next()
        val nfields = tokens.next().toInt
        val fields = (1 to nfields).map { _ =>
          val fname = tokens.next()
          val ftype = parseType(tokens)
          (fname, ftype)
        }.toList
        StructType(name, fields)
      case other => throw IllegalArgumentException(s"unknown type token: '$other'")

  def funcSigToPrefix(params: List[SyslType], ret: SyslType): String =
    s"${params.size} ${params.map(_.toPrefix).mkString(" ")}${if params.nonEmpty then " " else ""}${ret.toPrefix}"

  def funcSigFromPrefix(s: String): (List[SyslType], SyslType) =
    val tokens = s.split("\\s+").iterator
    val nparams = tokens.next().toInt
    val params = (1 to nparams).map(_ => parseType(tokens)).toList
    val ret = parseType(tokens)
    (params, ret)
