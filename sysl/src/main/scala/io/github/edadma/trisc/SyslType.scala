package io.github.edadma.trisc

enum SyslType:
  case IntType(width: Int)   // i8, i16, i32, i64
  case UIntType(width: Int)  // u8, u16, u32, u64
  case BoolType
  case VoidType
  case PtrType(pointee: SyslType)
  case ArrayType(elem: SyslType, size: Int)
  case FuncType(params: List[SyslType], returnType: SyslType, escaping: Boolean = false)
  case StructType(name: String, fields: List[(String, SyslType)], volatileFields: Set[Int] = Set.empty)
  case FloatType(width: Int)   // f32 (single-precision), f64 (double-precision)
  case StringType
  case SliceType(elem: SyslType)
  case RefType(inner: SyslType)  // &T — ref-counted heap reference
  case EnumType(name: String, variants: List[(String, List[(String, SyslType)])])  // tagged union
  case InterfaceType(name: String, methods: List[(String, List[SyslType], SyslType)])  // {itable_ptr, data_ptr}

  def isNumeric: Boolean = this match
    case _: IntType | _: UIntType => true
    case _: FloatType => true
    case _ => false

  def isIntegral: Boolean = this match
    case _: IntType | _: UIntType => true
    case _ => false

  def isFloat: Boolean = this match
    case _: FloatType => true
    case _ => false

  def isSigned: Boolean = this match
    case _: IntType => true
    case _ => false

  def isUnsigned: Boolean = this match
    case _: UIntType => true
    case _ => false

  def isBoolOrNumeric: Boolean = this match
    case _: IntType | _: UIntType | BoolType => true
    case _: FloatType => true
    case _ => false

  def isPointerLike: Boolean = this match
    case PtrType(_) | ArrayType(_, _) | SliceType(_) | RefType(_) => true
    case _ => false

  // Size in bytes
  def sizeOf: Long = this match
    case IntType(w) => w / 8
    case UIntType(w) => w / 8
    case BoolType => 1
    case VoidType => 0
    case PtrType(_) => 8
    case _: FuncType => 16           // {func_ptr(8), env_ptr(8)} — closure-ready fat pointer
    case InterfaceType(_, _) => 16   // {itable_ptr(8), data_ptr(8)} — Go-style interface
    case ArrayType(elem, size) => elem.sizeOf * size
    case FloatType(w) => w / 8
    case StringType => 16        // ptr(8) + len(8) — Go-style fat pointer
    case SliceType(_) => 24      // ptr(8) + len(4) + cap(4) + backref(8)
    case RefType(_) => 8         // pointer to heap object (refcount header + data)
    case st @ StructType(_, fields, _) =>
      var offset = 0L
      for (_, typ) <- fields do
        val align = typ.alignOf
        offset = ((offset + align - 1) / align) * align
        offset += typ.sizeOf
      // Pad to struct alignment for array stride
      val structAlign = st.alignOf
      ((offset + structAlign - 1) / structAlign) * structAlign
    case et @ EnumType(_, variants) =>
      // Layout: {tag: i32, padding, data: union of variant fields}
      val dataAlign = et.dataAlignOf
      val tagSize = 4L
      val dataOffset = if dataAlign > 4 then dataAlign else 4L
      val maxDataSize = if variants.isEmpty then 0L else variants.map { (_, fields) =>
        if fields.isEmpty then 0L
        else
          val st = StructType("", fields)
          st.sizeOf
      }.max
      val totalAlign = et.alignOf
      val raw = dataOffset + maxDataSize
      ((raw + totalAlign - 1) / totalAlign) * totalAlign

  def alignOf: Long = this match
    case IntType(w) => (w / 8).toLong.min(8)
    case UIntType(w) => (w / 8).toLong.min(8)
    case BoolType => 1
    case VoidType => 1
    case PtrType(_) => 8
    case _: FuncType => 8
    case InterfaceType(_, _) => 8
    case ArrayType(elem, _) => elem.alignOf
    case FloatType(w) => (w / 8).toLong.min(8)
    case StringType => 8
    case SliceType(_) => 8
    case RefType(_) => 8
    case StructType(_, fields, _) => if fields.isEmpty then 1 else fields.map(_._2.alignOf).max
    case EnumType(_, variants) =>
      val fieldAligns = variants.flatMap(_._2.map(_._2.alignOf))
      if fieldAligns.isEmpty then 4 else fieldAligns.max.max(4)  // at least 4 for tag

  // Width in bits (for integer types)
  def bitWidth: Int = this match
    case IntType(w) => w
    case UIntType(w) => w
    case FloatType(w) => w
    case BoolType => 8
    case PtrType(_) => 64
    case _ => 64

  override def toString: String = this match
    case IntType(8) => "i8"
    case IntType(16) => "i16"
    case IntType(32) => "int"
    case IntType(64) => "i64"
    case IntType(w) => s"i$w"
    case UIntType(8) => "u8"
    case UIntType(16) => "u16"
    case UIntType(32) => "u32"
    case UIntType(64) => "u64"
    case UIntType(w) => s"u$w"
    case FloatType(w) => s"f$w"
    case BoolType => "bool"
    case VoidType => "unit"
    case PtrType(t) => s"*$t"
    case ArrayType(t, n) => s"[$n]$t"
    case FuncType(params, ret, esc) => s"${if esc then "@escaping " else ""}(${params.mkString(", ")}) -> $ret"
    case StructType(name, _, _) => name
    case StringType => "string"
    case SliceType(t) => s"[]$t"
    case RefType(t) => s"&$t"
    case EnumType(name, _) => name
    case InterfaceType(name, _) => name

  def toPrefix: String = this match
    case IntType(w) => s"i$w"
    case UIntType(w) => s"u$w"
    case FloatType(w) => s"f$w"
    case BoolType => "bool"
    case VoidType => "void"
    case PtrType(t) => s"ptr ${t.toPrefix}"
    case ArrayType(t, n) => s"arr $n ${t.toPrefix}"
    case FuncType(params, ret, _) => s"func ${params.size} ${params.map(_.toPrefix).mkString(" ")}${if params.nonEmpty then " " else ""}${ret.toPrefix}"
    case StringType => "string"
    case SliceType(t) => s"slice ${t.toPrefix}"
    case RefType(t) => s"ref ${t.toPrefix}"
    case StructType(name, fields, _) => s"struct $name ${fields.size} ${fields.map((n, t) => s"$n ${t.toPrefix}").mkString(" ")}"
    case EnumType(name, variants) =>
      val vs = variants.map { (vn, fields) => s"$vn ${fields.size} ${fields.map((n, t) => s"$n ${t.toPrefix}").mkString(" ")}" }.mkString(" ")
      s"enum $name ${variants.size} $vs"
    case InterfaceType(name, methods) =>
      val ms = methods.map { (mn, params, ret) => s"$mn ${params.size} ${params.map(_.toPrefix).mkString(" ")}${if params.nonEmpty then " " else ""}${ret.toPrefix}" }.mkString(" ")
      s"iface $name ${methods.size} $ms"

  def isTuple: Boolean = this match
    case StructType(name, _, _) => name.startsWith("_Tuple")
    case _ => false

  // For EnumType: alignment of the data portion (excluding tag)
  def dataAlignOf: Long = this match
    case EnumType(_, variants) =>
      val fieldAligns = variants.flatMap(_._2.map(_._2.alignOf))
      if fieldAligns.isEmpty then 1 else fieldAligns.max
    case _ => 1

  // For EnumType: byte offset where variant data starts (after tag + padding)
  def dataOffset: Long = this match
    case et @ EnumType(_, _) =>
      val da = et.dataAlignOf
      if da > 4 then da else 4L
    case _ => 0

object SyslType:
  /** Mangle a type into a valid identifier suffix for tuple/generic struct names. */
  def mangleType(t: SyslType): String = t match
    case IntType(w) => s"i$w"
    case UIntType(w) => s"u$w"
    case FloatType(w) => s"f$w"
    case BoolType => "bool"
    case VoidType => "void"
    case StringType => "string"
    case PtrType(inner) => s"p${mangleType(inner)}"
    case RefType(inner) => s"r${mangleType(inner)}"
    case SliceType(elem) => s"s${mangleType(elem)}"
    case ArrayType(elem, size) => s"a${size}_${mangleType(elem)}"
    case FuncType(params, ret, _) => s"fn${params.length}_${params.map(mangleType).mkString("_")}_${mangleType(ret)}"
    case StructType(name, _, _) => name
    case EnumType(name, _) => name
    case InterfaceType(name, _) => name

  def tupleType(elemTypes: List[SyslType]): StructType =
    val suffix = elemTypes.map(mangleType).mkString("_")
    StructType(s"_Tuple${elemTypes.length}_$suffix", elemTypes.zipWithIndex.map((t, i) => (s"_$i", t)))

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

  // Canonical type aliases — floating-point
  val F32: FloatType = FloatType(32)
  val F64: FloatType = FloatType(64)

  // Source-level aliases
  val Byte: UIntType = U8
  val Char: UIntType = U32
  val Int: IntType = I32
  val Float: FloatType = F32
  val Double: FloatType = F64

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
      case s if s.startsWith("f") && s.drop(1).forall(_.isDigit) =>
        FloatType(s.drop(1).toInt)
      case "double" => F64
      case "float"  => F32
      // Legacy prefix names for backward compatibility
      case "int"  => I32
      case "uint" => U32
      case "long" => I64
      case "ulong" => U64
      case "char" => U32
      case "byte" => U8
      case "string" => StringType
      case "ptr"  => PtrType(parseType(tokens))
      case "ref"  => RefType(parseType(tokens))
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
      case "enum" =>
        val name = tokens.next()
        val nvariants = tokens.next().toInt
        val variants = (1 to nvariants).map { _ =>
          val vname = tokens.next()
          val nfields = tokens.next().toInt
          val fields = (1 to nfields).map { _ =>
            val fname = tokens.next()
            val ftype = parseType(tokens)
            (fname, ftype)
          }.toList
          (vname, fields)
        }.toList
        EnumType(name, variants)
      case "iface" =>
        val name = tokens.next()
        val nmethods = tokens.next().toInt
        val methods = (1 to nmethods).map { _ =>
          val mname = tokens.next()
          val nparams = tokens.next().toInt
          val params = (1 to nparams).map(_ => parseType(tokens)).toList
          val ret = parseType(tokens)
          (mname, params, ret)
        }.toList
        InterfaceType(name, methods)
      case other => throw IllegalArgumentException(s"unknown type token: '$other'")

  def funcSigToPrefix(params: List[SyslType], ret: SyslType): String =
    s"${params.size} ${params.map(_.toPrefix).mkString(" ")}${if params.nonEmpty then " " else ""}${ret.toPrefix}"

  def funcSigFromPrefix(s: String): (List[SyslType], SyslType) =
    val tokens = s.split("\\s+").iterator
    val nparams = tokens.next().toInt
    val params = (1 to nparams).map(_ => parseType(tokens)).toList
    val ret = parseType(tokens)
    (params, ret)
