package io.github.edadma.trisc

import SyslType.*

case class SymbolMeta(name: String, typ: SymbolMeta.Kind, isPrivate: Boolean, isExtern: Boolean = false, sourceFile: Option[String] = None)

object SymbolMeta:
  enum Kind:
    case Func(params: List[SyslType], returnType: SyslType)
    case Data(dataType: SyslType)
    case Struct(structType: SyslType.StructType)

class ModuleMeta(val symbols: List[SymbolMeta]):

  def toSmeta: String =
    val buf = new StringBuilder
    buf ++= "SMETA v1\n"
    var currentSource: Option[String] = None
    for sym <- symbols do
      if sym.sourceFile != currentSource && sym.sourceFile.isDefined then
        buf ++= s"SOURCE ${sym.sourceFile.get}\n"
        currentSource = sym.sourceFile
      val vis = if sym.isPrivate then "PRIVATE " else ""
      sym.typ match
        case SymbolMeta.Kind.Func(params, ret) =>
          buf ++= s"${vis}FUNC ${sym.name} ${SyslType.funcSigToPrefix(params, ret)}\n"
        case SymbolMeta.Kind.Data(dataType) =>
          buf ++= s"${vis}DATA ${sym.name} ${dataType.toPrefix}\n"
        case SymbolMeta.Kind.Struct(st) =>
          buf ++= s"${vis}STRUCT ${sym.name} ${st.toPrefix}\n"
    buf.toString

  def toAsmGlobals: String =
    val buf = new StringBuilder
    for sym <- symbols do
      if sym.isExtern then
        buf ++= s"extern ${sym.name}\n"
      else sym.typ match
        case SymbolMeta.Kind.Func(params, ret) =>
          buf ++= s"global ${sym.name}, func, ${SyslType.funcSigToPrefix(params, ret)}\n"
        case SymbolMeta.Kind.Data(dataType) =>
          buf ++= s"global ${sym.name}, data, ${dataType.toPrefix}\n"
        case SymbolMeta.Kind.Struct(_) => // type-only, no asm global
    buf.toString

  def publicSymbols: List[SymbolMeta] =
    // Deduplicate: if both an extern and a real definition exist for the same name,
    // keep only the real definition
    val byName = symbols.filter(!_.isPrivate).groupBy(_.name)
    byName.values.map { syms =>
      if syms.size > 1 then syms.find(!_.isExtern).getOrElse(syms.head)
      else syms.head
    }.toList

  /** Merge another ModuleMeta into this one, replacing symbols from the same source file. */
  def merge(other: ModuleMeta): ModuleMeta =
    val replacedSources = other.symbols.flatMap(_.sourceFile).toSet
    val kept = symbols.filterNot(s => s.sourceFile.exists(replacedSources.contains))
    new ModuleMeta(kept ++ other.symbols)

  /** Get the set of source files that define the given symbol names. */
  def sourceFilesFor(names: Set[String]): Set[String] =
    symbols.filter(s => names.contains(s.name)).flatMap(_.sourceFile).toSet

  /** Get all source files referenced in this meta. */
  def allSourceFiles: Set[String] =
    symbols.flatMap(_.sourceFile).toSet

object ModuleMeta:

  def fromProgram(program: TProgram, sourceFile: Option[String] = None): ModuleMeta =
    val syms = program.decls.collect {
      case TStructDecl(name, fields) =>
        SymbolMeta(name, SymbolMeta.Kind.Struct(SyslType.StructType(name, fields)), isPrivate = false, sourceFile = sourceFile)
      case TExternFuncDecl(name, params, returnType) =>
        SymbolMeta(name, SymbolMeta.Kind.Func(params, returnType), isPrivate = false, isExtern = true, sourceFile = sourceFile)
      case TExternVarDecl(name, typ) =>
        SymbolMeta(name, SymbolMeta.Kind.Data(typ), isPrivate = false, isExtern = true, sourceFile = sourceFile)
      case TFunDecl(name, params, returnType, _, isPrivate) =>
        SymbolMeta(name, SymbolMeta.Kind.Func(params.map(_.typ), returnType), isPrivate, sourceFile = sourceFile)
      case TVarDecl(name, typ, _, isPrivate) =>
        SymbolMeta(name, SymbolMeta.Kind.Data(typ), isPrivate, sourceFile = sourceFile)
    }
    new ModuleMeta(syms)

  def fromSmeta(source: String): ModuleMeta =
    val syms = scala.collection.mutable.ListBuffer[SymbolMeta]()
    var lineNum = 0
    var headerSeen = false
    var currentSource: Option[String] = None

    for rawLine <- source.linesIterator do
      lineNum += 1
      val line = rawLine.trim
      if line.nonEmpty then
        if !headerSeen then
          if line != "SMETA v1" then throw IllegalArgumentException(s"line $lineNum: expected SMETA v1 header")
          headerSeen = true
        else if line.startsWith("SOURCE ") then
          currentSource = Some(line.drop(7).trim)
        else
          val (isPrivate, rest) = if line.startsWith("PRIVATE ") then (true, line.drop(8)) else (false, line)
          val tokens = rest.split("\\s+").iterator
          val kind = tokens.next()
          val name = tokens.next()
          kind match
            case "FUNC" =>
              val nparams = tokens.next().toInt
              val params = (1 to nparams).map(_ => SyslType.parseType(tokens)).toList
              val ret = SyslType.parseType(tokens)
              syms += SymbolMeta(name, SymbolMeta.Kind.Func(params, ret), isPrivate, sourceFile = currentSource)
            case "DATA" =>
              val dataType = SyslType.parseType(tokens)
              syms += SymbolMeta(name, SymbolMeta.Kind.Data(dataType), isPrivate, sourceFile = currentSource)
            case "STRUCT" =>
              val st = SyslType.parseType(tokens).asInstanceOf[SyslType.StructType]
              syms += SymbolMeta(name, SymbolMeta.Kind.Struct(st), isPrivate, sourceFile = currentSource)
            case other =>
              throw IllegalArgumentException(s"line $lineNum: unknown symbol kind '$other'")

    if !headerSeen then throw IllegalArgumentException("empty or missing SMETA header")
    new ModuleMeta(syms.toList)
