package io.github.edadma.trisc

import SyslType.*

case class SymbolMeta(name: String, typ: SymbolMeta.Kind, isPrivate: Boolean)

object SymbolMeta:
  enum Kind:
    case Func(params: List[SyslType], returnType: SyslType)
    case Data(dataType: SyslType)

class ModuleMeta(val symbols: List[SymbolMeta]):

  def toSmeta: String =
    val buf = new StringBuilder
    buf ++= "SMETA v1\n"
    for sym <- symbols do
      val vis = if sym.isPrivate then "PRIVATE " else ""
      sym.typ match
        case SymbolMeta.Kind.Func(params, ret) =>
          buf ++= s"${vis}FUNC ${sym.name} ${SyslType.funcSigToPrefix(params, ret)}\n"
        case SymbolMeta.Kind.Data(dataType) =>
          buf ++= s"${vis}DATA ${sym.name} ${dataType.toPrefix}\n"
    buf.toString

  def toAsmGlobals: String =
    val buf = new StringBuilder
    for sym <- symbols if !sym.isPrivate do
      sym.typ match
        case SymbolMeta.Kind.Func(params, ret) =>
          buf ++= s"global ${sym.name}, func, ${SyslType.funcSigToPrefix(params, ret)}\n"
        case SymbolMeta.Kind.Data(dataType) =>
          buf ++= s"global ${sym.name}, data, ${dataType.toPrefix}\n"
    buf.toString

  def publicSymbols: List[SymbolMeta] = symbols.filter(!_.isPrivate)

object ModuleMeta:

  def fromProgram(program: TProgram): ModuleMeta =
    val syms = program.decls.collect {
      case TFunDecl(name, params, returnType, _, isPrivate) =>
        SymbolMeta(name, SymbolMeta.Kind.Func(params.map(_.typ), returnType), isPrivate)
      case TVarDecl(name, typ, _, isPrivate) =>
        SymbolMeta(name, SymbolMeta.Kind.Data(typ), isPrivate)
    }
    new ModuleMeta(syms)

  def fromSmeta(source: String): ModuleMeta =
    val syms = scala.collection.mutable.ListBuffer[SymbolMeta]()
    var lineNum = 0
    var headerSeen = false

    for rawLine <- source.linesIterator do
      lineNum += 1
      val line = rawLine.trim
      if line.nonEmpty then
        if !headerSeen then
          if line != "SMETA v1" then throw IllegalArgumentException(s"line $lineNum: expected SMETA v1 header")
          headerSeen = true
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
              syms += SymbolMeta(name, SymbolMeta.Kind.Func(params, ret), isPrivate)
            case "DATA" =>
              val dataType = SyslType.parseType(tokens)
              syms += SymbolMeta(name, SymbolMeta.Kind.Data(dataType), isPrivate)
            case other =>
              throw IllegalArgumentException(s"line $lineNum: unknown symbol kind '$other'")

    if !headerSeen then throw IllegalArgumentException("empty or missing SMETA header")
    new ModuleMeta(syms.toList)
