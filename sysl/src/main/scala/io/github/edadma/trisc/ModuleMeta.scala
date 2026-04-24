package io.github.edadma.trisc

import SyslType.*

case class SymbolMeta(name: String, typ: SymbolMeta.Kind, isPrivate: Boolean, isExtern: Boolean = false, sourceFile: Option[String] = None)

object SymbolMeta:
  enum Kind:
    case Func(params: List[SyslType], returnType: SyslType, isDef: Boolean = false, isPure: Boolean = false, modes: List[ParamMode] = Nil, effects: FuncEffects = FuncEffects.Unknown)
    case Data(dataType: SyslType)
    case Struct(structType: SyslType.StructType)
    case Enum(enumType: SyslType.EnumType)
    case Interface(ifaceType: SyslType.InterfaceType)
    case Impl(traitName: String, targetType: SyslType, methods: Map[String, String]) // methodName → mangledFuncName

case class TraitImplMeta(traitName: String, targetType: SyslType, methods: Map[String, String]) // methodName → mangledFuncName
case class GenericEnumInstanceMeta(mangledName: String, baseName: String, typeArgs: List[SyslType])

class ModuleMeta(val symbols: List[SymbolMeta], val genericTemplates: List[DeclAST] = Nil, val traitImpls: List[TraitImplMeta] = Nil, val genericEnumInstances: List[GenericEnumInstanceMeta] = Nil):

  def toSmeta: String =
    val buf = new StringBuilder
    buf ++= s"SMETA v${ModuleMeta.SMETA_VERSION}\n"
    var currentSource: Option[String] = None
    for sym <- symbols do
      if sym.sourceFile != currentSource && sym.sourceFile.isDefined then
        buf ++= s"SOURCE ${sym.sourceFile.get}\n"
        currentSource = sym.sourceFile
      val vis = if sym.isPrivate then "PRIVATE " else ""
      sym.typ match
        case SymbolMeta.Kind.Func(params, ret, isDef, isPure, modes, effects) =>
          // The FUNCP/DEFFUNCP keyword variants encode `#pure`. For `#reads`/`#writes`
          // (effects.isUnknown is false but isPure is also false), we emit an additional
          // `EFFECTS RW …` trailer.  When `isPure` is true we skip the EFFECTS trailer
          // (the `P` keyword already says everything).
          val kw = (isDef, isPure) match
            case (true, true)   => "DEFFUNCP"
            case (true, false)  => "DEFFUNC"
            case (false, true)  => "FUNCP"
            case (false, false) => "FUNC"
          val sig = SyslType.funcSigToPrefix(params, ret)
          val modeSuffix =
            if modes.nonEmpty && modes.exists(_ != ParamMode.In) then
              val codes = modes.map {
                case ParamMode.In    => 'I'
                case ParamMode.Out   => 'O'
                case ParamMode.Inout => 'U'
              }.mkString
              s" MODES $codes"
            else ""
          val effSuffix = if isPure || effects.isUnknown then "" else s" EFFECTS ${ModuleMeta.encodeEffects(effects)}"
          buf ++= s"${vis}$kw ${sym.name} $sig$modeSuffix$effSuffix\n"
        case SymbolMeta.Kind.Data(dataType) =>
          buf ++= s"${vis}DATA ${sym.name} ${dataType.toPrefix}\n"
        case SymbolMeta.Kind.Struct(st) =>
          buf ++= s"${vis}STRUCT ${sym.name} ${st.toPrefix}\n"
        case SymbolMeta.Kind.Enum(et) =>
          buf ++= s"${vis}ENUM ${sym.name} ${et.toPrefix}\n"
        case SymbolMeta.Kind.Interface(it) =>
          buf ++= s"${vis}IFACE ${sym.name} ${it.toPrefix}\n"
        case SymbolMeta.Kind.Impl(traitName, targetType, methods) =>
          val m = methods.map((k, v) => s"$k=$v").mkString(" ")
          buf ++= s"IMPL $traitName ${targetType.toPrefix} $m\n"
    // Emit generic enum instance mappings
    for inst <- genericEnumInstances do
      buf ++= s"GENINST ${inst.mangledName} ${inst.baseName} ${inst.typeArgs.length} ${inst.typeArgs.map(_.toPrefix).mkString(" ")}\n"
    // Emit impl registrations
    for impl <- traitImpls do
      val methods = impl.methods.map((k, v) => s"$k=$v").mkString(" ")
      buf ++= s"IMPL ${impl.traitName} ${impl.targetType.toPrefix} $methods\n"
    if genericTemplates.nonEmpty then
      buf ++= "TEMPLATES\n"
      for template <- genericTemplates do
        buf ++= SyslPrettyPrinter.declToSource(template)
        buf ++= "\n\n"
      buf ++= "TEMPLATES_END\n"
    buf.toString

  def toAsmGlobals: String =
    val buf = new StringBuilder
    for sym <- symbols do
      if sym.isExtern then
        buf ++= s"extern ${sym.name}\n"
      else sym.typ match
        case SymbolMeta.Kind.Func(params, ret, _, _, _, _) =>
          buf ++= s"global ${sym.name}, func, ${SyslType.funcSigToPrefix(params, ret)}\n"
        case SymbolMeta.Kind.Data(dataType) =>
          buf ++= s"global ${sym.name}, data, ${dataType.toPrefix}\n"
        case SymbolMeta.Kind.Struct(_) | SymbolMeta.Kind.Enum(_) | SymbolMeta.Kind.Interface(_) | SymbolMeta.Kind.Impl(_, _, _) => // type-only, no asm global
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
    new ModuleMeta(kept ++ other.symbols, genericTemplates ++ other.genericTemplates, traitImpls ++ other.traitImpls, genericEnumInstances ++ other.genericEnumInstances)

  /** Get the set of source files that define the given symbol names. */
  def sourceFilesFor(names: Set[String]): Set[String] =
    symbols.filter(s => names.contains(s.name)).flatMap(_.sourceFile).toSet

  /** Get all source files referenced in this meta. */
  def allSourceFiles: Set[String] =
    symbols.flatMap(_.sourceFile).toSet

object ModuleMeta:

  /** Bump this whenever the .smeta format changes. Stale files are silently ignored.
   *  v10 adds optional `EFFECTS <U|P|RW nR <names…> nW <names…>>` trailer on FUNC lines
   *  to carry `#reads`/`#writes` signatures across modules, and the same encoding inline
   *  on each IFACE method so per-method effect sigs round-trip. */
  val SMETA_VERSION = 10

  /** Encode a FuncEffects as space-separated tokens — `U` (Unknown), `P` (Pure), or
   *  `RW <nReads> <readsNames…> <nWrites> <writesNames…>`. Used both in the FUNC-line
   *  trailer (after `EFFECTS`) and in the inline IFACE-method tail. Names are already
   *  in their resolved (mangled) form so no further translation is needed on read. */
  def encodeEffects(eff: FuncEffects): String =
    if eff.isPure then "P"
    else if eff.isUnknown then "U"
    else
      val r = eff.reads.getOrElse(Set.empty).toList.sorted
      val w = eff.writes.getOrElse(Set.empty).toList.sorted
      s"RW ${r.size}${if r.nonEmpty then " " + r.mkString(" ") else ""} ${w.size}${if w.nonEmpty then " " + w.mkString(" ") else ""}"

  /** Inverse of `encodeEffects`. Reads exactly the tokens it expects. */
  def decodeEffects(tokens: Iterator[String]): FuncEffects =
    tokens.next() match
      case "U" => FuncEffects.Unknown
      case "P" => FuncEffects.Pure
      case "RW" =>
        val nR = tokens.next().toInt
        val r = (1 to nR).map(_ => tokens.next()).toSet
        val nW = tokens.next().toInt
        val w = (1 to nW).map(_ => tokens.next()).toSet
        FuncEffects(reads = Some(r), writes = Some(w))
      case other => throw IllegalArgumentException(s"unknown effects token '$other'")

  def fromProgram(program: TProgram, sourceFile: Option[String] = None): ModuleMeta =
    val syms = program.decls.collect {
      case TStructDecl(name, fields, _) =>
        SymbolMeta(name, SymbolMeta.Kind.Struct(SyslType.StructType(name, fields)), isPrivate = false, sourceFile = sourceFile)
      case TEnumDecl(name, members) =>
        // Simple enum: convert to EnumType with empty variant fields for serialization
        val variants: List[(String, List[(String, SyslType)])] = members.map((n, _) => (n, Nil))
        val et: SyslType.EnumType = SyslType.EnumType(name, variants)
        SymbolMeta(name, SymbolMeta.Kind.Enum(et), isPrivate = false, sourceFile = sourceFile)
      case TDataEnumDecl(name, et: SyslType.EnumType) =>
        SymbolMeta(name, SymbolMeta.Kind.Enum(et), isPrivate = false, sourceFile = sourceFile)
      case TInterfaceDecl(name, ifaceType) =>
        SymbolMeta(name, SymbolMeta.Kind.Interface(ifaceType), isPrivate = false, sourceFile = sourceFile)
      case TExternFuncDecl(name, params, returnType) =>
        SymbolMeta(name, SymbolMeta.Kind.Func(params, returnType), isPrivate = false, isExtern = true, sourceFile = sourceFile)
      case TExternVarDecl(name, typ) =>
        SymbolMeta(name, SymbolMeta.Kind.Data(typ), isPrivate = false, isExtern = true, sourceFile = sourceFile)
      case TFunDecl(name, params, returnType, _, isPrivate, attrs, isDef, _, effects) =>
        val isPure = attrs.exists(_.name == "pure")
        // Param modes: infer from the pointer-wrapping of declared param types. The
        // analyzer stores Out/Inout params with type `*T`; the TFunDecl exposes that
        // same type and marks the param with a `mode` attribute. We carry an explicit
        // `ParamMode` list here so the importing unit can re-apply autoIndirect + call
        // auto-wrap without needing to re-derive mode from the type alone.
        val modes = params.map(_.mode)
        val needModes = modes.exists(_ != ParamMode.In)
        SymbolMeta(name, SymbolMeta.Kind.Func(params.map(_.typ), returnType, isDef, isPure, if needModes then modes else Nil, effects), isPrivate, sourceFile = sourceFile)
      case TVarDecl(name, typ, _, isPrivate, _, _) =>
        SymbolMeta(name, SymbolMeta.Kind.Data(typ), isPrivate, sourceFile = sourceFile)
    }
    new ModuleMeta(syms)

  def fromSmeta(source: String): Option[ModuleMeta] =
    import scala.util.boundary, boundary.break
    boundary:
      val syms = scala.collection.mutable.ListBuffer[SymbolMeta]()
      val implMetas = scala.collection.mutable.ListBuffer[TraitImplMeta]()
      val genInstMetas = scala.collection.mutable.ListBuffer[GenericEnumInstanceMeta]()
      var lineNum = 0
      var headerSeen = false
      var currentSource: Option[String] = None
      var inTemplates = false
      val templateBuf = new StringBuilder

      for rawLine <- source.linesIterator do
        lineNum += 1
        if inTemplates then
          if rawLine.trim == "TEMPLATES_END" then
            inTemplates = false
          else
            templateBuf ++= rawLine
            templateBuf += '\n'
        else
          val line = rawLine.trim
          if line.nonEmpty then
            if !headerSeen then
              if !line.startsWith("SMETA") then break(None) // not a valid smeta file — treat as stale
              val version = line.stripPrefix("SMETA").trim.stripPrefix("v").trim.toIntOption.getOrElse(0)
              if version < SMETA_VERSION then break(None) // stale — caller should recompile from source
              headerSeen = true
            else if line == "TEMPLATES" then
              inTemplates = true
            else if line.startsWith("SOURCE ") then
              currentSource = Some(line.drop(7).trim)
            else if line.startsWith("GENINST ") then
              // GENINST mangledName baseName nArgs type1 type2 ...
              val tokens = line.drop(8).split("\\s+").iterator
              val mangledName = tokens.next()
              val baseName = tokens.next()
              val nArgs = tokens.next().toInt
              val typeArgs = (1 to nArgs).map(_ => SyslType.parseType(tokens)).toList
              genInstMetas += GenericEnumInstanceMeta(mangledName, baseName, typeArgs)
            else if line.startsWith("IMPL ") then
              // IMPL traitName typePrefix method1=mangled1 method2=mangled2 ...
              val tokens = line.drop(5).split("\\s+").iterator
              val traitName = tokens.next()
              val targetType = SyslType.parseType(tokens)
              val methods = scala.collection.mutable.Map[String, String]()
              while tokens.hasNext do
                val pair = tokens.next().split("=", 2)
                if pair.length == 2 then methods(pair(0)) = pair(1)
              implMetas += TraitImplMeta(traitName, targetType, methods.toMap)
            else
              val (isPrivate, rest) = if line.startsWith("PRIVATE ") then (true, line.drop(8)) else (false, line)
              val tokens = rest.split("\\s+").iterator
              val kind = tokens.next()
              val name = tokens.next()
              kind match
                case "FUNC" | "DEFFUNC" | "FUNCP" | "DEFFUNCP" =>
                  val isDef = kind == "DEFFUNC" || kind == "DEFFUNCP"
                  val isPure = kind == "FUNCP" || kind == "DEFFUNCP"
                  val nparams = tokens.next().toInt
                  val params = (1 to nparams).map(_ => SyslType.parseType(tokens)).toList
                  val ret = SyslType.parseType(tokens)
                  // Optional `MODES <IOU…>` and / or `EFFECTS <encoding>` trailers in any
                  // order. Both are absent for the trivial all-In, no-effects case.
                  var modes: List[ParamMode] = Nil
                  var effects: FuncEffects = if isPure then FuncEffects.Pure else FuncEffects.Unknown
                  while tokens.hasNext do
                    tokens.next() match
                      case "MODES" =>
                        val codes = tokens.next()
                        if codes.length != nparams then
                          throw IllegalArgumentException(s"line $lineNum: MODES length ${codes.length} != param count $nparams")
                        modes = codes.map {
                          case 'I' => ParamMode.In
                          case 'O' => ParamMode.Out
                          case 'U' => ParamMode.Inout
                          case c   => throw IllegalArgumentException(s"line $lineNum: unknown mode code '$c'")
                        }.toList
                      case "EFFECTS" =>
                        effects = ModuleMeta.decodeEffects(tokens)
                      case other =>
                        throw IllegalArgumentException(s"line $lineNum: unexpected token after FUNC signature: '$other'")
                  syms += SymbolMeta(name, SymbolMeta.Kind.Func(params, ret, isDef, isPure, modes, effects), isPrivate, sourceFile = currentSource)
                case "DATA" =>
                  val dataType = SyslType.parseType(tokens)
                  syms += SymbolMeta(name, SymbolMeta.Kind.Data(dataType), isPrivate, sourceFile = currentSource)
                case "STRUCT" =>
                  val st = SyslType.parseType(tokens).asInstanceOf[SyslType.StructType]
                  syms += SymbolMeta(name, SymbolMeta.Kind.Struct(st), isPrivate, sourceFile = currentSource)
                case "ENUM" =>
                  val et = SyslType.parseType(tokens).asInstanceOf[SyslType.EnumType]
                  syms += SymbolMeta(name, SymbolMeta.Kind.Enum(et), isPrivate, sourceFile = currentSource)
                case "IFACE" =>
                  val it = SyslType.parseType(tokens).asInstanceOf[SyslType.InterfaceType]
                  syms += SymbolMeta(name, SymbolMeta.Kind.Interface(it), isPrivate, sourceFile = currentSource)
                case other =>
                  throw IllegalArgumentException(s"line $lineNum: unknown symbol kind '$other'")

      if !headerSeen then None
      else
        // Parse generic templates from the TEMPLATES section
        val templates = if templateBuf.nonEmpty then
          val parser = new SyslParser
          parser.parseProgram(templateBuf.toString) match
            case Right(ast) =>
              ast.decls.filter {
                case StructDeclAST(_, _, tps, _, _)        => tps.nonEmpty
                case DataEnumDeclAST(_, _, tps, _)         => tps.nonEmpty
                case FunDeclAST(_, _, _, _, _, tps, _, _, _) => tps.nonEmpty
                case _: TraitDeclAST                        => true
                case _                                      => false
              }
            case Left(_) => Nil // silently ignore parse failures in templates
        else Nil
        Some(new ModuleMeta(syms.toList, templates, implMetas.toList, genInstMetas.toList))
