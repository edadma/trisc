package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

class SyslAnalyzer:
  case class AnalysisError(msg: String, node: Any = null) extends RuntimeException(msg)

  private case class SymInfo(name: String, typ: SyslType, mutable: Boolean)
  private case class FunInfo(name: String, params: List[(String, SyslType)], returnType: SyslType, isDef: Boolean = false)

  private val globalScope = new mutable.LinkedHashMap[String, SymInfo]
  private val functions = new mutable.LinkedHashMap[String, FunInfo]
  // Default parameter expressions: function name → list of defaults (one per param, None if no default)
  private val functionDefaults = new mutable.LinkedHashMap[String, List[Option[TExpr]]]
  private val structTypes = new mutable.LinkedHashMap[String, SyslType.StructType]
  private val enumTypes = new mutable.LinkedHashMap[String, Map[String, Long]]  // enum name → (member name → value)
  // Simple enums registered as EnumType so they can appear in type positions.
  // Distinct from dataEnumTypes because simple-enum `Name.Member` access still
  // lowers to TIntLit (integer constant), not TEnumConstruct.
  private val simpleEnumTypes = new mutable.LinkedHashMap[String, SyslType.EnumType]
  private val dataEnumTypes = new mutable.LinkedHashMap[String, SyslType.EnumType]  // data enum name → EnumType
  private val variantToEnum = new mutable.LinkedHashMap[String, (SyslType.EnumType, Int)]  // variant name → (enum type, variant index)
  private val interfaceTypes = new mutable.LinkedHashMap[String, SyslType.InterfaceType]  // interface name → InterfaceType
  private val moduleNamespaces = new mutable.LinkedHashMap[String, ModuleMeta]  // short name → module meta (for qualified imports)
  private val typeAliases = new mutable.LinkedHashMap[String, TypeAST]  // alias name → target type AST
  private val genericTypeAliases = new mutable.LinkedHashMap[String, (List[String], TypeAST)]  // name → (type params, target)
  private val methods = new mutable.LinkedHashMap[String, mutable.Set[String]]  // struct name → set of method names
  private val deprecations = new mutable.LinkedHashMap[String, Option[String]]  // name → optional reason
  private val warnedDeprecations = new mutable.HashSet[String]
  private val externalSymbols = new mutable.LinkedHashSet[String]
  private var scopeStack: mutable.ArrayBuffer[mutable.LinkedHashMap[String, SymInfo]] = null
  private val compileTimeConstants = new mutable.LinkedHashMap[String, Long] // val name → folded value (for constant propagation)
  private var loopDepth: Int = 0
  private var currentReturnType: SyslType = VoidType

  // Module-path name mangling: set from ModuleDeclAST during analyze()
  private var currentModule: Option[String] = None // e.g. "std_strings"

  private def mangleName(name: String): String =
    currentModule match
      case Some(mod) => s"${mod}__$name"
      case None => name

  // Names that must never be mangled: entry point + ABI-level allocation symbols
  // Names that must not be mangled: entry points, ABI-level symbols, and OS kernel
  // functions called from boot.asm. Future: replace with #[no_mangle] attribute.
  private val neverMangle = mutable.HashSet(
    "main", "malloc", "free", "calloc", "realloc", "sbrk",
    // OS kernel ABI (called from boot.asm):
    "kernel_init", "kernel_main", "schedule", "current_thread",
    "syscall_table", "syscall_ssp", "irq_handlers", "ticks",
    "thread_count", "query_thread_state", "query_thread_name", "query_thread_name_len",
    "sleep_until_current", "query_thread_ctx_switches", "query_thread_cpu_ticks",
    "query_total_ctx_switches", "kernel_set_watchdog", "kernel_panic",
    "check_stack_at", "suspend_thread", "resume_thread",
    "kernel_tls_set", "kernel_tls_get", "notify_send", "notify_wait_current",
    "notify_read", "event_wait_current", "event_set_bits", "event_clear_bits",
    "terminate_current", "query_thread_pid",
  )

  private def shouldMangle(name: String): Boolean =
    currentModule.isDefined && !neverMangle.contains(name)

  /** Register names that must not be mangled (e.g. extern declarations from sibling files). */
  def registerNoMangle(names: Iterable[String]): Unit =
    neverMangle ++= names

  /** Strip module prefix from a mangled name to get the short name.
    * Uses indexOf (first `__`) not lastIndexOf, because function names
    * can contain `_` (e.g. `_run_atexit` → mangled `mod___run_atexit`).
    */
  private def shortName(mangledName: String): String =
    mangledName.indexOf("__") match
      case -1 => mangledName
      case i  => mangledName.substring(i + 2)

  // Generic function support
  private val genericTemplates = new mutable.LinkedHashMap[String, FunDeclAST]
  private val instantiations = new mutable.LinkedHashMap[(String, List[SyslType]), String]
  private val specializedDecls = mutable.ListBuffer.empty[TDecl]
  private var typeEnv: Map[String, SyslType] = Map.empty

  // Generic struct support
  private val genericStructs = new mutable.LinkedHashMap[String, StructDeclAST]
  private val genericStructInstantiations = new mutable.LinkedHashMap[(String, List[SyslType]), SyslType.StructType]
  // Reverse map: mangled struct name -> (template name, concrete type args) for unification at call sites
  private val structToTemplate = new mutable.LinkedHashMap[String, (String, List[SyslType])]

  // Generic enum support
  private val genericEnums = new mutable.LinkedHashMap[String, DataEnumDeclAST]
  private val genericEnumInstantiations = new mutable.LinkedHashMap[(String, List[SyslType]), SyslType.EnumType]
  // variant name -> (generic enum name, variant index) for generic enum variants
  private val genericVariantToEnum = new mutable.LinkedHashMap[String, (String, Int)]
  // Reverse map: mangled enum name -> (template name, concrete type args) for unification at call sites
  private val enumToTemplate = new mutable.LinkedHashMap[String, (String, List[SyslType])]

  // Expected type for bidirectional inference (used by generic variant constructors)
  private var currentExpected: Option[SyslType] = None

  // Built-in binary operator → (trait name, method name). Extensible via #operator("sym") on trait methods.
  private val builtinBinaryOperatorTraits: Map[String, (String, String)] = Map(
    "<"  -> ("Ord", "lt"),  "<=" -> ("Ord", "le"),
    ">"  -> ("Ord", "gt"),  ">=" -> ("Ord", "ge"),
    "==" -> ("Eq",  "eq"),  "!=" -> ("Eq",  "ne"),
    "+"  -> ("Add", "add"), "-"  -> ("Sub", "sub"),
    "*"  -> ("Mul", "mul"), "/"  -> ("Div", "div"),
  )

  private val customBinaryOperatorTraits = new mutable.LinkedHashMap[String, (String, String)]

  private def lookupBinaryOperatorTrait(op: String): Option[(String, String)] =
    customBinaryOperatorTraits.get(op).orElse(builtinBinaryOperatorTraits.get(op))

  /** Register #operator / #op attributes from trait methods. */
  private def registerTraitOperatorEntries(traitName: String, methods: List[TraitMethodAST], node: Any): Unit =
    for m <- methods do
      val opAttrs = m.attributes.filter(a => a.name == "operator" || a.name == "op")
      if opAttrs.length > 1 then
        throw AnalysisError(s"trait method '${m.name}' has multiple #operator / #op attributes", m)
      opAttrs.headOption.foreach { attr =>
        val sym = extractOperatorSymbol(attr, m)
        if builtinBinaryOperatorTraits.contains(sym) then
          throw AnalysisError(
            s"operator '$sym' is reserved for built-in trait dispatch; use the standard trait (${builtinBinaryOperatorTraits(sym)._1}) instead of #operator",
            m,
          )
        customBinaryOperatorTraits.get(sym) match
          case Some((t, meth)) if t != traitName || meth != m.name =>
            throw AnalysisError(
              s"operator '$sym' is already bound to trait '$t' (method '$meth')",
              m,
            )
          case _ => ()
        if m.params.length != 2 then
          throw AnalysisError(
            s"trait method '${m.name}' with #operator(\"$sym\") must take exactly two parameters",
            m,
          )
        customBinaryOperatorTraits(sym) = (traitName, m.name)
      }

  private def extractOperatorSymbol(attr: Attribute, at: Any): String =
    attr.args match
      case List(AttrPositional(AttrLitString(s))) if s.nonEmpty => s
      case List(AttrNamed("sym", AttrLitString(s))) if s.nonEmpty => s
      case List(AttrNamed("symbol", AttrLitString(s))) if s.nonEmpty => s
      case _ =>
        throw AnalysisError(s"#${attr.name} requires a non-empty string literal, e.g. #operator(\"~\")", at)

  // Trait / impl support
  private case class TraitInfo(name: String, typeParam: String, methods: List[TraitMethodAST])
  private case class ImplMethodInfo(mangled: String, paramTypes: List[(String, SyslType)], retType: SyslType, body: FunBodyAST, isSynthesized: Boolean)
  private val traits = new mutable.LinkedHashMap[String, TraitInfo]
  // (traitName, targetType) -> (methodName -> mangledFunName)
  private val impls = new mutable.LinkedHashMap[(String, SyslType), mutable.LinkedHashMap[String, String]]
  // Methods to analyze (provided + synthesized defaults) keyed by (traitName, targetType)
  private val implMethodInfos = new mutable.LinkedHashMap[(String, SyslType), List[ImplMethodInfo]]
  // When analyzing a synthesized default method body, rewrite unqualified calls
  // to sibling trait methods to their impl's mangled names
  private var traitCallRewrite: Map[String, String] = Map.empty

  /** Get trait impl metadata for cross-unit serialization. */
  def getTraitImplMetas: List[TraitImplMeta] =
    impls.map { case ((traitName, targetType), methodMap) =>
      TraitImplMeta(traitName, targetType, methodMap.toMap)
    }.toList

  /** Get trait declaration AST nodes for serialization in TEMPLATES section. */
  def getTraitDecls: List[TraitDeclAST] =
    traits.values.map(t => TraitDeclAST(t.name, t.typeParam, t.methods)).toList

  /** Get generic enum instance mappings for cross-module type inference. */
  def getGenericEnumInstances: List[GenericEnumInstanceMeta] =
    enumToTemplate.map { case (mangledName, (baseName, typeArgs)) =>
      GenericEnumInstanceMeta(mangledName, baseName, typeArgs)
    }.toList

  private def pushScope(): Unit =
    scopeStack += new mutable.LinkedHashMap[String, SymInfo]

  private def popScope(): Unit =
    scopeStack.remove(scopeStack.length - 1)

  private def currentScope: mutable.LinkedHashMap[String, SymInfo] =
    scopeStack.last

  private val builtinFunctions = Map(
    "putchar" -> FunInfo("putchar", List("c" -> U32), U32),
    "print" -> FunInfo("print", List("n" -> I32), VoidType),
    "println" -> FunInfo("println", List("n" -> I32), VoidType),
    "puts" -> FunInfo("puts", List("s" -> StringType), VoidType),
    "puti" -> FunInfo("puti", List("n" -> I32), VoidType),
    "malloc" -> FunInfo("malloc", List("size" -> I64), PtrType(I8)),
    "free" -> FunInfo("free", List("ptr" -> PtrType(I8)), VoidType),
    "calloc" -> FunInfo("calloc", List("count" -> I64, "size" -> I64), PtrType(I8)),
    "realloc" -> FunInfo("realloc", List("ptr" -> PtrType(I8), "size" -> I64), PtrType(I8)),
    "sbrk" -> FunInfo("sbrk", List("increment" -> I32), PtrType(I8)),
    "abort" -> FunInfo("abort", Nil, VoidType),
    "panic" -> FunInfo("panic", List("msg" -> StringType), VoidType),
    "assert" -> FunInfo("assert", List("cond" -> BoolType, "msg" -> StringType), VoidType),
    "expect" -> FunInfo("expect", List("actual" -> I64, "expected" -> I64, "msg" -> StringType), VoidType),
  )

  def registerImport(meta: ModuleMeta, selectors: List[ImportSelector] = List(WildcardImport), modulePath: String = ""): Unit =
    // Qualified import: import std.strings → access as strings.foo
    selectors match
      case List(QualifiedImport) =>
        val nsName = modulePath.split("/").last
        moduleNamespaces(nsName) = meta
        return
      case _ =>
    // Symbol names in meta may be module-mangled (e.g. "std_strings__trim_space").
    // Strip the prefix for selector matching and local lookup keys, but keep
    // the mangled name in FunInfo.name so TCall/TFunDecl use it for codegen/linker.
    // Deduplicate by short name: if both a mangled and extern version exist, prefer non-extern.
    def dedup(syms: List[SymbolMeta]): List[SymbolMeta] =
      syms.groupBy(s => shortName(s.name)).values.map { group =>
        if group.size > 1 then group.find(!_.isExtern).getOrElse(group.head)
        else group.head
      }.toList
    val selectedSymbols = selectors match
      case List(WildcardImport) => dedup(meta.publicSymbols)
      case named =>
        val nameMap = named.collect { case NamedImport(n, r) => (n, r) }.toMap
        // Match selectors against short names (without module prefix)
        // When a struct or enum is imported by name, also pull in its methods (StructName_method)
        val directMatch = meta.publicSymbols.filter(sym => nameMap.contains(shortName(sym.name)))
        val importedTypeNames = directMatch.collect {
          case sym if sym.typ.isInstanceOf[SymbolMeta.Kind.Struct] => shortName(sym.name)
          case sym if sym.typ.isInstanceOf[SymbolMeta.Kind.Enum] => shortName(sym.name)
        }.toSet
        val withMethods = if importedTypeNames.isEmpty then directMatch
        else directMatch ++ meta.publicSymbols.filter { sym =>
          sym.typ.isInstanceOf[SymbolMeta.Kind.Func] &&
            importedTypeNames.exists(tn => shortName(sym.name).startsWith(s"${tn}_"))
        }
        dedup(withMethods)
    // Build alias map for renamed imports: alias -> original mangled name
    val aliasMap: Map[String, String] = selectors match
      case List(WildcardImport) => Map.empty
      case named =>
        named.collect { case NamedImport(n, Some(alias)) => (alias, n) }.toMap
    // Reverse: short-name -> alias
    val shortToAlias: Map[String, String] = selectors match
      case List(WildcardImport) => Map.empty
      case named =>
        named.collect { case NamedImport(n, Some(alias)) => (n, alias) }.toMap
    for sym <- selectedSymbols do
      val sn = shortName(sym.name)
      val localKey = shortToAlias.getOrElse(sn, sn) // use alias if provided
      sym.typ match
        case SymbolMeta.Kind.Func(params, returnType, isDef) =>
          val paramPairs = params.zipWithIndex.map((t, i) => (s"_p$i", t))
          if functions.contains(localKey) then
            // Allow same-module sibling re-registration (same mangled name) and externs
            val existing = functions(localKey)
            if !sym.isExtern && existing.name != sym.name then
              throw AnalysisError(s"imported symbol '$localKey' conflicts with existing function")
          else
            functions(localKey) = FunInfo(sym.name, paramPairs, returnType, isDef)
            externalSymbols += localKey
        case SymbolMeta.Kind.Data(dataType) =>
          if globalScope.contains(localKey) then
            val existing = globalScope(localKey)
            if !sym.isExtern && existing.name != sym.name then
              throw AnalysisError(s"imported symbol '$localKey' conflicts with existing global")
          else
            globalScope(localKey) = SymInfo(sym.name, dataType, mutable = false)
            externalSymbols += localKey
        case SymbolMeta.Kind.Struct(st) =>
          structTypes(shortName(sym.name)) = st
        case SymbolMeta.Kind.Interface(it) =>
          interfaceTypes(shortName(sym.name)) = it
        case SymbolMeta.Kind.Enum(et) =>
          val sn = shortName(sym.name)
          if et.variants.forall(_._2.isEmpty) then
            // Simple enum (no data variants) — register as both simpleEnumTypes and enumTypes
            simpleEnumTypes(sn) = et
            val members = et.variants.zipWithIndex.map { case ((vname, _), idx) => (vname, idx.toLong) }.toMap
            enumTypes(sn) = members
          else
            // Data enum — register in dataEnumTypes and variantToEnum
            linkImportedDataEnumToTemplate(et)
            dataEnumTypes(sn) = et
            // Mangled generic instances (e.g. ParseMaybe_i32) link via enumToTemplate when the suffix
            // parses as a monotype. Suffixes like _Tuple2 or func(...) do not parse — still do not
            // register Got/Miss on variantToEnum or the last imported instance wins and breaks seq/map.
            val isMangledGenericInstance =
              genericEnums.exists { case (base, decl) =>
                decl.typeParams.length == 1 && et.name.startsWith(base + "_") && et.name != base
              }
            if !enumToTemplate.contains(et.name) && !isMangledGenericInstance then
              for ((vname, _), idx) <- et.variants.zipWithIndex do
                variantToEnum(vname) = (et, idx)
        case SymbolMeta.Kind.Impl(traitName, targetType, methods) =>
          val key = (traitName, targetType)
          if !impls.contains(key) then
            impls(key) = mutable.LinkedHashMap.from(methods)

    // Register generic templates from imported module (needed for cross-module generic instantiation)
    if meta.genericTemplates.nonEmpty then
      registerGenericTemplatesFrom(ProgramAST(meta.genericTemplates))

    // Register generic enum instance mappings for cross-module type inference
    for inst <- meta.genericEnumInstances do
      if !enumToTemplate.contains(inst.mangledName) then
        enumToTemplate(inst.mangledName) = (inst.baseName, inst.typeArgs)

    // Register trait declarations from imported templates
    for template <- meta.genericTemplates do
      template match
        case TraitDeclAST(name, tparam, methods, _) =>
          if !traits.contains(name) then
            traits(name) = TraitInfo(name, tparam, methods)
            registerTraitOperatorEntries(name, methods, template)
        case _ =>

    // Register trait impl mappings from imported module
    for impl <- meta.traitImpls do
      val key = (impl.traitName, impl.targetType)
      if !impls.contains(key) then
        impls(key) = mutable.LinkedHashMap.from(impl.methods)

  def isExternal(name: String): Boolean = externalSymbols.contains(name)
  def externals: Set[String] = externalSymbols.toSet

  /** Inverse of `typeToMangled` for a single type (used in mangled generic enum names like `ParseMaybe_i32`). */
  private def parseMangledMonotype(s: String): Option[SyslType] =
    if s.isEmpty then None
    else if s.startsWith("slice") then parseMangledMonotype(s.drop(5)).map(SyslType.SliceType.apply)
    else if s.startsWith("ptr") then parseMangledMonotype(s.drop(3)).map(SyslType.PtrType.apply)
    else if s.startsWith("ref") then parseMangledMonotype(s.drop(3)).map(SyslType.RefType.apply)
    else
      s match
        case "i8" => Some(SyslType.I8)
        case "i16" => Some(SyslType.I16)
        case "i32" => Some(SyslType.I32)
        case "i64" => Some(SyslType.I64)
        case "u8" => Some(SyslType.U8)
        case "u16" => Some(SyslType.U16)
        case "u32" => Some(SyslType.U32)
        case "u64" => Some(SyslType.U64)
        case "bool" => Some(SyslType.BoolType)
        case "string" => Some(SyslType.StringType)
        case "void" => Some(SyslType.VoidType)
        case "f64" => Some(SyslType.DoubleType)
        case _ => None

  /** Link mangled imported enum names to generic templates for `unifyTypes` only. Do not call `instantiateGenericEnum` here — it would overwrite `variantToEnum` for shared variant names like `Got`/`Miss`. */
  private def linkImportedDataEnumToTemplate(et: SyslType.EnumType): Unit =
    if genericEnums.isEmpty then return
    for (baseName, decl) <- genericEnums if decl.typeParams.length == 1 do
      val prefix = baseName + "_"
      if et.name.startsWith(prefix) then
        val suffix = et.name.drop(prefix.length)
        parseMangledMonotype(suffix).foreach { t =>
          enumToTemplate(et.name) = (baseName, List(t))
        }

  /** Generic templates are omitted from `ModuleMeta` / typed `TProgram`; same-package siblings need the raw AST templates to resolve calls like `alt(...)`. */
  def registerGenericTemplatesFrom(program: ProgramAST): Unit =
    for decl <- program.decls do
      decl match
        case fd @ FunDeclAST(name, _, _, _, _, tps, _, _, _) if tps.nonEmpty =>
          if !genericTemplates.contains(name) && !functions.contains(name) then
            genericTemplates(name) = fd
        case sd @ StructDeclAST(name, _, tps, _) if tps.nonEmpty =>
          if !genericStructs.contains(name) then
            genericStructs(name) = sd
        case de @ DataEnumDeclAST(name, variants, tps, _) if tps.nonEmpty =>
          if !genericEnums.contains(name) then
            genericEnums(name) = de
            for (EnumVariantAST(vname, _), idx) <- variants.zipWithIndex do
              genericVariantToEnum.get(vname) match
                case Some((n, i)) =>
                  if n != name || i != idx then
                    throw AnalysisError(s"duplicate variant name: '$vname'", de)
                case None =>
                  // Import may have registered Got/Miss on variantToEnum when mangled linking failed;
                  // template wins so analyze(generic enum) does not see variantToEnum + empty genericVariantToEnum.
                  if variantToEnum.contains(vname) then variantToEnum.remove(vname)
                  genericVariantToEnum(vname) = (name, idx)
        case _ => ()

  def analyze(program: ProgramAST): TProgram =
    // Pass 0: forward-declare all type names so recursive references resolve.
    // Struct and enum names are registered as placeholder types; fields are
    // resolved in the next pass once all names are visible.
    val pass0Structs = mutable.HashSet[String]()
    val pass0Enums = mutable.HashSet[String]()
    for decl <- program.decls do
      decl match
        case StructDeclAST(name, _, typeParams, _) if typeParams.isEmpty =>
          if pass0Structs.contains(name) || structTypes.contains(name) then
            throw AnalysisError(s"duplicate struct: '$name'", decl)
          pass0Structs += name
          structTypes(name) = SyslType.StructType(name, Nil) // placeholder — fields filled below
        case DataEnumDeclAST(name, _, typeParams, _) if typeParams.isEmpty =>
          if pass0Enums.contains(name) || dataEnumTypes.contains(name) then
            throw AnalysisError(s"duplicate enum: '$name'", decl)
          pass0Enums += name
          dataEnumTypes(name) = SyslType.EnumType(name, Nil) // placeholder — variants filled below
        case _ => ()

    // Extract module path for name mangling
    for decl <- program.decls do
      decl match
        case ModuleDeclAST(path) =>
          currentModule = Some(path.mkString("_"))
        case _ =>

    // First pass: register all functions and globals
    for decl <- program.decls do
      decl match
        case _: ModuleDeclAST => // metadata only
        case _: ImportDeclAST => // handled later
        case ExternFuncDeclAST(name, params, returnType, _) =>
          if !functions.contains(name) && !builtinFunctions.contains(name) then
            val paramTypes = params.map(p => (p.name, resolveType(p.typ)))
            val retType = returnType.map(resolveType).getOrElse(VoidType)
            functions(name) = FunInfo(name, paramTypes, retType)
            externalSymbols += name
          // else: already registered from same-module sibling or import — skip
        case ExternVarDeclAST(name, typ, _) =>
          if !globalScope.contains(name) then
            val resolved = resolveType(typ)
            globalScope(name) = SymInfo(name, resolved, mutable = false)
            externalSymbols += name
          // else: already registered from same-module sibling or import — skip
        case sd @ StructDeclAST(name, fields, typeParams, _) =>
          if typeParams.nonEmpty then
            // Generic struct: store as template, don't resolve fields yet
            if genericStructs.contains(name) then
              throw AnalysisError(s"duplicate struct: '$name'", decl)
            genericStructs(name) = sd
          else
            if genericStructs.contains(name) then throw AnalysisError(s"duplicate struct: '$name'", decl)
            val resolvedFields = fields.map((n, t) => (n, resolveType(t)))
            // Update the placeholder with resolved fields
            structTypes(name) = SyslType.StructType(name, resolvedFields)
        case fd @ FunDeclAST(name, params, returnType, _, _, typeParams, _, _, isDef) =>
          // Duplicate-parameter-name check.
          val seenParams = mutable.HashSet[String]()
          for p <- params do
            if !seenParams.add(p.name) then
              val friendly = if p.name == "__self__"
                then "method '$name' already has an implicit 'self' parameter — remove the explicit 'self: *Type' declaration"
                else s"duplicate parameter name '${p.name}' in function '$name'"
              throw AnalysisError(friendly, decl)
          // Validate that any parameters with defaults come at the end (contiguous trailing).
          val firstDefaultIdx = params.indexWhere(_.default.isDefined)
          if firstDefaultIdx >= 0 then
            for i <- (firstDefaultIdx + 1) until params.length do
              if params(i).default.isEmpty then
                throw AnalysisError(
                  s"parameter '${params(i).name}' of '$name' must have a default value (all parameters after a defaulted parameter must also have defaults)",
                  decl,
                )
          if typeParams.nonEmpty then
            // Generic function: store as template, don't resolve types yet
            if genericTemplates.contains(name) || functions.contains(name) then
              throw AnalysisError(s"duplicate function: '$name'", decl)
            if params.exists(_.default.isDefined) then
              throw AnalysisError(s"generic function '$name' cannot have default parameter values (not yet supported)", decl)
            genericTemplates(name) = fd
          else
            val paramTypes = params.map(p => (p.name, resolveType(p.typ)))
            val retType = returnType.map(resolveType).getOrElse(VoidType)
            if functions.contains(name) || genericTemplates.contains(name) then
              throw AnalysisError(s"duplicate function: '$name'", decl)
            val mangledName = if shouldMangle(name) then mangleName(name) else name
            functions(name) = FunInfo(mangledName, paramTypes, retType, isDef && params.isEmpty)
            // Record #deprecated info
            for attr <- fd.attributes if attr.name == "deprecated" do
              val reason = attr.args.collectFirst { case AttrPositional(AttrLitString(s)) => s }
              deprecations(name) = reason
            // Register as method if name matches StructName_methodName pattern
            val underscoreIdx = name.indexOf('_')
            if underscoreIdx > 0 && params.nonEmpty && params.head.name == "__self__" then
              val structName = name.substring(0, underscoreIdx)
              val methodName = name.substring(underscoreIdx + 1)
              if structTypes.contains(structName) then
                methods.getOrElseUpdate(structName, mutable.Set.empty) += methodName
        case EnumDeclAST(name, members, _) =>
          if enumTypes.contains(name) then throw AnalysisError(s"duplicate enum: '$name'", decl)
          var nextValue = 0L
          val resolved = members.map { (memberName, explicitValue) =>
            val value = explicitValue.getOrElse(nextValue)
            nextValue = value + 1
            (memberName, value)
          }
          enumTypes(name) = resolved.toMap
          // Also register as an EnumType so it can appear in type positions
          // (e.g. `Result[int, TestError]`). Variants carry no payload.
          val variants = resolved.map((vname, _) => (vname, Nil: List[(String, SyslType)]))
          val et: SyslType.EnumType = SyslType.EnumType(name, variants)
          simpleEnumTypes(name) = et
          // Register bare variant names as constructors, so `NotFound` produces
          // a TEnumConstruct value usable where `TestError` is expected.
          // `TestError.NotFound` (qualified access) still yields the integer
          // constant via the enumTypes path for backward compatibility.
          for ((vname, _), idx) <- variants.zipWithIndex do
            if variantToEnum.contains(vname) || genericVariantToEnum.contains(vname) then
              throw AnalysisError(s"duplicate variant name: '$vname'")
            variantToEnum(vname) = (et, idx)
        case de @ DataEnumDeclAST(name, variants, typeParams, _) =>
          if typeParams.nonEmpty then
            // Generic enum: store template, don't resolve fields
            if genericEnums.contains(name) || dataEnumTypes.contains(name) || enumTypes.contains(name) then
              throw AnalysisError(s"duplicate enum: '$name'", decl)
            genericEnums(name) = de
            // Register bare variant names for inference at construction sites
            for (EnumVariantAST(vname, _), idx) <- variants.zipWithIndex do
              genericVariantToEnum.get(vname) match
                case Some((n, i)) if n == name && i == idx => () // already from registerGenericTemplatesFrom(sibling)
                case Some((n, i)) =>
                  throw AnalysisError(s"duplicate variant name: '$vname' ($n#$i vs '$name'#$idx)", decl)
                case None =>
                  if variantToEnum.contains(vname) then variantToEnum.remove(vname)
                  genericVariantToEnum(vname) = (name, idx)
          else
            if enumTypes.contains(name) || genericEnums.contains(name) then
              throw AnalysisError(s"duplicate enum: '$name'", decl)
            val resolvedVariants = variants.map { case EnumVariantAST(vname, fields) =>
              val resolvedFields = fields.map((fname, ftype) => (fname, resolveType(ftype)))
              (vname, resolvedFields)
            }
            val et: SyslType.EnumType = SyslType.EnumType(name, resolvedVariants)
            // Update the placeholder with resolved variants
            dataEnumTypes(name) = et
            for ((vname, _), idx) <- resolvedVariants.zipWithIndex do
              variantToEnum(vname) = (et, idx)
        case TypeAliasDeclAST(name, target, tparams, _) =>
          if typeAliases.contains(name) || genericTypeAliases.contains(name) then
            throw AnalysisError(s"duplicate type alias: '$name'", decl)
          if tparams.nonEmpty then
            genericTypeAliases(name) = (tparams, target)
          else
            typeAliases(name) = target
        case TraitDeclAST(name, tparam, methods, _) =>
          if traits.contains(name) then throw AnalysisError(s"duplicate trait: '$name'", decl)
          // Check no duplicate method names within the trait
          val methodNames = methods.map(_.name)
          if methodNames.distinct.length != methodNames.length then
            throw AnalysisError(s"duplicate method names in trait '$name'")
          traits(name) = TraitInfo(name, tparam, methods)
          registerTraitOperatorEntries(name, methods, decl)
        case InterfaceDeclAST(name, methodASTs, embeddedNames, _) =>
          if interfaceTypes.contains(name) then throw AnalysisError(s"duplicate interface: '$name'", decl)
          // Resolve embedded interfaces and flatten methods
          val embeddedMethods = embeddedNames.flatMap { en =>
            interfaceTypes.getOrElse(en, throw AnalysisError(s"embedded interface '$en' not found", decl)).methods
          }
          val ownMethods = methodASTs.map { m =>
            val paramTypes = m.params.map(p => resolveType(p.typ))
            val retType = resolveType(m.returnType)
            (m.name, paramTypes, retType)
          }
          val allMethods = embeddedMethods ++ ownMethods
          // Check for duplicate method names
          val names = allMethods.map(_._1)
          if names.distinct.length != names.length then
            throw AnalysisError(s"duplicate method names in interface '$name'")
          interfaceTypes(name) = SyslType.InterfaceType(name, allMethods)
        case ImplDeclAST(_, _, _, _) =>
          // Deferred to registerImpls after all traits are known
          ()
        case VarDeclAST(name, _, _, _, _, _) =>
          if globalScope.contains(name) then
            throw AnalysisError(s"duplicate global: '$name'", decl)

    // Intermediate pass: register impl blocks (traits now known; signatures may reference traits)
    for decl <- program.decls do
      decl match
        case ImplDeclAST(traitName, targetType, methods, _) =>
          val trait_ = traits.getOrElse(traitName,
            throw AnalysisError(s"impl references unknown trait '$traitName'", decl))
          val resolvedTarget = resolveType(targetType)
          if impls.contains((traitName, resolvedTarget)) then
            throw AnalysisError(s"duplicate impl: trait '$traitName' already implemented for ${resolvedTarget}")
          // Check required methods are all provided
          val providedNames = methods.map(_.name).toSet
          val missing = trait_.methods.filter(m => m.body.isEmpty && !providedNames.contains(m.name))
          if missing.nonEmpty then
            throw AnalysisError(s"impl ${traitName}[$resolvedTarget] missing required method(s): ${missing.map(_.name).mkString(", ")}")
          // Check each impl method exists in trait
          for m <- methods do
            if !trait_.methods.exists(_.name == m.name) then
              throw AnalysisError(s"impl method '${m.name}' is not declared in trait '$traitName'")
          // Register mangled functions for both provided methods and synthesized defaults
          val methodMap = mutable.LinkedHashMap.empty[String, String]
          val infos = mutable.ListBuffer.empty[ImplMethodInfo]
          val typeMangled = typeToMangled(resolvedTarget)
          val savedEnv = typeEnv
          typeEnv = Map(trait_.typeParam -> resolvedTarget)
          try
            for traitMethod <- trait_.methods do
              val rawMangled = s"${traitName}_${traitMethod.name}_${typeMangled}"
              val mangled = if shouldMangle(rawMangled) then mangleName(rawMangled) else rawMangled
              if functions.contains(mangled) then
                throw AnalysisError(s"impl method collides with existing function '$mangled'")
              val expectedParams = traitMethod.params.map(p => (p.name, resolveType(p.typ)))
              val expectedRet = resolveType(traitMethod.returnType)
              val providedOpt = methods.find(_.name == traitMethod.name)
              val (paramTypes, retType, body, synthesized) = providedOpt match
                case Some(implMethod) =>
                  val pTypes = implMethod.params.map(p => (p.name, resolveType(p.typ)))
                  val r = implMethod.returnType.map(resolveType).getOrElse(VoidType)
                  // Verify signature matches trait
                  if pTypes.map(_._2) != expectedParams.map(_._2) then
                    throw AnalysisError(s"impl method '${implMethod.name}' parameter types don't match trait: expected ${expectedParams.map(_._2).mkString("(", ", ", ")")}, got ${pTypes.map(_._2).mkString("(", ", ", ")")}")
                  if r != expectedRet then
                    throw AnalysisError(s"impl method '${implMethod.name}' return type doesn't match trait: expected $expectedRet, got $r")
                  (pTypes, r, implMethod.body, false)
                case None =>
                  // Synthesized default — body comes from the trait (we checked it's Some above)
                  (expectedParams, expectedRet, traitMethod.body.get, true)
              functions(mangled) = FunInfo(mangled, paramTypes, retType)
              methodMap(traitMethod.name) = mangled
              infos += ImplMethodInfo(mangled, paramTypes, retType, body, isSynthesized = synthesized)
          finally typeEnv = savedEnv
          impls((traitName, resolvedTarget)) = methodMap
          implMethodInfos((traitName, resolvedTarget)) = infos.toList
        case _ =>

    // Second pass: produce typed AST (skip generic templates; they're instantiated on demand)
    val tDecls = program.decls.flatMap {
      case f: FunDeclAST if f.typeParams.nonEmpty => Nil
      case s: StructDeclAST if s.typeParams.nonEmpty => Nil
      case e: DataEnumDeclAST if e.typeParams.nonEmpty => Nil
      case _: TraitDeclAST => Nil // traits emit nothing; only impls do
      case i: InterfaceDeclAST => List(TInterfaceDecl(i.name, interfaceTypes(i.name)))
      case impl: ImplDeclAST   => analyzeImplMethods(impl)
      case d => List(analyzeDecl(d))
    }
    TProgram(tDecls ++ specializedDecls.toList)

  private def analyzeDecl(decl: DeclAST): TDecl =
    decl match
      case ModuleDeclAST(path) =>
        TModuleDecl(path)

      case ImportDeclAST(modulePath, _) =>
        TImportDecl(modulePath)

      case ExternFuncDeclAST(name, params, returnType, _) =>
        val paramTypes = params.map(p => resolveType(p.typ))
        val retType = returnType.map(resolveType).getOrElse(VoidType)
        TExternFuncDecl(name, paramTypes, retType)

      case ExternVarDeclAST(name, typ, _) =>
        TExternVarDecl(name, resolveType(typ))

      case StructDeclAST(name, _, _, _) =>
        val st = structTypes(name)
        TStructDecl(name, st.fields)

      case EnumDeclAST(name, _, _) =>
        val members = enumTypes(name).toList.sortBy(_._2)
        TEnumDecl(name, members)

      case DataEnumDeclAST(name, _, _, _) =>
        TDataEnumDecl(name, dataEnumTypes(name))

      case TypeAliasDeclAST(name, target, tparams, _) =>
        if tparams.nonEmpty then TTypeAliasDecl(name, VoidType) // generic alias: type-only, no codegen
        else TTypeAliasDecl(name, resolveType(target))

      case fdAst @ FunDeclAST(name, params, _, body, isPrivate, _, _, attrs, _) =>
        scopeStack = new mutable.ArrayBuffer
        pushScope()
        val funInfo = functions(name)
        // Analyze default parameter expressions now (globals and earlier-declared
        // symbols are available; locals are not visible to defaults).
        if params.exists(_.default.isDefined) then
          val defaults = params.zip(funInfo.params).map { case (p, (_, pType)) =>
            p.default.map { defaultExpr =>
              val savedExp0 = currentExpected
              currentExpected = Some(pType)
              val tDefault0 = try analyzeExpr(defaultExpr) finally currentExpected = savedExp0
              val tDefault = coerceLiteral(tDefault0, pType)
              if !compatible(tDefault.typ, pType) then
                throw AnalysisError(
                  s"default value for parameter '${p.name}' of '$name' has type ${tDefault.typ}, expected $pType",
                  fdAst,
                )
              tDefault
            }
          }
          functionDefaults(name) = defaults
          if funInfo.name != name then functionDefaults(funInfo.name) = defaults
        val savedReturnType = currentReturnType
        currentReturnType = funInfo.returnType
        for (paramName, paramType) <- funInfo.params do
          currentScope(paramName) = SymInfo(paramName, paramType, true)
          // Auto-alias the implicit method receiver: `self` -> `__self__`
          // so method bodies can write `self.x` while the actual parameter
          // is named `__self__` to avoid conflicting with user-declared names.
          if paramName == "__self__" then
            currentScope("self") = SymInfo(paramName, paramType, true)
        val savedExp = currentExpected
        currentExpected = if funInfo.returnType == VoidType then None else Some(funInfo.returnType)
        val tBody = try body match
          case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
          case BlockBodyAST(stmts) => TBlockBody(analyzeBlock(stmts))
        finally currentExpected = savedExp
        // For def functions with no explicit return type, infer from body
        val retType = if funInfo.isDef && funInfo.returnType == VoidType then
          val inferred = tBody match
            case TExprBody(expr) => expr.typ
            case _ => VoidType
          // Update FunInfo so other references see the correct type
          functions(name) = funInfo.copy(returnType = inferred)
          inferred
        else funInfo.returnType
        val tParams = funInfo.params.map((n, t) => TParam(n, t))
        currentReturnType = savedReturnType
        scopeStack = null
        validateTestAttr(fdAst, funInfo)
        TFunDecl(funInfo.name, tParams, retType, tBody, isPrivate, attrs, funInfo.isDef)

      case VarDeclAST(name, typOpt, init, isPrivate, isMutable, _) =>
        scopeStack = new mutable.ArrayBuffer
        pushScope()
        val tInit0 = analyzeExpr(init)
        val declType = typOpt.map(resolveType).getOrElse(tInit0.typ)
        val tInit1 = coerceLiteral(tInit0, declType)
        // Constant folding: immutable vals with constant initializers become compile-time constants
        val tInit = if !isMutable then
          tryConstEval(tInit1) match
            case Some(n) =>
              val masked = maskToType(n, declType)
              val mangledName = if shouldMangle(name) then mangleName(name) else name
              compileTimeConstants(name) = masked
              compileTimeConstants(mangledName) = masked
              TIntLit(masked, declType)
            case None => tInit1
        else tInit1
        val mangledVarName = if shouldMangle(name) then mangleName(name) else name
        globalScope(name) = SymInfo(mangledVarName, declType, isMutable)
        scopeStack = null
        TVarDecl(mangledVarName, declType, tInit, isPrivate)

  private def warnDeprecated(name: String): Unit =
    if deprecations.contains(name) && !warnedDeprecations.contains(name) then
      warnedDeprecations += name
      val suffix = deprecations(name).map(r => s": $r").getOrElse("")
      System.err.println(s"warning: '$name' is deprecated$suffix")

  private def validateTestAttr(fd: FunDeclAST, info: FunInfo): Unit =
    fd.attributes.find(_.name == "test") match
      case None => ()
      case Some(attr) =>
        if fd.params.nonEmpty then
          throw AnalysisError(s"#test function '${fd.name}' must take zero parameters", fd)
        if info.returnType != VoidType then
          throw AnalysisError(s"#test function '${fd.name}' must return unit", fd)
        if fd.typeParams.nonEmpty then
          throw AnalysisError(s"#test function '${fd.name}' cannot be generic", fd)
        // Methods are registered via the StructName_methodName convention; reject those
        val underscoreIdx = fd.name.indexOf('_')
        if underscoreIdx > 0 && fd.params.nonEmpty && fd.params.head.name == "__self__" then
          throw AnalysisError(s"#test cannot be applied to a method ('${fd.name}')", fd)

  private def resolveType(t: TypeAST): SyslType = t match
    case NamedTypeAST(name, typeArgs) if typeArgs.nonEmpty =>
      val resolved = typeArgs.map(resolveType)
      if genericTypeAliases.contains(name) then
        val (tparams, target) = genericTypeAliases(name)
        if resolved.length != tparams.length then
          throw AnalysisError(s"type alias '$name' expects ${tparams.length} type argument(s), got ${resolved.length}")
        val savedEnv = typeEnv
        typeEnv = typeEnv ++ tparams.zip(resolved).toMap
        val result = resolveType(target)
        typeEnv = savedEnv
        result
      else if genericStructs.contains(name) then instantiateGenericStruct(name, resolved)
      else if genericEnums.contains(name) then instantiateGenericEnum(name, resolved)
      else throw AnalysisError(s"'$name' is not a generic type")
    case NamedTypeAST(name, _) if typeEnv.contains(name) => typeEnv(name)
    case NamedTypeAST(name, _) => name match
      case "int" | "i32" => I32
      case "char" => U32
      case "i64" => I64
      case "double" | "f64" => DoubleType
      case "byte" | "u8"  => U8
      case "i8"  => I8
      case "i16"  => I16
      case "u16"  => U16
      case "u32"  => U32
      case "u64"  => U64
      case "bool" => BoolType
      case "void" => VoidType
      case "string" => StringType
      case name if typeAliases.contains(name) => resolveType(typeAliases(name))
      case name if structTypes.contains(name) => structTypes(name)
      case name if dataEnumTypes.contains(name) => dataEnumTypes(name)
      case name if simpleEnumTypes.contains(name) => simpleEnumTypes(name)
      case name if interfaceTypes.contains(name) => interfaceTypes(name)
      case other => throw AnalysisError(s"unknown type: '$other'")
    case PtrTypeAST(inner) => PtrType(resolveType(inner))
    case ArrayTypeAST(size, elem) => ArrayType(resolveType(elem), size)
    case SliceTypeAST(elem) => SliceType(resolveType(elem))
    case TupleTypeAST(elems) => SyslType.tupleType(elems.map(resolveType))
    case FuncTypeAST(params, ret) => FuncType(params.map(resolveType), resolveType(ret))
    case RefTypeAST(inner) => RefType(resolveType(inner))

  /** `PtrType` / `RefType` may embed a recursive generic `StructType` placeholder (empty `fields`); use `structTypes`. */
  private def latestStruct(st: SyslType.StructType): SyslType.StructType =
    structTypes.getOrElse(st.name, st)

  /** Convert an expression AST to a type AST (for explicit type args parsed as index expressions). */
  private def exprToTypeAST(expr: ExpressionAST): TypeAST = expr match
    case VarRefAST(name) => NamedTypeAST(name)
    case _ => throw AnalysisError(s"expected type argument, got expression")

  /** Look up a method function by struct name and method name, trying both unmangled and mangled forms. */
  private def lookupMethod(structName: String, methodName: String): Option[FunInfo] =
    val shortName = s"${structName}_$methodName"
    functions.get(shortName).orElse {
      // Try with module prefix (mangled name)
      currentModule match
        case Some(mod) => functions.get(s"${mod}__$shortName")
        case None => None
    }.orElse {
      // Search all functions for a match (imported methods may have arbitrary module prefix)
      functions.values.find(f => SyslAnalyzer.this.shortName(f.name) == shortName)
    }

  private def satisfiesInterface(st: SyslType.StructType, iface: SyslType.InterfaceType): Boolean =
    val structName = st.name
    iface.methods.forall { (methodName, paramTypes, retType) =>
      lookupMethod(structName, methodName) match
        case Some(funInfo) =>
          val userParams = funInfo.params.drop(1).map(_._2)
          userParams == paramTypes && funInfo.returnType == retType
        case None => false
    }

  private def compatible(from: SyslType, to: SyslType): Boolean =
    (from, to) match
      case (a, b) if a == b => true
      // Name-based equality for nominal types — handles stale placeholders from
      // forward declarations where two StructType/EnumType with the same name
      // have different field/variant lists.
      case (StructType(n1, _), StructType(n2, _)) if n1 == n2 => true
      case (EnumType(n1, _), EnumType(n2, _)) if n1 == n2 => true
      case (IntType(a), IntType(b)) if a <= b => true    // signed widening
      case (UIntType(a), UIntType(b)) if a <= b => true  // unsigned widening
      case (IntType(a), UIntType(b)) if a <= b => true   // signed → unsigned widening
      case (UIntType(a), IntType(b)) if a <= b => true   // unsigned → signed widening
      case (DoubleType, DoubleType) => true
      case (_: IntType, DoubleType) => true    // signed int → float promotion
      case (_: UIntType, DoubleType) => true   // unsigned int → float promotion
      case (DoubleType, _: IntType) => true    // float → signed int (truncation)
      case (DoubleType, _: UIntType) => true   // float → unsigned int (truncation)
      // bool and int are NOT compatible — use explicit casts
      // int ↔ pointer: NOT compatible — use explicit casts: int(ptr), *i8(addr)
      case (_: FuncType, IntType(64) | UIntType(64)) => true // function pointer → i64 (entry point address)
      case (PtrType(_), PtrType(_)) => true           // any pointer ↔ any pointer (like C's void*)
      case (ArrayType(_, _), PtrType(_)) => true          // array decays to any pointer
      case (StringType, PtrType(I8 | U8)) => true          // string decays to *i8 / *byte
      case (ArrayType(e1, _), ArrayType(e2, _)) if e1 == e2 => true
      case (ArrayType(e1, _), SliceType(e2)) if e1 == e2 => true  // fixed array → slice
      case (SliceType(e1), SliceType(e2)) if e1 == e2 => true
      case (RefType(a), RefType(b)) if compatible(a, b) => true // same ref type (recursive check handles nominal types)
      case (RefType(inner), PtrType(_)) => true             // &T → *U (ref decays to pointer)
      // Interface satisfaction: struct/ptr/ref → interface (if methods match)
      case (st: StructType, iface: InterfaceType) => satisfiesInterface(st, iface)
      case (PtrType(st: StructType), iface: InterfaceType) => satisfiesInterface(st, iface)
      case (RefType(st: StructType), iface: InterfaceType) => satisfiesInterface(st, iface)
      // Interface-to-interface widening (superset of methods)
      case (InterfaceType(_, methodsA), InterfaceType(_, methodsB)) =>
        methodsB.forall(mb => methodsA.exists(_ == mb))
      case _ => false

  // Coerce integer literals to the target type (like Rust's untyped integer literals)
  private def coerceLiteral(expr: TExpr, target: SyslType): TExpr =
    expr match
      case TIntLit(value, _) if target.isIntegral => TIntLit(value, target)
      case TIntLit(0, _) if target.isInstanceOf[PtrType] => TIntLit(0, target) // null pointer
      // String literal → byte array: "hello" initializing [n]byte
      case TStringLit(s, _) if target.isInstanceOf[ArrayType] =>
        val ArrayType(elemType, size) = target: @unchecked
        if elemType != U8 && elemType != I8 then
          throw AnalysisError(s"cannot initialize [$size]$elemType from string literal (element type must be byte or i8)")
        val bytes = s.getBytes("UTF-8")
        if bytes.length > size then
          throw AnalysisError(s"string literal has ${bytes.length} bytes but array has only $size elements")
        val elems = bytes.map(b => TIntLit((b & 0xff).toLong, elemType)).toList ++
          List.fill(size - bytes.length)(TIntLit(0L, elemType))
        TArrayLit(elems, target)
      // Array literal → byte array: ['h', 'e', 'l'] initializing [n]byte
      case TArrayLit(elems, _) if target.isInstanceOf[ArrayType] =>
        val ArrayType(elemType, size) = target: @unchecked
        if (elemType == U8 || elemType == I8) && elems.forall(_.isInstanceOf[TIntLit]) then
          if elems.length > size then
            throw AnalysisError(s"array literal has ${elems.length} elements but target has only $size")
          val coerced = elems.map { case TIntLit(v, _) =>
            if v < 0 || v > 255 then
              throw AnalysisError(s"value $v does not fit in a byte")
            TIntLit(v, elemType)
          case e => e }
          val padded = coerced ++ List.fill(size - elems.length)(TIntLit(0L, elemType))
          TArrayLit(padded, target)
        else expr
      case _ => expr

  // Coerce integer literals to match the target's signedness only (preserving original width)
  private def coerceSignedness(expr: TExpr, target: SyslType): TExpr =
    (expr, target) match
      case (TIntLit(value, IntType(w)), _: UIntType) => TIntLit(value, UIntType(w))
      case (TIntLit(value, UIntType(w)), _: IntType) => TIntLit(value, IntType(w))
      case _ => expr

  /** Truncate a value to fit the given integer type's width, with sign-extension for signed types. */
  private def maskToType(value: Long, typ: SyslType): Long = typ match
    case IntType(8) => (value << 56) >> 56 // sign-extend from 8 bits
    case IntType(16) => (value << 48) >> 48
    case IntType(32) => (value << 32) >> 32
    case UIntType(8) => value & 0xFFL
    case UIntType(16) => value & 0xFFFFL
    case UIntType(32) => value & 0xFFFFFFFFL
    case _ => value // i64/u64/bool — no truncation needed

  /** Try to evaluate a typed expression as a compile-time integer constant. */
  private def tryConstEval(expr: TExpr): Option[Long] = expr match
    case TIntLit(n, _) => Some(n)
    case TBoolLit(b, _) => Some(if b then 1 else 0)
    case TVarRef(name, _) => compileTimeConstants.get(name)
    case TUnary("-", operand, _) => tryConstEval(operand).map(-_)
    case TUnary("~", operand, _) => tryConstEval(operand).map(~_)
    case TBinary(left, "+", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield l + r
    case TBinary(left, "-", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield l - r
    case TBinary(left, "*", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield l * r
    case TBinary(left, "/", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) if r != 0 yield l / r
    case TBinary(left, "%", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) if r != 0 yield l % r
    case TBinary(left, "<<", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield l << r.toInt
    case TBinary(left, ">>", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield l >> r.toInt
    case TBinary(left, "&", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield l & r
    case TBinary(left, "|", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield l | r
    case TBinary(left, "^", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield l ^ r
    case TCast(inner, _) => tryConstEval(inner)
    case _ => None

  private def lookup(name: String): SymInfo =
    if scopeStack != null then
      var i = scopeStack.length - 1
      while i >= 0 do
        if scopeStack(i).contains(name) then return scopeStack(i)(name)
        i -= 1
    if globalScope.contains(name) then globalScope(name)
    else throw AnalysisError(s"undefined variable: '$name'")

  private def tryLookup(name: String): Option[SymInfo] =
    if scopeStack != null then
      var i = scopeStack.length - 1
      while i >= 0 do
        if scopeStack(i).contains(name) then return Some(scopeStack(i)(name))
        i -= 1
    if globalScope.contains(name) then Some(globalScope(name))
    else None

  private def lookupOrCreate(name: String, typ: SyslType): SymInfo =
    if scopeStack != null then
      var i = scopeStack.length - 1
      while i >= 0 do
        if scopeStack(i).contains(name) then return scopeStack(i)(name)
        i -= 1
    if globalScope.contains(name) then globalScope(name)
    else
      val info = SymInfo(name, typ, true)
      if scopeStack != null then currentScope(name) = info
      else globalScope(name) = info
      info

  private def lookupFun(name: String): FunInfo =
    functions.getOrElse(name,
      builtinFunctions.getOrElse(name,
        throw AnalysisError(s"undefined function: '$name'")))

  // ===== Generic function support =====

  // Mangle a type to a name-safe identifier for use in instantiated function names
  private def typeToMangled(t: SyslType): String = t match
    case IntType(w)      => s"i$w"
    case UIntType(w)     => s"u$w"
    case BoolType        => "bool"
    case DoubleType      => "f64"
    case StringType      => "string"
    case VoidType        => "void"
    case PtrType(i)      => "ptr" + typeToMangled(i)
    case RefType(i)      => "ref" + typeToMangled(i)
    case ArrayType(e, n) => s"arr${n}${typeToMangled(e)}"
    case SliceType(e)    => "slice" + typeToMangled(e)
    case FuncType(ps, r) => "fn" + ps.map(typeToMangled).mkString("") + "Ret" + typeToMangled(r)
    case StructType(n, _)    => n
    case EnumType(n, _)      => n
    case InterfaceType(n, _) => n

  private def mangleGenericName(base: String, typeArgs: List[SyslType]): String =
    base + "_" + typeArgs.map(typeToMangled).mkString("_")

  // Unify a parameter TypeAST (which may contain type variables) against a concrete SyslType,
  // recording type variable bindings. Returns true if unification succeeded structurally.
  private def unifyTypes(param: TypeAST, arg: SyslType, typeParams: Set[String], env: mutable.Map[String, SyslType]): Unit =
    param match
      case NamedTypeAST(name, _) if typeParams.contains(name) =>
        env.get(name) match
          case Some(existing) if existing == arg => ()
          case Some(existing) =>
            throw AnalysisError(s"cannot infer type parameter '$name': seen both $existing and $arg")
          case None => env(name) = arg
      case PtrTypeAST(inner) => arg match
        case PtrType(a) => unifyTypes(inner, a, typeParams, env)
        case _ => () // type mismatch handled later by checkArgs
      case RefTypeAST(inner) => arg match
        case RefType(a) => unifyTypes(inner, a, typeParams, env)
        case _ => ()
      case ArrayTypeAST(_, inner) => arg match
        case ArrayType(a, _) => unifyTypes(inner, a, typeParams, env)
        case _ => ()
      case SliceTypeAST(inner) => arg match
        case SliceType(a) => unifyTypes(inner, a, typeParams, env)
        case _ => ()
      case FuncTypeAST(paramTypes, ret) => arg match
        case FuncType(argParams, argRet) =>
          if paramTypes.length == argParams.length then
            for (pt, at) <- paramTypes.zip(argParams) do unifyTypes(pt, at, typeParams, env)
          unifyTypes(ret, argRet, typeParams, env)
        case _ => ()
      case TupleTypeAST(elems) => arg match
        case StructType(_, fields) if elems.length == fields.length =>
          for (e, (_, ft)) <- elems.zip(fields) do unifyTypes(e, ft, typeParams, env)
        case _ => ()
      case NamedTypeAST(name, tArgs) if tArgs.nonEmpty =>
        // If this is a generic type alias, expand it and unify the expanded type
        if genericTypeAliases.contains(name) then
          val (tparams, target) = genericTypeAliases(name)
          if tArgs.length == tparams.length then
            // Substitute alias type params with the call's type args in the target TypeAST,
            // then unify the expanded structure against the argument type.
            // e.g., type Parser[T] = (string, int) -> Result[T, string]
            //   Parser[A] → substitute T→A in target → (string, int) -> Result[A, string]
            val subst = tparams.zip(tArgs).toMap
            val expanded = substituteTypeAST(target, subst)
            unifyTypes(expanded, arg, typeParams, env)
        else arg match
          case SyslType.StructType(argName, _) =>
            structToTemplate.get(argName) match
              case Some((templateName, concreteArgs)) if templateName == name && concreteArgs.length == tArgs.length =>
                for (p, a) <- tArgs.zip(concreteArgs) do unifyTypes(p, a, typeParams, env)
              case _ => ()
          case SyslType.EnumType(argName, _) =>
            enumToTemplate.get(argName) match
              case Some((templateName, concreteArgs)) if templateName == name && concreteArgs.length == tArgs.length =>
                for (p, a) <- tArgs.zip(concreteArgs) do unifyTypes(p, a, typeParams, env)
              case _ => ()
          case _ => ()
      case _ => () // concrete parameter type, nothing to infer

  /** Substitute named types in a TypeAST. Used to expand generic type alias params before unification. */
  private def substituteTypeAST(t: TypeAST, subst: Map[String, TypeAST]): TypeAST = t match
    case NamedTypeAST(name, Nil) if subst.contains(name) => subst(name)
    case NamedTypeAST(name, args) => NamedTypeAST(name, args.map(substituteTypeAST(_, subst)))
    case PtrTypeAST(inner) => PtrTypeAST(substituteTypeAST(inner, subst))
    case ArrayTypeAST(size, elem) => ArrayTypeAST(size, substituteTypeAST(elem, subst))
    case SliceTypeAST(elem) => SliceTypeAST(substituteTypeAST(elem, subst))
    case FuncTypeAST(params, ret) => FuncTypeAST(params.map(substituteTypeAST(_, subst)), substituteTypeAST(ret, subst))
    case TupleTypeAST(elems) => TupleTypeAST(elems.map(substituteTypeAST(_, subst)))
    case RefTypeAST(inner) => RefTypeAST(substituteTypeAST(inner, subst))

  // Instantiate a generic function with inferred type arguments, returning the mangled name
  // and FunInfo of the instantiated function. Reuses cached instantiations.
  // If an operator has a user-defined struct/enum operand, desugar to the corresponding trait call.
  // Returns None if no desugaring applies (use built-in dispatch).
  private def tryOperatorDispatch(op: String, tLeft: TExpr, tRight: TExpr): Option[TExpr] =
    lookupBinaryOperatorTrait(op) match
      case None => None
      case Some((traitName, methodName)) =>
        val operandType = tLeft.typ
        operandType match
          case _: SyslType.StructType | _: SyslType.EnumType =>
            if !traits.contains(traitName) then
              throw AnalysisError(s"operator '$op' on $operandType requires trait '$traitName' but it is not defined")
            impls.get((traitName, operandType)) match
              case Some(methodMap) =>
                val mangled = methodMap(methodName)
                val funInfo = functions(mangled)
                val checkedArgs = checkArgs(mangled, funInfo.params, List(tLeft, tRight))
                Some(TCall(mangled, checkedArgs, funInfo.returnType))
              case None =>
                throw AnalysisError(s"no impl of '$traitName' for $operandType: operator '$op' not defined")
          case _ => None

  // Instantiate a generic struct with concrete type arguments, returning its StructType
  private def instantiateGenericStruct(name: String, typeArgs: List[SyslType]): SyslType.StructType =
    val cacheKey = (name, typeArgs)
    genericStructInstantiations.get(cacheKey) match
      case Some(st) => st
      case None =>
        val template = genericStructs.getOrElse(name,
          throw AnalysisError(s"'$name' is not a generic struct"))
        if template.typeParams.length != typeArgs.length then
          throw AnalysisError(s"generic struct '$name' expects ${template.typeParams.length} type arg(s), got ${typeArgs.length}")
        val mangled = name + "_" + typeArgs.map(typeToMangled).mkString("_")
        // Insert a placeholder StructType to handle recursive field types
        val placeholder: SyslType.StructType = SyslType.StructType(mangled, Nil)
        genericStructInstantiations(cacheKey) = placeholder
        structTypes(mangled) = placeholder
        structToTemplate(mangled) = (name, typeArgs)
        val savedEnv = typeEnv
        typeEnv = typeEnv ++ template.typeParams.zip(typeArgs).toMap
        try
          val resolvedFields = template.fields.map((n, t) => (n, resolveType(t)))
          val st: SyslType.StructType = SyslType.StructType(mangled, resolvedFields)
          genericStructInstantiations(cacheKey) = st
          structTypes(mangled) = st
          specializedDecls += TStructDecl(mangled, resolvedFields)
          st
        finally typeEnv = savedEnv

  // Instantiate a generic enum with concrete type arguments, returning its EnumType
  private def instantiateGenericEnum(name: String, typeArgs: List[SyslType]): SyslType.EnumType =
    val cacheKey = (name, typeArgs)
    genericEnumInstantiations.get(cacheKey) match
      case Some(et) => et
      case None =>
        val template = genericEnums(name)
        if template.typeParams.length != typeArgs.length then
          throw AnalysisError(s"generic enum '$name' expects ${template.typeParams.length} type arg(s), got ${typeArgs.length}")
        val mangled = name + "_" + typeArgs.map(typeToMangled).mkString("_")
        val savedEnv = typeEnv
        typeEnv = typeEnv ++ template.typeParams.zip(typeArgs).toMap
        try
          val resolvedVariants = template.variants.map { case EnumVariantAST(vname, fields) =>
            val resolvedFields = fields.map((fname, ftype) => (fname, resolveType(ftype)))
            (vname, resolvedFields)
          }
          val et: SyslType.EnumType = SyslType.EnumType(mangled, resolvedVariants)
          genericEnumInstantiations(cacheKey) = et
          dataEnumTypes(mangled) = et
          enumToTemplate(mangled) = (name, typeArgs)
          specializedDecls += TDataEnumDecl(mangled, et)
          et
        finally typeEnv = savedEnv

  // Analyze each impl method (including synthesized defaults) as a mangled top-level function
  private def analyzeImplMethods(impl: ImplDeclAST): List[TDecl] =
    val resolvedTarget = resolveType(impl.targetType)
    val methodMap = impls((impl.traitName, resolvedTarget))
    val infos = implMethodInfos((impl.traitName, resolvedTarget))
    val trait_ = traits(impl.traitName)
    val savedEnv = typeEnv
    val savedRewrite = traitCallRewrite
    try
      // For synthesized defaults, set typeEnv + traitCallRewrite so T resolves and
      // unqualified calls to sibling trait methods route to the impl's mangled functions.
      infos.map { info =>
        if info.isSynthesized then
          typeEnv = Map(trait_.typeParam -> resolvedTarget)
          traitCallRewrite = methodMap.toMap
        else
          typeEnv = savedEnv
          traitCallRewrite = savedRewrite
        scopeStack = new mutable.ArrayBuffer
        pushScope()
        for (paramName, paramType) <- info.paramTypes do
          currentScope(paramName) = SymInfo(paramName, paramType, true)
        val tBody = info.body match
          case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
          case BlockBodyAST(stmts) => TBlockBody(analyzeBlock(stmts))
        val tParams = info.paramTypes.map((n, t) => TParam(n, t))
        scopeStack = null
        TFunDecl(info.mangled, tParams, info.retType, tBody, isPrivate = false)
      }
    finally
      typeEnv = savedEnv
      traitCallRewrite = savedRewrite

  // Resolve a trait method call like Ord.cmp(a, b) to the appropriate impl's mangled function
  private def analyzeTraitCall(traitName: String, methodName: String, tArgs: List[TExpr]): (String, FunInfo) =
    val trait_ = traits(traitName)
    val method = trait_.methods.find(_.name == methodName).getOrElse(
      throw AnalysisError(s"trait '$traitName' has no method '$methodName'"))
    if method.params.length != tArgs.length then
      throw AnalysisError(s"trait method '$traitName.$methodName' expects ${method.params.length} argument(s), got ${tArgs.length}")
    // Infer the target type by unifying each param type against the arg type, using typeParam as the variable
    val env = mutable.Map.empty[String, SyslType]
    for (p, a) <- method.params.zip(tArgs) do
      unifyTypes(p.typ, a.typ, Set(trait_.typeParam), env)
    val targetType = env.get(trait_.typeParam).getOrElse(
      throw AnalysisError(s"cannot infer target type for trait method '$traitName.$methodName'"))
    val methodMap = impls.getOrElse((traitName, targetType),
      throw AnalysisError(s"no impl of trait '$traitName' for type $targetType"))
    val mangled = methodMap(methodName)
    // Look up by full mangled name first, then by short name (for cross-module imports)
    val funInfo = functions.getOrElse(mangled,
      functions.getOrElse(shortName(mangled),
        throw AnalysisError(s"trait method '$traitName.$methodName' resolved to '$mangled' but function not found")))
    (mangled, funInfo)

  private def instantiateGeneric(
      name: String,
      argTypes: List[SyslType],
      explicitTypeArgs: List[SyslType] = Nil,
  ): (String, FunInfo) =
    val template = genericTemplates(name)
    val typeParams = template.typeParams
    // Infer type arguments (explicit type args from `f[T](...)` pre-seed the env)
    val env = mutable.Map.empty[String, SyslType]
    if explicitTypeArgs.nonEmpty then
      if explicitTypeArgs.length != typeParams.length then
        throw AnalysisError(
          s"generic function '$name' expects ${typeParams.length} type argument(s), got ${explicitTypeArgs.length}",
        )
      for (tp, ty) <- typeParams.zip(explicitTypeArgs) do env(tp) = ty
    if template.params.length != argTypes.length then
      throw AnalysisError(s"generic function '$name' expects ${template.params.length} argument(s), got ${argTypes.length}")
    for (p, a) <- template.params.zip(argTypes) do
      unifyTypes(p.typ, a, typeParams.toSet, env)
    // Require all type params to be pinned
    for tp <- typeParams if !env.contains(tp) do
      throw AnalysisError(s"cannot infer type parameter '$tp' for generic function '$name'")
    val inferredArgs = typeParams.map(env(_))
    // Check trait bounds
    for tp <- typeParams do
      val bounds = template.typeBounds.getOrElse(tp, Nil)
      val concreteType = env(tp)
      for traitName <- bounds do
        if !traits.contains(traitName) then
          throw AnalysisError(s"bound '$traitName' on type parameter '$tp' of '$name' refers to unknown trait")
        if !impls.contains((traitName, concreteType)) then
          throw AnalysisError(s"type $concreteType does not satisfy bound '$traitName' for type parameter '$tp' in call to '$name'")
    val cacheKey = (name, inferredArgs)
    instantiations.get(cacheKey) match
      case Some(mangled) => (mangled, functions(mangled))
      case None =>
        val mangled = mangleGenericName(name, inferredArgs)
        if functions.contains(mangled) && !externalSymbols.contains(mangled) && !externalSymbols.contains(shortName(mangled)) then
          // Already instantiated locally — reuse it
          instantiations(cacheKey) = mangled
          return (mangled, functions(mangled))
        // If the function exists as an imported symbol, we still need to re-instantiate
        // locally so the backend emits its body in this compilation unit.
        // Save and install typeEnv for this instantiation
        val savedEnv = typeEnv
        typeEnv = typeParams.zip(inferredArgs).toMap
        try
          // Resolve param/return types in the new env
          val paramTypes = template.params.map(p => (p.name, resolveType(p.typ)))
          val retType = template.returnType.map(resolveType).getOrElse(VoidType)
          val funInfo = FunInfo(mangled, paramTypes, retType)
          // Register before analyzing body to support recursion
          functions(mangled) = funInfo
          instantiations(cacheKey) = mangled
          // Analyze body in a fresh scope, using the mangled name
          val savedScopeStack = scopeStack
          val savedLoopDepth = loopDepth
          scopeStack = new mutable.ArrayBuffer
          loopDepth = 0
          pushScope()
          for (paramName, paramType) <- paramTypes do
            currentScope(paramName) = SymInfo(paramName, paramType, true)
            // Auto-alias `self` -> `__self__` for generic methods
            if paramName == "__self__" then
              currentScope("self") = SymInfo(paramName, paramType, true)
          val savedExpectedInst = currentExpected
          // Use only the *result* R of `func(...) -> R`, not the full function type, so nested
          // closures still treat outer parameters (e.g. alt's `a`, `b`) as captures rather than
          // mis-reading `func(string,int)->…` as the expected shape of a single-arg closure.
          currentExpected = template.returnType.map(resolveType).map {
            case ft: FuncType => ft.returnType
            case t => t
          }
          val tBody = try
            template.body match
              case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
              case BlockBodyAST(stmts) => TBlockBody(analyzeBlock(stmts))
          finally
            currentExpected = savedExpectedInst
          scopeStack = savedScopeStack
          loopDepth = savedLoopDepth
          val tParams = paramTypes.map((n, t) => TParam(n, t))
          specializedDecls += TFunDecl(mangled, tParams, retType, tBody, template.isPrivate)
          (mangled, funInfo)
        finally
          typeEnv = savedEnv

  private def checkArgs(name: String, params: List[(String, SyslType)], args: List[TExpr]): List[TExpr] =
    // Try to fill missing trailing args with defaults registered for the function.
    val filledArgs =
      if args.length < params.length then
        val defaults = functionDefaults.getOrElse(name, Nil)
        val missing = params.length - args.length
        val defaultsForMissing =
          if defaults.length == params.length then defaults.drop(args.length)
          else Nil
        if defaultsForMissing.length == missing && defaultsForMissing.forall(_.isDefined) then
          args ++ defaultsForMissing.map(_.get)
        else args
      else args
    if filledArgs.length != params.length then
      throw AnalysisError(s"function '$name' expects ${params.length} argument(s), got ${args.length}")
    filledArgs.zip(params).map { case (arg, (pName, pType)) =>
      val coerced = coerceLiteral(arg, pType)
      if !compatible(coerced.typ, pType) then
        throw AnalysisError(s"argument '$pName' of '$name' expects $pType, got ${coerced.typ}")
      // Insert explicit conversions for codegen
      (coerced.typ, pType) match
        case (StringType, PtrType(I8 | U8)) => TCast(coerced, pType)
        case (_: FuncType, IntType(64) | UIntType(64)) => TCast(coerced, pType)
        case (_, iface: InterfaceType) if !coerced.typ.isInstanceOf[InterfaceType] =>
          TInterfaceBox(coerced, iface)
        case _ => coerced
    }

  private def analyzeBlock(stmts: List[StmtAST]): List[TStmt] =
    stmts.map(analyzeStmt)

  private def analyzeStmt(stmt: StmtAST): TStmt =
    stmt match
      case VarStmtAST(name, typOpt, init, isMutable) =>
        val declared = typOpt.map(resolveType)
        val savedExp = currentExpected
        currentExpected = declared.orElse(currentExpected)
        val tInit0 = try analyzeExpr(init) finally currentExpected = savedExp
        val declType = declared.getOrElse(tInit0.typ)
        val tInit1 = coerceLiteral(tInit0, declType)
        // Constant folding for local immutable vals
        val tInit = if !isMutable && declType.isIntegral then
          tryConstEval(tInit1) match
            case Some(n) =>
              val masked = maskToType(n, declType)
              compileTimeConstants(name) = masked
              TIntLit(masked, declType)
            case None => tInit1
        else tInit1
        if typOpt.isDefined && !compatible(tInit.typ, declType) then
          throw AnalysisError(s"cannot assign ${tInit.typ} to $declType variable '$name'")
        // Box concrete type into interface if needed
        val tInitFinal = (tInit.typ, declType) match
          case (_, iface: InterfaceType) if !tInit.typ.isInstanceOf[InterfaceType] =>
            TInterfaceBox(tInit, iface)
          case _ => tInit
        // `_` is a discard binding: evaluate the initializer for its side effects
        // but don't bind any name. Multiple `_`s in the same scope don't collide.
        if name == "_" then
          TExprStmt(tInitFinal)
        else
          if scopeStack != null then
            currentScope(name) = SymInfo(name, declType, isMutable)
          TVarStmt(name, declType, tInitFinal)

      case DestructureStmtAST(names, init, isMutable) =>
        val tInit = analyzeExpr(init)
        // Extract struct type (works for value structs, refs, and pointers to structs)
        val st = tInit.typ match
          case s: StructType => s
          case RefType(s: StructType) => s
          case PtrType(s: StructType) => s
          case other => throw AnalysisError(s"cannot destructure non-struct type $other")
        if names.length != st.fields.length then
          throw AnalysisError(s"destructuring expects ${st.fields.length} names, got ${names.length}")
        // `_` names are discards — don't bind, don't count for existing/new check.
        val realNames = names.filter(_ != "_")
        val existingCount = realNames.count(n => tryLookup(n).isDefined)
        if isMutable || existingCount == 0 then
          // Declaration: create new variables (skip `_`)
          for (name, (_, fieldType)) <- names.zip(st.fields) if name != "_" do
            if scopeStack != null then
              currentScope(name) = SymInfo(name, fieldType, isMutable)
          TDestructureStmt(names, st.fields.map(_._2), tInit)
        else if existingCount == realNames.length then
          // All real names exist: parallel assignment (skip `_`)
          for name <- realNames do
            val sym = lookup(name)
            if !sym.mutable then throw AnalysisError(s"cannot assign to immutable variable '$name'")
          TDestructureAssignStmt(names, st.fields.map(_._2), tInit)
        else
          throw AnalysisError(s"cannot mix declared and undeclared names in destructuring")

      case AssignStmtAST(target, value) =>
        val tValue = analyzeExpr(value)
        val sym = lookupOrCreate(target, tValue.typ)
        if !sym.mutable then throw AnalysisError(s"cannot assign to immutable variable '$target'")
        TAssignStmt(sym.name, tValue)

      case CompoundAssignStmtAST(target, op, value) =>
        val sym = lookup(target)
        if !sym.mutable then throw AnalysisError(s"cannot assign to immutable variable '$target'")
        val tValue = analyzeExpr(value)
        TCompoundAssignStmt(sym.name, op, tValue)

      case DerefAssignStmtAST(pointer, value) =>
        val tPointer = analyzeExpr(pointer)
        val tValue = analyzeExpr(value)
        // Insert FuncType → i64 coercion when storing function pointer to *i64
        val coerced = (tValue.typ, tPointer.typ) match
          case (_: FuncType, PtrType(IntType(64) | UIntType(64))) => TCast(tValue, IntType(64))
          case _ => tValue
        TDerefAssignStmt(tPointer, coerced)

      case IndexAssignStmtAST(array, index, value) =>
        val tArray = analyzeExpr(array)
        val tIndex = analyzeExpr(index)
        val tValue = analyzeExpr(value)
        TIndexAssignStmt(tArray, tIndex, tValue)

      case FieldAssignStmtAST(obj, field, value) =>
        val tObj = analyzeExpr(obj)
        val tValue = analyzeExpr(value)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldAssignStmt(resolvedObj, idx, tValue)

      case FieldCompoundAssignStmtAST(obj, field, op, value) =>
        val tObj = analyzeExpr(obj)
        val tValue = analyzeExpr(value)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldCompoundAssignStmt(resolvedObj, idx, op, tValue)

      case ReturnStmtAST(value) =>
        TReturnStmt(value.map(analyzeExpr))

      case ForStmtAST(init, cond, update, body) =>
        pushScope()
        val tInit = analyzeStmt(init)
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"for condition must be bool, got ${tCond.typ}")
        loopDepth += 1
        pushScope()
        val tBody = analyzeBlock(body)
        popScope()
        val tUpdate = analyzeStmt(update)
        loopDepth -= 1
        popScope()
        TForStmt(tInit, tCond, tUpdate, tBody)

      case WhileStmtAST(cond, body) =>
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"while condition must be bool, got ${tCond.typ}")
        loopDepth += 1
        pushScope()
        val tBody = analyzeBlock(body)
        popScope()
        loopDepth -= 1
        TWhileStmt(tCond, tBody)

      case DoWhileStmtAST(cond, body) =>
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"do/while condition must be bool, got ${tCond.typ}")
        loopDepth += 1
        pushScope()
        val tBody = analyzeBlock(body)
        popScope()
        loopDepth -= 1
        TDoWhileStmt(tCond, tBody)

      case BreakStmtAST() =>
        if loopDepth == 0 then throw AnalysisError("break outside of loop")
        TBreakStmt

      case ContinueStmtAST() =>
        if loopDepth == 0 then throw AnalysisError("continue outside of loop")
        TContinueStmt

      case DeferStmtAST(body) =>
        TDeferStmt(analyzeStmt(body))

      case AsmStmtAST(code) =>
        TAsmStmt(code)

      case ExprStmtAST(expr) =>
        TExprStmt(analyzeExpr(expr))

  // Resolve a variant name to its (EnumType, variant index), consulting the scrutinee
  // type first (for monomorphized generic enums) and then the global variantToEnum map.
  private def resolveVariant(name: String, scrutineeType: SyslType): Option[(SyslType.EnumType, Int)] =
    scrutineeType match
      case et: SyslType.EnumType =>
        val idx = et.variants.indexWhere(_._1 == name)
        if idx >= 0 then Some((et, idx))
        else variantToEnum.get(name)
      case _ => variantToEnum.get(name)

  private def analyzePattern(pat: MatchPatternAST, scrutineeType: SyslType): TMatchPattern =
    pat match
      case WildcardPatternAST => TWildcard
      case ValuePatternAST(VarRefAST(name)) if resolveVariant(name, scrutineeType).isDefined =>
        // No-arg variant pattern (e.g., `Empty` in a match arm)
        val (et, variantIdx) = resolveVariant(name, scrutineeType).get
        val (_, variantFields) = et.variants(variantIdx)
        if variantFields.nonEmpty then throw AnalysisError(s"variant '$name' requires ${variantFields.length} argument(s) in pattern")
        TVariantPattern(et, variantIdx, Nil, Nil)
      case ValuePatternAST(expr) =>
        val tv = analyzeExpr(expr)
        val coerced = coerceLiteral(tv, scrutineeType)
        if !compatible(coerced.typ, scrutineeType) then
          throw AnalysisError(s"match pattern type ${coerced.typ} incompatible with ${scrutineeType}")
        TValuePattern(coerced)
      case RangePatternAST(low, high) =>
        val tLow = coerceLiteral(analyzeExpr(low), scrutineeType)
        val tHigh = coerceLiteral(analyzeExpr(high), scrutineeType)
        TRangePattern(tLow, tHigh)
      case DestructurePatternAST(name, fields) =>
        // Check if name is an enum variant first, then struct
        if resolveVariant(name, scrutineeType).isDefined then
          val (et, variantIdx) = resolveVariant(name, scrutineeType).get
          val (_, variantFields) = et.variants(variantIdx)
          if fields.length != variantFields.length then
            throw AnalysisError(s"variant '$name' has ${variantFields.length} fields, pattern has ${fields.length}")
          val bindings = fields.zip(variantFields).map { case (fieldPat, (fieldName, fieldType)) =>
            fieldPat match
              case WildcardPatternAST => None
              case ValuePatternAST(VarRefAST(bindName)) =>
                if scopeStack != null then
                  currentScope(bindName) = SymInfo(bindName, fieldType, false)
                Some(bindName)
              case ValuePatternAST(expr) => None
              case _ => throw AnalysisError(s"unsupported pattern in variant destructure")
          }
          TVariantPattern(et, variantIdx, bindings, variantFields.map(_._2))
        else
          val st = structTypes.getOrElse(name, throw AnalysisError(s"unknown struct or variant '$name' in match pattern"))
          if fields.length != st.fields.length then
            throw AnalysisError(s"struct '$name' has ${st.fields.length} fields, pattern has ${fields.length}")
          val bindings = fields.zip(st.fields).map { case (fieldPat, (fieldName, fieldType)) =>
            fieldPat match
              case WildcardPatternAST => None
              case ValuePatternAST(VarRefAST(bindName)) =>
                // In destructure context, bare names are bindings
                if scopeStack != null then
                  currentScope(bindName) = SymInfo(bindName, fieldType, false) // val binding
                Some(bindName)
              case ValuePatternAST(expr) =>
                // Literal value — not a binding
                None
              case _ => throw AnalysisError(s"unsupported pattern in struct destructure")
          }
          TDestructurePattern(st, bindings, st.fields.map(_._2))

  private def analyzeExpr(expr: ExpressionAST): TExpr =
    expr match
      case IntLitAST(n) => TIntLit(n, I32)
      case TypedIntLitAST(n, typeName) => TIntLit(n, resolveType(NamedTypeAST(typeName)))
      case FloatLitAST(d) => TFloatLit(d, DoubleType)
      case CharLitAST(c) => TIntLit(c.toLong, U32)
      case BoolLitAST(b) => TBoolLit(b, BoolType)
      case StringLitAST(s) => TStringLit(s, StringType)
      case StringLitExprAST(s) =>
        if s.startsWith("s:") then analyzeInterpolatedString(s.substring(2))
        else if s.startsWith("f:") then analyzeFormattedString(s.substring(2))
        else TStringLit(s, StringType)
      case TupleLitAST(elements) =>
        val tElems = elements.map(analyzeExpr)
        val tupleType = SyslType.tupleType(tElems.map(_.typ))
        TStructConstruct(tupleType, tElems)
      case ArrayDeclAST(size, typAST) =>
        val t = resolveType(typAST)
        TArrayDecl(size, t)

      case ArrayLitAST(elements) =>
        val tElems = elements.map(analyzeExpr)
        if tElems.isEmpty then
          // Empty array literal — element type comes from target context (e.g. [0]string = [])
          val elemType = currentExpected.flatMap {
            case SyslType.ArrayType(et, _) => Some(et)
            case _ => None
          }.getOrElse(throw AnalysisError("cannot infer element type for empty array literal []"))
          TArrayLit(Nil, SyslType.ArrayType(elemType, 0))
        else
          val elemType = tElems.head.typ
          TArrayLit(tElems, SyslType.ArrayType(elemType, tElems.length))

      case ClosureAST(params, body) =>
        // Infer parameter types from currentExpected (the target func type)
        val expectedFunc = currentExpected.collect { case ft: FuncType => ft }
        val typedParams = params.zipWithIndex.map { case (p, i) =>
          val paramType = p.typ match
            case Some(typeAST) => resolveType(typeAST)
            case None =>
              // Only use currentExpected when it is a function type with the *same arity*
              // as this closure. Otherwise a parser return type `func(string,int)->R`
              // would wrongly supply `string` as the type of a single-parameter combinator
              // argument (e.g. `ch` in `map(p, ch -> ...)`).
              expectedFunc match
                case Some(ft) if ft.params.length == params.length && i < ft.params.length =>
                  ft.params(i)
                case _ => throw AnalysisError(s"cannot infer type for closure parameter '${p.name}' — add a type annotation")
          TParam(p.name, paramType)
        }
        val expectedRet = expectedFunc.map(_.returnType).getOrElse(
          currentExpected match
            case Some(t) if t != VoidType => t
            case _ => VoidType
        )
        // Push scope with closure params
        pushScope()
        for p <- typedParams do
          currentScope(p.name) = SymInfo(p.name, p.typ, mutable = false)
        // Analyze body
        val savedExp = currentExpected
        currentExpected = if expectedRet == VoidType then None else Some(expectedRet)
        val tBody = try body match
          case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
          case BlockBodyAST(stmts) => TBlockBody(analyzeBlock(stmts))
        finally currentExpected = savedExp
        popScope()
        // Detect captures: variables referenced from enclosing scope (not globals, not params).
        // `locals` = params in scope at this point (outer closure params + any nested closure params).
        val paramNames = typedParams.map(_.name).toSet
        val captures = scala.collection.mutable.ListBuffer.empty[(String, SyslType)]
        def recordCapture(name: String, typ: SyslType, locals: Set[String]): Unit =
          if !locals.contains(name) && !globalScope.contains(name) && !functions.contains(name) && !builtinFunctions.contains(name) then
            if !captures.exists(_._1 == name) then captures += ((name, typ))
        def scanMatchPattern(pat: TMatchPattern, locals: Set[String]): Unit = pat match
          case TValuePattern(e) => scanCaptures(e, locals)
          case TRangePattern(lo, hi) => scanCaptures(lo, locals); scanCaptures(hi, locals)
          case _ => ()
        def patternBindings(pats: List[TMatchPattern]): Set[String] =
          val b = scala.collection.mutable.Set.empty[String]
          for p <- pats do
            p match
              case TVariantPattern(_, _, bindings, _) => bindings.flatten.foreach(b += _)
              case TDestructurePattern(_, bindings, _) => bindings.flatten.foreach(b += _)
              case _ => ()
          b.toSet
        def scanCaptures(expr: TExpr, locals: Set[String]): Unit = expr match
          case TVarRef(name, typ) => recordCapture(name, typ, locals)
          case TBinary(l, _, r, _) => scanCaptures(l, locals); scanCaptures(r, locals)
          case TUnary(_, e, _) => scanCaptures(e, locals)
          case TCall(_, args, _) => args.foreach(scanCaptures(_, locals))
          case TIndirectCall(c, args, _) => scanCaptures(c, locals); args.foreach(scanCaptures(_, locals))
          case TIndex(a, i, _) => scanCaptures(a, locals); scanCaptures(i, locals)
          case TFieldAccess(o, _, _) => scanCaptures(o, locals)
          case TDeref(e, _) => scanCaptures(e, locals)
          case TCast(e, _) => scanCaptures(e, locals)
          case TAddrOf(n, t) => recordCapture(n, t, locals)
          case TAddrOfIndex(a, i, _) => scanCaptures(a, locals); scanCaptures(i, locals)
          case TAddrOfField(o, _, _) => scanCaptures(o, locals)
          case TFieldPreInc(o, _, _) => scanCaptures(o, locals)
          case TFieldPreDec(o, _, _) => scanCaptures(o, locals)
          case TFieldPostInc(o, _, _) => scanCaptures(o, locals)
          case TFieldPostDec(o, _, _) => scanCaptures(o, locals)
          case TIfExpr(c, th, el, _) =>
            scanCaptures(c, locals)
            scanStmtSeq(th, locals)
            el.foreach(scanStmtSeq(_, locals))
          case TMatchExpr(scrutinee, arms, default, _) =>
            scanCaptures(scrutinee, locals)
            for arm <- arms do
              arm.patterns.foreach(scanMatchPattern(_, locals))
              val armLocals = locals ++ patternBindings(arm.patterns)
              arm.guard.foreach(scanCaptures(_, armLocals))
              scanStmtSeq(arm.body, armLocals)
            default.foreach(scanStmtSeq(_, locals))
          case TEnumConstruct(_, _, args) => args.foreach(scanCaptures(_, locals))
          case TStructConstruct(_, args) => args.foreach(scanCaptures(_, locals))
          case TArrayLit(elems, _) => elems.foreach(scanCaptures(_, locals))
          case TNew(_, args) => args.foreach(scanCaptures(_, locals))
          case TNewEnum(_, _, args) => args.foreach(scanCaptures(_, locals))
          case TNewArray(_, size) => scanCaptures(size, locals)
          case TLen(e, _) => scanCaptures(e, locals)
          case TCap(e, _) => scanCaptures(e, locals)
          case TSliceExpr(arr, lo, hi, _) =>
            scanCaptures(arr, locals)
            lo.foreach(scanCaptures(_, locals))
            hi.foreach(scanCaptures(_, locals))
          case TAppend(slc, elem, _) => scanCaptures(slc, locals); scanCaptures(elem, locals)
          case TStringFromPtr(ptr, len, _) => scanCaptures(ptr, locals); scanCaptures(len, locals)
          case TStringFromSlice(slc, _) => scanCaptures(slc, locals)
          case TStr(e) => scanCaptures(e, locals)
          case TClosure(innerParams, _, innerBody, _) =>
            val innerLocals = locals ++ innerParams.map(_.name).toSet
            innerBody match
              case TExprBody(e) => scanCaptures(e, innerLocals)
              case TBlockBody(stmts) => scanStmtSeq(stmts, innerLocals)
          case _ => ()
        /** Walk statements in order; extend locals with val/destructure bindings so they are not mistaken for captures. */
        def scanStmtSeq(stmts: List[TStmt], startLocals: Set[String]): Unit =
          var L = startLocals
          for stmt <- stmts do L = scanStmtInSeq(stmt, L)
        def scanStmtInSeq(stmt: TStmt, locals: Set[String]): Set[String] = stmt match
          case TVarStmt(name, _, init) =>
            scanCaptures(init, locals)
            if name == "_" then locals else locals + name
          case TDestructureStmt(names, _, init) =>
            scanCaptures(init, locals)
            locals ++ names.filter(_ != "_").toSet
          case TDestructureAssignStmt(_, _, init) =>
            scanCaptures(init, locals)
            locals
          case TExprStmt(e) =>
            scanCaptures(e, locals)
            locals
          case TAssignStmt(_, v) =>
            scanCaptures(v, locals)
            locals
          case TCompoundAssignStmt(_, _, v) =>
            scanCaptures(v, locals)
            locals
          case TDerefAssignStmt(ptr, v) =>
            scanCaptures(ptr, locals)
            scanCaptures(v, locals)
            locals
          case TIndexAssignStmt(arr, idx, v) =>
            scanCaptures(arr, locals)
            scanCaptures(idx, locals)
            scanCaptures(v, locals)
            locals
          case TFieldAssignStmt(obj, _, v) =>
            scanCaptures(obj, locals)
            scanCaptures(v, locals)
            locals
          case TFieldCompoundAssignStmt(obj, _, _, v) =>
            scanCaptures(obj, locals)
            scanCaptures(v, locals)
            locals
          case TReturnStmt(Some(e)) =>
            scanCaptures(e, locals)
            locals
          case TReturnStmt(None) => locals
          case TWhileStmt(c, body) =>
            scanCaptures(c, locals)
            scanStmtSeq(body, locals)
            locals
          case TForStmt(init, c, upd, body) =>
            var Lf = scanStmtInSeq(init, locals)
            scanCaptures(c, Lf)
            Lf = scanStmtInSeq(upd, Lf)
            scanStmtSeq(body, Lf)
            locals
          case TDoWhileStmt(c, body) =>
            scanStmtSeq(body, locals)
            scanCaptures(c, locals)
            locals
          case TDeferStmt(inner) =>
            scanStmtInSeq(inner, locals)
            locals
          case TAsmStmt(_) => locals
          case TBreakStmt | TContinueStmt => locals
          case _ => locals
        tBody match
          case TExprBody(e) => scanCaptures(e, paramNames)
          case TBlockBody(stmts) => scanStmtSeq(stmts, paramNames)
        // Determine actual return type from body
        val actualRet = tBody match
          case TExprBody(e) => e.typ
          case TBlockBody(stmts) =>
            stmts.lastOption match
              case Some(TExprStmt(e)) => e.typ
              case _ => VoidType
        TClosure(typedParams, actualRet, tBody, captures.toList)

      case AsmExprAST(code) =>
        TAsmExpr(code, currentReturnType)

      case SizeofTypeAST(typAST) =>
        val t = resolveType(typAST)
        TSizeof(t.sizeOf, I32)

      case SizeofExprAST(VarRefAST(name)) if structTypes.contains(name) =>
        // sizeof(StructName) — treat as type sizeof
        TSizeof(structTypes(name).sizeOf, I32)

      case SizeofExprAST(VarRefAST(name)) if dataEnumTypes.contains(name) =>
        TSizeof(dataEnumTypes(name).sizeOf, I32)

      case SizeofExprAST(inner) =>
        val tInner = analyzeExpr(inner)
        TSizeof(tInner.typ.sizeOf, I32)

      case FieldPreIncAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPreInc(resolvedObj, idx, structType.fields(idx)._2)

      case FieldPreDecAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPreDec(resolvedObj, idx, structType.fields(idx)._2)

      case FieldPostIncAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPostInc(resolvedObj, idx, structType.fields(idx)._2)

      case FieldPostDecAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPostDec(resolvedObj, idx, structType.fields(idx)._2)

      case NewArrayAST(size, elemTypeAST) =>
        val tSize = analyzeExpr(size)
        val elemType = resolveType(elemTypeAST)
        TNewArray(elemType, tSize)

      case NewExprAST(typeName, args) =>
        // Check variant name first (e.g. `new Ok(42)`) before resolving as type
        variantToEnum.get(typeName) match
          case Some((et, idx)) =>
            val tArgs = args.map(analyzeExpr)
            val variantFields = et.variants(idx)._2
            if tArgs.length != variantFields.length then
              throw AnalysisError(s"variant '${et.variants(idx)._1}' expects ${variantFields.length} field(s), got ${tArgs.length}")
            val checkedArgs = tArgs.zip(variantFields).map { case (arg, (fieldName, fieldType)) =>
              val coerced = coerceLiteral(arg, fieldType)
              if !compatible(coerced.typ, fieldType) then
                throw AnalysisError(s"field '$fieldName' expects $fieldType, got ${coerced.typ}")
              coerced
            }
            TNewEnum(et, idx, checkedArgs)
          case None =>
            val t = resolveType(NamedTypeAST(typeName))
            t match
              case st: StructType =>
                val tArgs = args.map(analyzeExpr)
                if tArgs.length != st.fields.length then
                  throw AnalysisError(s"new '${st.name}' expects ${st.fields.length} field(s), got ${tArgs.length}")
                val checkedArgs = tArgs.zip(st.fields).map { case (arg, (fieldName, fieldType)) =>
                  val coerced = coerceLiteral(arg, fieldType)
                  if !compatible(coerced.typ, fieldType) then
                    throw AnalysisError(s"field '$fieldName' of '${st.name}' expects $fieldType, got ${coerced.typ}")
                  coerced
                }
                TNew(st, checkedArgs)
              case _ =>
                throw AnalysisError(s"'new' requires a struct type or enum variant name, got '$typeName'")

      case StructInitAST(typeName) =>
        val t = resolveType(NamedTypeAST(typeName))
        t match
          case st: StructType => TStructLit(st)
          case _ => throw AnalysisError(s"'$typeName' is not a struct type")

      case UninitDeclAST(typAST) =>
        val t = resolveType(typAST)
        t match
          case st: StructType => TStructLit(st)
          case ArrayType(elem, size) => TArrayDecl(size, t)
          case _ => TIntLit(0, t)  // zero-initialize scalars and pointers

      case VarRefAST(name) =>
        if name == "_" then
          throw AnalysisError("cannot read from '_' — it is a write-only discard binding")
        // Check if name is a function (used as a value = function pointer)
        if functions.contains(name) then
          val f = functions(name)
          if f.isDef then
            // Auto-call: bare reference to a def function emits a call
            TCall(f.name, Nil, f.returnType)
          else
            TFuncRef(f.name, FuncType(f.params.map(_._2), f.returnType))
        else if builtinFunctions.contains(name) then
          val f = builtinFunctions(name)
          TFuncRef(name, FuncType(f.params.map(_._2), f.returnType))
        else
          // Check for no-arg enum variant before falling through to variable lookup
          tryLookup(name) match
            case Some(sym) => TVarRef(sym.name, sym.typ)
            case None =>
              if variantToEnum.contains(name) then
                val (et, idx) = variantToEnum(name)
                val (_, fields) = et.variants(idx)
                if fields.nonEmpty then throw AnalysisError(s"variant '$name' requires ${fields.length} argument(s)")
                TEnumConstruct(et, idx, Nil)
              else if genericVariantToEnum.contains(name) then
                val (enumName, idx) = genericVariantToEnum(name)
                val template = genericEnums(enumName)
                val variant = template.variants(idx)
                if variant.fields.nonEmpty then
                  throw AnalysisError(s"variant '$name' requires ${variant.fields.length} argument(s)")
                // No args to infer from — must use currentExpected
                currentExpected match
                  case Some(et: SyslType.EnumType) =>
                    // Find this enum's instantiation args from its mangled name
                    val matching = genericEnumInstantiations.collectFirst {
                      case ((n, args), inst) if n == enumName && inst.name == et.name => args
                    }
                    matching match
                      case Some(args) => TEnumConstruct(et, idx, Nil)
                      case None =>
                        throw AnalysisError(s"expected enum type $et does not match variant '$name' of generic enum '$enumName'")
                  case _ =>
                    throw AnalysisError(s"cannot infer type parameters for no-arg variant '$name' of generic enum '$enumName' — use explicit type annotation")
              else
                val sym = lookup(name)  // will throw proper error
                TVarRef(sym.name, sym.typ)

      case AddrOfAST(name) =>
        // &name on a function (including def) gives a function pointer
        if functions.contains(name) then
          val f = functions(name)
          TFuncRef(f.name, FuncType(f.params.map(_._2), f.returnType))
        else
          val sym = lookup(name)
          TAddrOf(sym.name, PtrType(sym.typ))

      case AddrOfFieldAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot take address of field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TAddrOfField(resolvedObj, idx, PtrType(structType.fields(idx)._2))

      case AddrOfIndexAST(array, index) =>
        val tArray = analyzeExpr(array)
        val tIndex = analyzeExpr(index)
        val elemType = tArray.typ match
          case ArrayType(elem, _) => elem
          case PtrType(elem) => elem
          case SliceType(elem) => elem
          case RefType(SliceType(elem)) => elem
          case _ => throw AnalysisError(s"cannot take address of index on ${tArray.typ}")
        TAddrOfIndex(tArray, tIndex, PtrType(elemType))

      case DerefAST(inner) =>
        val tInner = analyzeExpr(inner)
        val resultType = tInner.typ match
          case PtrType(t) => t
          case RefType(t) => t
          case ArrayType(t, _) => t
          case StringType => throw AnalysisError("cannot dereference string — use indexing instead")
          case SliceType(_) => throw AnalysisError("cannot dereference slice — use indexing instead")
          case t => throw AnalysisError(s"cannot dereference $t")
        TDeref(tInner, resultType)

      case IndexAST(arr, index) =>
        val tArr = analyzeExpr(arr)
        val tIndex = analyzeExpr(index)
        val elemType = tArr.typ match
          case ArrayType(elem, _) => elem
          case PtrType(elem) => elem
          case SliceType(elem) => elem
          case RefType(SliceType(elem)) => elem
          case StringType => U8
          case t => throw AnalysisError(s"cannot index $t")
        TIndex(tArr, tIndex, elemType)

      case SliceExprAST(arr, low, high) =>
        val tArr = analyzeExpr(arr)
        val tLow = low.map(analyzeExpr)
        val tHigh = high.map(analyzeExpr)
        val elemType = tArr.typ match
          case SliceType(elem) => elem
          case RefType(SliceType(elem)) => elem
          case ArrayType(elem, _) => elem
          case t => throw AnalysisError(s"cannot sub-slice $t")
        TSliceExpr(tArr, tLow, tHigh, SliceType(elemType))

      case FieldAccessAST(VarRefAST(nsName), member) if moduleNamespaces.contains(nsName) =>
        // Qualified import access: strings.MAX_LEN
        val meta = moduleNamespaces(nsName)
        val sym = meta.publicSymbols.find(s => shortName(s.name) == member)
          .getOrElse(throw AnalysisError(s"module '$nsName' has no symbol '$member'"))
        sym.typ match
          case SymbolMeta.Kind.Data(dataType) => TVarRef(sym.name, dataType)
          case SymbolMeta.Kind.Func(params, retType, _) => TFuncRef(sym.name, SyslType.FuncType(params, retType))
          case SymbolMeta.Kind.Struct(st) => throw AnalysisError(s"'$nsName.$member' is a struct type, not a value")
          case SymbolMeta.Kind.Enum(_) => throw AnalysisError(s"'$nsName.$member' is an enum type, not a value")
          case SymbolMeta.Kind.Interface(_) => throw AnalysisError(s"'$nsName.$member' is an interface type, not a value")
          case SymbolMeta.Kind.Impl(_, _, _) =>
            throw AnalysisError(s"'$nsName.$member' is a trait implementation, not a value")

      case FieldAccessAST(VarRefAST(enumName), member) if enumTypes.contains(enumName) =>
        val members = enumTypes(enumName)
        if !members.contains(member) then throw AnalysisError(s"enum $enumName has no member '$member'")
        TIntLit(members(member), I32)

      case FieldAccessAST(VarRefAST(enumName), variantName) if dataEnumTypes.contains(enumName) =>
        val et = dataEnumTypes(enumName)
        val idx = et.variants.indexWhere(_._1 == variantName)
        if idx < 0 then throw AnalysisError(s"enum $enumName has no variant '$variantName'")
        TEnumConstruct(et, idx, Nil)

      case FieldAccessAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        // Auto-dereference pointers to structs (p.x works like (*p).x)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldAccess(resolvedObj, idx, structType.fields(idx)._2)

      case PreIncAST(name) => val s = lookup(name); TPreInc(s.name, s.typ)
      case PreDecAST(name) => val s = lookup(name); TPreDec(s.name, s.typ)
      case PostIncAST(name) => val s = lookup(name); TPostInc(s.name, s.typ)
      case PostDecAST(name) => val s = lookup(name); TPostDec(s.name, s.typ)

      case UnaryAST(op, operand) =>
        val tOperand = analyzeExpr(operand)
        val resultType = op match
          case "-" => tOperand.typ
          case "~" =>
            if !tOperand.typ.isIntegral then throw AnalysisError(s"unary ~ requires integral type, got ${tOperand.typ}")
            tOperand.typ
          case "!" =>
            if tOperand.typ != BoolType then throw AnalysisError(s"unary ! requires bool, got ${tOperand.typ}")
            BoolType
        TUnary(op, tOperand, resultType)

      case BinaryAST(left, op, right) =>
        val tLeft0 = analyzeExpr(left)
        val tRight0 = analyzeExpr(right)
        // Coerce integer literal signedness to match the other operand (preserve width)
        val tLeft = if tRight0.typ.isIntegral then coerceSignedness(tLeft0, tRight0.typ) else tLeft0
        val tRight = if tLeft.typ.isIntegral then coerceSignedness(tRight0, tLeft.typ) else tRight0
        // Try to desugar operator to a trait call when operands are user-defined types
        val dispatchedOpt = tryOperatorDispatch(op, tLeft, tRight)
        if dispatchedOpt.isDefined then return dispatchedOpt.get
        val resultType = op match
          case "+" if tLeft.typ == StringType && tRight.typ == StringType => StringType // string concatenation
          case "+" | "-" if tLeft.typ == StringType && tRight.typ.isNumeric =>
            throw AnalysisError("pointer arithmetic not allowed on string")
          case "+" | "-" if tLeft.typ.isPointerLike && tRight.typ.isNumeric =>
            // Pointer arithmetic always yields a pointer (array decays)
            tLeft.typ match
              case PtrType(_) => tLeft.typ
              case ArrayType(elem, _) => PtrType(elem)
              case _ => tLeft.typ
          case "+" | "-" | "*" | "/" =>
            if !tLeft.typ.isNumeric || !tRight.typ.isNumeric then
              throw AnalysisError(s"operator $op requires numeric types, got ${tLeft.typ} $op ${tRight.typ}")
            // Promote to wider type; float wins over int; no mixed signed/unsigned
            (tLeft.typ, tRight.typ) match
              case (DoubleType, _) | (_, DoubleType) => DoubleType
              case (IntType(a), IntType(b)) => IntType(a max b)
              case (UIntType(a), UIntType(b)) => UIntType(a max b)
              case (UIntType(a), IntType(b)) if a < b => IntType(b)   // unsigned fits in signed
              case (IntType(a), UIntType(b)) if b < a => IntType(a)   // unsigned fits in signed
              case (l, r) if l.isIntegral && r.isIntegral =>
                throw AnalysisError(s"cannot mix signed and unsigned in $op: ${tLeft.typ} $op ${tRight.typ}")
              case _ => tLeft.typ
          case "%" | "&" | "|" | "^" | "<<" | ">>" =>
            if !tLeft.typ.isIntegral || !tRight.typ.isIntegral then
              throw AnalysisError(s"operator $op requires integral types, got ${tLeft.typ} $op ${tRight.typ}")
            (tLeft.typ, tRight.typ) match
              case (IntType(a), IntType(b)) => IntType(a max b)
              case (UIntType(a), UIntType(b)) => UIntType(a max b)
              case (UIntType(a), IntType(b)) if a < b => IntType(b)   // unsigned fits in signed
              case (IntType(a), UIntType(b)) if b < a => IntType(a)   // unsigned fits in signed
              case _ =>
                throw AnalysisError(s"cannot mix signed and unsigned in $op: ${tLeft.typ} $op ${tRight.typ}")
          case "==" | "!=" | "<" | ">" | "<=" | ">=" =>
            // Disallow mixed signed/unsigned comparisons unless unsigned fits in signed
            if tLeft.typ.isIntegral && tRight.typ.isIntegral then
              (tLeft.typ, tRight.typ) match
                case (UIntType(a), IntType(b)) if a >= b =>
                  throw AnalysisError(s"cannot compare signed and unsigned: ${tLeft.typ} $op ${tRight.typ}")
                case (IntType(a), UIntType(b)) if b >= a =>
                  throw AnalysisError(s"cannot compare signed and unsigned: ${tLeft.typ} $op ${tRight.typ}")
                case _ => // ok: same signedness, or unsigned fits in signed
            BoolType
          case "&&" | "||" =>
            if tLeft.typ != BoolType then throw AnalysisError(s"$op requires bool operands, got ${tLeft.typ}")
            if tRight.typ != BoolType then throw AnalysisError(s"$op requires bool operands, got ${tRight.typ}")
            BoolType
          case _ => throw AnalysisError(s"unknown operator: $op")
        // Insert implicit int→float promotion casts for mixed operands
        val promotedLeft = if resultType == DoubleType && tLeft.typ.isIntegral then TCast(tLeft, DoubleType) else tLeft
        val promotedRight = if resultType == DoubleType && tRight.typ.isIntegral then TCast(tRight, DoubleType) else tRight
        TBinary(promotedLeft, op, promotedRight, resultType)

      case CastAST(targetTypeAST, inner) =>
        val tInner = analyzeExpr(inner)
        val target = resolveType(targetTypeAST)
        // Validate cast is possible
        (tInner.typ, target) match
          case (from, to) if from == to => // no-op cast
          // bool conversions
          case (from, BoolType) if from.isNumeric => // numeric to bool: != 0
          case (BoolType, to) if to.isNumeric => // bool to numeric: true=1, false=0
          case (_: PtrType | _: RefType | _: FuncType, BoolType) => // pointer/ref/func to bool: null check
          // float conversions
          case (from, DoubleType) if from.isIntegral => // int to float (cvt)
          case (DoubleType, to) if to.isIntegral => // float to int (fint)
          // integer conversions
          case (from, to) if from.isIntegral && to.isIntegral => // int ↔ int (signed/unsigned, any width)
          // pointer conversions
          case (_: PtrType, to) if to.isIntegral => // pointer to integer
          case (from, _: PtrType) if from.isIntegral => // integer to pointer
          case (_: PtrType, _: PtrType) => // pointer to pointer (like C's void* cast)
          case (StringType, PtrType(I8 | U8)) => // string to *i8/*byte decay
          // ref conversions
          case (_: RefType, _: PtrType) => // ref to raw pointer (&T → *U)
          case (_: RefType, to) if to.isIntegral => // ref to integer (address)
          // func conversions
          case (_: FuncType, to) if to.isIntegral => // func to integer (address)
          case (_: FuncType, _: PtrType) => // func to pointer
          case (from, to) => throw AnalysisError(s"cannot cast $from to $to")
        TCast(tInner, target)

      case CallAST("str", args) =>
        if args.size != 1 then throw AnalysisError("str() takes exactly 1 argument")
        val tArg = analyzeExpr(args.head)
        tArg.typ match
          case StringType => tArg // identity — already a string
          case t if t.isNumeric || t == BoolType || t == DoubleType => TStr(tArg)
          case t => throw AnalysisError(s"str() not supported on $t")

      case CallAST("string", args) =>
        args.size match
          case 2 =>
            // string(ptr, len) — construct string from *byte + length
            val tPtr = analyzeExpr(args(0))
            val tLen = analyzeExpr(args(1))
            if !tPtr.typ.isInstanceOf[PtrType] then
              throw AnalysisError(s"string() first argument must be a pointer, got ${tPtr.typ}")
            if !tLen.typ.isIntegral then
              throw AnalysisError(s"string() second argument must be an integer, got ${tLen.typ}")
            TStringFromPtr(tPtr, tLen, StringType)
          case 1 =>
            // string(slice) — construct string from []byte slice
            val tSlice = analyzeExpr(args(0))
            tSlice.typ match
              case SliceType(U8 | I8) => TStringFromSlice(tSlice, StringType)
              case RefType(SliceType(U8 | I8)) => TStringFromSlice(tSlice, StringType)
              case other => throw AnalysisError(s"string() from single argument requires []byte or &[]byte, got $other")
          case n => throw AnalysisError(s"string() takes 1 or 2 arguments, got $n")

      case CallAST("len", args) =>
        if args.size != 1 then throw AnalysisError("len() takes exactly 1 argument")
        val tArg = analyzeExpr(args.head)
        tArg.typ match
          case StringType | SliceType(_) | ArrayType(_, _) | RefType(SliceType(_)) => TLen(tArg, I32)
          case t => throw AnalysisError(s"len() not supported on $t")

      case CallAST("cap", args) =>
        if args.size != 1 then throw AnalysisError("cap() takes exactly 1 argument")
        val tArg = analyzeExpr(args.head)
        tArg.typ match
          case SliceType(_) => TCap(tArg, I32)
          case RefType(SliceType(_)) => TCap(tArg, I32)
          case ArrayType(_, _) => TCap(tArg, I32)
          case t => throw AnalysisError(s"cap() not supported on $t")

      case CallAST("append", args) =>
        if args.size != 2 then throw AnalysisError("append() takes exactly 2 arguments")
        val tSlice = analyzeExpr(args(0))
        val tElem = analyzeExpr(args(1))
        val elemType = tSlice.typ match
          case SliceType(elem) => elem
          case t => throw AnalysisError(s"append() requires []T, got $t")
        val coerced = coerceLiteral(tElem, elemType)
        if !compatible(coerced.typ, elemType) then
          throw AnalysisError(s"cannot append ${coerced.typ} to []$elemType")
        TAppend(tSlice, coerced, SliceType(elemType))

      // Generic struct/function constructor with explicit type args: Name[T](args)
      // The parser sees this as IndirectCallAST(IndexAST(VarRefAST(name), typeExpr), args)
      case IndirectCallAST(IndexAST(VarRefAST(name), typeExpr), args)
        if genericStructs.contains(name) || genericTemplates.contains(name) =>
        val typeArg = resolveType(exprToTypeAST(typeExpr))
        val tArgs = args.map(analyzeExpr)
        if genericStructs.contains(name) then
          val st = instantiateGenericStruct(name, List(typeArg))
          if tArgs.length != st.fields.length then
            throw AnalysisError(s"struct '${st.name}' has ${st.fields.length} field(s), got ${tArgs.length} argument(s)")
          val checkedArgs = tArgs.zip(st.fields).map { case (arg, (fieldName, fieldType)) =>
            val coerced = coerceLiteral(arg, fieldType)
            if !compatible(coerced.typ, fieldType) then
              throw AnalysisError(s"field '$fieldName' of '${st.name}' expects $fieldType, got ${coerced.typ}")
            coerced
          }
          TStructConstruct(st, checkedArgs)
        else
          val (mangled, funInfo) = instantiateGeneric(name, tArgs.map(_.typ), List(typeArg))
          val checkedArgs = checkArgs(mangled, funInfo.params, tArgs)
          TCall(mangled, checkedArgs, funInfo.returnType)

      case IndirectCallAST(callee, args) =>
        val tCallee = analyzeExpr(callee)
        val tArgs = args.map(analyzeExpr)
        tCallee.typ match
          case FuncType(paramTypes, returnType) =>
            val params = paramTypes.zipWithIndex.map { case (t, i) => (s"arg$i", t) }
            val checkedArgs = checkArgs("<indirect>", params, tArgs)
            TIndirectCall(tCallee, checkedArgs, returnType)
          case other =>
            throw AnalysisError(s"cannot call expression of type $other as a function")

      case MethodCallAST(VarRefAST(nsName), method, args) if moduleNamespaces.contains(nsName) =>
        // Qualified import call: strings.has_prefix(s, prefix)
        val meta = moduleNamespaces(nsName)
        val tArgs = args.map(analyzeExpr)
        // Find the function in the module's symbols
        val funcSym = meta.publicSymbols.find(s => shortName(s.name) == method)
          .getOrElse(throw AnalysisError(s"module '$nsName' has no function '$method'"))
        funcSym.typ match
          case SymbolMeta.Kind.Func(params, returnType, _) =>
            val paramPairs = params.zipWithIndex.map((t, i) => (s"_p$i", t))
            val checkedArgs = checkArgs(s"$nsName.$method", paramPairs, tArgs)
            TCall(funcSym.name, checkedArgs, returnType)
          case _ => throw AnalysisError(s"'$nsName.$method' is not a function")

      case MethodCallAST(VarRefAST(name), method, args) if traits.contains(name) =>
        // Trait method call: Ord.cmp(a, b)
        val tArgs = args.map(analyzeExpr)
        val (mangled, funInfo) = analyzeTraitCall(name, method, tArgs)
        val checkedArgs = checkArgs(mangled, funInfo.params, tArgs)
        TCall(mangled, checkedArgs, funInfo.returnType)

      case MethodCallAST(obj, method, args) =>
        val tObj = analyzeExpr(obj)
        val tArgs = args.map(analyzeExpr)
        // Interface dispatch
        tObj.typ match
          case iface: InterfaceType =>
            val methodIdx = iface.methods.indexWhere(_._1 == method)
            if methodIdx < 0 then throw AnalysisError(s"interface ${iface.name} has no method '$method'")
            val (_, paramTypes, retType) = iface.methods(methodIdx)
            val params = paramTypes.zipWithIndex.map((t, i) => (s"arg$i", t))
            val checkedArgs = checkArgs(s"${iface.name}.$method", params, tArgs)
            return TInterfaceDispatch(tObj, methodIdx, checkedArgs, retType)
          case _ => ()
        // Determine the struct type (defer self-arg computation until we know it's a method)
        val structType = tObj.typ match
          case st: StructType          => st
          case PtrType(st: StructType) => st
          case RefType(st: StructType) => st
          case other => throw AnalysisError(s"cannot call method '$method' on $other")
        val structName = structType.name
        val funcName = s"${structName}_$method"
        if functions.contains(funcName) then
          // It's a real method — build self argument (need address for value structs)
          val selfArg = tObj.typ match
            case st @ StructType(_, _) =>
              tObj match
                case TVarRef(n, _) => TAddrOf(n, PtrType(st))
                case TFieldAccess(innerObj, idx, _) => TAddrOfField(innerObj, idx, PtrType(st))
                case TIndex(arr, idx, _) => TAddrOfIndex(arr, idx, PtrType(st))
                case _ => TTempAddr(tObj, PtrType(st))  // method on temporary — copy into temp cell
            case _ => tObj // PtrType or RefType — already a pointer
          val funInfo = functions(funcName)
          val checkedArgs = checkArgs(funcName, funInfo.params.tail, tArgs) // .tail skips self param
          TCall(funInfo.name, selfArg :: checkedArgs, funInfo.returnType)
        else if structToTemplate.contains(structName) && {
          val (templateName, _) = structToTemplate(structName)
          genericTemplates.contains(s"${templateName}_$method")
        } then
          // Generic struct method — instantiate from template
          val (templateName, _) = structToTemplate(structName)
          val templateFuncName = s"${templateName}_$method"
          val selfArg = tObj.typ match
            case st @ StructType(_, _) =>
              tObj match
                case TVarRef(n, _) => TAddrOf(n, PtrType(st))
                case TFieldAccess(innerObj, idx, _) => TAddrOfField(innerObj, idx, PtrType(st))
                case TIndex(arr, idx, _) => TAddrOfIndex(arr, idx, PtrType(st))
                case _ => TTempAddr(tObj, PtrType(st))
            case _ => tObj
          val allArgTypes = selfArg.typ :: tArgs.map(_.typ)
          val (mangled, funInfo) = instantiateGeneric(templateFuncName, allArgTypes)
          val checkedArgs = checkArgs(mangled, funInfo.params.tail, tArgs)
          TCall(mangled, selfArg :: checkedArgs, funInfo.returnType)
        else
          // Fall back to calling a function-typed field
          structType.fields.zipWithIndex.find(_._1._1 == method) match
            case Some(((_, FuncType(paramTypes, returnType)), idx)) =>
              val fieldAccess = TFieldAccess(tObj, idx, FuncType(paramTypes, returnType))
              val params = paramTypes.zipWithIndex.map { case (t, i) => (s"arg$i", t) }
              val checkedArgs = checkArgs(s"$structName.$method", params, tArgs)
              TIndirectCall(fieldAccess, checkedArgs, returnType)
            case Some(((_, other), _)) =>
              throw AnalysisError(s"field '$method' of struct $structName is $other, not a function")
            case None =>
              throw AnalysisError(s"struct $structName has no method or field '$method'")

      case CallAST(name, args) =>
        // Determine expected types for args if callee has known concrete signature
        val argExpected: List[Option[SyslType]] =
          if traitCallRewrite.contains(name) then
            val mangled = traitCallRewrite(name)
            functions(mangled).params.map(p => Some(p._2))
          else if functions.contains(name) || builtinFunctions.contains(name) then
            lookupFun(name).params.map(p => Some(p._2))
          else if structTypes.contains(name) then
            structTypes(name).fields.map(f => Some(f._2))
          else
            List.fill(args.length)(None)
        val tArgs = args.zip(argExpected.padTo(args.length, None)).map { case (a, exp) =>
          val saved = currentExpected
          currentExpected = exp.orElse(saved)
          try analyzeExpr(a) finally currentExpected = saved
        }
        // Check for trait-method-call rewrite (inside a synthesized default body)
        if traitCallRewrite.contains(name) then
          val mangled = traitCallRewrite(name)
          val funInfo = functions(mangled)
          val checkedArgs = checkArgs(mangled, funInfo.params, tArgs)
          TCall(mangled, checkedArgs, funInfo.returnType)
        else
        // Check if it's a direct function call or an indirect call through a variable
        if genericTemplates.contains(name) then
          val (mangled, funInfo) = instantiateGeneric(name, tArgs.map(_.typ))
          val checkedArgs = checkArgs(mangled, funInfo.params, tArgs)
          TCall(mangled, checkedArgs, funInfo.returnType)
        else if functions.contains(name) || builtinFunctions.contains(name) then
          warnDeprecated(name)
          val funInfo = lookupFun(name)
          if funInfo.isDef && funInfo.params.isEmpty && tArgs.nonEmpty then
            // Auto-call def, then indirect-call the result with the provided args
            val autoCall = TCall(funInfo.name, Nil, funInfo.returnType)
            funInfo.returnType match
              case FuncType(fParams, fRet) =>
                val paramPairs = fParams.zipWithIndex.map((t, i) => (s"_p$i", t))
                val checkedArgs = checkArgs(name, paramPairs, tArgs)
                TIndirectCall(autoCall, checkedArgs, fRet)
              case _ => throw AnalysisError(s"def '$name' returns ${funInfo.returnType}, not a callable type")
          else
            val checkedArgs = checkArgs(name, funInfo.params, tArgs)
            TCall(funInfo.name, checkedArgs, funInfo.returnType)
        else if structTypes.contains(name) then
          // Struct constructor: Point(10, 20)
          val st = structTypes(name)
          if tArgs.length != st.fields.length then
            throw AnalysisError(s"struct '${st.name}' has ${st.fields.length} field(s), got ${tArgs.length} argument(s)")
          val checkedArgs = tArgs.zip(st.fields).map { case (arg, (fieldName, fieldType)) =>
            val coerced = coerceLiteral(arg, fieldType)
            if !compatible(coerced.typ, fieldType) then
              throw AnalysisError(s"field '$fieldName' of '${st.name}' expects $fieldType, got ${coerced.typ}")
            coerced
          }
          TStructConstruct(st, checkedArgs)
        else if genericStructs.contains(name) then
          // Generic struct constructor: Pair(1, 2) — infer type args from argument types
          val template = genericStructs(name)
          if tArgs.length != template.fields.length then
            throw AnalysisError(s"generic struct '$name' has ${template.fields.length} field(s), got ${tArgs.length} argument(s)")
          val env = mutable.Map.empty[String, SyslType]
          for ((_, ftype), arg) <- template.fields.zip(tArgs) do
            unifyTypes(ftype, arg.typ, template.typeParams.toSet, env)
          for tp <- template.typeParams if !env.contains(tp) do
            throw AnalysisError(s"cannot infer type parameter '$tp' for generic struct '$name'")
          val inferredArgs = template.typeParams.map(env(_))
          val st = instantiateGenericStruct(name, inferredArgs)
          val checkedArgs = tArgs.zip(st.fields).map { case (arg, (fieldName, fieldType)) =>
            val coerced = coerceLiteral(arg, fieldType)
            if !compatible(coerced.typ, fieldType) then
              throw AnalysisError(s"field '$fieldName' of '${st.name}' expects $fieldType, got ${coerced.typ}")
            coerced
          }
          TStructConstruct(st, checkedArgs)
        else if variantToEnum.contains(name) then
          // Enum variant constructor: Circle(5)
          val (et, variantIdx) = variantToEnum(name)
          val (_, variantFields) = et.variants(variantIdx)
          if tArgs.length != variantFields.length then
            throw AnalysisError(s"variant '$name' has ${variantFields.length} field(s), got ${tArgs.length} argument(s)")
          val checkedArgs = tArgs.zip(variantFields).map { case (arg, (fieldName, fieldType)) =>
            val coerced = coerceLiteral(arg, fieldType)
            if !compatible(coerced.typ, fieldType) then
              throw AnalysisError(s"variant '$name' field '$fieldName' expects $fieldType, got ${coerced.typ}")
            coerced
          }
          TEnumConstruct(et, variantIdx, checkedArgs)
        else if genericVariantToEnum.contains(name) then
          // Generic enum variant constructor: Some(42) — infer T from args, then fall back to expected type
          val (enumName, variantIdx) = genericVariantToEnum(name)
          val template = genericEnums(enumName)
          val variant = template.variants(variantIdx)
          if variant.fields.length != tArgs.length then
            throw AnalysisError(s"variant '$name' has ${variant.fields.length} field(s), got ${tArgs.length} argument(s)")
          val env = mutable.Map.empty[String, SyslType]
          for ((_, ftype), arg) <- variant.fields.zip(tArgs) do
            unifyTypes(ftype, arg.typ, template.typeParams.toSet, env)
          // For any type params not inferred from args, try pulling them from currentExpected
          val missing = template.typeParams.filter(tp => !env.contains(tp))
          if missing.nonEmpty then
            currentExpected match
              case Some(SyslType.EnumType(expectedName, _)) if genericEnumInstantiations.exists { case ((n, _), et) => et.name == expectedName && n == enumName } =>
                val (_, expectedArgs) = genericEnumInstantiations.collectFirst { case ((n, args), et) if et.name == expectedName && n == enumName => (n, args) }.get
                for (tp, arg) <- template.typeParams.zip(expectedArgs) if !env.contains(tp) do
                  env(tp) = arg
              case _ => ()
          for tp <- template.typeParams if !env.contains(tp) do
            throw AnalysisError(s"cannot infer type parameter '$tp' for variant '$name' of generic enum '$enumName' — use explicit type annotation")
          val inferredArgs = template.typeParams.map(env(_))
          val et = instantiateGenericEnum(enumName, inferredArgs)
          val (_, variantFields) = et.variants(variantIdx)
          val checkedArgs = tArgs.zip(variantFields).map { case (arg, (fieldName, fieldType)) =>
            val coerced = coerceLiteral(arg, fieldType)
            if !compatible(coerced.typ, fieldType) then
              throw AnalysisError(s"variant '$name' field '$fieldName' expects $fieldType, got ${coerced.typ}")
            coerced
          }
          TEnumConstruct(et, variantIdx, checkedArgs)
        else
          // Try as a variable of FuncType
          val sym = lookup(name)
          sym.typ match
            case FuncType(paramTypes, returnType) =>
              val params = paramTypes.zipWithIndex.map { case (t, i) => (s"arg$i", t) }
              val checkedArgs = checkArgs(name, params, tArgs)
              TIndirectCall(TVarRef(name, sym.typ), checkedArgs, returnType)
            case other =>
              throw AnalysisError(s"'$name' is not a function (type: $other)")

      case TryAST(inner) =>
        val tInner = analyzeExpr(inner)
        val enumType = tInner.typ match
          case et: SyslType.EnumType => et
          case other => throw AnalysisError(s"'?' operator requires an enum type (Option/Result-style), got $other")
        if enumType.variants.length != 2 then
          throw AnalysisError(s"'?' operator requires a 2-variant enum, got ${enumType.variants.length} variants")
        val (successName, successFields) = enumType.variants(0)
        val (failureName, failureFields) = enumType.variants(1)
        if successFields.isEmpty then
          throw AnalysisError(s"'?' operator: first variant '$successName' must have at least 1 field")
        // Single field → unwrap to that type; multiple fields → unwrap to tuple
        val successType = if successFields.length == 1 then successFields(0)._2
          else SyslType.tupleType(successFields.map(_._2))
        // Verify the enclosing function's return type matches
        currentExpected match
          case Some(et: SyslType.EnumType) if et.name == enumType.name => ()
          case Some(other) =>
            throw AnalysisError(s"'?' on $enumType requires enclosing function to return $enumType, got $other")
          case None =>
            throw AnalysisError(s"'?' operator requires enclosing function with matching return type")
        // Build: match tInner { Success(v) -> v; Failure(e) -> return Failure(e) }
        val successBindNames = successFields.indices.map(i => s"_try_v$i").toList
        val failureBindNames = failureFields.indices.map(i => s"_try_e$i").toList
        // Failure arm: return Failure(e0, e1, ...)
        val failureReconstructArgs: List[TExpr] = failureBindNames.zip(failureFields).map {
          case (bindName, (_, ft)) => TVarRef(bindName, ft)
        }
        val failureReturnValue = TEnumConstruct(enumType, 1, failureReconstructArgs)
        val failureArm = TMatchArm(
          List(TVariantPattern(enumType, 1, failureBindNames.map(Some(_)), failureFields.map(_._2))),
          None,
          List(TReturnStmt(Some(failureReturnValue)))
        )
        // Success arm: unwrap single field or construct tuple
        val successExpr: TExpr = if successFields.length == 1 then
          TVarRef(successBindNames.head, successFields.head._2)
        else
          TStructConstruct(successType.asInstanceOf[SyslType.StructType],
            successBindNames.zip(successFields).map { case (name, (_, ft)) => TVarRef(name, ft) })
        val successArm = TMatchArm(
          List(TVariantPattern(enumType, 0, successBindNames.map(Some(_)), successFields.map(_._2))),
          None,
          List(TExprStmt(successExpr))
        )
        TMatchExpr(tInner, List(successArm, failureArm), None, successType)

      case IfExprAST(cond, thenBody, elseBody) =>
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"if condition must be bool, got ${tCond.typ}")
        pushScope()
        val tThen = analyzeBlock(thenBody)
        popScope()
        val tElse = elseBody.map { stmts => pushScope(); val r = analyzeBlock(stmts); popScope(); r }
        val resultType = tThen.lastOption match
          case Some(TExprStmt(e)) => e.typ
          case _ => VoidType
        TIfExpr(tCond, tThen, tElse, resultType)

      case MatchExprAST(scrutinee, arms, default) =>
        val tScrutinee = analyzeExpr(scrutinee)
        val tArms = arms.map { arm =>
          pushScope()
          val tPatterns = arm.patterns.map(p => analyzePattern(p, tScrutinee.typ))
          val tGuard = arm.guard.map { g =>
            val tg = analyzeExpr(g)
            if tg.typ != BoolType then throw AnalysisError(s"match guard must be bool, got ${tg.typ}")
            tg
          }
          val tBody = analyzeBlock(arm.body)
          popScope()
          TMatchArm(tPatterns, tGuard, tBody)
        }
        val tDefault = default.map { stmts => pushScope(); val r = analyzeBlock(stmts); popScope(); r }
        val resultType = tArms.headOption.flatMap(_.body.lastOption) match
          case Some(TExprStmt(e)) => e.typ
          case _ => VoidType
        TMatchExpr(tScrutinee, tArms, tDefault, resultType)

  private def analyzeInterpolatedString(s: String): TExpr =
    // Parse $name, ${expr}, $$ patterns in Scala-style interpolated string
    val parts = mutable.ArrayBuffer[Either[String, String]]() // Left = literal, Right = expression text
    val buf = new StringBuilder
    var i = 0
    while i < s.length do
      if s(i) == '$' then
        if i + 1 < s.length && s(i + 1) == '$' then
          // $$ → literal $
          buf += '$'
          i += 2
        else if i + 1 < s.length && s(i + 1) == '{' then
          // ${expr}
          if buf.nonEmpty then
            parts += Left(buf.toString)
            buf.clear()
          i += 2 // skip ${
          var depth = 1
          while i < s.length && depth > 0 do
            if s(i) == '{' then depth += 1
            else if s(i) == '}' then depth -= 1
            if depth > 0 then buf += s(i)
            i += 1
          if depth != 0 then throw AnalysisError("unterminated '${' in string interpolation")
          parts += Right(buf.toString.trim)
          buf.clear()
        else if i + 1 < s.length && (s(i + 1).isLetter || s(i + 1) == '_') then
          // $name — identifier chars
          if buf.nonEmpty then
            parts += Left(buf.toString)
            buf.clear()
          i += 1 // skip $
          while i < s.length && (s(i).isLetterOrDigit || s(i) == '_') do
            buf += s(i)
            i += 1
          parts += Right(buf.toString)
          buf.clear()
        else
          // Lone $ at end or before non-identifier — literal
          buf += '$'
          i += 1
      else
        buf += s(i)
        i += 1
    if buf.nonEmpty then parts += Left(buf.toString)

    // Build a chain of TBinary("+") on StringType
    val parser = new SyslParser
    val tExprs: List[TExpr] = parts.toList.map {
      case Left(lit) => TStringLit(lit, SyslType.StringType)
      case Right(exprText) =>
        parser.parseExpression(exprText) match
          case Right(ast) =>
            val analyzed = analyzeExpr(ast)
            analyzed.typ match
              case SyslType.StringType => analyzed
              case t if t.isNumeric || t == SyslType.BoolType || t == SyslType.DoubleType => TStr(analyzed)
              case t => throw AnalysisError(s"cannot interpolate value of type $t into string")
          case Left(err) => throw AnalysisError(s"parse error in string interpolation: $err")
    }
    tExprs.reduceLeft((l, r) => TBinary(l, "+", r, SyslType.StringType))

  private def parseFmtSpec(s: String, pos: Int): (FmtSpec, Int) =
    var i = pos
    if i >= s.length || s(i) != '%' then return (FmtSpec('s'), pos)
    i += 1
    var zeroPad = false
    var leftAlign = false
    var showSign = false
    var parsing = true
    while i < s.length && parsing do
      s(i) match
        case '0' => zeroPad = true; i += 1
        case '-' => leftAlign = true; i += 1
        case '+' => showSign = true; i += 1
        case _   => parsing = false
    var width = 0
    while i < s.length && s(i).isDigit do
      width = width * 10 + (s(i) - '0')
      i += 1
    if i >= s.length then throw AnalysisError("missing verb in format spec")
    val verb = s(i)
    i += 1
    val upperCase = verb == 'X'
    val normalVerb = verb.toLower match
      case v @ ('d' | 'x' | 'o' | 'b' | 's' | 'c') => v
      case _ => if verb == 'X' then 'x' else throw AnalysisError(s"unknown format verb '%$verb'")
    (FmtSpec(normalVerb, width, zeroPad, leftAlign, showSign, upperCase), i)

  private def analyzeFormattedString(s: String): TExpr =
    val parts = mutable.ArrayBuffer[TExpr]()
    val buf = new StringBuilder
    var i = 0
    def flushLiteral(): Unit =
      if buf.nonEmpty then
        parts += TStringLit(buf.toString, SyslType.StringType)
        buf.clear()
    while i < s.length do
      if s(i) == '$' then
        if i + 1 < s.length && s(i + 1) == '$' then
          buf += '$'; i += 2
        else if i + 1 < s.length && s(i + 1) == '{' then
          flushLiteral()
          i += 2
          var depth = 1
          val exprBuf = new StringBuilder
          while i < s.length && depth > 0 do
            if s(i) == '{' then depth += 1
            else if s(i) == '}' then depth -= 1
            if depth > 0 then exprBuf += s(i)
            i += 1
          if depth != 0 then throw AnalysisError("unterminated '${' in f-string")
          val (spec, newI) = parseFmtSpec(s, i)
          i = newI
          val parser = new SyslParser
          parser.parseExpression(exprBuf.toString.trim) match
            case Right(ast) => parts += wrapWithFmtSpec(analyzeExpr(ast), spec)
            case Left(err) => throw AnalysisError(s"parse error in f-string: $err")
        else if i + 1 < s.length && (s(i + 1).isLetter || s(i + 1) == '_') then
          flushLiteral()
          i += 1
          val nameBuf = new StringBuilder
          while i < s.length && (s(i).isLetterOrDigit || s(i) == '_') do
            nameBuf += s(i); i += 1
          val (spec, newI) = parseFmtSpec(s, i)
          i = newI
          val parser = new SyslParser
          parser.parseExpression(nameBuf.toString) match
            case Right(ast) => parts += wrapWithFmtSpec(analyzeExpr(ast), spec)
            case Left(err) => throw AnalysisError(s"parse error in f-string: $err")
        else
          buf += '$'; i += 1
      else if s(i) == '%' && i + 1 < s.length && s(i + 1) == '%' then
        buf += '%'; i += 2
      else
        buf += s(i); i += 1
    flushLiteral()
    if parts.isEmpty then TStringLit("", SyslType.StringType)
    else parts.toList.reduceLeft((l, r) => TBinary(l, "+", r, SyslType.StringType))

  private def wrapWithFmtSpec(expr: TExpr, spec: FmtSpec): TExpr =
    spec.verb match
      case 'd' | 'x' | 'o' | 'b' =>
        if !expr.typ.isNumeric then
          throw AnalysisError(s"format verb '%${spec.verb}' requires numeric type, got ${expr.typ}")
        if spec.verb == 'd' && spec.width == 0 && !spec.zeroPad && !spec.leftAlign && !spec.showSign then TStr(expr)
        else TFmtStr(expr, spec)
      case 's' =>
        if expr.typ == SyslType.StringType then
          if spec.width == 0 && !spec.leftAlign then expr else TFmtStr(expr, spec)
        else if expr.typ.isNumeric || expr.typ == SyslType.BoolType || expr.typ == SyslType.DoubleType then
          if spec.width == 0 && !spec.leftAlign then TStr(expr) else TFmtStr(TStr(expr), spec)
        else throw AnalysisError(s"cannot format value of type ${expr.typ} with %s")
      case 'c' =>
        if !expr.typ.isNumeric then
          throw AnalysisError(s"format verb '%c' requires numeric type, got ${expr.typ}")
        TFmtStr(expr, spec)
      case _ => throw AnalysisError(s"unknown format verb '%${spec.verb}'")
