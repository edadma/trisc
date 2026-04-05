package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

class SyslAnalyzer:
  case class AnalysisError(msg: String, node: Any = null) extends RuntimeException(msg)

  private case class SymInfo(name: String, typ: SyslType, mutable: Boolean)
  private case class FunInfo(name: String, params: List[(String, SyslType)], returnType: SyslType)

  private val globalScope = new mutable.LinkedHashMap[String, SymInfo]
  private val functions = new mutable.LinkedHashMap[String, FunInfo]
  private val structTypes = new mutable.LinkedHashMap[String, SyslType.StructType]
  private val enumTypes = new mutable.LinkedHashMap[String, Map[String, Long]]  // enum name → (member name → value)
  private val dataEnumTypes = new mutable.LinkedHashMap[String, SyslType.EnumType]  // data enum name → EnumType
  private val variantToEnum = new mutable.LinkedHashMap[String, (SyslType.EnumType, Int)]  // variant name → (enum type, variant index)
  private val typeAliases = new mutable.LinkedHashMap[String, TypeAST]  // alias name → target type AST
  private val methods = new mutable.LinkedHashMap[String, mutable.Set[String]]  // struct name → set of method names
  private val deprecations = new mutable.LinkedHashMap[String, Option[String]]  // name → optional reason
  private val warnedDeprecations = new mutable.HashSet[String]
  private val externalSymbols = new mutable.LinkedHashSet[String]
  private var scopeStack: mutable.ArrayBuffer[mutable.LinkedHashMap[String, SymInfo]] = null
  private var loopDepth: Int = 0

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

  // Expected type for bidirectional inference (used by generic variant constructors)
  private var currentExpected: Option[SyslType] = None

  // Operator desugaring: operator → (trait name, method name). Requires user to define
  // the traits and provide impls for their types.
  private val operatorToTrait: Map[String, (String, String)] = Map(
    "<"  -> ("Ord", "lt"),  "<=" -> ("Ord", "le"),
    ">"  -> ("Ord", "gt"),  ">=" -> ("Ord", "ge"),
    "==" -> ("Eq",  "eq"),  "!=" -> ("Eq",  "ne"),
    "+"  -> ("Add", "add"), "-"  -> ("Sub", "sub"),
    "*"  -> ("Mul", "mul"), "/"  -> ("Div", "div"),
  )

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
  )

  def registerImport(meta: ModuleMeta, selectors: List[ImportSelector] = List(WildcardImport)): Unit =
    val selectedSymbols = selectors match
      case List(WildcardImport) => meta.publicSymbols
      case named =>
        val nameMap = named.collect { case NamedImport(n, r) => (n, r) }.toMap
        meta.publicSymbols.filter(sym => nameMap.contains(sym.name)).map { sym =>
          nameMap(sym.name) match
            case Some(alias) => sym.copy(name = alias)
            case None => sym
        }
    for sym <- selectedSymbols do
      sym.typ match
        case SymbolMeta.Kind.Func(params, returnType) =>
          val paramPairs = params.zipWithIndex.map((t, i) => (s"_p$i", t))
          if functions.contains(sym.name) then
            if !sym.isExtern then
              throw AnalysisError(s"imported symbol '${sym.name}' conflicts with existing function")
          else
            functions(sym.name) = FunInfo(sym.name, paramPairs, returnType)
            externalSymbols += sym.name
        case SymbolMeta.Kind.Data(dataType) =>
          if globalScope.contains(sym.name) then
            if !sym.isExtern then
              throw AnalysisError(s"imported symbol '${sym.name}' conflicts with existing global")
          else
            globalScope(sym.name) = SymInfo(sym.name, dataType, mutable = false)
            externalSymbols += sym.name
        case SymbolMeta.Kind.Struct(st) =>
          structTypes(sym.name) = st

  def isExternal(name: String): Boolean = externalSymbols.contains(name)
  def externals: Set[String] = externalSymbols.toSet

  def analyze(program: ProgramAST): TProgram =
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
            if genericStructs.contains(name) || structTypes.contains(name) then
              throw AnalysisError(s"duplicate struct: '$name'", decl)
            genericStructs(name) = sd
          else
            if structTypes.contains(name) || genericStructs.contains(name) then throw AnalysisError(s"duplicate struct: '$name'", decl)
            val resolvedFields = fields.map((n, t) => (n, resolveType(t)))
            structTypes(name) = SyslType.StructType(name, resolvedFields)
        case fd @ FunDeclAST(name, params, returnType, _, _, typeParams, _, _) =>
          // Duplicate-parameter-name check.
          val seenParams = mutable.HashSet[String]()
          for p <- params do
            if !seenParams.add(p.name) then
              val friendly = if p.name == "__self__"
                then "method '$name' already has an implicit 'self' parameter — remove the explicit 'self: *Type' declaration"
                else s"duplicate parameter name '${p.name}' in function '$name'"
              throw AnalysisError(friendly, decl)
          if typeParams.nonEmpty then
            // Generic function: store as template, don't resolve types yet
            if genericTemplates.contains(name) || functions.contains(name) then
              throw AnalysisError(s"duplicate function: '$name'", decl)
            genericTemplates(name) = fd
          else
            val paramTypes = params.map(p => (p.name, resolveType(p.typ)))
            val retType = returnType.map(resolveType).getOrElse(VoidType)
            if functions.contains(name) || genericTemplates.contains(name) then
              throw AnalysisError(s"duplicate function: '$name'", decl)
            functions(name) = FunInfo(name, paramTypes, retType)
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
        case de @ DataEnumDeclAST(name, variants, typeParams, _) =>
          if typeParams.nonEmpty then
            // Generic enum: store template, don't resolve fields
            if genericEnums.contains(name) || dataEnumTypes.contains(name) || enumTypes.contains(name) then
              throw AnalysisError(s"duplicate enum: '$name'", decl)
            genericEnums(name) = de
            // Register bare variant names for inference at construction sites
            for (EnumVariantAST(vname, _), idx) <- variants.zipWithIndex do
              if genericVariantToEnum.contains(vname) || variantToEnum.contains(vname) then
                throw AnalysisError(s"duplicate variant name: '$vname'")
              genericVariantToEnum(vname) = (name, idx)
          else
            if dataEnumTypes.contains(name) || enumTypes.contains(name) || genericEnums.contains(name) then
              throw AnalysisError(s"duplicate enum: '$name'", decl)
            val resolvedVariants = variants.map { case EnumVariantAST(vname, fields) =>
              val resolvedFields = fields.map((fname, ftype) => (fname, resolveType(ftype)))
              (vname, resolvedFields)
            }
            val et: SyslType.EnumType = SyslType.EnumType(name, resolvedVariants)
            dataEnumTypes(name) = et
            for ((vname, _), idx) <- resolvedVariants.zipWithIndex do
              variantToEnum(vname) = (et, idx)
        case TypeAliasDeclAST(name, target, _) =>
          if typeAliases.contains(name) then throw AnalysisError(s"duplicate type alias: '$name'", decl)
          typeAliases(name) = target
        case TraitDeclAST(name, tparam, methods, _) =>
          if traits.contains(name) then throw AnalysisError(s"duplicate trait: '$name'", decl)
          // Check no duplicate method names within the trait
          val methodNames = methods.map(_.name)
          if methodNames.distinct.length != methodNames.length then
            throw AnalysisError(s"duplicate method names in trait '$name'")
          traits(name) = TraitInfo(name, tparam, methods)
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
              val mangled = s"${traitName}_${traitMethod.name}_${typeMangled}"
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

      case TypeAliasDeclAST(name, target, _) =>
        TTypeAliasDecl(name, resolveType(target))

      case fdAst @ FunDeclAST(name, params, _, body, isPrivate, _, _, attrs) =>
        scopeStack = new mutable.ArrayBuffer
        pushScope()
        val funInfo = functions(name)
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
        val tParams = funInfo.params.map((n, t) => TParam(n, t))
        scopeStack = null
        validateTestAttr(fdAst, funInfo)
        TFunDecl(name, tParams, funInfo.returnType, tBody, isPrivate, attrs)

      case VarDeclAST(name, typOpt, init, isPrivate, isMutable, _) =>
        scopeStack = new mutable.ArrayBuffer
        pushScope()
        val tInit0 = analyzeExpr(init)
        val declType = typOpt.map(resolveType).getOrElse(tInit0.typ)
        val tInit = coerceLiteral(tInit0, declType)
        globalScope(name) = SymInfo(name, declType, isMutable)
        scopeStack = null
        TVarDecl(name, declType, tInit, isPrivate)

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
          throw AnalysisError(s"#test function '${fd.name}' must return void", fd)
        if fd.typeParams.nonEmpty then
          throw AnalysisError(s"#test function '${fd.name}' cannot be generic", fd)
        // Methods are registered via the StructName_methodName convention; reject those
        val underscoreIdx = fd.name.indexOf('_')
        if underscoreIdx > 0 && fd.params.nonEmpty && fd.params.head.name == "__self__" then
          throw AnalysisError(s"#test cannot be applied to a method ('${fd.name}')", fd)

  private def resolveType(t: TypeAST): SyslType = t match
    case NamedTypeAST(name, typeArgs) if typeArgs.nonEmpty =>
      val resolved = typeArgs.map(resolveType)
      if genericStructs.contains(name) then instantiateGenericStruct(name, resolved)
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
      case other => throw AnalysisError(s"unknown type: '$other'")
    case PtrTypeAST(inner) => PtrType(resolveType(inner))
    case ArrayTypeAST(size, elem) => ArrayType(resolveType(elem), size)
    case SliceTypeAST(elem) => SliceType(resolveType(elem))
    case TupleTypeAST(elems) => SyslType.tupleType(elems.map(resolveType))
    case FuncTypeAST(params, ret) => FuncType(params.map(resolveType), resolveType(ret))
    case RefTypeAST(inner) => RefType(resolveType(inner))

  private def compatible(from: SyslType, to: SyslType): Boolean =
    (from, to) match
      case (a, b) if a == b => true
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
      case (RefType(a), RefType(b)) if a == b => true      // same ref type
      case (RefType(inner), PtrType(_)) => true             // &T → *U (ref decays to pointer)
      case _ => false

  // Coerce integer literals to the target type (like Rust's untyped integer literals)
  private def coerceLiteral(expr: TExpr, target: SyslType): TExpr =
    expr match
      case TIntLit(value, _) if target.isIntegral => TIntLit(value, target)
      case TIntLit(0, _) if target.isInstanceOf[PtrType] => TIntLit(0, target) // null pointer
      case _ => expr

  // Coerce integer literals to match the target's signedness only (preserving original width)
  private def coerceSignedness(expr: TExpr, target: SyslType): TExpr =
    (expr, target) match
      case (TIntLit(value, IntType(w)), _: UIntType) => TIntLit(value, UIntType(w))
      case (TIntLit(value, UIntType(w)), _: IntType) => TIntLit(value, IntType(w))
      case _ => expr

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
    case StructType(n, _) => n
    case EnumType(n, _)   => n

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
      case NamedTypeAST(name, tArgs) if tArgs.nonEmpty => arg match
        case SyslType.StructType(argName, _) =>
          structToTemplate.get(argName) match
            case Some((templateName, concreteArgs)) if templateName == name && concreteArgs.length == tArgs.length =>
              for (p, a) <- tArgs.zip(concreteArgs) do unifyTypes(p, a, typeParams, env)
            case _ => ()
        case _ => ()
      case _ => () // concrete parameter type, nothing to infer

  // Instantiate a generic function with inferred type arguments, returning the mangled name
  // and FunInfo of the instantiated function. Reuses cached instantiations.
  // If an operator has a user-defined struct/enum operand, desugar to the corresponding trait call.
  // Returns None if no desugaring applies (use built-in dispatch).
  private def tryOperatorDispatch(op: String, tLeft: TExpr, tRight: TExpr): Option[TExpr] =
    operatorToTrait.get(op) match
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
    (mangled, functions(mangled))

  private def instantiateGeneric(name: String, argTypes: List[SyslType]): (String, FunInfo) =
    val template = genericTemplates(name)
    val typeParams = template.typeParams
    // Infer type arguments
    val env = mutable.Map.empty[String, SyslType]
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
        if functions.contains(mangled) then
          throw AnalysisError(s"generic instantiation '$mangled' collides with existing function")
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
          val tBody = template.body match
            case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
            case BlockBodyAST(stmts) => TBlockBody(analyzeBlock(stmts))
          scopeStack = savedScopeStack
          loopDepth = savedLoopDepth
          val tParams = paramTypes.map((n, t) => TParam(n, t))
          specializedDecls += TFunDecl(mangled, tParams, retType, tBody, template.isPrivate)
          (mangled, funInfo)
        finally
          typeEnv = savedEnv

  private def checkArgs(name: String, params: List[(String, SyslType)], args: List[TExpr]): List[TExpr] =
    if args.length != params.length then
      throw AnalysisError(s"function '$name' expects ${params.length} argument(s), got ${args.length}")
    args.zip(params).map { case (arg, (pName, pType)) =>
      val coerced = coerceLiteral(arg, pType)
      if !compatible(coerced.typ, pType) then
        throw AnalysisError(s"argument '$pName' of '$name' expects $pType, got ${coerced.typ}")
      // Insert explicit cast for string→*i8 decay so codegen can handle it
      (coerced.typ, pType) match
        case (StringType, PtrType(I8 | U8)) => TCast(coerced, pType)
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
        val tInit = coerceLiteral(tInit0, declType)
        if typOpt.isDefined && !compatible(tInit.typ, declType) then
          throw AnalysisError(s"cannot assign ${tInit.typ} to $declType variable '$name'")
        // `_` is a discard binding: evaluate the initializer for its side effects
        // but don't bind any name. Multiple `_`s in the same scope don't collide.
        if name == "_" then
          TExprStmt(tInit)
        else
          if scopeStack != null then
            currentScope(name) = SymInfo(name, declType, isMutable)
          TVarStmt(name, declType, tInit)

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
        // Check if this is declaration or assignment (when no val/var prefix)
        val existingCount = names.count(n => tryLookup(n).isDefined)
        if isMutable || existingCount == 0 then
          // Declaration: create new variables
          for (name, (_, fieldType)) <- names.zip(st.fields) do
            if scopeStack != null then
              currentScope(name) = SymInfo(name, fieldType, isMutable)
          TDestructureStmt(names, st.fields.map(_._2), tInit)
        else if existingCount == names.length then
          // All exist: parallel assignment
          for name <- names do
            val sym = lookup(name)
            if !sym.mutable then throw AnalysisError(s"cannot assign to immutable variable '$name'")
          TDestructureAssignStmt(names, st.fields.map(_._2), tInit)
        else
          throw AnalysisError(s"cannot mix declared and undeclared names in destructuring")

      case AssignStmtAST(target, value) =>
        val tValue = analyzeExpr(value)
        val sym = lookupOrCreate(target, tValue.typ)
        if !sym.mutable then throw AnalysisError(s"cannot assign to immutable variable '$target'")
        TAssignStmt(target, tValue)

      case CompoundAssignStmtAST(target, op, value) =>
        val sym = lookup(target)
        if !sym.mutable then throw AnalysisError(s"cannot assign to immutable variable '$target'")
        val tValue = analyzeExpr(value)
        TCompoundAssignStmt(target, op, tValue)

      case DerefAssignStmtAST(pointer, value) =>
        val tPointer = analyzeExpr(pointer)
        val tValue = analyzeExpr(value)
        TDerefAssignStmt(tPointer, tValue)

      case IndexAssignStmtAST(array, index, value) =>
        val tArray = analyzeExpr(array)
        val tIndex = analyzeExpr(index)
        val tValue = analyzeExpr(value)
        TIndexAssignStmt(tArray, tIndex, tValue)

      case FieldAssignStmtAST(obj, field, value) =>
        val tObj = analyzeExpr(obj)
        val tValue = analyzeExpr(value)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldAssignStmt(resolvedObj, idx, tValue)

      case FieldCompoundAssignStmtAST(obj, field, op, value) =>
        val tObj = analyzeExpr(obj)
        val tValue = analyzeExpr(value)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
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
        val elemType = tElems.head.typ
        TArrayLit(tElems, SyslType.ArrayType(elemType, tElems.length))

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
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPreInc(resolvedObj, idx, structType.fields(idx)._2)

      case FieldPreDecAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPreDec(resolvedObj, idx, structType.fields(idx)._2)

      case FieldPostIncAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPostInc(resolvedObj, idx, structType.fields(idx)._2)

      case FieldPostDecAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPostDec(resolvedObj, idx, structType.fields(idx)._2)

      case NewArrayAST(size, elemTypeAST) =>
        val tSize = analyzeExpr(size)
        val elemType = resolveType(elemTypeAST)
        TNewArray(elemType, tSize)

      case NewExprAST(typeName, args) =>
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
          case _ => throw AnalysisError(s"'new' requires a struct type, got $t")

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
          TFuncRef(name, FuncType(f.params.map(_._2), f.returnType))
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
                TVarRef(name, sym.typ)

      case AddrOfAST(name) =>
        val sym = lookup(name)
        TAddrOf(name, PtrType(sym.typ))

      case AddrOfFieldAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot take address of field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TAddrOfField(resolvedObj, idx, PtrType(structType.fields(idx)._2))

      case AddrOfIndexAST(array, index) =>
        val tArray = analyzeExpr(array)
        val tIndex = analyzeExpr(index)
        val elemType = tArray.typ match
          case ArrayType(elem, _) => elem
          case PtrType(elem) => elem
          case _ => throw AnalysisError(s"cannot take address of index on ${tArray.typ}")
        TAddrOfIndex(tArray, tIndex, PtrType(elemType))

      case DerefAST(inner) =>
        val tInner = analyzeExpr(inner)
        val resultType = tInner.typ match
          case PtrType(t) => t
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
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case RefType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldAccess(resolvedObj, idx, structType.fields(idx)._2)

      case PreIncAST(name) => TPreInc(name, lookup(name).typ)
      case PreDecAST(name) => TPreDec(name, lookup(name).typ)
      case PostIncAST(name) => TPostInc(name, lookup(name).typ)
      case PostDecAST(name) => TPostDec(name, lookup(name).typ)

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
          case (from, BoolType) if from.isNumeric => // numeric to bool: != 0
          case (BoolType, to) if to.isNumeric => // bool to numeric: true=1, false=0
          case (from, DoubleType) if from.isIntegral => // int to float (cvt)
          case (DoubleType, to) if to.isIntegral => // float to int (fint)
          case (from, to) if from.isIntegral && to.isIntegral => // integer to integer (including signed↔unsigned)
          case (_: PtrType, to) if to.isIntegral => // pointer to integer
          case (from, _: PtrType) if from.isIntegral => // integer to pointer
          case (StringType, PtrType(I8 | U8)) => // string to *i8/*u8 decay
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

      case MethodCallAST(VarRefAST(name), method, args) if traits.contains(name) =>
        // Trait method call: Ord.cmp(a, b)
        val tArgs = args.map(analyzeExpr)
        val (mangled, funInfo) = analyzeTraitCall(name, method, tArgs)
        val checkedArgs = checkArgs(mangled, funInfo.params, tArgs)
        TCall(mangled, checkedArgs, funInfo.returnType)

      case MethodCallAST(obj, method, args) =>
        val tObj = analyzeExpr(obj)
        val tArgs = args.map(analyzeExpr)
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
                case _ => throw AnalysisError(s"cannot take address of expression for method call")
            case _ => tObj // PtrType or RefType — already a pointer
          val funInfo = functions(funcName)
          val checkedArgs = checkArgs(funcName, funInfo.params.tail, tArgs) // .tail skips self param
          TCall(funcName, selfArg :: checkedArgs, funInfo.returnType)
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
          val checkedArgs = checkArgs(name, funInfo.params, tArgs)
          TCall(name, checkedArgs, funInfo.returnType)
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
        if successFields.length != 1 then
          throw AnalysisError(s"'?' operator: first variant '$successName' must have exactly 1 field, got ${successFields.length}")
        val successType = successFields(0)._2
        // Verify the enclosing function's return type matches
        currentExpected match
          case Some(et: SyslType.EnumType) if et.name == enumType.name => ()
          case Some(other) =>
            throw AnalysisError(s"'?' on $enumType requires enclosing function to return $enumType, got $other")
          case None =>
            throw AnalysisError(s"'?' operator requires enclosing function with matching return type")
        // Build: match tInner { Success(v) -> v; Failure(e) -> return Failure(e) }
        val successBindName = "_try_v"
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
        val successArm = TMatchArm(
          List(TVariantPattern(enumType, 0, List(Some(successBindName)), List(successType))),
          None,
          List(TExprStmt(TVarRef(successBindName, successType)))
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
