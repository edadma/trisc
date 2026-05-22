package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

trait SyslAnalyzerTypes:
  self: SyslAnalyzer =>

  protected def resolveType(t: TypeAST): SyslType = t match
    case NamedTypeAST(name, typeArgs) if typeArgs.nonEmpty =>
      val resolved = typeArgs.map(resolveType)
      if genericTypeAliases.contains(name) then
        val (tparams, target, isNew) = genericTypeAliases(name)
        val defaults = genericTypeAliasDefaults.getOrElse(name, Map.empty)
        val bounds = genericTypeAliasBounds.getOrElse(name, Map.empty)
        val filled = fillTypeArgsFromDefaults(name, "type alias", tparams, defaults, resolved)
        if isNew then instantiateGenericNominalAlias(name, tparams, target, filled)
        else
          // Transparent alias: enforce bounds + bring assoc bindings into scope so
          // the body can reference projections like `T::Item`. The nominal path
          // (instantiateGenericNominalAlias) does the same.
          enforceBoundsAndBuildAssocs(s"type alias '$name'", tparams, filled, bounds) match
            case Left(err) => throw AnalysisError(err)
            case Right(_)  => ()
          val savedEnv = typeEnv
          val savedAssocs = assocBindingsEnv
          typeEnv = typeEnv ++ tparams.zip(filled).toMap
          assocBindingsEnv = buildAssocBindingsEnv(tparams, filled, bounds)
          val result =
            try resolveType(target)
            finally
              typeEnv = savedEnv
              assocBindingsEnv = savedAssocs
          result
      else if genericStructs.contains(name) then instantiateGenericStruct(name, resolved)
      else if genericEnums.contains(name) then instantiateGenericEnum(name, resolved)
      else throw AnalysisError(s"'$name' is not a generic type")
    // Phase B — bare reference to a generic alias/struct/enum (no `[...]`).
    // Only resolves if every type parameter has a default; otherwise we fall
    // through and let later cases produce the appropriate diagnostic.
    case NamedTypeAST(name, Nil) if !typeEnv.contains(name) && genericTypeAliases.contains(name)
        && {
          val (tparams, _, _) = genericTypeAliases(name)
          val defaults = genericTypeAliasDefaults.getOrElse(name, Map.empty)
          tparams.nonEmpty && tparams.forall(defaults.contains)
        } =>
      val (tparams, target, isNew) = genericTypeAliases(name)
      val defaults = genericTypeAliasDefaults(name)
      val bounds = genericTypeAliasBounds.getOrElse(name, Map.empty)
      val filled = fillTypeArgsFromDefaults(name, "type alias", tparams, defaults, Nil)
      if isNew then instantiateGenericNominalAlias(name, tparams, target, filled)
      else
        enforceBoundsAndBuildAssocs(s"type alias '$name'", tparams, filled, bounds) match
          case Left(err) => throw AnalysisError(err)
          case Right(_)  => ()
        val savedEnv = typeEnv
        val savedAssocs = assocBindingsEnv
        typeEnv = typeEnv ++ tparams.zip(filled).toMap
        assocBindingsEnv = buildAssocBindingsEnv(tparams, filled, bounds)
        val result =
          try resolveType(target)
          finally
            typeEnv = savedEnv
            assocBindingsEnv = savedAssocs
        result
    case NamedTypeAST(name, Nil) if !typeEnv.contains(name) && genericStructs.contains(name)
        && {
          val st = genericStructs(name)
          st.typeParams.nonEmpty && st.typeParams.forall(st.typeParamDefaults.contains)
        } =>
      instantiateGenericStruct(name, Nil)
    case NamedTypeAST(name, Nil) if !typeEnv.contains(name) && genericEnums.contains(name)
        && {
          val en = genericEnums(name)
          en.typeParams.nonEmpty && en.typeParams.forall(en.typeParamDefaults.contains)
        } =>
      instantiateGenericEnum(name, Nil)
    case NamedTypeAST(name, _) if typeEnv.contains(name) => typeEnv(name)
    case NamedTypeAST(name, _) => name match
      case "int" | "i32" => I32
      case "uint" | "u32" => U32
      case "long" | "i64" => I64
      case "ulong" | "u64" => U64
      case "char" => U32
      case "double" | "f64" => F64
      case "float" | "f32"  => F32
      case "byte" | "u8"  => U8
      case "i8"  => I8
      case "short" | "i16"  => I16
      case "ushort" | "u16"  => U16
      case "bool" => BoolType
      case "unit" => UnitType
      case "string" => StringType
      case name if typeAliases.contains(name) =>
        resolvedNamedTypes.getOrElseUpdate(name, {
          val (target, isNew, rangeAst, predAst) = typeAliases(name)
          val base = resolveType(target)
          val tr = rangeAst.map(ra => evalRangeBound(name, ra, base))
          val predFunc = predAst.map(pe => materializePredicateFunc(name, pe, base))
          if isNew || tr.nonEmpty || predFunc.nonEmpty then NamedType(name, base, isNew, tr, predFunc)
          else base
        })
      case name if structTypes.contains(name) => structTypes(name)
      case name if dataEnumTypes.contains(name) => dataEnumTypes(name)
      case name if simpleEnumTypes.contains(name) => simpleEnumTypes(name)
      case name if interfaceTypes.contains(name) => interfaceTypes(name)
      case other => throw AnalysisError(s"unknown type: '$other'")
    case PtrTypeAST(inner) => PtrType(resolveType(inner))
    case PtrNonNullTypeAST(inner) =>
      val innerType = resolveType(inner)
      val base = PtrType(innerType)
      val aliasName = s"NonNull_${SyslType.mangleType(innerType)}"
      val funcName = s"__pred_$aliasName"
      if !functions.contains(funcName) then
        functions(funcName) = FunInfo(funcName, List(("value", base)), base)
        val nullLit = TIntLit(0L, base)
        val body = TBlockBody(List(
          contract("type predicate",
            TBinary(TVarRef("value", base), "!=", nullLit, BoolType),
            s"not null pointer"),
          TExprStmt(TVarRef("value", base))
        ))
        specializedDecls += TFunDecl(funcName, List(TParam("value", base)), base, body)
      NamedType(aliasName, base, nominal = false, range = None, predicateFunc = Some(funcName))
    case ArrayTypeAST(size, elem) => ArrayType(resolveType(elem), size)
    case SliceTypeAST(elem) => SliceType(resolveType(elem))
    case TupleTypeAST(elems) => SyslType.tupleType(elems.map(resolveType))
    case ByNameTypeAST(inner) =>
      // `=> T` is parser-restricted to parameter position. The function-decl
      // collector also peels the marker explicitly so it can record which
      // params are by-name. This branch handles the remaining cases (trait
      // method param patterns, impl method patterns, etc.) by silently
      // converting to `() -> T` — the underlying storage type. The grammar
      // rules out any other position; if it ever shows up elsewhere, it
      // behaves as a zero-arg function, which is the safe interpretation.
      FuncType(Nil, resolveType(inner), effects = FuncEffects.Unknown)
    case ProjectionTypeAST(qualifier, member) =>
      // Phase A2: associated-type projection in type position. Inside an impl
      // method monomorphization, `assocBindingsEnv` carries the impl's
      // `type Item = X` bindings; we look up by member name. The qualifier
      // (typically `Self`, but also any of the trait's type parameters) is
      // tolerated as long as it names something currently in scope —
      // typeEnv (a trait/impl type param) or the literal `Self`. Phase A3
      // generalizes this to use sites where `T: SomeTrait` is a generic-fn
      // bound; for now, projection outside an impl context is rejected.
      if assocBindingsEnv.contains(member) then
        // Validate qualifier is meaningful in current scope. `Self` always
        // works; any other ident must currently be a type parameter
        // (i.e. in typeEnv) — that filters out typos like `Foo::Item`
        // when `Foo` isn't a trait param.
        if qualifier != "Self" && !typeEnv.contains(qualifier) then
          throw AnalysisError(s"associated-type projection '$qualifier::$member': '$qualifier' is not a known type parameter or 'Self' in this context")
        assocBindingsEnv(member)
      else if assocBindingsEnv.isEmpty then
        throw AnalysisError(s"associated-type projection '$qualifier::$member' has no impl context; projection at use sites is not yet supported (Phase A3)")
      else
        throw AnalysisError(s"associated type '$member' not bound in current impl (available: ${assocBindingsEnv.keys.toList.sorted.mkString(", ")})")
    case FuncTypeAST(params, ret, esc, eff) =>
      // Resolve raw names in #reads/#writes through globalScope to mangled form so subset
      // checks at indirect-call sites compare apples-to-apples with the caller's #reads/#writes
      // (which are also stored mangled by `resolveEffects`). Pure/Unknown carry no names.
      val resolvedEff = if eff.isPure || (eff.reads.isEmpty && eff.writes.isEmpty) then eff
        else
          def resolveOne(n: String, kind: String): String =
            globalScope.get(n) match
              case Some(sym) if sym.mutable && !sym.isConst => sym.name
              case Some(_) => throw AnalysisError(s"#$kind on function type references '$n' which is not mutable")
              case None    => throw AnalysisError(s"#$kind on function type references unknown global '$n'")
          val r = eff.reads.map(_.map(resolveOne(_, "reads")))
          val w = eff.writes.map(_.map(resolveOne(_, "writes")))
          FuncEffects(eff.isPure, r, w)
      FuncType(params.map(resolveType), resolveType(ret), esc, resolvedEff)
    case RefTypeAST(inner) => RefType(resolveType(inner))

  /** AST-level constant evaluation for pre-pass const-initializer folding. Handles numeric
   * literals, unary/binary arithmetic on ints, and references to already-folded consts. */
  protected def evalConstExprAST(e: ExpressionAST): Option[Long] = e match
    case IntLitAST(v)         => Some(v)
    case TypedIntLitAST(v, _) => Some(v)
    case CharLitAST(c)        => Some(c.toLong)
    case BoolLitAST(b)        => Some(if b then 1L else 0L)
    case UnaryAST("-", inner) => evalConstExprAST(inner).map(-_)
    case UnaryAST("+", inner) => evalConstExprAST(inner)
    case UnaryAST("~", inner) => evalConstExprAST(inner).map(~_)
    case UnaryAST("!", inner) => evalConstExprAST(inner).map(v => if v == 0 then 1L else 0L)
    case BinaryAST(l, op, r) =>
      for
        a <- evalConstExprAST(l)
        b <- evalConstExprAST(r)
        result <- op match
          case "+"  => Some(a + b)
          case "-"  => Some(a - b)
          case "*"  => Some(a * b)
          case "/"  => if b == 0 then None else Some(a / b)
          case "%"  => if b == 0 then None else Some(a % b)
          case "&"  => Some(a & b)
          case "|"  => Some(a | b)
          case "^"  => Some(a ^ b)
          case "<<" => Some(a << b.toInt)
          case ">>" => Some(a >> b.toInt)
          case _    => None
      yield result
    case VarRefAST(n) if compileTimeConstants.contains(n) => Some(compileTimeConstants(n))
    case _ => None

  /** Does the AST tree mention a function call anywhere? Used by the module-level
    * `const` pre-pass to distinguish "definitely unfoldable" RHSs (no calls, so the
    * folder's failure is final) from "might be foldable later" RHSs (contains a
    * call, so we defer to the main pass where the typed-AST const-evaluation
    * driver can run `#const fn` bodies through the interpreter). */
  protected def astContainsCall(e: ExpressionAST): Boolean = e match
    case _: CallAST | _: MethodCallAST | _: IndirectCallAST | _: GenericCallAST => true
    case UnaryAST(_, inner) => astContainsCall(inner)
    case BinaryAST(l, _, r) => astContainsCall(l) || astContainsCall(r)
    case _ => false

  /** Does this AST reference any name that the pre-pass has already deferred?
    * A const binding that consumes such a name must itself defer — its
    * dependency isn't in `compileTimeConstants` until after the main pass
    * runs the const-fn driver. */
  protected def astReferencesDeferred(e: ExpressionAST, deferred: scala.collection.Set[String]): Boolean = e match
    case VarRefAST(n) => deferred.contains(n)
    case UnaryAST(_, inner) => astReferencesDeferred(inner, deferred)
    case BinaryAST(l, _, r) => astReferencesDeferred(l, deferred) || astReferencesDeferred(r, deferred)
    case _ => false

  /** Evaluate a `within` range bound as a compile-time literal against the base numeric type.
   * Supports numeric literals with optional unary sign and references to `const` names. */
  protected def evalRangeBound(aliasName: String, ra: RangeAST, base: SyslType): TypeRange =
    def evalInt(e: ExpressionAST, sign: Long = 1): Long = e match
      case IntLitAST(v)         => sign * v
      case TypedIntLitAST(v, _) => sign * v
      case CharLitAST(c)        => sign * c.toLong
      case UnaryAST("-", inner) => evalInt(inner, -sign)
      case UnaryAST("+", inner) => evalInt(inner, sign)
      case VarRefAST(n) if compileTimeConstants.contains(n) => sign * compileTimeConstants(n)
      case _ => throw AnalysisError(s"range bound for '$aliasName' must be an integer literal or const")
    def evalFloat(e: ExpressionAST, sign: Double = 1.0): Double = e match
      case FloatLitAST(v)       => sign * v
      case IntLitAST(v)         => sign * v.toDouble
      case TypedIntLitAST(v, _) => sign * v.toDouble
      case UnaryAST("-", inner) => evalFloat(inner, -sign)
      case UnaryAST("+", inner) => evalFloat(inner, sign)
      case VarRefAST(n) if compileTimeConstants.contains(n) => sign * compileTimeConstants(n).toDouble
      case _ => throw AnalysisError(s"range bound for '$aliasName' must be a numeric literal or const")
    base.underlying match
      case _: IntType | _: UIntType =>
        val lo = evalInt(ra.lo)
        val hi = evalInt(ra.hi)
        if lo > hi || (lo == hi && ra.exclusiveHi) then
          throw AnalysisError(s"empty range in type '$aliasName': $lo..${if ra.exclusiveHi then "<" else ""}$hi")
        IntRange(lo, hi, ra.exclusiveHi)
      case _: FloatType =>
        val lo = evalFloat(ra.lo)
        val hi = evalFloat(ra.hi)
        if lo > hi || (lo == hi && ra.exclusiveHi) then
          throw AnalysisError(s"empty range in type '$aliasName': $lo..${if ra.exclusiveHi then "<" else ""}$hi")
        FloatRange(lo, hi, ra.exclusiveHi)
      case other =>
        throw AnalysisError(s"'within' requires a numeric base type, but '$aliasName' has base $other")

  /** Generate (once per enum) a synthetic `__str_<EnumName>(v: T) -> string` that returns the
   * variant name of `v`. Used by `str()` on data-enum values. */
  protected def materializeEnumStrFunc(et: SyslType.EnumType): String =
    val funcName = s"__str_${et.name}"
    if functions.contains(funcName) then return funcName
    functions(funcName) = FunInfo(funcName, List(("v", et)), StringType)
    val arms = et.variants.zipWithIndex.map { case ((vname, vFields), idx) =>
      val bindings = vFields.map(_ => None)
      val fieldTypes = vFields.map(_._2)
      val pattern = TVariantPattern(et, idx, bindings, fieldTypes)
      TMatchArm(List(pattern), None, List(TExprStmt(TStringLit(vname, StringType))))
    }
    val matchExpr = TMatchExpr(TVarRef("v", et), arms.toList, None, StringType)
    val body = TBlockBody(List(TExprStmt(matchExpr)))
    specializedDecls += TFunDecl(funcName, List(TParam("v", et)), StringType, body)
    funcName

  /** Generate a synthetic predicate-checker function for a `where`-constrained named type.
   * The function takes `value: base`, traps if the predicate is false, and returns value.
   * Caches by alias name so repeated resolutions reuse the same synth function. */
  protected def materializePredicateFunc(aliasName: String, predExpr: ExpressionAST, base: SyslType): String =
    val funcName = s"__pred_${aliasName}"
    if functions.contains(funcName) then return funcName
    // Register function info so that TCalls to it resolve during analysis.
    functions(funcName) = FunInfo(funcName, List(("value", base)), base)
    // Analyze the predicate in a scope with `value: base`.
    val savedScope = scopeStack
    scopeStack = new mutable.ArrayBuffer
    pushScope()
    currentScope("value") = SymInfo("value", base, mutable = false)
    val tPred = try analyzeExpr(predExpr) finally scopeStack = savedScope
    if tPred.typ != BoolType then
      throw AnalysisError(s"where-predicate for '$aliasName' must be bool, got ${tPred.typ}")
    // Body: if !predicate then abort(); value
    //   compiled as a block with a contract check + trailing expression return
    val body = TBlockBody(List(
      contract("type predicate", tPred, s"type predicate '$aliasName'"),
      TExprStmt(TVarRef("value", base))
    ))
    specializedDecls += TFunDecl(funcName, List(TParam("value", base)), base, body)
    funcName

  /** Generate a synthetic `__image_<EnumName>(v: i32) -> string` that returns the variant name
   * matching the integer value of a simple-enum value. Unknown values return "?". */
  protected def materializeEnumImageFunc(et: SyslType.EnumType): String =
    val funcName = s"__image_${et.name}"
    if functions.contains(funcName) then return funcName
    val members = enumTypes(et.name)
    functions(funcName) = FunInfo(funcName, List(("v", I32)), StringType)
    val arms = et.variants.map { case (vname, _) =>
      val value = members(vname)
      TMatchArm(List(TValuePattern(TIntLit(value, I32))), None, List(TExprStmt(TStringLit(vname, StringType))))
    }
    val default = Some(List[TStmt](TExprStmt(TStringLit("?", StringType))))
    val matchExpr = TMatchExpr(TVarRef("v", I32), arms.toList, default, StringType)
    specializedDecls += TFunDecl(funcName, List(TParam("v", I32)), StringType,
      TBlockBody(List(TExprStmt(matchExpr))))
    funcName

  /** Generate a synthetic `__pos_<EnumName>(v: i32) -> i32` that returns the 0-based declaration
   * position matching the integer value. Traps on an unknown value. */
  protected def materializeEnumPosFunc(et: SyslType.EnumType): String =
    val funcName = s"__pos_${et.name}"
    if functions.contains(funcName) then return funcName
    val members = enumTypes(et.name)
    functions(funcName) = FunInfo(funcName, List(("v", I32)), I32)
    val arms = et.variants.zipWithIndex.map { case ((vname, _), idx) =>
      val value = members(vname)
      TMatchArm(List(TValuePattern(TIntLit(value, I32))), None, List(TExprStmt(TIntLit(idx.toLong, I32))))
    }
    val trap = contract("type attribute", TBoolLit(false, BoolType), s"invalid enum value for ${et.name}::Pos")
    val default = Some(List[TStmt](trap, TExprStmt(TIntLit(-1L, I32))))
    val matchExpr = TMatchExpr(TVarRef("v", I32), arms.toList, default, I32)
    specializedDecls += TFunDecl(funcName, List(TParam("v", I32)), I32,
      TBlockBody(List(TExprStmt(matchExpr))))
    funcName

  /** Generate a synthetic `__val_<EnumName>(p: i32) -> i32` that returns the integer value at
   * position `p` (0-based). Traps on a position out of range. */
  protected def materializeEnumValFunc(et: SyslType.EnumType): String =
    val funcName = s"__val_${et.name}"
    if functions.contains(funcName) then return funcName
    val members = enumTypes(et.name)
    functions(funcName) = FunInfo(funcName, List(("p", I32)), I32)
    val arms = et.variants.zipWithIndex.map { case ((vname, _), idx) =>
      val value = members(vname)
      TMatchArm(List(TValuePattern(TIntLit(idx.toLong, I32))), None, List(TExprStmt(TIntLit(value, I32))))
    }
    val trap = contract("type attribute", TBoolLit(false, BoolType), s"out-of-range position for ${et.name}::Val")
    val default = Some(List[TStmt](trap, TExprStmt(TIntLit(-1L, I32))))
    val matchExpr = TMatchExpr(TVarRef("p", I32), arms.toList, default, I32)
    specializedDecls += TFunDecl(funcName, List(TParam("p", I32)), I32,
      TBlockBody(List(TExprStmt(matchExpr))))
    funcName

  /** Generate `__valid_<EnumName>(v: i32) -> bool` — true iff `v` equals one of the declared
   * variant values, false otherwise. Non-throwing complement to the Val/Pos trap paths. */
  protected def materializeEnumValidFunc(et: SyslType.EnumType): String =
    val funcName = s"__valid_${et.name}"
    if functions.contains(funcName) then return funcName
    val members = enumTypes(et.name)
    functions(funcName) = FunInfo(funcName, List(("v", I32)), BoolType)
    val arms = et.variants.map { case (vname, _) =>
      val value = members(vname)
      TMatchArm(List(TValuePattern(TIntLit(value, I32))), None, List(TExprStmt(TBoolLit(true, BoolType))))
    }
    val default = Some(List[TStmt](TExprStmt(TBoolLit(false, BoolType))))
    val matchExpr = TMatchExpr(TVarRef("v", I32), arms.toList, default, BoolType)
    specializedDecls += TFunDecl(funcName, List(TParam("v", I32)), BoolType,
      TBlockBody(List(TExprStmt(matchExpr))))
    funcName

  /** Generate `__value_<EnumName>(s: string) -> i32` — match input string against each variant
   * name (structural equality via TValuePattern on string) and return the matching variant's
   * integer value. Traps on unknown input. */
  protected def materializeEnumValueFunc(et: SyslType.EnumType): String =
    val funcName = s"__value_${et.name}"
    if functions.contains(funcName) then return funcName
    val members = enumTypes(et.name)
    functions(funcName) = FunInfo(funcName, List(("s", StringType)), I32)
    val arms = et.variants.map { case (vname, _) =>
      val value = members(vname)
      TMatchArm(List(TValuePattern(TStringLit(vname, StringType))), None, List(TExprStmt(TIntLit(value, I32))))
    }
    val trap = contract("type attribute", TBoolLit(false, BoolType), s"no variant matches string for ${et.name}::Value")
    val default = Some(List[TStmt](trap, TExprStmt(TIntLit(-1L, I32))))
    val matchExpr = TMatchExpr(TVarRef("s", StringType), arms.toList, default, I32)
    specializedDecls += TFunDecl(funcName, List(TParam("s", StringType)), I32,
      TBlockBody(List(TExprStmt(matchExpr))))
    funcName

  /** Generate `__succ_<EnumName>(v: i32) -> i32` — next variant value by declaration order.
   * Traps if `v` is the last variant or not a known variant. */
  protected def materializeEnumSuccFunc(et: SyslType.EnumType): String =
    val funcName = s"__succ_${et.name}"
    if functions.contains(funcName) then return funcName
    val members = enumTypes(et.name)
    functions(funcName) = FunInfo(funcName, List(("v", I32)), I32)
    val arms = et.variants.zipWithIndex.dropRight(1).map { case ((vname, _), idx) =>
      val value = members(vname)
      val nextValue = members(et.variants(idx + 1)._1)
      TMatchArm(List(TValuePattern(TIntLit(value, I32))), None, List(TExprStmt(TIntLit(nextValue, I32))))
    }
    val trap = contract("type attribute", TBoolLit(false, BoolType), s"no successor for ${et.name}::Succ (past last variant)")
    val default = Some(List[TStmt](trap, TExprStmt(TIntLit(-1L, I32))))
    val matchExpr = TMatchExpr(TVarRef("v", I32), arms.toList, default, I32)
    specializedDecls += TFunDecl(funcName, List(TParam("v", I32)), I32,
      TBlockBody(List(TExprStmt(matchExpr))))
    funcName

  /** Generate `__pred_<EnumName>(v: i32) -> i32` — previous variant value by declaration order.
   * Traps if `v` is the first variant or not a known variant. */
  protected def materializeEnumPredFunc(et: SyslType.EnumType): String =
    val funcName = s"__pred_${et.name}"
    if functions.contains(funcName) then return funcName
    val members = enumTypes(et.name)
    functions(funcName) = FunInfo(funcName, List(("v", I32)), I32)
    val arms = et.variants.zipWithIndex.drop(1).map { case ((vname, _), idx) =>
      val value = members(vname)
      val prevValue = members(et.variants(idx - 1)._1)
      TMatchArm(List(TValuePattern(TIntLit(value, I32))), None, List(TExprStmt(TIntLit(prevValue, I32))))
    }
    val trap = contract("type attribute", TBoolLit(false, BoolType), s"no predecessor for ${et.name}::Pred (past first variant)")
    val default = Some(List[TStmt](trap, TExprStmt(TIntLit(-1L, I32))))
    val matchExpr = TMatchExpr(TVarRef("v", I32), arms.toList, default, I32)
    specializedDecls += TFunDecl(funcName, List(TParam("v", I32)), I32,
      TBlockBody(List(TExprStmt(matchExpr))))
    funcName

  /** Generate `__succ_<AliasName>(v: base) -> base` — v+1 with upper-bound trap. */
  protected def materializeWithinSuccFunc(aliasName: String, nt: NamedType, base: SyslType, range: IntRange): String =
    val funcName = s"__succ_${aliasName}"
    if functions.contains(funcName) then return funcName
    functions(funcName) = FunInfo(funcName, List(("v", base)), base)
    val hi = if range.exclusiveHi then range.hi - 1 else range.hi
    val check = contract("type attribute",
      TBinary(TVarRef("v", base), "<", TIntLit(hi, base), BoolType),
      s"no successor for $aliasName::Succ (value is at upper bound)")
    val result = TBinary(TVarRef("v", base), "+", TIntLit(1L, base), base)
    specializedDecls += TFunDecl(funcName, List(TParam("v", base)), base,
      TBlockBody(List(check, TExprStmt(result))))
    funcName

  /** Generate `__pred_<AliasName>(v: base) -> base` — v-1 with lower-bound trap. */
  protected def materializeWithinPredFunc(aliasName: String, nt: NamedType, base: SyslType, range: IntRange): String =
    val funcName = s"__pred_${aliasName}"
    if functions.contains(funcName) then return funcName
    functions(funcName) = FunInfo(funcName, List(("v", base)), base)
    val check = contract("type attribute",
      TBinary(TVarRef("v", base), ">", TIntLit(range.lo, base), BoolType),
      s"no predecessor for $aliasName::Pred (value is at lower bound)")
    val result = TBinary(TVarRef("v", base), "-", TIntLit(1L, base), base)
    specializedDecls += TFunDecl(funcName, List(TParam("v", base)), base,
      TBlockBody(List(check, TExprStmt(result))))
    funcName

  /** Resolve a `Type::First` or `Type::Last` attribute to a compile-time constant expression. */
  protected def analyzeTypeFirstLast(typeName: String, typ: SyslType, isFirst: Boolean): TExpr =
    val attrName = if isFirst then "First" else "Last"
    typ match
      case nt @ NamedType(_, base, nominal, Some(IntRange(lo, hi, excl)), _) =>
        val value = if isFirst then lo else (if excl then hi - 1 else hi)
        val lit = TIntLit(value, base)
        if nominal then TCast(lit, nt) else applyTargetType(lit, nt)
      case NamedType(_, _, _, Some(FloatRange(_, _, _)), _) =>
        throw AnalysisError(s"$typeName::$attrName on a float-within type is not yet supported")
      case NamedType(_, _, _, None, _) =>
        throw AnalysisError(s"$typeName::$attrName requires a range-constrained type")
      case et @ EnumType(name, variants) if simpleEnumTypes.contains(name) =>
        if variants.isEmpty then throw AnalysisError(s"enum '$name' has no members")
        val members = enumTypes(name)
        val picked = if isFirst then variants.head._1 else variants.last._1
        TIntLit(members(picked), I32)
      case other =>
        throw AnalysisError(s"$typeName::$attrName requires a range-constrained type or simple enum, got $other")

  /** Resolve a `Type::Image(x)` attribute — returns a string representation of x. */
  protected def analyzeTypeImage(typeName: String, typ: SyslType, argAst: ExpressionAST): TExpr =
    val tArg = analyzeExpr(argAst)
    typ match
      case et @ EnumType(name, _) if simpleEnumTypes.contains(name) =>
        val asInt = if tArg.typ == I32 then tArg else TCast(tArg, I32)
        TCall(materializeEnumImageFunc(et), List(asInt), StringType)
      case nt @ NamedType(_, base, _, _, _) if base.isNumeric || base == BoolType =>
        val unwrapped = if tArg.typ == nt then TCast(tArg, base) else tArg
        TStr(unwrapped)
      case other =>
        throw AnalysisError(s"$typeName::Image requires a simple enum or numeric constrained type, got $other")

  /** Resolve a `Type::Pos(x)` attribute — returns the declaration position (0-based) for x. */
  protected def analyzeTypePos(typeName: String, typ: SyslType, argAst: ExpressionAST): TExpr =
    val tArg = analyzeExpr(argAst)
    typ match
      case et @ EnumType(name, _) if simpleEnumTypes.contains(name) =>
        val asInt = if tArg.typ == I32 then tArg else TCast(tArg, I32)
        TCall(materializeEnumPosFunc(et), List(asInt), I32)
      case other =>
        throw AnalysisError(s"$typeName::Pos requires a simple enum, got $other")

  /** Resolve a `Type::Val(n)` attribute — returns the enum value at position n. */
  protected def analyzeTypeVal(typeName: String, typ: SyslType, argAst: ExpressionAST): TExpr =
    val tArg = analyzeExpr(argAst)
    typ match
      case et @ EnumType(name, _) if simpleEnumTypes.contains(name) =>
        if !tArg.typ.isIntegral then
          throw AnalysisError(s"$typeName::Val argument must be integer, got ${tArg.typ}")
        val asInt = if tArg.typ == I32 then tArg else TCast(tArg, I32)
        TCall(materializeEnumValFunc(et), List(asInt), I32)
      case other =>
        throw AnalysisError(s"$typeName::Val requires a simple enum, got $other")

  /** Resolve `Type::Valid(x)` — returns bool without trapping. Checks whether `x` satisfies
   * the type's constraints: range bounds for `within`-int types, known variant value for
   * simple enums. Accepts the base numeric / integer type as argument. */
  protected def analyzeTypeValid(typeName: String, typ: SyslType, argAst: ExpressionAST): TExpr =
    val tArg = analyzeExpr(argAst)
    typ match
      case nt @ NamedType(_, base, _, Some(IntRange(lo, hi, excl)), _) =>
        if !tArg.typ.isIntegral then
          throw AnalysisError(s"$typeName::Valid expects integer, got ${tArg.typ}")
        val v = if tArg.typ == base then tArg else TCast(tArg, base)
        val loOk = TBinary(v, ">=", TIntLit(lo, base), BoolType)
        val hiOp = if excl then "<" else "<="
        val hiOk = TBinary(v, hiOp, TIntLit(hi, base), BoolType)
        TBinary(loOk, "&&", hiOk, BoolType)
      case NamedType(_, _, _, Some(FloatRange(_, _, _)), _) =>
        throw AnalysisError(s"$typeName::Valid on a float-within type is not yet supported")
      case NamedType(_, _, _, None, _) =>
        throw AnalysisError(s"$typeName::Valid requires a range-constrained type")
      case et @ EnumType(name, _) if simpleEnumTypes.contains(name) =>
        if !tArg.typ.isIntegral then
          throw AnalysisError(s"$typeName::Valid expects integer, got ${tArg.typ}")
        val asInt = if tArg.typ == I32 then tArg else TCast(tArg, I32)
        TCall(materializeEnumValidFunc(et), List(asInt), BoolType)
      case other =>
        throw AnalysisError(s"$typeName::Valid requires a range-constrained type or simple enum, got $other")

  /** Resolve `Type::Value(s)` — parse a string into a simple-enum value. */
  protected def analyzeTypeValueString(typeName: String, typ: SyslType, argAst: ExpressionAST): TExpr =
    val tArg = analyzeExpr(argAst)
    if tArg.typ != StringType then
      throw AnalysisError(s"$typeName::Value argument must be string, got ${tArg.typ}")
    typ match
      case et @ EnumType(name, _) if simpleEnumTypes.contains(name) =>
        TCall(materializeEnumValueFunc(et), List(tArg), I32)
      case other =>
        throw AnalysisError(s"$typeName::Value requires a simple enum, got $other")

  /** Resolve `Type::Succ(x)` / `Type::Pred(x)` — next/previous value. For simple enums,
   * steps by declaration order; for int `within` types, by one. Both trap at the boundary. */
  protected def analyzeTypeSuccPred(typeName: String, typ: SyslType, argAst: ExpressionAST, isSucc: Boolean): TExpr =
    val tArg = analyzeExpr(argAst)
    val attrName = if isSucc then "Succ" else "Pred"
    typ match
      case et @ EnumType(name, _) if simpleEnumTypes.contains(name) =>
        val asInt = if tArg.typ == I32 then tArg else TCast(tArg, I32)
        val fn = if isSucc then materializeEnumSuccFunc(et) else materializeEnumPredFunc(et)
        TCall(fn, List(asInt), I32)
      case nt @ NamedType(_, base, _, Some(range: IntRange), _) =>
        val unwrapped = if tArg.typ == nt then TCast(tArg, base) else tArg
        val fn = if isSucc then materializeWithinSuccFunc(typeName, nt, base, range)
                 else materializeWithinPredFunc(typeName, nt, base, range)
        val raw = TCall(fn, List(unwrapped), base)
        // Re-wrap in the named type (range-check at entry happens on the arg; the result is in-range by construction)
        if nt.nominal then TCast(raw, nt) else applyTargetType(raw, nt)
      case other =>
        throw AnalysisError(s"$typeName::$attrName requires a simple enum or range-constrained int, got $other")

  /** `PtrType` / `RefType` may embed a recursive generic `StructType` placeholder (empty `fields`); use `structTypes`. */
  protected def latestStruct(st: SyslType.StructType): SyslType.StructType =
    structTypes.getOrElse(st.name, st)

  /** A captured `InterfaceType` may carry the pre-main-pass placeholder (empty
   *  `methods`) if its enclosing data-enum's variant payload was resolved
   *  during pass 0.5 before the main pass populated iface methods. The post-
   *  main-pass `resolveStructsAndEnums()` refreshes non-recursive enum variant
   *  fields, but a self-referencing enum's `Cons(value: I, next: &Self)`
   *  captures the placeholder into the EnumType stored under the recursive
   *  reference, and any function param typed `&Self` reads back the stale
   *  iface during dispatch. Re-look up by name to defeat that staleness. */
  protected def latestInterface(it: SyslType.InterfaceType): SyslType.InterfaceType =
    if it.methods.isEmpty then interfaceTypes.getOrElse(it.name, it) else it

  /** A function param typed by an `EnumType` is captured at function
   *  registration in the main pass — before the post-main-pass
   *  `resolveStructsAndEnums()` rewrites `dataEnumTypes` with variant payloads
   *  whose iface types now carry their full method list. The captured enum
   *  reference is the pre-rewrite snapshot, so variant patterns reading
   *  `et.variants(i)._2` see empty-methods ifaces and `MethodCallAST` on the
   *  bound payload OOBs. Refresh at variant-resolution time, the same way
   *  `latestStruct` refreshes field-access on struct field types. */
  protected def latestEnum(et: SyslType.EnumType): SyslType.EnumType =
    dataEnumTypes.getOrElse(et.name, et)

  /** Convert an expression AST to a type AST (for explicit type args parsed as index expressions).
   *  The expression-position grammar parses generic type args as expressions, so this maps
   *  the relevant shapes back to types: `A` (VarRef), `Parser[A]` (Index of VarRef),
   *  `(A, B)` (TupleLit), and `[]A` / `[5]A` (TypeRefExprAST wrappers emitted by the parser
   *  for slice / array type literals appearing in expression position). */
  protected def exprToTypeAST(expr: ExpressionAST): TypeAST = expr match
    case TypeRefExprAST(t) => t
    case VarRefAST(name) => NamedTypeAST(name)
    case TupleLitAST(elems) => TupleTypeAST(elems.map(exprToTypeAST))
    case IndexAST(VarRefAST(name), arg) => NamedTypeAST(name, List(exprToTypeAST(arg)))
    // Zero-param fn type: `() -> R` is greedy-parsed by closureExpr's
    // `"(" ~ ")" ~ "->" ~> closureBody` form, so it arrives as a ClosureAST(Nil, body).
    // Lift it back into a FuncTypeAST when the body is a type-shaped expression.
    // Multi-param fn types (`(P, ...) -> R`) come through funcTypeRef as TypeRefExprAST.
    case ClosureAST(Nil, ExprBodyAST(retExpr)) =>
      FuncTypeAST(Nil, exprToTypeAST(retExpr))
    case _ => throw AnalysisError(s"expected type argument, got expression")

  /** Look up a method function by struct name and method name, trying both unmangled and mangled forms. */
  protected def lookupMethod(structName: String, methodName: String): Option[FunInfo] =
    val shortName = s"${structName}_$methodName"
    functions.get(shortName).orElse {
      // Try with module prefix (mangled name)
      currentModule match
        case Some(mod) => functions.get(s"${mod}__$shortName")
        case None => None
    }.orElse {
      // Search all functions for a match (imported methods may have arbitrary module prefix)
      functions.values.find(f => self.shortName(f.name) == shortName)
    }

  protected def satisfiesInterface(st: SyslType.StructType, iface: SyslType.InterfaceType): Boolean =
    val structName = st.name
    iface.methods.forall { (methodName, paramTypes, retType, ifaceEffects) =>
      val funInfoOpt = lookupMethod(structName, methodName).orElse {
        // Generic struct method: the impl lives as `${templateName}_$method` in
        // `genericTemplates`. Instantiate it for this concrete struct so we can
        // structurally compare its monomorphized signature against the iface.
        // Instantiation is idempotent (call sites would do the same).
        structToTemplate.get(structName).flatMap { case (templateName, _) =>
          val templateFuncName = s"${templateName}_$methodName"
          if genericTemplates.contains(templateFuncName) then
            try
              val allArgTypes: List[SyslType] = SyslType.PtrType(st) :: paramTypes
              val (mangled, fi) = instantiateGeneric(templateFuncName, allArgTypes)
              // Box dispatch (interpreter + every codegen) looks up methods
              // under `${structName}_$methodName`. For monomorphized generic
              // struct methods the real function is named `${templateName}_${methodName}_${typeArgs}`,
              // which dispatch can't find. Emit a thin forwarding TFunDecl
              // under the dispatch-expected name so every backend Just Works.
              val aliasName = s"${structName}_$methodName"
              if !functions.contains(aliasName) then
                val aliasFi = fi.copy(name = aliasName)
                functions(aliasName) = aliasFi
                val aliasParams = fi.params.map((n, t) => TParam(n, t))
                val aliasArgs: List[TExpr] = fi.params.map((n, t) => TVarRef(n, t))
                val aliasCall = TCall(mangled, aliasArgs, fi.returnType)
                val aliasBody =
                  if fi.returnType == SyslType.UnitType then TBlockBody(List(TExprStmt(aliasCall)))
                  else TExprBody(aliasCall)
                specializedDecls += TFunDecl(aliasName, aliasParams, fi.returnType, aliasBody, isPrivate = true)
              Some(fi)
            catch case _: AnalysisError => None
          else None
        }
      }
      funInfoOpt match
        case Some(funInfo) =>
          val userParams = funInfo.params.drop(1).map(_._2)
          val structuralOk = userParams == paramTypes && funInfo.returnType == retType
          // Effect subtyping: the implementing method's effects must satisfy the interface's
          // declared effects. If the interface method has no annotation, anything passes.
          val effectsOk = effectsSatisfy(funInfoEffects(funInfo), ifaceEffects)
          structuralOk && effectsOk
        case None => false
    }

  /** Strict-but-name-aware element equality for slice/array element types. Plain `==`
   *  fails when two `EnumType` instances share a name but capture different snapshots
   *  of the variant list (e.g. the field type stored in a recursive enum variant
   *  declaration was resolved with a stale placeholder). For nominal types we trust the
   *  name; for primitives and structural types we keep `==` (no widening — `[]i8` must
   *  not silently flow into `[]i64`). Recurses through nested slice/array/ref/ptr so
   *  shapes like `[][]Tree` work. */
  protected def nominallyEqual(a: SyslType, b: SyslType): Boolean = (a, b) match
    case _ if a == b => true
    case (StructType(n1, _, _), StructType(n2, _, _)) => n1 == n2
    case (EnumType(n1, _), EnumType(n2, _))           => n1 == n2
    case (SliceType(e1), SliceType(e2))               => nominallyEqual(e1, e2)
    case (ArrayType(e1, n1), ArrayType(e2, n2))       => n1 == n2 && nominallyEqual(e1, e2)
    case (RefType(e1), RefType(e2))                   => nominallyEqual(e1, e2)
    case (PtrType(e1), PtrType(e2))                   => nominallyEqual(e1, e2)
    case _                                            => false

  protected def compatible(from: SyslType, to: SyslType): Boolean =
    (from, to) match
      case (a, b) if a == b => true
      // Name-based equality for nominal types — handles stale placeholders from
      // forward declarations where two StructType/EnumType with the same name
      // have different field/variant lists.
      case (StructType(n1, _, _), StructType(n2, _, _)) if n1 == n2 => true
      case (EnumType(n1, _), EnumType(n2, _)) if n1 == n2 => true
      // Named/derived types:
      //   - same-name NamedType ↔ NamedType: compatible
      //   - non-nominal (subtype) NamedType ↔ base: compatible (range checked at assignment)
      //   - nominal (derived) NamedType ↔ base: NOT compatible (explicit cast required)
      case (NamedType(n1, _, _, _, _), NamedType(n2, _, _, _, _)) if n1 == n2 => true
      case (NamedType(_, b, false, _, _), other) => compatible(b, other)
      case (other, NamedType(_, b, false, _, _)) => compatible(other, b)
      case (IntType(a), IntType(b)) if a <= b => true    // signed widening
      case (UIntType(a), UIntType(b)) if a <= b => true  // unsigned widening
      case (IntType(a), UIntType(b)) if a <= b => true   // signed → unsigned widening
      case (UIntType(a), IntType(b)) if a <= b => true   // unsigned → signed widening
      case (FloatType(a), FloatType(b)) if a <= b => true  // f32 → f64 widening
      case (_: IntType, _: FloatType) => true    // signed int → float promotion
      case (_: UIntType, _: FloatType) => true   // unsigned int → float promotion
      case (_: FloatType, _: IntType) => true    // float → signed int (truncation)
      case (_: FloatType, _: UIntType) => true   // float → unsigned int (truncation)
      // bool and int are NOT compatible — use explicit casts
      // int ↔ pointer: NOT compatible — use explicit casts: int(ptr), *i8(addr)
      // FuncType compatibility ignores escaping flag — escaping is an optimization hint, not a type distinction
      case (FuncType(p1, r1, _, eff1), FuncType(p2, r2, _, eff2)) =>
        // Structural compat: arity, parameter & return types. Plus effect subtyping —
        // the *actual* (LHS) callee must provide at least the guarantees the *expected*
        // (RHS) slot demands. Pure ⊆ any RW ⊆ Unknown (more guarantees → fewer effects).
        // (eff2.isAnnotated && eff1.isUnknown): slot wants annotated, actual is unknown → reject.
        p1.length == p2.length && p1.zip(p2).forall((a, b) => compatible(a, b)) && compatible(r1, r2) &&
          effectsSatisfy(eff1, eff2)
      case (_: FuncType, IntType(64) | UIntType(64)) => true // function pointer → i64 (entry point address)
      case (PtrType(_), PtrType(_)) => true           // any pointer ↔ any pointer (like C's void*)
      case (ArrayType(_, _), PtrType(_)) => true          // array decays to any pointer
      case (StringType, PtrType(I8 | U8)) => true          // string decays to *i8 / *byte
      case (ArrayType(e1, _), ArrayType(e2, _)) if nominallyEqual(e1, e2) => true
      case (ArrayType(e1, _), SliceType(e2)) if nominallyEqual(e1, e2) => true  // fixed array → slice
      case (SliceType(e1), SliceType(e2)) if nominallyEqual(e1, e2) => true
      case (RefType(a), RefType(b)) if compatible(a, b) => true // same ref type (recursive check handles nominal types)
      case (RefType(inner), PtrType(_)) => true             // &T → *U (ref decays to pointer)
      // Note: *T → T is NOT compatible. Implicit deref-and-copy hides cost (memcpy of pointee).
      // Write *ptr explicitly. Exception: `self` in methods (handled in checkArgs).
      // Interface satisfaction: struct/ptr/ref → interface (if methods match)
      case (st: StructType, iface: InterfaceType) => satisfiesInterface(st, iface)
      case (PtrType(st: StructType), iface: InterfaceType) => satisfiesInterface(st, iface)
      case (RefType(st: StructType), iface: InterfaceType) => satisfiesInterface(st, iface)
      // Interface-to-interface widening (superset of methods)
      case (InterfaceType(_, methodsA), InterfaceType(_, methodsB)) =>
        methodsB.forall(mb => methodsA.exists(_ == mb))
      case _ => false

  // Coerce integer literals to the target type (like Rust's untyped integer literals)
  /** Audit item #28: catch literal-overflow assignments that would silently
   *  truncate. Returns Some(error message) if `value` does not fit in the
   *  bit-width of the target sized-int type, None otherwise. i64/u64 are
   *  unconstrained — every Long fits. */
  protected def literalRangeMsg(value: Long, target: SyslType): Option[String] =
    val (lo, hi, name) = target match
      case IntType(8)  => (-128L,         127L,        "i8")
      case IntType(16) => (-32768L,       32767L,      "i16")
      case IntType(32) => (-2147483648L,  2147483647L, "int")
      case UIntType(8)  => (0L,           0xFFL,       "u8")
      case UIntType(16) => (0L,           0xFFFFL,     "u16")
      case UIntType(32) => (0L,           0xFFFFFFFFL, "u32")
      case _ => return None
    if value < lo || value > hi then
      Some(s"literal $value does not fit in $name (range $lo..$hi); cast explicitly: `$name(...)` if truncation is intended")
    else None

  protected def coerceLiteral(expr: TExpr, target: SyslType): TExpr =
    // Don't auto-promote an untyped literal to a nominal NamedType — a cast is required.
    target match
      case NamedType(_, _, true, _, _) => return expr
      case _ =>
    expr match
      case TIntLit(value, _) if target.isIntegral =>
        literalRangeMsg(value, target).foreach(msg => throw AnalysisError(msg))
        TIntLit(value, target)
      case TUnary("-", TIntLit(value, _), _) if target.isIntegral =>
        // Constant-fold negation so the range check applies to the resulting
        // value. Without this, `var x: u8 = -1` slipped past the TIntLit-only
        // arm above — the inner `1` fits u8, but the negated -1 does not. Same
        // for `var x: i8 = -129` (inner 129 already overflows i8 anyway, but
        // the diagnostic from this arm will name the negated value, which is
        // what the user wrote in source). i64/u64 unconstrained as before.
        val negated = -value
        literalRangeMsg(negated, target).foreach(msg => throw AnalysisError(msg))
        TIntLit(negated, target)
      case TUnary("~", TIntLit(value, innerType), _) if target.isIntegral =>
        // Constant-fold bitwise NOT. The width of the bit-flip is determined by
        // the *inner* literal's type — `~0u8` flips 8 bits → 0xFF, while `~0i32`
        // flips 32 bits → -1 (sign-extended). Result is then range-checked
        // against the target so `var x: u8 = ~0` (where the bare `~0` is i32 →
        // -1) lands on the same "literal -1 does not fit in u8" diagnostic
        // shape as `var x: u8 = -1`. To get u8(0xFF) the user writes `~0u8`.
        val maskAndSign: Option[(Long, Boolean)] = innerType.underlying match
          case IntType(8)   => Some((0xFFL,         true))
          case IntType(16)  => Some((0xFFFFL,       true))
          case IntType(32)  => Some((0xFFFFFFFFL,   true))
          case IntType(64)  => Some((-1L,           true))
          case UIntType(8)  => Some((0xFFL,         false))
          case UIntType(16) => Some((0xFFFFL,       false))
          case UIntType(32) => Some((0xFFFFFFFFL,   false))
          case UIntType(64) => Some((-1L,           false))
          case _            => None
        maskAndSign match
          case None => expr
          case Some((mask, signed)) =>
            val raw = (~value) & mask
            val highBit = mask - (mask >>> 1)         // 0x80 / 0x8000 / 0x80000000 / 0x8000…
            val computed = if signed && (raw & highBit) != 0 then raw | ~mask else raw
            literalRangeMsg(computed, target).foreach(msg => throw AnalysisError(msg))
            TIntLit(computed, target)
      case TIntLit(0, _) if target.isInstanceOf[PtrType] => TIntLit(0, target) // null pointer
      // Float literal → narrower float type (untyped float literal coercion)
      case TFloatLit(value, _) if target.isFloat => TFloatLit(value, target)
      // String literal → byte array: "hello" initializing [n]byte
      case TStringLit(s, _) if target.isInstanceOf[ArrayType] =>
        val ArrayType(elemType, size) = target: @unchecked
        if elemType != U8 && elemType != I8 then
          throw AnalysisError(s"cannot initialize [$size]$elemType from string literal (element type must be byte or i8)")
        val bytes = s.getBytes("ISO-8859-1")
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

  // Apply a target type at a produce-site (var init, assign, param bind, return, cast).
  // For a NamedType target with a `within` range, validates literal values at compile-time and
  // inserts a runtime TRangeCheck for non-literal values. Also re-wraps static type so downstream
  // code sees the NamedType.
  protected def applyTargetType(expr: TExpr, target: SyslType): TExpr =
    target match
      case nt @ NamedType(aliasName, base, _, rangeOpt, predFuncOpt) =>
        // Step 1: range check (if any). Literal values are validated at compile time.
        // With contracts disabled, both the compile-time literal check and the runtime
        // TRangeCheck emission are skipped — the value is cast without any guard.
        val afterRange: TExpr = rangeOpt match
          case Some(range) if contractsEnabled =>
            val literalChecked: Option[TExpr] = (expr, range) match
              case (TIntLit(v, _), IntRange(lo, hi, excl)) =>
                val ok = if excl then v >= lo && v < hi else v >= lo && v <= hi
                if !ok then throw AnalysisError(s"value $v is out of range for type '$aliasName' (${lo}..${if excl then "<" else ""}${hi})")
                Some(if expr.typ == nt then expr else TCast(expr, nt))
              case (TFloatLit(v, _), FloatRange(lo, hi, excl)) =>
                val ok = if excl then v >= lo && v < hi else v >= lo && v <= hi
                if !ok then throw AnalysisError(s"value $v is out of range for type '$aliasName' (${lo}..${if excl then "<" else ""}${hi})")
                Some(if expr.typ == nt then expr else TCast(expr, nt))
              case _ => None
            literalChecked.getOrElse(TRangeCheck(expr, range, aliasName, nt))
          case Some(_) =>
            // Contracts off: bare cast, no range verification.
            if expr.typ == nt then expr else TCast(expr, nt)
          case None => expr
        // Step 2: where-predicate check (if any). Synth function does the trap and returns value.
        val afterPred: TExpr = predFuncOpt match
          case Some(predFunc) =>
            val arg = if afterRange.typ == nt then afterRange
                      else if afterRange.typ.underlying == base then afterRange
                      else TCast(afterRange, base)
            TCall(predFunc, List(arg), nt)
          case None => afterRange
        // Step 3: final type re-wrap (nominal types without range/predicate).
        if afterPred.typ != nt && rangeOpt.isEmpty && predFuncOpt.isEmpty then TCast(afterPred, nt)
        else afterPred
      case _ => expr

  // Coerce integer literals to match the target's signedness only (preserving original width)
  protected def coerceSignedness(expr: TExpr, target: SyslType): TExpr =
    (expr, target) match
      case (TIntLit(value, IntType(w)), _: UIntType) => TIntLit(value, UIntType(w))
      case (TIntLit(value, UIntType(w)), _: IntType) => TIntLit(value, IntType(w))
      case _ => expr

  /** Truncate a value to fit the given integer type's width, with sign-extension for signed types. */
  protected def maskToType(value: Long, typ: SyslType): Long = typ match
    case IntType(8) => (value << 56) >> 56 // sign-extend from 8 bits
    case IntType(16) => (value << 48) >> 48
    case IntType(32) => (value << 32) >> 32
    case UIntType(8) => value & 0xFFL
    case UIntType(16) => value & 0xFFFFL
    case UIntType(32) => value & 0xFFFFFFFFL
    case _ => value // i64/u64/bool — no truncation needed

  /** Narrow a `Double` to the binary precision of the target float type. The
    * interpreter folds every float in `Double` precision; an `f32`-declared
    * const must round-trip through `Float` to match the bit pattern a runtime
    * `f32` computation would produce. */
  protected def narrowFloatToType(value: Double, typ: SyslType): Double = typ match
    case FloatType(32) => value.toFloat.toDouble
    case _             => value

  /** Try to evaluate a typed expression as a compile-time integer constant. */
  protected def tryConstEval(expr: TExpr): Option[Long] = expr match
    case TIntLit(n, _) => Some(n)
    case TBoolLit(b, _) => Some(if b then 1 else 0)
    case TSizeof(n, _) => Some(n)
    case TVarRef(name, _) => compileTimeConstants.get(name)
    case TUnary("-", operand, _) => tryConstEval(operand).map(-_)
    case TUnary("~", operand, _) => tryConstEval(operand).map(~_)
    case TUnary("!", operand, _) => tryConstEval(operand).map(v => if v == 0 then 1L else 0L)
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
    case TBinary(left, "==", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield if l == r then 1L else 0L
    case TBinary(left, "!=", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield if l != r then 1L else 0L
    case TBinary(left, "<",  right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield if l <  r then 1L else 0L
    case TBinary(left, ">",  right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield if l >  r then 1L else 0L
    case TBinary(left, "<=", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield if l <= r then 1L else 0L
    case TBinary(left, ">=", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield if l >= r then 1L else 0L
    case TBinary(left, "&&", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield if (l != 0 && r != 0) then 1L else 0L
    case TBinary(left, "||", right, _) => for l <- tryConstEval(left); r <- tryConstEval(right) yield if (l != 0 || r != 0) then 1L else 0L
    case TCast(inner, _) => tryConstEval(inner)
    case TCall(name, args, _) => evaluateConstCall(name, args)
    case _ => None

  /** Cap on the number of statements a single const-evaluation run may
    * execute before being aborted with a clear diagnostic. Ten million is
    * deliberately generous (covers a degree-N lookup table for N ≈ 1024 with
    * a few thousand stmts per entry) while keeping a runaway recursion from
    * hanging the compiler. */
  protected val CONST_EVAL_STEP_LIMIT: Long = 10_000_000L

  /** Try to evaluate a call expression at compile time. Returns `Some(n)`
    * exactly when:
    *   - the callee resolves to a `#const fn` we have already analyzed in
    *     this compilation unit (forward references through SMETA are
    *     deferred to a later stage of the const-evaluation feature);
    *   - every argument folds to a scalar `Long` via `tryConstEval` (this
    *     is the v1 limitation — float, string, and aggregate arguments
    *     route through the still-deferred richer evaluator);
    *   - the embedded interpreter runs to completion within
    *     `CONST_EVAL_STEP_LIMIT` statements without throwing.
    *
    * On any failure (callee unknown, arg fails to fold, interpreter panics,
    * step limit) we return `None` and let the existing
    * "initializer is not compile-time evaluable" diagnostic fire. A future
    * stage will lift this to a richer error so the user sees *why* the
    * folder gave up — for now the binary signal is enough. */
  protected def evaluateConstCall(name: String, args: List[TExpr]): Option[Long] =
    evaluateConstCallValue(name, args).collect { case Value.IntVal(n) => n }

  /** Float counterpart to `evaluateConstCall`. Returns `Some(d)` exactly when
    * the callee is a known `#const fn`, every argument folds, and the
    * interpreter returns a `FloatVal` (or an `IntVal` widenable to a Double).
    * Used by `tryConstEvalFloat` for the `TCall` case. */
  protected def evaluateConstCallFloat(name: String, args: List[TExpr]): Option[Double] =
    evaluateConstCallValue(name, args).flatMap {
      case Value.FloatVal(d) => Some(d)
      case Value.IntVal(n)   => Some(n.toDouble)
      case _                 => None
    }

  /** Shared driver for the int / float specializations above. Folds each
    * argument with the appropriate type-driven folder (integral args use
    * `tryConstEval` → `IntVal`; float args use `tryConstEvalFloat` →
    * `FloatVal`), loads every accumulated `#const fn` into a fresh
    * `SyslInterpreter` capped at `CONST_EVAL_STEP_LIMIT`, and returns the
    * raw result `Value`. Any failure (callee unknown, arg fails to fold,
    * runtime throw) collapses to `None`. */
  protected def evaluateConstCallValue(name: String, args: List[TExpr]): Option[Value] =
    constFunDecls.get(name).flatMap { fn =>
      val foldedArgs = args.foldLeft(Option(List.empty[Value])) { (acc, a) =>
        for as <- acc; v <- foldArgValue(a) yield as :+ v
      }
      foldedArgs.flatMap { argValues =>
        val interp = new SyslInterpreter(_ => ())
        interp.setStepLimit(CONST_EVAL_STEP_LIMIT)
        interp.load(TProgram(constFunDecls.values.toSet.toList))
        try Some(interp.callByName(fn.name, argValues))
        catch case _: Throwable => None
      }
    }

  /** Fold a typed argument into the `Value` shape the interpreter expects.
    * Dispatches on the argument's static type so a float arg becomes a
    * `FloatVal` and an integer arg becomes an `IntVal`. Other kinds fail
    * folding for now — Stage 2c lifts this for aggregate const arguments. */
  protected def foldArgValue(a: TExpr): Option[Value] =
    if a.typ.isFloat then tryConstEvalFloat(a).map(Value.FloatVal(_))
    else tryConstEval(a).map(Value.IntVal(_))

  /** Try to fold a typed expression to an aggregate (array or struct) literal
    * whose elements are themselves literal-composed. Returns `Some(folded)`
    * where `folded` is a tree of `TIntLit` / `TFloatLit` / `TBoolLit` /
    * `TArrayLit` / `TStructConstruct` nodes — the shape every backend already
    * accepts as a module-level static initializer. Used by the `const`-binding
    * analyzer to materialize the result of `const TABLE: [N]T = const_fn(…)`
    * as static data, with no per-backend codegen changes required.
    *
    *   - `TArrayLit` / `TStructConstruct` already in literal-of-literals shape
    *     pass through unchanged.
    *   - `TArrayLit` / `TStructConstruct` with element / arg expressions that
    *     each fold (scalar, float, or recursively aggregate) get rebuilt with
    *     the folded sub-expressions.
    *   - `TCall` to an aggregate-returning `#const fn` routes through the
    *     embedded interpreter via `evaluateConstCallValue`; the resulting
    *     `Value` is reconstituted as a typed literal tree by the interpreter's
    *     `valueToConstExpr`.
    *
    * Anything else returns `None` and lets the caller emit the standard
    * "initializer is not compile-time evaluable" diagnostic. */
  protected def tryConstEvalAggregate(expr: TExpr): Option[TExpr] = expr match
    case TArrayLit(elems, t) =>
      val folded = elems.foldLeft(Option(List.empty[TExpr])) { (acc, e) =>
        for as <- acc; ef <- foldExprValue(e) yield as :+ ef
      }
      folded.map(es => TArrayLit(es, t))
    case TStructConstruct(st, args) =>
      val folded = args.foldLeft(Option(List.empty[TExpr])) { (acc, a) =>
        for as <- acc; af <- foldExprValue(a) yield as :+ af
      }
      folded.map(as => TStructConstruct(st, as))
    case TCall(name, args, t) =>
      evaluateConstCallValue(name, args).flatMap { v =>
        val interp = new SyslInterpreter(_ => ())
        interp.valueToConstExpr(v, t)
      }
    case _ => None

  /** Fold one expression into a literal-composed form, dispatching on its
    * static type. Integer/bool/char yield `TIntLit`; float yields `TFloatLit`;
    * array/struct recurse through `tryConstEvalAggregate`. Returns `None` for
    * any kind we can't yet express as a static literal. Shared by the
    * aggregate folder and (eventually) any caller that needs a literal-shape
    * fold-result independent of int-vs-float dispatch. */
  protected def foldExprValue(e: TExpr): Option[TExpr] =
    val t = e.typ
    if t.isIntegral || t == SyslType.BoolType then
      tryConstEval(e).map(n => TIntLit(maskToType(n, t), t))
    else if t.isFloat then
      tryConstEvalFloat(e).map(d => TFloatLit(narrowFloatToType(d, t), t))
    else t match
      case _: SyslType.ArrayType | _: SyslType.StructType => tryConstEvalAggregate(e)
      case _ => None

  /** Try to evaluate a typed expression as a compile-time `Double`. Mirrors
    * `tryConstEval` for the float case: handles float literals, integer
    * literals widened to double, cross-binding lookups in either
    * `compileTimeFloats` or (widened) `compileTimeConstants`, unary minus,
    * the basic float arithmetic operators, casts (integer→float widening
    * or float→float retag), and `TCall` to a `#const fn` returning a
    * float type. */
  protected def tryConstEvalFloat(expr: TExpr): Option[Double] = expr match
    case TFloatLit(d, _) => Some(d)
    case TIntLit(n, t) if t.isIntegral => Some(n.toDouble)
    case TBoolLit(b, _) => Some(if b then 1.0 else 0.0)
    case TVarRef(name, _) =>
      compileTimeFloats.get(name).orElse(compileTimeConstants.get(name).map(_.toDouble))
    case TUnary("-", operand, _) => tryConstEvalFloat(operand).map(-_)
    case TUnary("+", operand, _) => tryConstEvalFloat(operand)
    case TBinary(left, "+", right, _) => for l <- tryConstEvalFloat(left); r <- tryConstEvalFloat(right) yield l + r
    case TBinary(left, "-", right, _) => for l <- tryConstEvalFloat(left); r <- tryConstEvalFloat(right) yield l - r
    case TBinary(left, "*", right, _) => for l <- tryConstEvalFloat(left); r <- tryConstEvalFloat(right) yield l * r
    case TBinary(left, "/", right, _) => for l <- tryConstEvalFloat(left); r <- tryConstEvalFloat(right) yield l / r
    case TCast(inner, _) =>
      if inner.typ.isFloat then tryConstEvalFloat(inner)
      else if inner.typ.isIntegral then tryConstEval(inner).map(_.toDouble)
      else None
    case TCall(name, args, t) if t.isFloat => evaluateConstCallFloat(name, args)
    case _ => None

