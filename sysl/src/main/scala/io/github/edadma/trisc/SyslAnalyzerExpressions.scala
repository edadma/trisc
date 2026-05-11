package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

/** Extracted from SyslAnalyzer.scala for navigability: analyzeExpr alone is
 *  ~1700 lines, and the surrounding interpolated/formatted-string helpers
 *  travel with it. Self-typed onto SyslAnalyzer so it can call back into
 *  analyzeStmt / analyzeBlock and read shared mutable state (functions,
 *  traits, structTypes, scopeStack, etc.). The companion access elevation
 *  pass changed every `private` member SyslAnalyzer used to `protected`. */
trait SyslAnalyzerExpressions:
  self: SyslAnalyzer =>

  /** Build the diagnostic for a forbidden mixed-signedness binary operation. The
   *  message names both operand types and suggests the two cast directions
   *  available — picking either side gives a working uniform-signedness expression.
   *  `kind` is `"mix"` for arithmetic / bitwise / shift, `"compare"` for relational. */
  protected def signednessMismatchMsg(op: String, left: SyslType, right: SyslType, kind: String): String =
    val verb = if kind == "compare" then "compare signed and unsigned" else s"mix signed and unsigned in $op"
    val (uType, sType) = (left, right) match
      case (u: SyslType.UIntType, s: SyslType.IntType) => (u.toString, s.toString)
      case (s: SyslType.IntType, u: SyslType.UIntType) => (u.toString, s.toString)
      case _ => (right.toString, left.toString)
    s"cannot $verb: $left $op $right — cast one side explicitly: " +
      s"`$uType(...)` to make both unsigned, or `$sType(...)` to make both signed " +
      s"(values larger than the signed type's max will wrap on the unsigned→signed cast)."

  protected def analyzeExpr(expr: ExpressionAST): TExpr =
    expr match
      case IntLitAST(n) =>
        // Promote to i64 if value doesn't fit in any 32-bit type.
        // Values up to 0xFFFFFFFF fit in u32, and negative values down to
        // -0x80000000 fit in i32, so only values outside that range need i64.
        if n > 0xFFFFFFFFL || n < -0x80000000L then TIntLit(n, I64)
        else TIntLit(n, I32)
      case TypedIntLitAST(n, typeName) => TIntLit(n, resolveType(NamedTypeAST(typeName)))
      case FloatLitAST(d) => TFloatLit(d, F64)
      case CharLitAST(c) => TIntLit(c.toLong, U32)
      case BoolLitAST(b) => TBoolLit(b, BoolType)
      case UnitLitAST()  => TUnitLit(UnitType)
      case StringLitAST(s) => TStringLit(s, StringType)
      case StringLitExprAST(s) =>
        if s.startsWith("s:") then analyzeInterpolatedString(s.substring(2))
        else if s.startsWith("f:") then analyzeFormattedString(s.substring(2))
        else TStringLit(s, StringType)
      case TypeAttrAST(typeName, attr, argOpt) =>
        val resolved: SyslType =
          if typeAliases.contains(typeName) then resolveType(NamedTypeAST(typeName))
          else if simpleEnumTypes.contains(typeName) then simpleEnumTypes(typeName)
          else throw AnalysisError(s"'$typeName' is not a type with attributes (expected constrained type or simple enum)")
        attr match
          case "First" | "Last" =>
            if argOpt.isDefined then throw AnalysisError(s"$typeName::$attr takes no arguments")
            analyzeTypeFirstLast(typeName, resolved, attr == "First")
          case "Range" =>
            throw AnalysisError(s"$typeName::Range is only valid in a 'for i in $typeName::Range' loop")
          case "Image" =>
            val arg = argOpt.getOrElse(throw AnalysisError(s"$typeName::Image requires one argument"))
            analyzeTypeImage(typeName, resolved, arg)
          case "Pos" =>
            val arg = argOpt.getOrElse(throw AnalysisError(s"$typeName::Pos requires one argument"))
            analyzeTypePos(typeName, resolved, arg)
          case "Val" =>
            val arg = argOpt.getOrElse(throw AnalysisError(s"$typeName::Val requires one argument"))
            analyzeTypeVal(typeName, resolved, arg)
          case "Succ" =>
            val arg = argOpt.getOrElse(throw AnalysisError(s"$typeName::Succ requires one argument"))
            analyzeTypeSuccPred(typeName, resolved, arg, isSucc = true)
          case "Pred" =>
            val arg = argOpt.getOrElse(throw AnalysisError(s"$typeName::Pred requires one argument"))
            analyzeTypeSuccPred(typeName, resolved, arg, isSucc = false)
          case "Value" =>
            val arg = argOpt.getOrElse(throw AnalysisError(s"$typeName::Value requires one argument"))
            analyzeTypeValueString(typeName, resolved, arg)
          case "Valid" =>
            val arg = argOpt.getOrElse(throw AnalysisError(s"$typeName::Valid requires one argument"))
            analyzeTypeValid(typeName, resolved, arg)
          case other =>
            throw AnalysisError(s"unknown type attribute: $typeName::$other (expected First, Last, Range, Image, Value, Valid, Pos, Val, Succ, Pred)")
      case TupleLitAST(elements) =>
        val tElems = elements.map(analyzeExpr)
        val tupleType = SyslType.tupleType(tElems.map(_.typ))
        TStructConstruct(tupleType, tElems)
      case ArrayDeclAST(size, typAST) =>
        val t = resolveType(typAST)
        TArrayDecl(size, t)

      case ArrayLitAST(elements) =>
        val tElems = elements.map(analyzeExpr)
        // The expected slice type, if context demands one. When `currentExpected`
        // is `[]T`, the literal must produce a slice descriptor (so `append`,
        // `len`, etc. work); when it's `[N]T` (or there is none), the literal
        // stays a fixed-size array. Without this, the long-standing footgun was:
        // `var xs: []int = [1, 2, 3]` silently produced a `[3]int` and any
        // later `append(xs, …)` would panic at runtime with "append requires a
        // slice". The wrap below is the same `arr[:]` operation users had to
        // write by hand (`(new [0]T)[:0]`); doing it in the analyzer makes
        // annotation-driven inference do what the user expects.
        val sliceTarget: Option[SyslType] = currentExpected.map(_.underlying) match
          case Some(SyslType.SliceType(et)) => Some(et)
          case _ => None
        if tElems.isEmpty then
          // Empty array literal — element type comes from `currentExpected`. The
          // canonical use case is the empty-accumulator idiom: `var xs: []int = []`,
          // `f() -> []int = []`, `Bag([])`, `match { ... -> [] }`. This mirrors the
          // expected-type-from-context rule that variant constructors with phantom
          // type parameters already use for `None`-style zero-data variants.
          //
          // Slice expected → produce a [0]T literal then wrap in TSliceExpr so the
          // runtime gets a proper SliceVal{cells, len:0, cap:0}. Fixed-array [0]T
          // expected → match directly. Other expected types fall through to the
          // unambiguous error.
          val elemType: SyslType = currentExpected.map(_.underlying) match
            case Some(SyslType.SliceType(et))      => et
            case Some(SyslType.ArrayType(et, 0))   => et
            case Some(SyslType.ArrayType(_, n))    =>
              throw AnalysisError(s"empty literal `[]` cannot satisfy fixed-array type with $n element(s)")
            case _ =>
              throw AnalysisError("cannot infer element type for empty array literal []")
          val arr = TArrayLit(Nil, SyslType.ArrayType(elemType, 0))
          sliceTarget match
            case Some(_) => TSliceExpr(arr, None, None, SyslType.SliceType(elemType))
            case None    => arr
        else
          val elemType = tElems.head.typ
          val arr = TArrayLit(tElems, SyslType.ArrayType(elemType, tElems.length))
          sliceTarget match
            case Some(et) if compatible(elemType, et) =>
              TSliceExpr(arr, None, None, SyslType.SliceType(et))
            case _ => arr

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
          // `_` discard binders get a unique synthetic name. Multiple `_` slots
          // are distinct (no name collision in the TParam list / codegen frame)
          // and `_` in the body still falls through to the placeholder rule.
          val finalName =
            if p.name == "_" then
              val n = s"__discard_$discardParamCounter"
              discardParamCounter += 1
              n
            else p.name
          TParam(finalName, paramType)
        }
        val expectedRet = expectedFunc.map(_.returnType).getOrElse(
          currentExpected match
            case Some(t) if t != UnitType => t
            case _ => UnitType
        )
        // Push scope with closure params (discard params are NOT bound — `__discard_<n>`
        // is unspellable in source and the placeholder rule for `_` in expression position
        // is preserved).
        pushScope()
        for (p, src) <- typedParams.zip(params) do
          if src.name != "_" then
            currentScope(p.name) = SymInfo(p.name, p.typ, mutable = false)
        // Analyze body
        val savedExp = currentExpected
        currentExpected = if expectedRet == UnitType then None else Some(expectedRet)
        val tBody = try body match
          case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
          case BlockBodyAST(stmts, contracts) =>
            // Route through the contract-aware analysis path so require/ensure
            // clauses become TContractCheck nodes baked into the body. Closures
            // can therefore carry contracts even when they capture outer-scope
            // state — the inner-def lift (in liftInnerDefClusters) is still
            // preferred for the no-capture case (avoids closure-construction
            // overhead), but capturing closures with contracts now compile
            // straight to TClosure with TContractCheck nodes.
            //
            // selfMangledName="" — variant clauses on a closure are not yet
            // supported (would need to know the synthesized name at runtime).
            // The capture scanner below picks up any vars referenced by the
            // contract expressions and adds them to the closure's captures,
            // so a `require x >= base` inside a closure correctly captures
            // `base` from outer scope.
            analyzeBlockWithContracts(stmts, contracts, expectedRet)
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
          def walk(p: TMatchPattern): Unit = p match
            case TVariantPattern(_, _, bindings, _, nested) =>
              bindings.flatten.foreach(b += _)
              nested.flatten.foreach(walk)
            case TDestructurePattern(_, bindings, _, nested) =>
              bindings.flatten.foreach(b += _)
              nested.flatten.foreach(walk)
            case _ => ()
          for p <- pats do walk(p)
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
          case TClosure(innerParams, _, innerBody, _, _, _, _) =>
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
          case TVarStmt(name, _, init, _, _) =>
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
          case TWhileStmt(c, body, _) =>
            scanCaptures(c, locals)
            scanStmtSeq(body, locals)
            locals
          case TForStmt(init, c, upd, body, _) =>
            var Lf = scanStmtInSeq(init, locals)
            scanCaptures(c, Lf)
            Lf = scanStmtInSeq(upd, Lf)
            scanStmtSeq(body, Lf)
            locals
          case TDoWhileStmt(c, body, _) =>
            scanStmtSeq(body, locals)
            scanCaptures(c, locals)
            locals
          case TLoopStmt(body, _) =>
            scanStmtSeq(body, locals)
            locals
          case TDeferStmt(inner) =>
            scanStmtInSeq(inner, locals)
            locals
          case TAsmStmt(_) => locals
          case _: TBreakStmt | _: TContinueStmt => locals
          // analyzeBlockWithContracts (now used by the closure-body path so
          // require/ensure on capturing inner defs work) emits TContractCheck
          // nodes whose conditions may reference outer-scope state — without
          // this arm those references are silently dropped from the capture
          // list and the closure runs with a stale env. Same for TMultiStmt
          // which the contract path uses to wrap snapshot decls + checks.
          case TContractCheck(_, e, _) =>
            scanCaptures(e, locals)
            locals
          case TMultiStmt(xs) =>
            scanStmtSeq(xs, locals)
            locals
          case _ => locals
        tBody match
          case TExprBody(e) => scanCaptures(e, paramNames)
          case TBlockBody(stmts) => scanStmtSeq(stmts, paramNames)
        // Determine actual return type. When the call-site context supplied an
        // expected FuncType (e.g. inner defs always pin one, or a callback
        // arg's parameter type), use it directly — body inspection would lose
        // the type when the body has been rewritten by analyzeBlockWithContracts
        // (the trailing TExprStmt gets wrapped into TMultiStmt + return). Only
        // anonymous closures with no context fall back to last-stmt inference.
        val actualRet = expectedFunc match
          case Some(ft) => ft.returnType
          case None => tBody match
            case TExprBody(e) => e.typ
            case TBlockBody(stmts) =>
              stmts.lastOption match
                case Some(TExprStmt(e)) => e.typ
                case _ => UnitType
        // Determine if this closure escapes — it does if the expected type is @escaping,
        // or if there is no expected type (e.g. assigned to a local with no annotation).
        val escapesFlag = expectedFunc match
          case Some(ft) => ft.escaping
          case None => true  // conservative: no context → assume escaping
        // Infer closure effects from the body. The inference produces one of:
        //   - Pure (no module-level reads/writes, no allocation, all calls pure)
        //   - RW(R, W) (specific module-level vars read/written, all called functions
        //     annotated so their effects could be absorbed)
        //   - Unknown (something un-summarizable — `new`/append/asm/impure-unannotated call
        //     or a write to a captured outer local)
        val inferredEffects = inferClosureEffects(tBody, typedParams.map(_.name))
        TClosure(typedParams, actualRet, tBody, captures.toList, escapesFlag, inferredEffects)

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
          case PtrType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
          case RefType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPreInc(resolvedObj, idx, structType.fields(idx)._2)

      case FieldPreDecAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
          case RefType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPreDec(resolvedObj, idx, structType.fields(idx)._2)

      case FieldPostIncAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
          case RefType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPostInc(resolvedObj, idx, structType.fields(idx)._2)

      case FieldPostDecAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
          case RefType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
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
        // #address MMIO var: read lowers to *(addr as *T). Keep the cast explicit so
        // each codegen can emit a volatile load at the literal address.
        if fixedAddressVars.contains(name) then
          val (addr, typ) = fixedAddressVars(name)
          return TDeref(TCast(TIntLit(addr, I64), PtrType(typ)), typ)
        // Local shadow: a function-local binding (param, val, var, pattern-binder,
        // implicit local from bare `name = expr`) takes precedence over any like-named
        // global function. Without this, `dispatch = (a: int) -> a` followed by a bare
        // `dispatch` reference would resolve to a global `dispatch(...)` function instead
        // of the just-created local closure value. See also the matching check in CallAST.
        lookupLocal(name) match
          case Some(sym) if sym.isConst =>
            val v = compileTimeConstants.getOrElse(sym.name,
              compileTimeConstants.getOrElse(name,
                throw AnalysisError(s"const '$name' missing folded value")))
            return TIntLit(v, sym.typ)
          case Some(sym) if sym.isByName =>
            // By-name param: storage is `() -> T`, every reference auto-calls
            // (no memoization). Forwarding to another by-name slot at a call
            // site re-wraps as `() -> name()`, which is correct (each outer
            // eval re-enters the original thunk).
            val thunkType = FuncType(Nil, sym.typ, effects = FuncEffects.Unknown)
            return TIndirectCall(TVarRef(sym.name, thunkType), Nil, sym.typ)
          case Some(sym) if sym.autoIndirect =>
            return TDeref(TVarRef(sym.name, PtrType(sym.typ)), sym.typ)
          case Some(sym) =>
            return TVarRef(sym.name, sym.typ)
          case None => ()
        // Check if name is a function (used as a value = function pointer)
        if functions.contains(name) then
          val f = functions(name)
          if f.autoCallsBare then
            // Auto-call: bare reference to a `def` or parameterless function
            // emits a call. Both forms are designed to read like a value at
            // the use site — the call is implicit.
            TCall(f.name, Nil, f.returnType)
          else
            TFuncRef(f.name, FuncType(f.params.map(_._2), f.returnType, effects = funInfoEffects(f)))
        else if builtinFunctions.contains(name) then
          val f = builtinFunctions(name)
          TFuncRef(name, FuncType(f.params.map(_._2), f.returnType))
        else
          // Check for no-arg enum variant before falling through to variable lookup
          tryLookup(name) match
            case Some(sym) if sym.isConst =>
              // Inline compile-time constant — no load, no storage reference.
              val v = compileTimeConstants.getOrElse(sym.name,
                compileTimeConstants.getOrElse(name,
                  throw AnalysisError(s"const '$name' missing folded value")))
              TIntLit(v, sym.typ)
            case Some(sym) if sym.isByName =>
              val thunkType = FuncType(Nil, sym.typ, effects = FuncEffects.Unknown)
              TIndirectCall(TVarRef(sym.name, thunkType), Nil, sym.typ)
            case Some(sym) if sym.autoIndirect =>
              // Out/Inout param: reads auto-dereference the hidden pointer so the body
              // sees a plain T value.
              TDeref(TVarRef(sym.name, PtrType(sym.typ)), sym.typ)
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
          TFuncRef(f.name, FuncType(f.params.map(_._2), f.returnType, effects = funInfoEffects(f)))
        else
          val sym = lookup(name)
          // User-defined `#operator("&")`: when the local's type sits OUTSIDE
          // the built-in domain (i.e. it's a struct, enum, or nominal alias),
          // try a user-impl dispatch first. The trait method receives the
          // *value* of the local (not its address), matching the prefix-op
          // contract for other custom operators.
          if !builtinPrefixDomainContains("&", sym.typ) && customUnaryOperatorTraits.contains("&") then
            return tryUnaryOperatorDispatch("&", TVarRef(sym.name, sym.typ))
          TAddrOf(sym.name, PtrType(sym.typ))

      case AddrOfFieldAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
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
        // Built-in `*` owns pointer/ref/array shapes (matched on `.underlying`
        // to preserve the existing nominal-alias unwrap). For anything else,
        // try a user `#operator("*")` impl before raising the no-deref error.
        tInner.typ.underlying match
          case PtrType(t) => return TDeref(tInner, t)
          case RefType(t) => return TDeref(tInner, t)
          case ArrayType(t, _) => return TDeref(tInner, t)
          case StringType => throw AnalysisError("cannot dereference string — use indexing instead")
          case SliceType(_) => throw AnalysisError("cannot dereference slice — use indexing instead")
          case _ => ()
        if customUnaryOperatorTraits.contains("*") then
          return tryUnaryOperatorDispatch("*", tInner)
        throw AnalysisError(s"cannot dereference ${tInner.typ}")

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
        tArr.typ match
          case StringType =>
            TSliceExpr(tArr, tLow, tHigh, StringType)
          case SliceType(elem) =>
            TSliceExpr(tArr, tLow, tHigh, SliceType(elem))
          case RefType(SliceType(elem)) =>
            TSliceExpr(tArr, tLow, tHigh, SliceType(elem))
          case ArrayType(elem, _) =>
            TSliceExpr(tArr, tLow, tHigh, SliceType(elem))
          case t => throw AnalysisError(s"cannot sub-slice $t")

      case FieldAccessAST(VarRefAST(nsName), member) if moduleNamespaces.contains(nsName) =>
        // Qualified import access: strings.MAX_LEN
        val meta = moduleNamespaces(nsName)
        val sym = meta.publicSymbols.find(s => shortName(s.name) == member)
          .getOrElse(throw AnalysisError(s"module '$nsName' has no symbol '$member'"))
        sym.typ match
          case SymbolMeta.Kind.Data(dataType, _) => TVarRef(sym.name, dataType)
          case SymbolMeta.Kind.Const(constType, value) => TIntLit(value, constType)
          case SymbolMeta.Kind.Func(params, retType, _, _, _, eff, _) => TFuncRef(sym.name, SyslType.FuncType(params, retType, effects = eff))
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
        // Auto-dereference pointers to structs (p.x works like (*p).x).
        // Non-struct receivers can still match a parameterless extension
        // method (`extension (s: string) def shout -> string = s` makes
        // `"hi".shout` legal); route through tryExtensionDispatch with
        // empty args before raising the no-field error.
        val structOpt: Option[(TExpr, StructType)] = tObj.typ match
          case st: StructType          => Some((tObj, st))
          case PtrType(st: StructType) => Some((TDeref(tObj, latestStruct(st)), st))
          case RefType(st: StructType) => Some((TDeref(tObj, latestStruct(st)), st))
          case _                       => None
        if structOpt.isEmpty then
          // `s.len` on string / slice / array / ref-slice is the field-access
          // sugar for `len(s)` — produce the same TLen the call-form does.
          // Symmetric to the `CallAST("len", args)` arm earlier in this match.
          // Without this, every other indexable type's UX papercut path was
          // "use len(x)"; with it, both spellings work.
          if field == "len" then
            tObj.typ.underlying match
              case StringType | _: SliceType | _: ArrayType | RefType(_: SliceType) =>
                return TLen(tObj, I32)
              case _ =>
          return tryExtensionDispatch(field, tObj, Nil).getOrElse(
            throw AnalysisError(s"cannot access field '$field' on ${tObj.typ}"))
        val (resolvedObj, structType0) = structOpt.get
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then
          // No matching field. Before reaching for an extension, try the
          // existing struct-method convention — a top-level function named
          // `<StructName>_<field>` is a real method and must beat any
          // extension of the same name (the no-surprises rule).
          val methodFnName = s"${structType.name}_$field"
          if functions.contains(methodFnName) then
            val funInfo = functions(methodFnName)
            // Build self argument (mirrors the MethodCallAST struct path).
            val selfArg = tObj.typ match
              case st: StructType => tObj match
                case TVarRef(n, _)                  => TAddrOf(n, PtrType(st))
                case TFieldAccess(innerObj, i, _)   => TAddrOfField(innerObj, i, PtrType(st))
                case TIndex(arr, i, _)              => TAddrOfIndex(arr, i, PtrType(st))
                case _                              => TTempAddr(tObj, PtrType(st))
              case _ => tObj
            return TCall(funInfo.name, List(selfArg), funInfo.returnType)
          return tryExtensionDispatch(field, tObj, Nil).getOrElse(
            throw AnalysisError(s"struct ${structType.name} has no field '$field'"))
        TFieldAccess(resolvedObj, idx, structType.fields(idx)._2)

      case PreIncAST(name) => val s = lookup(name); TPreInc(s.name, s.typ)
      case PreDecAST(name) => val s = lookup(name); TPreDec(s.name, s.typ)
      case PostIncAST(name) => val s = lookup(name); TPostInc(s.name, s.typ)
      case PostDecAST(name) => val s = lookup(name); TPostDec(s.name, s.typ)

      case UnaryAST(op, operand) =>
        val tOperand = analyzeExpr(operand)
        // `&` falls through the parser as `UnaryAST("&", expr)` whenever the
        // operand isn't one of the bare-identifier / field / index lvalue
        // shapes the parser handles directly. Lvalue-vs-rvalue + built-in-vs-
        // user-impl decisions move here:
        //   1. If a `#operator("&")` impl matches the operand type AND the
        //      type is outside `&`'s built-in domain, dispatch to it.
        //   2. Else if the operand reduces to a recognizable lvalue, lower
        //      to the existing built-in `TAddrOf*` family.
        //   3. Else error — neither shape applies.
        if op == "&" then
          // `&funcName` (where `funcName` is a global function) is just the
          // function pointer. `analyzeExpr(VarRefAST(name))` already lowered
          // it to a `TFuncRef` by the time we get here, so the `&` is a
          // no-op — return the operand unchanged. Same applies to a bare
          // function reference produced through any other path.
          tOperand match
            case fr: TFuncRef => return fr
            case _ => ()
          if customUnaryOperatorTraits.contains("&") &&
             !builtinPrefixDomainContains("&", tOperand.typ) then
            tryUnaryOperatorDispatchOpt("&", tOperand) match
              case Some(t) => return t
              case None    => () // fall through to built-in lvalue lowering
          tOperand match
            case TVarRef(n, t)             => return TAddrOf(n, PtrType(t))
            case TFieldAccess(obj, idx, t) => return TAddrOfField(obj, idx, PtrType(t))
            case TIndex(arr, ix, t)        => return TAddrOfIndex(arr, ix, PtrType(t))
            case TDeref(p, _)              => return p // &*p = p
            case _ =>
              throw AnalysisError(
                s"cannot take address of ${tOperand.typ}: not an lvalue and no #operator(\"&\") impl matches")
        // Dispatch order for `-`, `!`, `~`: built-in semantics own their
        // natural operand types (numeric, bool, integral). For an operand
        // *outside* that domain, fall through to a user `#operator(<sigil>)`
        // impl if one is registered. Pure-user prefix ops (e.g. `<>`) skip
        // the built-in branch entirely and go straight to dispatch.
        val isBuiltinSigil = op == "-" || op == "!" || op == "~"
        if isBuiltinSigil && builtinPrefixDomainContains(op, tOperand.typ) then
          val resultType = op match
            case "-" => tOperand.typ
            case "~" => tOperand.typ
            case "!" => BoolType
            case _   => throw AnalysisError(s"impossible op: $op")
          return TUnary(op, tOperand, resultType)
        if customUnaryOperatorTraits.contains(op) then
          return tryUnaryOperatorDispatch(op, tOperand)
        val resultType = op match
          case "-" => throw AnalysisError(s"unary - requires numeric type, got ${tOperand.typ}; bind it via #operator(\"-\") on a single-param trait method")
          case "~" => throw AnalysisError(s"unary ~ requires integral type, got ${tOperand.typ}; bind it via #operator(\"~\") on a single-param trait method")
          case "!" => throw AnalysisError(s"unary ! requires bool, got ${tOperand.typ}; bind it via #operator(\"!\") on a single-param trait method")
          case other =>
            throw AnalysisError(
              s"unknown prefix operator '$other' on ${tOperand.typ}; bind it via #operator(\"$other\") on a single-param trait method")
        TUnary(op, tOperand, resultType)

      case BinaryAST(left0, op, right0) =>
        // By-name auto-wrap on operator dispatch: if the operator resolves to a
        // trait method whose param at slot 0/1 is `=> T`, wrap the operand AST
        // in `ClosureAST(Nil, ExprBodyAST(...))` BEFORE analyzing. Both the
        // LHS-bound and RHS-bound by-name slots are supported, since the trait
        // declaration is the source of truth and impls must match it. Without
        // the wrap, the operand evaluates eagerly at the operator site —
        // defeating the whole point of a `or_op(a: T, b: => T)` declaration.
        val (lByName, rByName): (Boolean, Boolean) =
          lookupBinaryOperatorTrait(op) match
            case Some((tn, mn)) if traits.contains(tn) =>
              traits(tn).methods.find(_.name == mn) match
                case Some(tm) if tm.params.length >= 2 =>
                  (tm.params(0).typ.isInstanceOf[ByNameTypeAST],
                   tm.params(1).typ.isInstanceOf[ByNameTypeAST])
                case _ => (false, false)
            case _ => (false, false)
        val left  = if lByName then ClosureAST(Nil, ExprBodyAST(left0))  else left0
        val right = if rByName then ClosureAST(Nil, ExprBodyAST(right0)) else right0
        val tLeft00 = analyzeExpr(left)
        // Operator-dispatch expected-type forwarding (Bug B): if `op` resolves
        // to a trait and the LHS already pins down enough type-vars, push the
        // matching impl's second-formal-param type as `currentExpected` while
        // analyzing the RHS. Without this, a closure-literal RHS with a `_`
        // placeholder fails its own type inference *before* dispatch even runs.
        val rhsExpected: Option[SyslType] =
          lookupBinaryOperatorTrait(op).flatMap { case (tn, mn) =>
            expectedTypeForBinaryOpRhs(tn, mn, tLeft00.typ, currentExpected)
          }
        val tRight00 =
          if rhsExpected.isEmpty then analyzeExpr(right)
          else
            val savedExp = currentExpected
            currentExpected = rhsExpected
            try analyzeExpr(right) finally currentExpected = savedExp
        // Operator overloading on nominal aliases (`type Parser[A] = new ...`,
        // `type Meters = new f64`, etc.) needs the dispatcher to see the outer
        // NamedType — not the underlying — or `impl Concat[Parser[X], ...]`
        // never matches an operand whose static type is `Parser[i32]`.
        // Try dispatch first, before any nominal-unwrap or signedness coercion.
        // Strict mode: at least one operand can't fall back to built-in arithmetic
        // (struct, enum, or nominal alias of a non-numeric). Lenient otherwise so
        // `Meters + Meters` without an impl still does plain int arithmetic.
        def couldFallToArith(t: SyslType): Boolean =
          t.underlying.isNumeric || t.underlying == StringType || t.underlying == BoolType
        val strict = !couldFallToArith(tLeft00.typ) || !couldFallToArith(tRight00.typ)
        val dispatchedOpt = tryOperatorDispatch(op, tLeft00, tRight00, strict)
        if dispatchedOpt.isDefined then return dispatchedOpt.get
        // Handle NamedType operands:
        //   nominal + nominal (same name)  → result keeps that nominal type
        //   nominal + anything else         → error (explicit cast required)
        //   non-nominal (subtype) wrappers  → unwrapped to the base for arithmetic
        val isCmp = Set("==", "!=", "<", ">", "<=", ">=").contains(op)
        val nominalResult: Option[SyslType] = (tLeft00.typ, tRight00.typ) match
          case (l @ NamedType(n1, _, true, _, _), r @ NamedType(n2, _, true, _, _)) =>
            if n1 != n2 then
              throw AnalysisError(s"cannot apply '$op' between nominal types $n1 and $n2; cast explicitly")
            if isCmp then None else Some(l)
          case (NamedType(n, _, true, _, _), other) =>
            throw AnalysisError(s"cannot apply '$op' between nominal type $n and ${other}; cast explicitly")
          case (other, NamedType(n, _, true, _, _)) =>
            throw AnalysisError(s"cannot apply '$op' between ${other} and nominal type $n; cast explicitly")
          case _ => None
        val tLeft0 = if tLeft00.typ.isInstanceOf[NamedType] then TCast(tLeft00, tLeft00.typ.underlying) else tLeft00
        val tRight0 = if tRight00.typ.isInstanceOf[NamedType] then TCast(tRight00, tRight00.typ.underlying) else tRight00
        // Coerce integer literal signedness to match the other operand (preserve width)
        val tLeft = if tRight0.typ.isIntegral then coerceSignedness(tLeft0, tRight0.typ) else tLeft0
        val tRight = if tLeft.typ.isIntegral then coerceSignedness(tRight0, tLeft.typ) else tRight0
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
              case (FloatType(a), FloatType(b)) => FloatType(a max b)
              case (_: FloatType, _) => tLeft.typ
              case (_, _: FloatType) => tRight.typ
              case (IntType(a), IntType(b)) => IntType(a max b)
              case (UIntType(a), UIntType(b)) => UIntType(a max b)
              case (UIntType(a), IntType(b)) if a < b => IntType(b)   // unsigned fits in signed
              case (IntType(a), UIntType(b)) if b < a => IntType(a)   // unsigned fits in signed
              case (l, r) if l.isIntegral && r.isIntegral =>
                throw AnalysisError(signednessMismatchMsg(op, tLeft.typ, tRight.typ, "mix"))
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
                throw AnalysisError(signednessMismatchMsg(op, tLeft.typ, tRight.typ, "mix"))
          case "==" | "!=" | "<" | ">" | "<=" | ">=" =>
            // Disallow mixed signed/unsigned comparisons unless unsigned fits in signed
            if tLeft.typ.isIntegral && tRight.typ.isIntegral then
              (tLeft.typ, tRight.typ) match
                case (UIntType(a), IntType(b)) if a >= b =>
                  throw AnalysisError(signednessMismatchMsg(op, tLeft.typ, tRight.typ, "compare"))
                case (IntType(a), UIntType(b)) if b >= a =>
                  throw AnalysisError(signednessMismatchMsg(op, tLeft.typ, tRight.typ, "compare"))
                case _ => // ok: same signedness, or unsigned fits in signed
            BoolType
          case "&&" | "||" =>
            if tLeft.typ != BoolType then throw AnalysisError(s"$op requires bool operands, got ${tLeft.typ}")
            if tRight.typ != BoolType then throw AnalysisError(s"$op requires bool operands, got ${tRight.typ}")
            BoolType
          case _ =>
            // Unknown operator. Distinguish three cases for the user:
            //   (a) the operator IS bound to a trait, but neither operand is a user
            //       type that impls that trait → suggest casting / type-wrapping
            //   (b) the operator looks user-defined (composed of op chars) but is
            //       not bound anywhere → tell them how to bind it via #operator
            //   (c) the operator is genuinely garbage (shouldn't happen post-parse)
            lookupBinaryOperatorTrait(op) match
              case Some((traitName, _)) =>
                throw AnalysisError(
                  s"operator '$op' is bound to trait '$traitName', but no impl matches operand types (${tLeft.typ}, ${tRight.typ}) — operands must be a struct, enum, or nominal alias (`type T = new ...`) that impls '$traitName'",
                )
              case None if op.forall(c => "+-*/%<>=!&|^~".contains(c)) =>
                throw AnalysisError(
                  s"operator '$op' is not bound; declare it via `#operator(\"$op\")` on a trait method",
                )
              case None =>
                throw AnalysisError(s"unknown operator: $op")
        // Insert implicit int→float promotion / float-width casts for mixed operands
        val promotedLeft  = if resultType.isFloat && tLeft.typ  != resultType then TCast(tLeft,  resultType) else tLeft
        val promotedRight = if resultType.isFloat && tRight.typ != resultType then TCast(tRight, resultType) else tRight
        val finalResultType = nominalResult.getOrElse(resultType)
        val binExpr = TBinary(promotedLeft, op, promotedRight, resultType)
        if nominalResult.isDefined then TCast(binExpr, finalResultType) else binExpr

      case CastAST(targetTypeAST, inner) =>
        val tInner = analyzeExpr(inner)
        val target = resolveType(targetTypeAST)
        // Validate cast is possible. NamedTypes unwrap to their bases for the validity check —
        // wrapping/unwrapping into named is always allowed when the bases are cast-compatible.
        (tInner.typ.underlying, target.underlying) match
          case (from, to) if from == to => // no-op cast
          // bool conversions
          case (from, BoolType) if from.isNumeric => // numeric to bool: != 0
          case (BoolType, to) if to.isNumeric => // bool to numeric: true=1, false=0
          case (_: PtrType | _: RefType | _: FuncType, BoolType) => // pointer/ref/func to bool: null check
          // float conversions
          case (from, _: FloatType) if from.isIntegral => // int to float (cvt)
          case (_: FloatType, to) if to.isIntegral => // float to int (fint)
          case (_: FloatType, _: FloatType) => // float ↔ float (fpext / fptrunc)
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
          // array decay conversions
          case (ArrayType(_, _), _: PtrType) => // array decays to pointer (address of first element)
          case (ArrayType(_, _), to) if to.isIntegral => // array to integer (address of first element as int)
          case (_, _) => throw AnalysisError(s"cannot cast ${tInner.typ} to $target")
        // If the target is a constrained NamedType, emit a range check (compile- or run-time)
        // instead of a plain cast. The check takes the cast-to-underlying value, not the raw input.
        target match
          case nt @ NamedType(_, base, _, Some(_), _) =>
            val coreCast = if tInner.typ.underlying == base then tInner else TCast(tInner, base)
            applyTargetType(coreCast, nt)
          case _ => TCast(tInner, target)

      case CallAST("old", args) if inEnsureAnalysis =>
        if args.length != 1 then throw AnalysisError("old() takes exactly 1 argument")
        // Temporarily suspend the intercept so nested `old(old(...))` cases fall through
        // to a normal CallAST (undefined function) — we disallow nesting for now.
        val savedMode = inEnsureAnalysis
        inEnsureAnalysis = false
        val tArg = try analyzeExpr(args.head) finally inEnsureAnalysis = savedMode
        val snapshotName = s"__old_${oldSnapshotCounter}"
        oldSnapshotCounter += 1
        oldSnapshots += ((snapshotName, tArg.typ, tArg))
        if scopeStack != null then
          currentScope(snapshotName) = SymInfo(snapshotName, tArg.typ, mutable = false)
        TVarRef(snapshotName, tArg.typ)

      case CallAST("loop_entry", args) if inLoopInvariantAnalysis =>
        if args.length != 1 then throw AnalysisError("loop_entry() takes exactly 1 argument")
        // Suspend the intercept so nested `loop_entry(loop_entry(...))` falls through.
        val savedMode = inLoopInvariantAnalysis
        inLoopInvariantAnalysis = false
        val tArg = try analyzeExpr(args.head) finally inLoopInvariantAnalysis = savedMode
        val snapshotName = s"__loop_entry_${loopEntrySnapshotCounter}"
        loopEntrySnapshotCounter += 1
        loopEntrySnapshots += ((snapshotName, tArg.typ, tArg))
        if scopeStack != null then
          currentScope(snapshotName) = SymInfo(snapshotName, tArg.typ, mutable = false)
        TVarRef(snapshotName, tArg.typ)

      case CallAST("loop_entry", _) =>
        throw AnalysisError("loop_entry() is only valid inside a loop invariant")

      case CallAST(name, args) if integerArithIntrinsics.contains(name) =>
        if args.size != 2 then throw AnalysisError(s"$name() takes exactly 2 arguments")
        val tA = analyzeExpr(args(0))
        val tB = analyzeExpr(args(1))
        if !tA.typ.isIntegral then throw AnalysisError(s"$name() requires integer arguments, got ${tA.typ}")
        if tA.typ != tB.typ then throw AnalysisError(s"$name() requires both arguments to have the same type, got ${tA.typ} and ${tB.typ}")
        TIntrinsicCall(name, List(tA, tB), tA.typ)

      case CallAST("str", args) =>
        if args.size != 1 then throw AnalysisError("str() takes exactly 1 argument")
        val tArg = analyzeExpr(args.head)
        tArg.typ.underlying match
          case StringType => tArg // identity — already a string
          case t if t.isNumeric || t == BoolType => TStr(tArg)
          case et: EnumType =>
            val funcName = materializeEnumStrFunc(et)
            TCall(funcName, List(tArg), StringType)
          case t => throw AnalysisError(s"str() not supported on $t")

      case CallAST("string", args) =>
        args.size match
          case 2 =>
            // string(ptr, len) — construct string from *byte + length
            val tPtr = analyzeExpr(args(0))
            val tLen = analyzeExpr(args(1))
            if !tPtr.typ.isInstanceOf[PtrType] && !tPtr.typ.isInstanceOf[ArrayType] then
              throw AnalysisError(s"string() first argument must be a pointer or array, got ${tPtr.typ}")
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
        if genericStructs.contains(name) || genericTemplates.contains(name) || genericTypeAliases.contains(name) =>
        val typeArg = resolveType(exprToTypeAST(typeExpr))
        // For a generic-alias cast `Parser[i32](closure)`, propagate the alias's underlying
        // type as the expected type for the (single) cast argument so that closure-shaped
        // args get parameter inference, return-type context, and downstream type-arg
        // inference for variant constructors that don't pin all type params from arg types.
        //
        // For a generic-function call `success[[]int]([])`, do the analogous thing per-arg:
        // substitute the explicit type args into each formal param's TypeAST and use the
        // result as the expected type during that arg's analysis. Without this, an empty
        // `[]` (or any context-dependent literal) at the call site fails with "cannot infer
        // element type" — the analyzer has all the info to know it's `[]int` but never
        // forwards it. This mirrors the var-decl/return/non-generic-arg paths the
        // array-lit-to-slice fix already plugged into.
        val tArgs =
          if genericTypeAliases.contains(name) && args.length == 1 then
            val target = resolveType(NamedTypeAST(name, List(exprToTypeAST(typeExpr))))
            val savedExp = currentExpected
            currentExpected = Some(target.underlying)
            try args.map(analyzeExpr) finally currentExpected = savedExp
          else if genericTemplates.contains(name) then
            val template = genericTemplates(name)
            val typeArgAST = exprToTypeAST(typeExpr)
            // Single-tparam case is the only shape this AST node carries (the parser
            // produces IndirectCallAST(IndexAST(...), args) with one type-arg slot).
            val subst: Map[String, TypeAST] =
              if template.typeParams.length == 1 then Map(template.typeParams.head -> typeArgAST)
              else Map.empty
            if template.params.length == args.length && subst.nonEmpty then
              args.zip(template.params).map { case (a, p) =>
                val expected =
                  try Some(resolveType(substituteTypeAST(p.typ, subst)))
                  catch case _: Throwable => None
                val savedExp = currentExpected
                currentExpected = expected.orElse(savedExp)
                try analyzeExpr(a) finally currentExpected = savedExp
              }
            else
              args.map(analyzeExpr)
          else
            args.map(analyzeExpr)
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
        else if genericTypeAliases.contains(name) then
          // Generic-alias cast: `Parser[i32](closure)`. For nominal aliases this wraps
          // the value in a TCast to the NamedType (the explicit-cast requirement is what
          // makes the type nominal in the first place). For transparent aliases the cast
          // is a no-op — use the resolved underlying type directly.
          if tArgs.length != 1 then
            throw AnalysisError(s"generic alias '$name[...]' cast expects exactly 1 argument, got ${tArgs.length}")
          val target = resolveType(NamedTypeAST(name, List(exprToTypeAST(typeExpr))))
          val arg = tArgs.head
          target match
            case nt @ SyslType.NamedType(_, base, true, _, _) =>
              val coreCast =
                if arg.typ.underlying == base.underlying then arg
                else if compatible(arg.typ, base) then arg
                else throw AnalysisError(s"cannot cast ${arg.typ} to '$name[...]' (underlying $base)")
              TCast(coreCast, nt)
            case other =>
              if compatible(arg.typ, other) then arg
              else throw AnalysisError(s"cannot cast ${arg.typ} to transparent alias '$name[...]' (= $other)")
        else
          val (mangled, funInfo) = instantiateGeneric(name, tArgs.map(_.typ), List(typeArg))
          val checkedArgs = checkArgs(mangled, funInfo.params, tArgs, funInfo.modes)
          TCall(mangled, checkedArgs, funInfo.returnType)

      case IndirectCallAST(callee, args) =>
        val tCallee = analyzeExpr(callee)
        val tArgs = args.map(analyzeExpr)
        tCallee.typ.underlying match
          case FuncType(paramTypes, returnType, _, _) =>
            val params = paramTypes.zipWithIndex.map { case (t, i) => (s"arg$i", t) }
            val checkedArgs = checkArgs("<indirect>", params, tArgs)
            TIndirectCall(tCallee, checkedArgs, returnType)
          case other =>
            throw AnalysisError(s"cannot call expression of type ${tCallee.typ} as a function")

      // Explicit multi-type-arg call: `Name[T1, T2, ...](args)`. The single-type-arg
      // form rides the IndirectCallAST(IndexAST(...), args) pattern above; this branch
      // handles ≥2 type args, parsed as `GenericCallAST(VarRefAST(name), typeArgs, args)`.
      case GenericCallAST(VarRefAST(name), typeExprs, args)
        if genericStructs.contains(name) || genericTemplates.contains(name) || genericTypeAliases.contains(name) =>
        val typeArgs = typeExprs.map(t => resolveType(exprToTypeAST(t)))
        // Per-arg expected-type forwarding mirrors the single-arg path: substitute
        // explicit type args into the callee's formal param/target types so closure
        // shapes and context-dependent literals at the call site get inference.
        val tArgs =
          if genericTypeAliases.contains(name) then
            // Generic alias cast — must take exactly 1 value arg, so multi-type-arg
            // alias casts are exotic but legal. Forward the alias's underlying type
            // to that arg.
            val target = resolveType(NamedTypeAST(name, typeExprs.map(exprToTypeAST)))
            val savedExp = currentExpected
            currentExpected = Some(target.underlying)
            try args.map(analyzeExpr) finally currentExpected = savedExp
          else if genericTemplates.contains(name) then
            val template = genericTemplates(name)
            val typeArgASTs = typeExprs.map(exprToTypeAST)
            val subst: Map[String, TypeAST] =
              if template.typeParams.length == typeArgASTs.length then
                template.typeParams.zip(typeArgASTs).toMap
              else Map.empty
            if template.params.length == args.length && subst.nonEmpty then
              args.zip(template.params).map { case (a, p) =>
                val expected =
                  try Some(resolveType(substituteTypeAST(p.typ, subst)))
                  catch case _: Throwable => None
                val savedExp = currentExpected
                currentExpected = expected.orElse(savedExp)
                try analyzeExpr(a) finally currentExpected = savedExp
              }
            else
              args.map(analyzeExpr)
          else
            args.map(analyzeExpr)
        if genericStructs.contains(name) then
          val st = instantiateGenericStruct(name, typeArgs)
          if tArgs.length != st.fields.length then
            throw AnalysisError(s"struct '${st.name}' has ${st.fields.length} field(s), got ${tArgs.length} argument(s)")
          val checkedArgs = tArgs.zip(st.fields).map { case (arg, (fieldName, fieldType)) =>
            val coerced = coerceLiteral(arg, fieldType)
            if !compatible(coerced.typ, fieldType) then
              throw AnalysisError(s"field '$fieldName' of '${st.name}' expects $fieldType, got ${coerced.typ}")
            coerced
          }
          TStructConstruct(st, checkedArgs)
        else if genericTypeAliases.contains(name) then
          if tArgs.length != 1 then
            throw AnalysisError(s"generic alias '$name[...]' cast expects exactly 1 argument, got ${tArgs.length}")
          val target = resolveType(NamedTypeAST(name, typeExprs.map(exprToTypeAST)))
          val arg = tArgs.head
          target match
            case nt @ SyslType.NamedType(_, base, true, _, _) =>
              val coreCast =
                if arg.typ.underlying == base.underlying then arg
                else if compatible(arg.typ, base) then arg
                else throw AnalysisError(s"cannot cast ${arg.typ} to '$name[...]' (underlying $base)")
              TCast(coreCast, nt)
            case other =>
              if compatible(arg.typ, other) then arg
              else throw AnalysisError(s"cannot cast ${arg.typ} to transparent alias '$name[...]' (= $other)")
        else
          val (mangled, funInfo) = instantiateGeneric(name, tArgs.map(_.typ), typeArgs)
          val checkedArgs = checkArgs(mangled, funInfo.params, tArgs, funInfo.modes)
          TCall(mangled, checkedArgs, funInfo.returnType)

      case GenericCallAST(callee, typeExprs, args) =>
        // Fall-through for unrecognized callees (e.g. method calls) — for now,
        // refuse with a clear error. Future extension can route obj.method[A, B](v)
        // through a parallel pattern.
        throw AnalysisError(s"explicit multi-type-arg call requires a generic struct, alias, or function name")

      case MethodCallAST(VarRefAST(nsName), method, args) if moduleNamespaces.contains(nsName) =>
        // Qualified import call: strings.has_prefix(s, prefix)
        val meta = moduleNamespaces(nsName)
        val tArgs = args.map(analyzeExpr)
        // Find the function in the module's symbols
        val funcSym = meta.publicSymbols.find(s => shortName(s.name) == method)
          .getOrElse(throw AnalysisError(s"module '$nsName' has no function '$method'"))
        funcSym.typ match
          case SymbolMeta.Kind.Func(params, returnType, _, _, modes, _, _) =>
            val paramPairs = params.zipWithIndex.map((t, i) => (s"_p$i", t))
            val checkedArgs = checkArgs(s"$nsName.$method", paramPairs, tArgs, modes)
            TCall(funcSym.name, checkedArgs, returnType)
          case _ => throw AnalysisError(s"'$nsName.$method' is not a function")

      case MethodCallAST(VarRefAST(name), method, args) if traits.contains(name) =>
        // Trait method call: Ord.cmp(a, b)
        val tArgs = args.map(analyzeExpr)
        val (mangled, funInfo) = analyzeTraitCall(name, method, tArgs)
        val checkedArgs = checkArgs(mangled, funInfo.params, tArgs, funInfo.modes)
        TCall(mangled, checkedArgs, funInfo.returnType)

      case MethodCallAST(obj, method, args) =>
        val tObj = analyzeExpr(obj)
        val tArgs = args.map(analyzeExpr)
        // Interface dispatch
        tObj.typ match
          case iface: InterfaceType =>
            val methodIdx = iface.methods.indexWhere(_._1 == method)
            if methodIdx < 0 then throw AnalysisError(s"interface ${iface.name} has no method '$method'")
            val (_, paramTypes, retType, _) = iface.methods(methodIdx)
            val params = paramTypes.zipWithIndex.map((t, i) => (s"arg$i", t))
            val checkedArgs = checkArgs(s"${iface.name}.$method", params, tArgs)
            return TInterfaceDispatch(tObj, methodIdx, checkedArgs, retType)
          case _ => ()
        // Determine the struct type (defer self-arg computation until we know it's a method).
        // Non-struct receivers can still match an `extension (recv: T)` block, so we route
        // through tryExtensionDispatch before raising the no-method error.
        val structTypeOpt: Option[StructType] = tObj.typ match
          case st: StructType          => Some(st)
          case PtrType(st: StructType) => Some(st)
          case RefType(st: StructType) => Some(st)
          case _                       => None
        if structTypeOpt.isEmpty then
          return tryExtensionDispatch(method, tObj, tArgs).getOrElse(
            throw AnalysisError(s"cannot call method '$method' on ${tObj.typ}"))
        val structType = structTypeOpt.get
        val structName = structType.name
        val funcName = s"${structName}_$method"
        if functions.contains(funcName) then
          // It's a real method — build self argument (need address for value structs)
          val selfArg = tObj.typ match
            case st @ StructType(_, _, _) =>
              tObj match
                case TVarRef(n, _) => TAddrOf(n, PtrType(st))
                case TFieldAccess(innerObj, idx, _) => TAddrOfField(innerObj, idx, PtrType(st))
                case TIndex(arr, idx, _) => TAddrOfIndex(arr, idx, PtrType(st))
                case _ => TTempAddr(tObj, PtrType(st))  // method on temporary — copy into temp cell
            case _ => tObj // PtrType or RefType — already a pointer
          val funInfo = functions(funcName)
          val checkedArgs = checkArgs(funcName, funInfo.params.tail, tArgs, funInfo.modes.drop(1)) // .tail skips self param
          TCall(funInfo.name, selfArg :: checkedArgs, funInfo.returnType)
        else if structToTemplate.contains(structName) && {
          val (templateName, _) = structToTemplate(structName)
          genericTemplates.contains(s"${templateName}_$method")
        } then
          // Generic struct method — instantiate from template
          val (templateName, _) = structToTemplate(structName)
          val templateFuncName = s"${templateName}_$method"
          val selfArg = tObj.typ match
            case st @ StructType(_, _, _) =>
              tObj match
                case TVarRef(n, _) => TAddrOf(n, PtrType(st))
                case TFieldAccess(innerObj, idx, _) => TAddrOfField(innerObj, idx, PtrType(st))
                case TIndex(arr, idx, _) => TAddrOfIndex(arr, idx, PtrType(st))
                case _ => TTempAddr(tObj, PtrType(st))
            case _ => tObj
          val allArgTypes = selfArg.typ :: tArgs.map(_.typ)
          val (mangled, funInfo) = instantiateGeneric(templateFuncName, allArgTypes)
          val checkedArgs = checkArgs(mangled, funInfo.params.tail, tArgs, funInfo.modes.drop(1))
          TCall(mangled, selfArg :: checkedArgs, funInfo.returnType)
        else
          // Fall back to calling a function-typed field
          structType.fields.zipWithIndex.find(_._1._1 == method) match
            case Some(((_, FuncType(paramTypes, returnType, esc, eff)), idx)) =>
              val fieldAccess = TFieldAccess(tObj, idx, FuncType(paramTypes, returnType, esc, eff))
              val params = paramTypes.zipWithIndex.map { case (t, i) => (s"arg$i", t) }
              val checkedArgs = checkArgs(s"$structName.$method", params, tArgs)
              TIndirectCall(fieldAccess, checkedArgs, returnType)
            case Some(((_, other), _)) =>
              throw AnalysisError(s"field '$method' of struct $structName is $other, not a function")
            case None =>
              tryExtensionDispatch(method, tObj, tArgs).getOrElse(
                throw AnalysisError(s"struct $structName has no method or field '$method'"))

      case CallAST(name, args0) =>
        // Call-by-name auto-wrap: if `name` resolves to a known function with
        // by-name params, wrap each positional arg at a by-name slot in
        // `ClosureAST(Nil, ExprBodyAST(arg))` BEFORE analysis. This makes
        // `use_lazy(bump())` desugar to `use_lazy(() -> bump())` so the arg is
        // never evaluated at the call site. The wrap must be at the AST level —
        // wrapping after analysis would already have evaluated the expression's
        // side effects. Named args go through `resolveNamedArgsTyped`, which
        // also calls `wrapByNameAtIndex`. Indirect calls (local-shadow path)
        // can't carry by-name info — only registered functions do.
        val args =
          if args0.exists(_.isInstanceOf[NamedArgAST]) then args0
          else
            val byNameInfoOpt: Option[List[Boolean]] =
              if traitCallRewrite.contains(name) then
                Some(functions(traitCallRewrite(name)).byName)
              else if functions.contains(name) then Some(functions(name).byName)
              else if builtinFunctions.contains(name) then Some(builtinFunctions(name).byName)
              else None
            byNameInfoOpt match
              case Some(flags) if flags.nonEmpty && flags.exists(identity) =>
                args0.zipWithIndex.map { case (a, i) =>
                  if i < flags.length && flags(i) then ClosureAST(Nil, ExprBodyAST(a))
                  else a
                }
              case _ => args0
        // Local shadow: if `name` is bound in the current function's scope chain
        // AND the binding is a function/closure value, treat the call as an indirect
        // call through that local. This must run BEFORE the global-function lookup
        // below — without it, a pattern-bound `f: (string) -> int` from a destructured
        // variant field would silently fall through to a like-named top-level
        // `f(int) -> int`, causing a misleading argument-type error.
        lookupLocal(name) match
          case Some(sym) =>
            // Match through nominal aliases: a local `p: Parser[int]` whose
            // underlying type is a FuncType is callable, and must shadow any
            // like-named global.
            //
            // By-name composition: if `b: => T` and the user writes `b(args)`,
            // we must first auto-evaluate the thunk to get a value of type T,
            // *then* apply args to that value. The visible-to-user type of `b`
            // is `T`; the storage type is `() -> T`. So the inner step is
            // `TIndirectCall(TVarRef(b, () -> T), Nil, T)`, and the outer step
            // applies args through that result the same way it would for a
            // bare local of type T. If T is not callable, fall through to
            // surface a normal "not a function" diagnostic at checkArgs.
            sym.typ.underlying match
              case ft: FuncType =>
                val expectedTypes = ft.params.map(t => Some(t): Option[SyslType])
                val tArgs = args.zip(expectedTypes.padTo(args.length, None)).map { case (a, exp) =>
                  val saved = currentExpected
                  currentExpected = exp.orElse(saved)
                  try analyzeExpr(a) finally currentExpected = saved
                }
                val paramPairs = ft.params.zipWithIndex.map((t, i) => (s"_p$i", t))
                val checkedArgs = checkArgs(name, paramPairs, tArgs)
                val callee: TExpr =
                  if sym.isByName then
                    val thunkType = FuncType(Nil, sym.typ, effects = FuncEffects.Unknown)
                    TIndirectCall(TVarRef(sym.name, thunkType), Nil, sym.typ)
                  else
                    TVarRef(name, sym.typ)
                return TIndirectCall(callee, checkedArgs, ft.returnType)
              case _ if sym.isByName =>
                throw AnalysisError(
                  s"by-name parameter '$name' has type ${sym.typ}, which is not callable; cannot apply arguments")
              case _ => () // local exists but isn't callable — fall through to global
          case None => ()
        // For each param, the expected arg type during analysis. For Out/Inout the
        // body-visible type is the inner T (not the hidden `*T`), so the user-written
        // arg is analyzed against T — matching what's actually written at the call site.
        def expectedFor(paramTypes: List[SyslType], modes: List[ParamMode]): List[SyslType] =
          paramTypes.zipWithIndex.map { case (t, i) =>
            val m = if modes.isEmpty then ParamMode.In else modes(i)
            (m, t) match
              case (ParamMode.Out | ParamMode.Inout, PtrType(inner)) => inner
              case _ => t
          }
        // If any named args are present, resolve them via param-name lookup
        // and type the resulting positional list. Otherwise, use the fast path.
        val tArgs: List[TExpr] =
          if args.exists(_.isInstanceOf[NamedArgAST]) then
            val (paramNames, paramTypes): (List[String], List[SyslType]) =
              if traitCallRewrite.contains(name) then
                val p = functions(traitCallRewrite(name)).params
                (p.map(_._1), p.map(_._2))
              else if functions.contains(name) || builtinFunctions.contains(name) then
                val p = lookupFun(name).params
                (p.map(_._1), p.map(_._2))
              else if structTypes.contains(name) then
                val f = structTypes(name).fields
                (f.map(_._1), f.map(_._2))
              else
                throw AnalysisError(s"named arguments are not supported for '$name'")
            // For named-args, use the body-visible expected types (unwrap *T for out/inout).
            val modesForNamed =
              if traitCallRewrite.contains(name) then functions(traitCallRewrite(name)).modes
              else if functions.contains(name) || builtinFunctions.contains(name) then lookupFun(name).modes
              else Nil
            resolveNamedArgsTyped(name, paramNames, expectedFor(paramTypes, modesForNamed), args)
          else
            // For generic-template calls without explicit type arguments, analyze args
            // left-to-right and incrementally bind type parameters as soon as earlier
            // arguments pin them. After each arg is analyzed, unify the formal-param
            // TypeAST against the actual arg type to grow the binding env; for the next
            // arg, substitute env into its formal TypeAST and use the resolved type as
            // `currentExpected`. Without this, a placeholder closure (`_ + _`) at arg
            // index ≥ 1 fails because its expected type still mentions a free type
            // variable — even when an earlier sibling arg has already pinned it.
            // Tolerant: if substitution still leaves type-vars free (resolveType throws)
            // or the partial unify fails, we just don't supply an expected type for that
            // arg — `instantiateGeneric` will surface the real failure later.
            if !traitCallRewrite.contains(name)
               && !functions.contains(name) && !builtinFunctions.contains(name)
               && !structTypes.contains(name) && !variantToEnum.contains(name)
               && !genericVariantToEnum.contains(name)
               && genericTemplates.contains(name)
               && genericTemplates(name).params.length == args.length then
              val template = genericTemplates(name)
              val tparamSet = template.typeParams.toSet
              val env = mutable.Map.empty[String, SyslType]
              args.zip(template.params).map { case (a, formal) =>
                val expectedOpt: Option[SyslType] =
                  val savedTE = typeEnv
                  typeEnv = typeEnv ++ env.toMap
                  try Some(resolveType(formal.typ))
                  catch case _: Throwable => None
                  finally typeEnv = savedTE
                val saved = currentExpected
                currentExpected = expectedOpt.orElse(saved)
                val tA = try analyzeExpr(a) finally currentExpected = saved
                try unifyTypes(formal.typ, tA.typ, tparamSet, env)
                catch case _: Throwable => ()
                tA
              }
            else
              // Determine expected types for args if callee has known concrete signature.
              // Variant constructors are critical here: a no-arg variant (e.g. `None`) inside
              // another variant's args would otherwise inherit the OUTER expected type and
              // misresolve. By passing each field's type as expected, the inner variant can
              // disambiguate to the right enum instantiation.
              val argExpected: List[Option[SyslType]] =
                if traitCallRewrite.contains(name) then
                  val mangled = traitCallRewrite(name)
                  val fi = functions(mangled)
                  expectedFor(fi.params.map(_._2), fi.modes).map(Some(_))
                else if functions.contains(name) || builtinFunctions.contains(name) then
                  val fi = lookupFun(name)
                  expectedFor(fi.params.map(_._2), fi.modes).map(Some(_))
                else if structTypes.contains(name) then
                  structTypes(name).fields.map(f => Some(f._2))
                else if variantToEnum.contains(name) then
                  val (et, variantIdx) = variantToEnum(name)
                  et.variants(variantIdx)._2.map(f => Some(f._2))
                else if genericVariantToEnum.contains(name) then
                  val (enumName, variantIdx) = genericVariantToEnum(name)
                  val template = genericEnums(enumName)
                  val variant = template.variants(variantIdx)
                  // Use currentExpected (the enum's instantiation) to recover type args,
                  // then resolve each field's TypeAST under that substitution.
                  val typeArgs: Option[List[SyslType]] = currentExpected match
                    case Some(et: SyslType.EnumType) =>
                      genericEnumInstantiations.collectFirst {
                        case ((n, args), inst) if n == enumName && inst.name == et.name => args
                      }
                    case _ => None
                  typeArgs match
                    case Some(tArgs) =>
                      val savedEnv = typeEnv
                      typeEnv = typeEnv ++ template.typeParams.zip(tArgs).toMap
                      try variant.fields.map(f => Some(resolveType(f._2)))
                      finally typeEnv = savedEnv
                    case None => List.fill(args.length)(None)
                else
                  List.fill(args.length)(None)
              args.zip(argExpected.padTo(args.length, None)).map { case (a, exp) =>
                val saved = currentExpected
                currentExpected = exp.orElse(saved)
                try analyzeExpr(a) finally currentExpected = saved
              }
        // Check for trait-method-call rewrite (inside a synthesized default body)
        if traitCallRewrite.contains(name) then
          val mangled = traitCallRewrite(name)
          val funInfo = functions(mangled)
          val checkedArgs = checkArgs(mangled, funInfo.params, tArgs, funInfo.modes)
          TCall(mangled, checkedArgs, funInfo.returnType)
        else
        // Check if it's a direct function call or an indirect call through a variable
        if genericTemplates.contains(name) then
          val (mangled, funInfo) = instantiateGeneric(name, tArgs.map(_.typ))
          val checkedArgs = checkArgs(mangled, funInfo.params, tArgs, funInfo.modes)
          TCall(mangled, checkedArgs, funInfo.returnType)
        else if functions.contains(name) || builtinFunctions.contains(name) then
          warnDeprecated(name)
          val funInfo = lookupFun(name)
          if funInfo.autoCallsBare && funInfo.params.isEmpty && tArgs.nonEmpty then
            // Auto-call def or parameterless, then indirect-call the result
            // with the provided args. Mirrors the bare-VarRef auto-call path.
            val autoCall = TCall(funInfo.name, Nil, funInfo.returnType)
            funInfo.returnType match
              case FuncType(fParams, fRet, _, _) =>
                val paramPairs = fParams.zipWithIndex.map((t, i) => (s"_p$i", t))
                val checkedArgs = checkArgs(name, paramPairs, tArgs)
                TIndirectCall(autoCall, checkedArgs, fRet)
              case _ => throw AnalysisError(s"'$name' returns ${funInfo.returnType}, not a callable type")
          else
            val checkedArgs = checkArgs(name, funInfo.params, tArgs, funInfo.modes)
            TCall(funInfo.name, checkedArgs, funInfo.returnType)
        else if typeAliases.contains(name) then
          // Named-type cast: Meters(3), SafeAge(x). Resolves to a TCast whose target is the
          // NamedType — for constrained variants this is further wrapped in a TRangeCheck by
          // the downstream applyTargetType call via CastAST handling logic.
          if tArgs.length != 1 then
            throw AnalysisError(s"cast '$name' expects exactly 1 argument, got ${tArgs.length}")
          val target = resolveType(NamedTypeAST(name))
          val coreCast = if tArgs.head.typ.underlying == target.underlying then tArgs.head
                        else TCast(tArgs.head, target.underlying)
          applyTargetType(coreCast, target) match
            case e if e.typ == target => e
            case e => TCast(e, target)
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
          for ((_, ftype, _), arg) <- template.fields.zip(tArgs) do
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
          // Try as a variable of FuncType (including a nominal alias whose underlying is a FuncType)
          val sym = lookup(name)
          sym.typ.underlying match
            case FuncType(paramTypes, returnType, _, _) =>
              val params = paramTypes.zipWithIndex.map { case (t, i) => (s"arg$i", t) }
              val checkedArgs = checkArgs(name, params, tArgs)
              TIndirectCall(TVarRef(name, sym.typ), checkedArgs, returnType)
            case _ =>
              throw AnalysisError(s"'$name' is not a function (type: ${sym.typ})")

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
        // Verify the enclosing function's return type matches. Reads from
        // `currentReturnType` (the function-level field), NOT `currentExpected`
        // — the latter is the *immediate* expected type, which gets overridden
        // by inner contexts (var-decl LHS, field-assign LHS, closure body
        // expected, etc.). The `?` operator's contract is about where it
        // returns to, which is the enclosing function only.
        currentReturnType match
          case et: SyslType.EnumType if et.name == enumType.name => ()
          case other =>
            throw AnalysisError(s"'?' on $enumType requires enclosing function to return $enumType, got $other")
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
        // Pick a non-void branch type if one exists (e.g., `if cond then panic("...") else x`
        // — first branch is void but overall expression is x's type). Fall back to the then
        // branch's type, or UnitType if the then branch has no trailing expression.
        val branchLastTypes = (tThen.lastOption :: tElse.toList.flatMap(_.lastOption.map(Some(_)))).collect {
          case Some(TExprStmt(e)) => e.typ
        }
        // When both non-void branches are integral, widen to the larger type so the
        // result slot fits the value of either branch (e.g. `if c then byte else -1`
        // must store ch as int, not byte — otherwise -1 truncates to 255).
        val nonVoid = branchLastTypes.filter(_ != UnitType)
        val resultType = nonVoid match
          case List(a, b) if a.isIntegral && b.isIntegral =>
            (a, b) match
              case (FloatType(x), FloatType(y)) => FloatType(x max y)
              case (_: FloatType, _) => a
              case (_, _: FloatType) => b
              case (IntType(x), IntType(y))   => IntType(x max y)
              case (UIntType(x), UIntType(y)) => UIntType(x max y)
              case (UIntType(x), IntType(y)) if x < y => IntType(y)
              case (IntType(x), UIntType(y)) if y < x => IntType(x)
              case _ => a
          case _ => nonVoid.headOption.orElse(branchLastTypes.headOption).getOrElse(UnitType)
        TIfExpr(tCond, tThen, tElse, resultType)

      case QuantifierAST(kind, name, lo, hi, inclusive, pred) =>
        val tLo = analyzeExpr(lo)
        val tHi = analyzeExpr(hi)
        if !tLo.typ.isIntegral then throw AnalysisError(s"quantifier range lower bound must be integral, got ${tLo.typ}")
        if !tHi.typ.isIntegral then throw AnalysisError(s"quantifier range upper bound must be integral, got ${tHi.typ}")
        // Bound variable type: prefer lo's type; bumping both to a wider common type isn't
        // worth the analyzer machinery yet — use I64 if either side is wider than I32.
        val nameType: SyslType =
          if tLo.typ.bitWidth > 32 || tHi.typ.bitWidth > 32 then I64 else tLo.typ
        pushScope()
        currentScope(name) = SymInfo(name, nameType, mutable = false)
        val tPred = analyzeExpr(pred)
        popScope()
        if tPred.typ != BoolType then throw AnalysisError(s"quantifier predicate must be bool, got ${tPred.typ}")
        TQuantifier(kind, name, nameType, tLo, tHi, inclusive, tPred)

      case MatchExprAST(scrutinee, arms, default) =>
        val tScrutinee = analyzeExpr(scrutinee)
        // The match expression's own expected type (if any) propagates into each arm's
        // body and into the optional `else` body. This is what lets variant constructors
        // with phantom type parameters (e.g. `Failure(m, n)` for `ParseResult[A]` where
        // `A` doesn't appear in `Failure`'s fields) infer their type args from context.
        val matchExpectedOpt = currentExpected
        val tArms = arms.map { arm =>
          pushScope()
          val prelude = scala.collection.mutable.ListBuffer.empty[StmtAST]
          val tPatterns = arm.patterns.map(p => analyzePattern(p, tScrutinee.typ, prelude))
          val tGuard = arm.guard.map { g =>
            val tg = analyzeExpr(g)
            if tg.typ != BoolType then throw AnalysisError(s"match guard must be bool, got ${tg.typ}")
            tg
          }
          val savedExp = currentExpected
          currentExpected = matchExpectedOpt
          // Synthetic prelude statements (e.g. `val a = sym._0` for nested tuple
          // patterns) are introduced before the user's arm body.
          val bodyWithPrelude = prelude.toList ++ arm.body
          val tBody = try analyzeBlock(bodyWithPrelude) finally currentExpected = savedExp
          popScope()
          TMatchArm(tPatterns, tGuard, tBody)
        }
        val tDefault = default.map { stmts =>
          pushScope()
          val savedExp = currentExpected
          currentExpected = matchExpectedOpt
          val r = try analyzeBlock(stmts) finally currentExpected = savedExp
          popScope()
          r
        }
        // Exhaustiveness check for matches on enum types. A guarded arm does not cover
        // its variant (the guard could be false). Wildcard or default provides full coverage.
        tScrutinee.typ.underlying match
          case et: EnumType if tDefault.isEmpty =>
            val coveredVariants = mutable.Set.empty[Int]
            var wildcardCovers = false
            for arm <- tArms; pat <- arm.patterns do
              if arm.guard.isEmpty then pat match
                case TWildcard => wildcardCovers = true
                case TVariantPattern(_, idx, _, _, nested) if nested.forall(_.isEmpty) =>
                  // Nested sub-patterns may fail to match — only an arm with NO
                  // active nested pattern fully covers its variant.
                  coveredVariants += idx
                case _ =>
            if !wildcardCovers then
              val missing = et.variants.zipWithIndex.collect {
                case ((vname, _), idx) if !coveredVariants.contains(idx) => vname
              }
              if missing.nonEmpty then
                throw AnalysisError(
                  s"non-exhaustive match on enum '${et.name}': missing variant(s): ${missing.mkString(", ")}"
                )
          case _ => // non-enum or has default — skip
        // Pick a non-void arm type if one exists (e.g., one arm panics, another returns a value).
        // Fall back to the first arm's last-expression type, or UnitType if no arm ends with an expression.
        val armLastTypes = tArms.flatMap(_.body.lastOption).collect { case TExprStmt(e) => e.typ } ++
          tDefault.toList.flatMap(_.lastOption).collect { case TExprStmt(e) => e.typ }
        val resultType = armLastTypes.find(_ != UnitType).orElse(armLastTypes.headOption).getOrElse(UnitType)
        TMatchExpr(tScrutinee, tArms, tDefault, resultType)

  protected def analyzeInterpolatedString(s: String): TExpr =
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
              case t if t.isNumeric || t == SyslType.BoolType => TStr(analyzed)
              case t => throw AnalysisError(s"cannot interpolate value of type $t into string")
          case Left(err) => throw AnalysisError(s"parse error in string interpolation: $err")
    }
    tExprs.reduceLeft((l, r) => TBinary(l, "+", r, SyslType.StringType))

  protected def parseFmtSpec(s: String, pos: Int): (FmtSpec, Int) =
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

  protected def analyzeFormattedString(s: String): TExpr =
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

  protected def wrapWithFmtSpec(expr: TExpr, spec: FmtSpec): TExpr =
    spec.verb match
      case 'd' | 'x' | 'o' | 'b' =>
        if !expr.typ.isNumeric then
          throw AnalysisError(s"format verb '%${spec.verb}' requires numeric type, got ${expr.typ}")
        if spec.verb == 'd' && spec.width == 0 && !spec.zeroPad && !spec.leftAlign && !spec.showSign then TStr(expr)
        else TFmtStr(expr, spec)
      case 's' =>
        if expr.typ == SyslType.StringType then
          if spec.width == 0 && !spec.leftAlign then expr else TFmtStr(expr, spec)
        else if expr.typ.isNumeric || expr.typ == SyslType.BoolType then
          if spec.width == 0 && !spec.leftAlign then TStr(expr) else TFmtStr(TStr(expr), spec)
        else throw AnalysisError(s"cannot format value of type ${expr.typ} with %s")
      case 'c' =>
        if !expr.typ.isNumeric then
          throw AnalysisError(s"format verb '%c' requires numeric type, got ${expr.typ}")
        TFmtStr(expr, spec)
      case _ => throw AnalysisError(s"unknown format verb '%${spec.verb}'")
