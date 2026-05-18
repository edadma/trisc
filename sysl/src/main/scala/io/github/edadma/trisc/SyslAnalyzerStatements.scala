package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

trait SyslAnalyzerStatements:
  self: SyslAnalyzer =>

  protected def analyzeBlock(stmts: List[StmtAST]): List[TStmt] =
    // Two-pass scope walk for inner defs at this block level. Pre-binding all
    // sibling names with their declared signatures BEFORE analyzing any body
    // is what lets `def f` and `def g` cross-reference each other regardless
    // of source order. Without this, an inner-def body referencing a sibling
    // declared later would see "name is not in scope".
    //
    // Defs that fail their own arm checks later (type params, missing return
    // type) are pre-bound here too; the failing arm throws when its body runs
    // and the pre-bind has no observable effect.
    val innerDecls: List[FunDeclAST] = stmts.collect {
      case InnerFunStmtAST(d) if d.typeParams.isEmpty && d.returnType.nonEmpty => d
    }
    val siblingNames: Set[String] = innerDecls.map(_.name).toSet
    if scopeStack != null && innerDecls.nonEmpty then
      for d <- innerDecls do
        val ret = resolveType(d.returnType.get)
        val pTypes = d.params.map(p => resolveType(p.typ))
        val ft: SyslType = FuncType(pTypes, ret, escaping = true)
        currentScope(d.name) = SymInfo(d.name, ft, mutable = false)
    val tStmts = stmts.map(analyzeStmt)
    // Two reasons to run the lift pass:
    //   (a) sibling cross-references (mutual recursion) — needs forward-ref support.
    //   (b) any inner def carries a require/ensure contract — closures have no
    //       contract-emission stage, so the def must be lifted to a top-level fn
    //       where `analyzeBlockWithContracts` can wire the checks in.
    val anyContracts = innerDecls.exists(_.body match
      case BlockBodyAST(_, cs) => cs.nonEmpty
      case _                    => false)
    if siblingNames.size <= 1 && !anyContracts then tStmts
    else liftInnerDefClusters(tStmts, innerDecls, siblingNames)

  /** Lift inner defs to top-level synthesized functions when they need it.
   *
   *  Two reasons trigger a lift:
   *
   *  1. **Cross-reference cluster.** Any inner def whose body captures a
   *     sibling other than itself participates in a cluster. The whole cluster
   *     is the transitive closure under the "captures sibling" relation.
   *     Lifting them to top-level removes the forward-reference problem
   *     (capture-by-value would otherwise read garbage).
   *
   *  2. **Contracts.** Any inner def with `require`/`ensure` clauses must be
   *     lifted because the closure analyzer's body path drops contracts on the
   *     floor — only `analyzeBlockWithContracts` (used by top-level fns) emits
   *     them as `TContractCheck` nodes. Single self-recursive inner defs with
   *     no contracts and no sibling refs stay on the existing TClosure path.
   *
   *  Captures of outer-scope variables disqualify a lift target — the lift
   *  cannot preserve the captures, so we reject with a clear diagnostic that
   *  explains whether the issue is contracts or cross-references.
   *
   *  Every backend already handles `TIndirectCall` of a `TFuncRef` (top-level
   *  fn pointer with null env), so lifting needs no per-backend support. */
  protected def liftInnerDefClusters(
      tStmts: List[TStmt],
      innerDecls: List[FunDeclAST],
      siblingNames: Set[String],
  ): List[TStmt] =
    val declByName: Map[String, FunDeclAST] = innerDecls.map(d => d.name -> d).toMap
    case class Inner(name: String, closure: TClosure, ft: SyslType, vol: Boolean, ghost: Boolean,
                     contracts: List[ContractClauseAST])
    val innerStmts: List[Inner] = tStmts.collect {
      case TVarStmt(name, ft, c: TClosure, vol, ghost) if siblingNames.contains(name) =>
        val cs = declByName.get(name).map(_.body).collect {
          case BlockBodyAST(_, cs) => cs
        }.getOrElse(Nil)
        Inner(name, c, ft, vol, ghost, cs)
    }
    if innerStmts.isEmpty then return tStmts
    // Edges: each sibling → set of OTHER siblings it captures (self-capture
    // doesn't count — that's handled by the existing selfName mechanism for
    // non-lifted defs, or by the rewriter for lifted ones).
    val crossRefs: Map[String, Set[String]] = innerStmts.map { i =>
      i.name -> i.closure.captures.iterator.collect {
        case (capName, _) if siblingNames.contains(capName) && capName != i.name => capName
      }.toSet
    }.toMap
    // Cluster: closure of all siblings reachable from any node with non-empty
    // cross-refs OR reachable as a target of cross-refs. Catches both ends of
    // an `f→g` edge.
    val cluster = scala.collection.mutable.Set.empty[String]
    val seeds = crossRefs.iterator.flatMap { case (n, refs) =>
      if refs.nonEmpty then Iterator(n) ++ refs.iterator else Iterator.empty
    }.toSet
    val toVisit = scala.collection.mutable.Queue.from(seeds)
    while toVisit.nonEmpty do
      val n = toVisit.dequeue()
      if !cluster.contains(n) then
        cluster += n
        toVisit ++= crossRefs.getOrElse(n, Set.empty)
    // Lift set:
    //   - Every cluster member (forward-ref support).
    //   - Contract-bearing inner defs WITHOUT outer-scope captures (lifting
    //     gives a slightly faster direct call vs. closure indirection, and
    //     the existing top-level contract path was the original support).
    // Contract-bearing inner defs WITH outer-scope captures stay as
    // TClosures; the closure analyzer routes their bodies through
    // `analyzeBlockWithContracts`, baking TContractCheck nodes into the
    // closure body. The closure's capture scanner (with TContractCheck +
    // TMultiStmt arms) picks up any contract-only capture refs.
    def hasOuterCapture(i: Inner): Boolean =
      i.closure.captures.exists((c, _) => !siblingNames.contains(c))
    val contractBearersLiftable: Set[String] = innerStmts.iterator.collect {
      case i if i.contracts.nonEmpty && !hasOuterCapture(i) => i.name
    }.toSet
    val liftSet: Set[String] = cluster.toSet ++ contractBearersLiftable
    if liftSet.isEmpty then return tStmts
    // Validate: every lift target's captures must be siblings (incl. self).
    // The only remaining rejection case is a CLUSTER member with outer
    // captures — the lift can't preserve them and dropping silently would
    // break the program. Contract-bearers with outer captures are excluded
    // from `liftSet` above, so they don't reach this check.
    for i <- innerStmts; if liftSet.contains(i.name) do
      val outerCaptures = i.closure.captures.iterator.collect {
        case (capName, _) if !siblingNames.contains(capName) => capName
      }.toList
      if outerCaptures.nonEmpty then
        val others = liftSet.iterator.filter(_ != i.name).toList.sorted
        throw AnalysisError(
          s"inner def '${i.name}' is part of a cross-referencing cluster (with ${others.mkString(", ")}) " +
            s"that needs to be lifted to top-level for forward references to work, " +
            s"but it captures outer-scope variables: ${outerCaptures.mkString(", ")}. " +
            "The lift cannot preserve these captures — promote the cluster to top-level fns explicitly, " +
            "or refactor to avoid the captures (e.g. pass the captured value as an extra parameter).",
        )
    // Synthesize unique mangled names for every lift target. Sorted for
    // deterministic output.
    val mangled: Map[String, String] = liftSet.toList.sorted.map { name =>
      val m = s"_inner_${innerDefLiftCounter}_$name"
      innerDefLiftCounter += 1
      name -> m
    }.toMap
    // Rewrite body refs: TVarRef(liftedSiblingName, ft) → TFuncRef(mangled, ft).
    // Self-refs are rewritten too — the lifted top-level fn calls itself by its
    // mangled name. mapTExpr applies `f` to TVarRef nodes and visits subexprs.
    val rewriter: TExpr => TExpr = {
      case TVarRef(n, ft) if liftSet.contains(n) => TFuncRef(mangled(n), ft)
      case other                                  => other
    }
    for i <- innerStmts; if liftSet.contains(i.name) do
      val liftedBody: TFunBody =
        if i.contracts.nonEmpty then
          // Re-analyze body via the contract-aware path so require/ensure emit
          // as TContractCheck. The closure body we already have ignored the
          // contracts (closure analyzer drops them); we read them back from the
          // original FunDeclAST and analyze fresh, with the lifted fn's params
          // bound in a new scope.
          analyzeContractedInnerBody(declByName(i.name), i.closure, mangled(i.name), siblingNames, rewriter)
        else
          // No contracts: extract the analyzed closure body and rewrite siblings.
          i.closure.body match
            case TBlockBody(stmts) => TBlockBody(stmts.map(s => mapTStmt(s)(rewriter)))
            case TExprBody(e)      => TExprBody(mapTExpr(e)(rewriter))
      specializedDecls += TFunDecl(
        mangled(i.name), i.closure.params, i.closure.returnType, liftedBody,
        isPrivate = true, effects = i.closure.effects,
      )
    // Replace each lift-target TVarStmt's RHS with a TFuncRef to its lifted twin.
    // The local var still holds a callable value; calls go through TIndirectCall
    // of TFuncRef, which every backend lowers to a near-direct call with a null
    // env pointer.
    tStmts.map {
      case TVarStmt(name, ft, _: TClosure, vol, ghost) if liftSet.contains(name) =>
        TVarStmt(name, ft, TFuncRef(mangled(name), ft), vol, ghost)
      case other => other
    }

  /** Re-analyze a contract-bearing inner-def body in a top-level-fn-like
   *  context, then rewrite sibling refs. Mirrors the FunDeclAST arm of
   *  `analyzeDecl`: install a FRESH scope stack (so outer-scope vars are
   *  invisible — same as a real top-level fn), pre-bind siblings + params,
   *  set returnType + expected type, call `analyzeBlockWithContracts`,
   *  restore. The mangled name is passed as `selfMangledName` so a `variant`
   *  clause (rare on inner defs) resolves recursive calls correctly.
   *
   *  Outer-scope captures inside the body or contracts surface as
   *  "undefined variable" during this re-analysis. We catch and re-throw
   *  with a wrapper that names the inner def + the offending var, so the
   *  user sees a clear "promote to top-level fn" diagnostic instead of a
   *  bare lookup failure. */
  protected def analyzeContractedInnerBody(
      decl: FunDeclAST,
      closure: TClosure,
      mangledName: String,
      siblingNames: Set[String],
      rewriter: TExpr => TExpr,
  ): TFunBody =
    val (stmts, contracts) = decl.body match
      case BlockBodyAST(s, cs) => (s, cs)
      case _                    => (Nil, Nil) // unreachable — only BlockBody carries contracts
    val savedReturnType = currentReturnType
    val savedExpected = currentExpected
    val savedScopeStack = scopeStack
    // Fresh scope stack: outer-scope vars become invisible. Top-level fns +
    // globals stay visible via separate maps in `lookup`. Siblings are
    // pre-bound by copying their SymInfo from the saved stack so cross-refs
    // still resolve.
    scopeStack = new mutable.ArrayBuffer[mutable.LinkedHashMap[String, SymInfo]]
    pushScope()
    try
      // Pre-bind self + all siblings (incl. self for self-recursive defs).
      // The rewriter converts the resulting TVarRef → TFuncRef(mangled) after
      // analysis, so the analysis-time binding only needs the FuncType.
      for siblingName <- siblingNames do
        savedScopeStack.iterator.collectFirst {
          case s if s.contains(siblingName) => s(siblingName)
        }.foreach(sym => currentScope(siblingName) = sym)
      for (param, tParam) <- decl.params.zip(closure.params) do
        currentScope(param.name) = SymInfo(param.name, tParam.typ, mutable = true)
      currentReturnType = closure.returnType
      currentExpected = if closure.returnType == UnitType then None else Some(closure.returnType)
      val analyzed =
        try
          analyzeBlockWithContracts(
            stmts, contracts, closure.returnType,
            mangledName, decl.params.map(_.name),
          )
        catch
          case e: AnalysisError if e.getMessage.contains("undefined variable") =>
            val pat = "undefined variable: '([^']+)'".r
            val outer = pat.findFirstMatchIn(e.getMessage).map(_.group(1)).getOrElse("(unknown)")
            throw AnalysisError(
              s"inner def '${decl.name}' has require/ensure clauses but captures outer-scope variable: $outer. " +
                "Contracts on inner defs are supported only when the def can be lifted to a top-level fn — " +
                "promote it explicitly, or refactor to avoid the captures (e.g. pass the captured value as " +
                "an extra parameter).",
            )
      analyzed match
        case TBlockBody(ss) => TBlockBody(ss.map(s => mapTStmt(s)(rewriter)))
        case other          => other
    finally
      scopeStack = savedScopeStack
      currentReturnType = savedReturnType
      currentExpected = savedExpected

  /** Analyze a function block body together with its `require` / `ensure` contract clauses.
   * Generates require checks at entry, injects a `__result__` local, and rewrites every
   * `return v` so it stores v into `__result__`, runs ensure checks, then returns. */
  protected def analyzeBlockWithContracts(
      stmts: List[StmtAST],
      contracts: List[ContractClauseAST],
      returnType: SyslType,
      selfMangledName: String = "",
      paramNames: List[String] = Nil,
  ): TFunBody =
    if contracts.isEmpty then return TBlockBody(analyzeBlock(stmts))
    variantCallCounter = 0
    val variantClauses = contracts.collect { case c @ ContractClauseAST(ContractVariant, _, _) => c }
    if variantClauses.length > 1 then
      throw AnalysisError(s"function may declare at most one `variant` clause, got ${variantClauses.length}")
    // Pre-declare __result__ in the function scope so that `result` aliased to it resolves
    // during ensure analysis, and later references inside the injected rewrite work.
    val hasResult = returnType != UnitType
    if hasResult then
      currentScope("__result__") = SymInfo("__result__", returnType, mutable = true)
      currentScope("result") = SymInfo("__result__", returnType, mutable = false)
    val requireChecks: List[TStmt] = contracts.collect { case ContractClauseAST(ContractRequire, e, msg) =>
      val te = analyzeExpr(e)
      if te.typ != BoolType then throw AnalysisError(s"require expression must be bool, got ${te.typ}")
      contract("precondition", te, msg.getOrElse("precondition"))
    }
    // Enable `old()` interception while analyzing ensure clauses. Snapshot declarations
    // accumulated during analysis are emitted as TVarStmts at the very top of the body so
    // they capture values *before* any mutation in the body.
    val savedEnsureMode = inEnsureAnalysis
    val snapshotsBefore = oldSnapshots.length
    inEnsureAnalysis = true
    val ensureChecks: List[TStmt] = try contracts.collect { case ContractClauseAST(ContractEnsure, e, msg) =>
      val te = analyzeExpr(e)
      if te.typ != BoolType then throw AnalysisError(s"ensure expression must be bool, got ${te.typ}")
      contract("postcondition", te, msg.getOrElse("postcondition"))
    } finally inEnsureAnalysis = savedEnsureMode
    val capturedSnapshots = oldSnapshots.drop(snapshotsBefore).toList
    oldSnapshots.remove(snapshotsBefore, capturedSnapshots.length)
    val snapshotDecls: List[TStmt] = capturedSnapshots.map { (name, typ, expr) =>
      TVarStmt(name, typ, expr)
    }
    // Drop the `result` alias so user code in the body cannot pick it up unintentionally.
    // `__result__` stays in scope — the body rewrite references it.
    if hasResult then currentScope.remove("result")
    val tStmts = analyzeBlock(stmts)
    // Lower the optional `variant` clause: snapshot at entry, wrap every direct recursive
    // call with a runtime check. The snapshot decl is prepended to the final body.
    //
    // δ.2: lex-tuple variants (`variant { a, b }` parsed as TupleLitAST) are
    // verification-only — no runtime decreaser is feasible without lex-comparison
    // machinery. Why3 verifies lex-order termination statically. Skip the wrap.
    val (variantPrefix, variantBody) = variantClauses.headOption match
      case Some(vc) if selfMangledName.nonEmpty && !vc.expr.isInstanceOf[TupleLitAST] =>
        lowerFunctionVariant(selfMangledName, paramNames, vc, tStmts)
      case _ => (Nil, tStmts)
    val rewritten = rewriteReturnsForEnsure(variantBody, returnType, ensureChecks)
    val finalized = finalizeFallThroughReturn(rewritten, returnType, ensureChecks)
    val resultDecl: List[TStmt] =
      if hasResult then List(TVarStmt("__result__", returnType, zeroExprFor(returnType)))
      else Nil
    TBlockBody(snapshotDecls ++ variantPrefix ++ resultDecl ++ requireChecks ++ finalized)

  protected def analyzeStmt(stmt: StmtAST): TStmt =
    stmt match
      case VarStmtAST(name, typOpt, init, isMutable, isVolatile, isConst, isGhost) =>
        if isGhost && isConst then
          throw AnalysisError(s"#ghost on local '$name' is incompatible with const (ghost decls are stripped from codegen)")
        val declared = typOpt.map(resolveType)
        val savedExp = currentExpected
        currentExpected = declared.orElse(currentExpected)
        val tInit0 = try analyzeExpr(init) finally currentExpected = savedExp
        val declType = declared.getOrElse(tInit0.typ)
        val tInit1 = coerceLiteral(tInit0, declType)
        // `const`: initializer must be compile-time-evaluable; no storage is emitted.
        if isConst then
          if !declType.isIntegral then
            throw AnalysisError(s"const '$name' must have an integer type (found $declType); float/string/aggregate const is not yet supported")
          tryConstEval(tInit1) match
            case Some(n) =>
              val masked = maskToType(n, declType)
              compileTimeConstants(name) = masked
              if scopeStack != null then
                currentScope(name) = SymInfo(name, declType, false, isConst = true)
              return TExprStmt(TIntLit(masked, declType)) // no-op placeholder, dropped by codegen
            case None =>
              throw AnalysisError(s"const '$name' initializer is not compile-time evaluable")
        // Constant folding for local immutable vals (skip for ghost — ghost decls are
        // stripped before codegen so folding has no benefit and would also short-circuit
        // the discipline check on the initializer).
        val tInit = if !isMutable && !isGhost && declType.isIntegral then
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
        val tInitBoxed = (tInit.typ, declType) match
          case (_, iface: InterfaceType) if !tInit.typ.isInstanceOf[InterfaceType] =>
            TInterfaceBox(tInit, iface)
          case _ => tInit
        // Apply target type for named/constrained types (range checks + re-wrapping)
        val tInitFinal = applyTargetType(tInitBoxed, declType)
        // `_` is a discard binding: evaluate the initializer for its side effects
        // but don't bind any name. Multiple `_`s in the same scope don't collide.
        if name == "_" then
          TExprStmt(tInitFinal)
        else
          if scopeStack != null then
            currentScope(name) = SymInfo(name, declType, isMutable, isGhost = isGhost)
          val baseStmt = TVarStmt(name, declType, tInitFinal, isVolatile, isGhost = isGhost)
          // Fire struct invariants on the freshly-initialized value, if any are declared.
          // Skipped for ghost locals — there's no runtime check to fire (the decl is stripped).
          val checks = if isGhost then Nil else declType match
            case st: StructType if structInvariants.contains(st.name) =>
              buildStructInvariantChecks(VarRefAST(name), st.name)
            case _ => Nil
          if checks.isEmpty then baseStmt else TMultiStmt(baseStmt :: checks)

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

      case AssignStmtAST(target, value) if fixedAddressVars.contains(target) =>
        // #address MMIO write: `reg = v` lowers to `*(addr as *T) = v`.
        val (addr, typ) = fixedAddressVars(target)
        val tValue0 = analyzeExpr(value)
        val tValue = applyTargetType(coerceLiteral(tValue0, typ), typ)
        TDerefAssignStmt(TCast(TIntLit(addr, I64), PtrType(typ)), tValue)

      case AssignStmtAST(target, value) =>
        // Did this name already exist (param, prior decl, or global), or are we about
        // to implicitly create a fresh local? Capture this BEFORE analyzing the RHS so
        // the newly-bound case can be distinguished. Bare `name = expr` (no `var`/`val`)
        // inside a function body is sysl's implicit-local syntax — when the analyzer
        // creates a fresh local, downstream passes need to see it as a binding (TVarStmt),
        // not a write to an existing variable (TAssignStmt). Closure capture-detection
        // walks TAssignStmt as an assignment to an outer name, so emitting TAssignStmt
        // here would incorrectly mark a freshly-created inner local as a captured outer.
        val existedBefore = tryLookup(target).isDefined
        // For an existing target, forward its declared type as the RHS expected type so
        // bidirectional inference fires (e.g. `xs = []` when `xs: []int` is in scope
        // produces a slice, not a stuck "cannot infer element type"). For a fresh local,
        // the type still flows the other way — RHS → new var.
        val savedExp = currentExpected
        currentExpected = if existedBefore then Some(tryLookup(target).get.typ) else None
        val tValue0 = try analyzeExpr(value) finally currentExpected = savedExp
        val sym = lookupOrCreate(target, tValue0.typ)
        if !sym.mutable then throw AnalysisError(s"cannot assign to immutable variable '$target'")
        val tValue = applyTargetType(tValue0, sym.typ)
        // Out/Inout param: lower `x = v` to `*ptr = v` so the store flows back through
        // the caller's lvalue.
        val baseStmt: TStmt =
          if sym.autoIndirect then TDerefAssignStmt(TVarRef(sym.name, PtrType(sym.typ)), tValue)
          else if !existedBefore then TVarStmt(sym.name, sym.typ, tValue)
          else TAssignStmt(sym.name, tValue)
        val checks = sym.typ match
          case st: StructType if structInvariants.contains(st.name) =>
            buildStructInvariantChecks(VarRefAST(target), st.name)
          case _ => Nil
        if checks.isEmpty then baseStmt else TMultiStmt(baseStmt :: checks)

      case CompoundAssignStmtAST(target, op, value) if fixedAddressVars.contains(target) =>
        // #address MMIO read-modify-write: `reg += v` lowers to `*(ptr) = *(ptr) op v`.
        val (addr, typ) = fixedAddressVars(target)
        val tValue0 = analyzeExpr(value)
        val tValue = coerceLiteral(tValue0, typ)
        val ptrExpr = TCast(TIntLit(addr, I64), PtrType(typ))
        val loadExpr = TDeref(ptrExpr, typ)
        TDerefAssignStmt(ptrExpr, TBinary(loadExpr, op, tValue, typ))

      case CompoundAssignStmtAST(target, op, value) =>
        val sym = lookup(target)
        if !sym.mutable then throw AnalysisError(s"cannot assign to immutable variable '$target'")
        val tValue = analyzeExpr(value)
        // Out/Inout param: lower `x += v` to `*ptr = *ptr op v` for the same reason as
        // plain assignment.
        val baseStmt: TStmt =
          if sym.autoIndirect then
            val ptr = TVarRef(sym.name, PtrType(sym.typ))
            val load = TDeref(ptr, sym.typ)
            TDerefAssignStmt(ptr, TBinary(load, op, tValue, sym.typ))
          else TCompoundAssignStmt(sym.name, op, tValue)
        val checks = sym.typ match
          case st: StructType if structInvariants.contains(st.name) =>
            buildStructInvariantChecks(VarRefAST(target), st.name)
          case _ => Nil
        if checks.isEmpty then baseStmt else TMultiStmt(baseStmt :: checks)

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
        // Auto-box into iface when the element type is an interface — same
        // pattern as var-decl / fn-arg / struct-construct / new-struct paths.
        // owns=true: the array/slice may outlive the source's scope; the slot
        // must own its data buffer.
        val elemType = tArray.typ.underlying match
          case SyslType.ArrayType(et, _) => Some(et)
          case SyslType.SliceType(et) => Some(et)
          case SyslType.RefType(SyslType.SliceType(et)) => Some(et)
          case SyslType.RefType(SyslType.ArrayType(et, _)) => Some(et)
          case _ => None
        val tValueBoxed = (elemType, tValue.typ) match
          case (Some(iface: SyslType.InterfaceType), vt) if !vt.isInstanceOf[SyslType.InterfaceType] =>
            TInterfaceBox(tValue, iface, owns = true)
          case _ => tValue
        TIndexAssignStmt(tArray, tIndex, tValueBoxed)

      case FieldAssignStmtAST(obj, field, value) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          // Use latestStruct on the deref'd type — for self-referential generic
          // structs the field's StructType may still be the placeholder created
          // during monomorphization (with empty .fields). Codegen reads obj.typ
          // directly, so a stale annotation here propagates a 0-field StructType
          // that crashes index lookups.
          case PtrType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
          case RefType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        // Forward the field's declared type as the RHS expected type so
        // bidirectional inference fires (parallels the var-decl rule —
        // `self.buf = []` when `buf: []byte` produces a slice, not a stuck
        // "cannot infer element type").
        val savedExp = currentExpected
        currentExpected = Some(structType.fields(idx)._2)
        val tValue = try analyzeExpr(value) finally currentExpected = savedExp
        // Auto-box into iface when the field type is an interface — same
        // shape as the other coercion sites. owns=true because the
        // containing struct may outlive the source's scope.
        val fieldType = structType.fields(idx)._2
        val tValueBoxed = (fieldType, tValue.typ) match
          case (iface: SyslType.InterfaceType, vt) if !vt.isInstanceOf[SyslType.InterfaceType] =>
            TInterfaceBox(tValue, iface, owns = true)
          case _ => tValue
        val assign = TFieldAssignStmt(resolvedObj, idx, tValueBoxed)
        val checks = buildStructInvariantChecks(obj, structType.name)
        if checks.isEmpty then assign else TMultiStmt(assign :: checks)

      case FieldCompoundAssignStmtAST(obj, field, op, value) =>
        val tObj = analyzeExpr(obj)
        val tValue = analyzeExpr(value)
        val (resolvedObj, structType0) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
          case RefType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val structType = latestStruct(structType0)
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        val assign = TFieldCompoundAssignStmt(resolvedObj, idx, op, tValue)
        val checks = buildStructInvariantChecks(obj, structType.name)
        if checks.isEmpty then assign else TMultiStmt(assign :: checks)

      case ReturnStmtAST(value) =>
        TReturnStmt(value.map { v =>
          val tv = analyzeExpr(v)
          // Auto-box into iface when the return type is an interface — same
          // pattern as the var-decl, fn-arg, and struct-construct paths.
          // Without this, `return c` where `c: Tally` and return type
          // `IntHolder` would emit a raw struct value where the caller
          // expected an iface descriptor (UAF / type confusion).
          val tvBoxed = (tv.typ, currentReturnType) match
            case (_, iface: SyslType.InterfaceType) if !tv.typ.isInstanceOf[SyslType.InterfaceType] =>
              // owns=true: the iface escapes this frame, so the data buffer
              // must outlive the source's stack alloca. Codegens that emit
              // pointer-to-source for value structs heap-copy when owns=true.
              TInterfaceBox(tv, iface, owns = true)
            case _ => tv
          applyTargetType(tvBoxed, currentReturnType)
        })

      case ForStmtAST(init, cond, update, body, label) =>
        checkLoopLabelUnique(label)
        val (invariants, bodyAfterInvs) = extractLeadingInvariants(body)
        // Hoist variant state to caller scope (before pushScope for for-init).
        val (preDecls, rewrittenBody) = extractVariants(bodyAfterInvs)
        val tPreDecls = preDecls.map(analyzeStmt)
        pushScope()
        val tInit = analyzeStmt(init)
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"for condition must be bool, got ${tCond.typ}")
        loopDepth += 1
        loopLabelStack += label
        pushScope()
        val (tSnapshotDecls, tInvariants) = buildLoopInvariantChecks(invariants)
        val tBody = tInvariants ++ analyzeBlock(rewrittenBody)
        popScope()
        val tUpdate = analyzeStmt(update)
        loopLabelStack.remove(loopLabelStack.length - 1)
        loopDepth -= 1
        popScope()
        // loop_entry snapshots need the for-scope bindings (including the induction var)
        // visible — bundle them with init so they run once, after init, before cond.
        val finalInit = if tSnapshotDecls.isEmpty then tInit else TMultiStmt(tInit :: tSnapshotDecls)
        val loopStmt = TForStmt(finalInit, tCond, tUpdate, tBody, label)
        if tPreDecls.isEmpty then loopStmt else TMultiStmt(tPreDecls ++ List(loopStmt))

      case WhileStmtAST(cond, body, label) =>
        checkLoopLabelUnique(label)
        val (invariants, bodyAfterInvs) = extractLeadingInvariants(body)
        val (preDecls, rewrittenBody) = extractVariants(bodyAfterInvs)
        val tPreDecls = preDecls.map(analyzeStmt)
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"while condition must be bool, got ${tCond.typ}")
        loopDepth += 1
        loopLabelStack += label
        pushScope()
        val (tSnapshotDecls, tInvariants) = buildLoopInvariantChecks(invariants)
        val tBody = tInvariants ++ analyzeBlock(rewrittenBody)
        popScope()
        loopLabelStack.remove(loopLabelStack.length - 1)
        loopDepth -= 1
        val loopStmt = TWhileStmt(tCond, tBody, label)
        val pre = tPreDecls ++ tSnapshotDecls
        if pre.isEmpty then loopStmt else TMultiStmt(pre ++ List(loopStmt))

      case DoWhileStmtAST(cond, body, label) =>
        checkLoopLabelUnique(label)
        val (invariants, bodyAfterInvs) = extractLeadingInvariants(body)
        val (preDecls, rewrittenBody) = extractVariants(bodyAfterInvs)
        val tPreDecls = preDecls.map(analyzeStmt)
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"do/while condition must be bool, got ${tCond.typ}")
        loopDepth += 1
        loopLabelStack += label
        pushScope()
        val (tSnapshotDecls, tInvariants) = buildLoopInvariantChecks(invariants)
        val tBody = tInvariants ++ analyzeBlock(rewrittenBody)
        popScope()
        loopLabelStack.remove(loopLabelStack.length - 1)
        loopDepth -= 1
        val loopStmt = TDoWhileStmt(tCond, tBody, label)
        val pre = tPreDecls ++ tSnapshotDecls
        if pre.isEmpty then loopStmt else TMultiStmt(pre ++ List(loopStmt))

      case LoopStmtAST(body, label) =>
        checkLoopLabelUnique(label)
        val (invariants, bodyAfterInvs) = extractLeadingInvariants(body)
        val (preDecls, rewrittenBody) = extractVariants(bodyAfterInvs)
        val tPreDecls = preDecls.map(analyzeStmt)
        loopDepth += 1
        loopLabelStack += label
        pushScope()
        val (tSnapshotDecls, tInvariants) = buildLoopInvariantChecks(invariants)
        val tBody = tInvariants ++ analyzeBlock(rewrittenBody)
        popScope()
        loopLabelStack.remove(loopLabelStack.length - 1)
        loopDepth -= 1
        val loopStmt = TLoopStmt(tBody, label)
        val pre = tPreDecls ++ tSnapshotDecls
        if pre.isEmpty then loopStmt else TMultiStmt(pre ++ List(loopStmt))

      case VariantStmtAST(_) =>
        throw AnalysisError("variant statement must appear at the top level of a loop body")

      case BreakStmtAST(label) =>
        if loopDepth == 0 then throw AnalysisError("break outside of loop")
        label.foreach { lbl =>
          if !loopLabelStack.contains(Some(lbl)) then
            throw AnalysisError(s"break '$lbl': no enclosing loop with that label")
        }
        TBreakStmt(label)

      case ContinueStmtAST(label) =>
        if loopDepth == 0 then throw AnalysisError("continue outside of loop")
        label.foreach { lbl =>
          if !loopLabelStack.contains(Some(lbl)) then
            throw AnalysisError(s"continue '$lbl': no enclosing loop with that label")
        }
        TContinueStmt(label)

      case DeferStmtAST(body) =>
        TDeferStmt(analyzeStmt(body))

      case AsmStmtAST(code) =>
        TAsmStmt(code)

      case BlockStmtAST(stmts) =>
        // Synthetic group node from parser desugars. Lowers as a flat
        // sequence in the enclosing scope; the existing TMultiStmt is the
        // equivalent typed shape and is handled by every backend. No new
        // lexical scope — captured locals must remain visible to following
        // statements in the same block (e.g. the for-each desugar's
        // `__foreach_src_v_N` is the loop-source binding for the ForStmt
        // that comes next in the block).
        TMultiStmt(stmts.map(analyzeStmt))

      case InvariantStmtAST(_, _) =>
        throw AnalysisError("invariant statement must appear at the top of a loop body, before any other statement")

      case AssumeStmtAST(e, msg) =>
        val te = analyzeExpr(e)
        if te.typ != BoolType then throw AnalysisError(s"assume expression must be bool, got ${te.typ}")
        contract("assume", te, msg.getOrElse("assume"))

      case InnerFunStmtAST(decl) =>
        // `def name(params) -> ret body` inside a function body — desugar to a named
        // local closure with self-reference support. The name is pre-bound so calls
        // inside the body resolve locally; the resulting TClosure carries selfName so
        // the interpreter can wire a self-cell into the captured env after construction.
        if decl.typeParams.nonEmpty then
          throw AnalysisError(s"inner def '${decl.name}' cannot declare type parameters")
        // Inner defs with `require`/`ensure` contracts are NOT processed via the closure
        // analyzer (which has no contract-emission stage). They are picked up post-pass
        // by `liftInnerDefClusters` which re-analyzes the body via the contract-aware
        // path — see that helper. We still emit a TClosure here so the post-pass has a
        // uniform shape to operate on; the closure body's contracts are silently
        // dropped by the closure analyzer, but the lift re-reads them from the
        // original FunDeclAST. Validation of "no outer-scope captures" also lives in
        // the post-pass so the error message can reference the captures directly.
        val retTypeAST = decl.returnType.getOrElse(
          throw AnalysisError(s"inner def '${decl.name}' must declare an explicit return type")
        )
        val retType = resolveType(retTypeAST)
        val paramTypes = decl.params.map(p => resolveType(p.typ))
        val funcType: SyslType = FuncType(paramTypes, retType, escaping = true)
        // Pre-bind the name in the enclosing scope so self-references in the body resolve.
        if scopeStack != null then
          currentScope(decl.name) = SymInfo(decl.name, funcType, mutable = false)
        // Reuse the closure analyzer by synthesizing a ClosureAST and pinning its expected
        // type. The closure analyzer pushes its own scope for params, analyzes the body,
        // and runs capture detection — which will pick up the self-name as a capture.
        val closureParams = decl.params.map(p => ClosureParamAST(p.name, Some(p.typ)))
        val closureAST = ClosureAST(closureParams, decl.body)
        val savedExp = currentExpected
        currentExpected = Some(funcType)
        val tClosure0 = try analyzeExpr(closureAST) finally currentExpected = savedExp
        val tClosure = tClosure0 match
          case c: TClosure =>
            // Only set selfName when the body actually self-references — otherwise
            // there's nothing to wire (and capture detection won't have added the
            // name to captures, so backends don't need a slot for it).
            val isSelfReferenced = c.captures.exists(_._1 == decl.name)
            if isSelfReferenced then c.copy(selfName = Some(decl.name)) else c
          case other => throw AnalysisError(s"inner def '${decl.name}': expected closure, got $other")
        TVarStmt(decl.name, funcType, tClosure)

      case ExprStmtAST(expr) =>
        TExprStmt(analyzeExpr(expr))

  // Resolve a variant name to its (EnumType, variant index), consulting the scrutinee
  // type first (for monomorphized generic enums) and then the global variantToEnum map.
  protected def resolveVariant(name: String, scrutineeType: SyslType): Option[(SyslType.EnumType, Int)] =
    scrutineeType match
      case etRaw: SyslType.EnumType =>
        val et = latestEnum(etRaw)
        val idx = et.variants.indexWhere(_._1 == name)
        if idx >= 0 then Some((et, idx))
        else variantToEnum.get(name)
      case _ => variantToEnum.get(name)

  // Counter for synthesizing unique outer-bindings for nested tuple patterns.
  protected var tuplePatternCounter: Int = 0
  protected def freshTupleBindName(): String =
    tuplePatternCounter += 1
    s"_match_tup_$tuplePatternCounter"

  protected def isTupleStructType(t: SyslType): Boolean = t.underlying match
    case st: SyslType.StructType => st.name.startsWith("_Tuple")
    case _ => false

  /** Analyze a field pattern inside a destructure (variant or struct). For most shapes
   *  this returns the user-visible binding name (or None for wildcard / literal). For a
   *  nested tuple pattern `(a, b)` the helper introduces a fresh synthetic outer name
   *  bound to the field, then appends `val a = sym._0; val b = sym._1` to `prelude` —
   *  these are prepended to the arm body before it's analyzed.
   *
   *  For a nested **variant** pattern (e.g. `Outer(Inner(v))`'s `Inner(v)` slot), the
   *  helper allocates a synthetic outer name AND records the recursive pattern in
   *  `nestedOut`. Each backend's match-arm dispatcher applies the nested pattern after
   *  the outer discriminator passes; bindings inside the nested pattern are added to
   *  the arm scope via `analyzePattern`'s recursion. Returns the synthetic name in
   *  the binding slot, plus the nested pattern in `nestedOut`. */
  protected def analyzeFieldPattern(
      fieldPat: MatchPatternAST,
      fieldType: SyslType,
      prelude: scala.collection.mutable.ListBuffer[StmtAST],
      nestedOut: scala.collection.mutable.ListBuffer[Option[TMatchPattern]],
  ): Option[String] = fieldPat match
    case WildcardPatternAST =>
      nestedOut += None
      None
    case ValuePatternAST(VarRefAST(bindName)) =>
      // If the name is a no-arg variant of the field's enum type, treat as
      // a nested variant pattern (zero-arg form), not a binding. Otherwise
      // it's a fresh binding name. Without this, `Wrap(A)` (where `A` is a
      // no-arg variant of `Inner`) would silently shadow the variant and
      // match unconditionally — wrong behavior.
      resolveVariant(bindName, fieldType) match
        case Some((et, variantIdx)) =>
          val (_, variantFields) = et.variants(variantIdx)
          if variantFields.isEmpty then
            val syn = freshTupleBindName()
            if scopeStack != null then
              currentScope(syn) = SymInfo(syn, fieldType, false)
            nestedOut += Some(TVariantPattern(et, variantIdx, Nil, Nil))
            Some(syn)
          else
            // Variant requires args but pattern wrote bare name — that's the
            // user's bug, but the existing dispatcher message is clearer.
            if scopeStack != null then
              currentScope(bindName) = SymInfo(bindName, fieldType, false)
            nestedOut += None
            Some(bindName)
        case None =>
          if scopeStack != null then
            currentScope(bindName) = SymInfo(bindName, fieldType, false)
          nestedOut += None
          Some(bindName)
    case ValuePatternAST(TupleLitAST(elems)) =>
      fieldType.underlying match
        case st: SyslType.StructType if st.name.startsWith("_Tuple") =>
          if elems.length != st.fields.length then
            throw AnalysisError(
              s"tuple pattern has ${elems.length} elements but field type has ${st.fields.length}"
            )
          val syn = freshTupleBindName()
          if scopeStack != null then
            currentScope(syn) = SymInfo(syn, fieldType, false)
          elems.zip(st.fields).foreach { case (elemExpr, (fname, _)) =>
            elemExpr match
              case VarRefAST("_") => ()
              case VarRefAST(bindName) =>
                prelude += VarStmtAST(
                  bindName, None,
                  FieldAccessAST(VarRefAST(syn), fname),
                  isMutable = false,
                )
              case _ => ()
          }
          nestedOut += None
          Some(syn)
        case other =>
          throw AnalysisError(s"tuple pattern requires tuple type, got $other")
    case dp: DestructurePatternAST =>
      // Nested variant or struct pattern. Bind a synthetic outer name to the
      // field, recurse to analyze the nested pattern under that synthetic
      // scrutinee type, and stash it in `nestedOut`. The interpreter / codegen
      // applies the nested pattern after the outer discriminator passes; if it
      // doesn't match, the arm doesn't match and we fall through.
      val syn = freshTupleBindName()
      if scopeStack != null then
        currentScope(syn) = SymInfo(syn, fieldType, false)
      val nested = analyzePattern(dp, fieldType, prelude)
      nestedOut += Some(nested)
      Some(syn)
    case ValuePatternAST(_) =>
      nestedOut += None
      None
    case _ => throw AnalysisError(s"unsupported pattern in destructure")

  protected def analyzePattern(
      pat: MatchPatternAST,
      scrutineeType: SyslType,
      prelude: scala.collection.mutable.ListBuffer[StmtAST],
  ): TMatchPattern =
    pat match
      case WildcardPatternAST => TWildcard
      case ValuePatternAST(VarRefAST(name)) if resolveVariant(name, scrutineeType).isDefined =>
        // No-arg variant pattern (e.g., `Empty` in a match arm)
        val (et, variantIdx) = resolveVariant(name, scrutineeType).get
        val (_, variantFields) = et.variants(variantIdx)
        if variantFields.nonEmpty then throw AnalysisError(s"variant '$name' requires ${variantFields.length} argument(s) in pattern")
        TVariantPattern(et, variantIdx, Nil, Nil)
      // Bare-name binding pattern at top level: any identifier that is NOT
      // a struct, variant, or visible value becomes a fresh binding capturing
      // the whole scrutinee. Mirrors the destructure field-binding convention
      // (see `analyzeFieldPattern` at line 6923) — but unlike the destructure
      // path, top-level names must defer to existing module-level vals/consts
      // so dispatchers like `cmd match { CMD_FOO -> ... ; CMD_BAR -> ... }`
      // keep doing value-comparison against the const. Without this
      // `tryLookup` guard, the const would be shadowed by a fresh binding
      // that always matches the scrutinee, collapsing every match arm into
      // the first one.
      case ValuePatternAST(VarRefAST(name))
          if scopeStack != null
            && !structTypes.contains(name)
            && resolveVariant(name, scrutineeType).isEmpty
            && tryLookup(name).isEmpty =>
        currentScope(name) = SymInfo(name, scrutineeType, false)
        TBindPattern(name, scrutineeType)
      // Top-level tuple pattern on a tuple-typed scrutinee — destructure directly.
      case ValuePatternAST(TupleLitAST(elems)) if isTupleStructType(scrutineeType) =>
        val st = scrutineeType.underlying.asInstanceOf[SyslType.StructType]
        if elems.length != st.fields.length then
          throw AnalysisError(
            s"tuple pattern has ${elems.length} elements but scrutinee type has ${st.fields.length}"
          )
        val bindings = elems.zip(st.fields).map { case (elemExpr, (_, fty)) =>
          elemExpr match
            case VarRefAST("_") => None
            case VarRefAST(bindName) =>
              if scopeStack != null then
                currentScope(bindName) = SymInfo(bindName, fty, false)
              Some(bindName)
            case _ => None
        }
        TDestructurePattern(st, bindings, st.fields.map(_._2))
      case ValuePatternAST(expr) =>
        val tv = analyzeExpr(expr)
        val coerced = coerceLiteral(tv, scrutineeType)
        if !compatible(coerced.typ, scrutineeType) then
          // Simple-enum scrutinee tolerates an integer-typed pattern: variant
          // access `Color.Red` analyzes to `TIntLit(value, I32)` because simple
          // enums are represented as i32 at runtime, but a fn parameter typed
          // `c: Color` keeps the nominal EnumType, so the structural compare
          // would otherwise reject the well-formed pattern. Scoped to the
          // pattern site only — global `compatible(int, EnumType)` would also
          // affect arg-passing where the runtime ABI mismatches (params of
          // simple-enum type are address-represented, raw ints are scalars).
          val simpleEnumOk = scrutineeType.underlying match
            case SyslType.EnumType(_, variants) =>
              variants.forall(_._2.isEmpty) && coerced.typ.isIntegral
            case _ => false
          if !simpleEnumOk then
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
          val nestedOut = new scala.collection.mutable.ListBuffer[Option[TMatchPattern]]
          val bindings = fields.zip(variantFields).map { case (fieldPat, (_, fieldType)) =>
            analyzeFieldPattern(fieldPat, fieldType, prelude, nestedOut)
          }
          val nested = if nestedOut.exists(_.isDefined) then nestedOut.toList else Nil
          TVariantPattern(et, variantIdx, bindings, variantFields.map(_._2), nested)
        else
          val st = structTypes.getOrElse(name, throw AnalysisError(s"unknown struct or variant '$name' in match pattern"))
          if fields.length != st.fields.length then
            throw AnalysisError(s"struct '$name' has ${st.fields.length} fields, pattern has ${fields.length}")
          val nestedOut = new scala.collection.mutable.ListBuffer[Option[TMatchPattern]]
          val bindings = fields.zip(st.fields).map { case (fieldPat, (_, fieldType)) =>
            analyzeFieldPattern(fieldPat, fieldType, prelude, nestedOut)
          }
          val nested = if nestedOut.exists(_.isDefined) then nestedOut.toList else Nil
          TDestructurePattern(st, bindings, st.fields.map(_._2), nested)
