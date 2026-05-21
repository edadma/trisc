package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

/** Extracted from SyslAnalyzer.scala for navigability: the #pure / #reads /
 *  #writes / #ghost validators plus the inferred-effects engine for closures
 *  (`inferClosureEffects`) and the effect-lattice helpers (`effectsSatisfy`,
 *  `lubEffect`). Self-typed onto SyslAnalyzer so it can call back into shared
 *  mutable state (functions, globalScope, ghostNames, …) and helpers
 *  (`resolveEffects` caches results in this trait but reads `globalScope`,
 *  `builtinFunctions`, `purePermittedBuiltins` from the host). */
trait SyslAnalyzerContracts:
  self: SyslAnalyzer =>

  /** Names of builtins that are safe to call from a #pure function. Arithmetic/comparison
   *  intrinsics aren't calls (they lower to TBinary/TUnary) so they don't need listing.
   *  `assert` is allowed because its only observable effect is termination — consistent
   *  with Ada's policy of letting pragma Assert live in pure functions. IO, allocation
   *  (malloc/free/…), and side-effecting traps (panic/abort/expect) are *not* listed. */
  protected val purePermittedBuiltins: Set[String] = Set("assert")

  /** Analyze a #pure function body. Reject any construct that could have observable
   *  side effects on state outside the function: writes to non-local vars, writes
   *  through pointers/fields, calls to non-pure user functions, indirect calls, asm,
   *  and IO/allocation builtins. Local-variable mutation is fine — it cannot escape.
   *  Called after body analysis so the typed AST is complete; purity of callees is
   *  read from their FunInfo, which was populated in the pre-collection pass. */
  protected def validatePureFn(funcName: String, body: TFunBody, paramNames: List[String], isDefFn: Boolean = false): Unit =
    val localVars = mutable.HashSet.from(paramNames)
    val prefix = if isDefFn then s"def function '$funcName'" else s"#pure function '$funcName'"

    def reject(msg: String): Nothing = throw AnalysisError(s"$prefix $msg")

    def isPureCallee(callee: String): Boolean =
      // Self-recursion is always fine (the function has isPure=true in the table).
      if callee == funcName then true
      else if purePermittedBuiltins.contains(callee) then true
      else if builtinFunctions.contains(callee) then false // other builtins are impure
      else
        // TCall carries the mangled name (for codegen), but the `functions` map is
        // keyed by the *local* name. Look up by key first; if that misses, fall back
        // to scanning FunInfo.name so cross-module calls (e.g. `mymod__sq`) resolve.
        val info = functions.get(callee).orElse(functions.values.find(_.name == callee))
        info match
          case Some(i) => i.isPure
          case None    => false // unknown: conservative reject

    def checkExpr(e: TExpr): Unit = e match
      case _: TIntLit | _: TFloatLit | _: TBoolLit | _: TStringLit | _: TUnitLit => ()
      case _: TVarRef | _: TAddrOf | _: TAddrLit | _: TFuncRef | _: TSizeof | _: TArrayDecl => ()
      case TArrayLit(els, _)               => els.foreach(checkExpr)
      case TAddrOfIndex(a, i, _)           => checkExpr(a); checkExpr(i)
      case TAddrOfField(o, _, _)           => checkExpr(o)
      case TTempAddr(e, _)                 => checkExpr(e)
      case TDeref(e, _)                    => checkExpr(e)
      case TIndex(e, i, _)                 => checkExpr(e); checkExpr(i)
      case TFieldAccess(o, _, _)           => checkExpr(o)
      case _: TFieldPreInc =>
        reject("cannot increment/decrement struct fields (side effect)")
      case _: TFieldPreDec =>
        reject("cannot increment/decrement struct fields (side effect)")
      case _: TFieldPostInc =>
        reject("cannot increment/decrement struct fields (side effect)")
      case _: TFieldPostDec =>
        reject("cannot increment/decrement struct fields (side effect)")
      case _: TStructLit                   => ()
      case TStructConstruct(_, args)       => args.foreach(checkExpr)
      case TPreInc(n, _) =>
        if !localVars.contains(n) then reject(s"cannot mutate non-local '$n'")
      case TPreDec(n, _) =>
        if !localVars.contains(n) then reject(s"cannot mutate non-local '$n'")
      case TPostInc(n, _) =>
        if !localVars.contains(n) then reject(s"cannot mutate non-local '$n'")
      case TPostDec(n, _) =>
        if !localVars.contains(n) then reject(s"cannot mutate non-local '$n'")
      case TUnary(_, o, _)                 => checkExpr(o)
      case TBinary(l, _, r, _)             => checkExpr(l); checkExpr(r)
      case TCall(callee, args, _) =>
        if !isPureCallee(callee) then reject(s"cannot call impure function '$callee'")
        args.foreach(checkExpr)
      case TIndirectCall(callee, args, _) =>
        // Allowed only if the callee's FuncType carries `#pure` — that's the only effect
        // signature compatible with `#pure` discipline (no module effects, no allocation,
        // etc.). Unknown / RW callees are rejected.
        callee.typ match
          case FuncType(_, _, _, eff) if eff.isPure => checkExpr(callee); args.foreach(checkExpr)
          case _ => reject("cannot make indirect call (callee is not declared `#pure`)")
      case TCast(e, _)                     => checkExpr(e)
      case TIfExpr(c, t, el, _)            => checkExpr(c); t.foreach(checkStmt); el.foreach(_.foreach(checkStmt))
      case TMatchExpr(e, arms, deflt, _) =>
        checkExpr(e)
        for arm <- arms do
          arm.guard.foreach(checkExpr)
          arm.body.foreach(checkStmt)
        deflt.foreach(_.foreach(checkStmt))
      case _: TEnumConstruct               => () // data-less construction
      case _: TNew =>
        reject("cannot heap-allocate (`new`) — allocation is observable")
      case _: TNewEnum =>
        reject("cannot heap-allocate (`new`) — allocation is observable")
      case _: TNewArray =>
        reject("cannot heap-allocate (`new`) — allocation is observable")
      case TLen(e, _)                      => checkExpr(e)
      case TCap(e, _)                      => checkExpr(e)
      case TSliceExpr(a, lo, hi, _)        => checkExpr(a); lo.foreach(checkExpr); hi.foreach(checkExpr)
      case TAppend(_, _, _)                => reject("cannot append to a slice (allocating side effect)")
      case TStringFromPtr(p, l, _)         => checkExpr(p); checkExpr(l)
      case TStringFromSlice(s, _)          => checkExpr(s)
      case TStr(e)                         => checkExpr(e)
      case TFmtStr(e, _)                   => checkExpr(e)
      case _: TClosure                     => reject("cannot construct closures (may capture mutable state)")
      case TInterfaceBox(e, _, _)          => checkExpr(e)
      case TInterfaceDispatch(ifaceVal, methodIdx, args, _) =>
        // Allowed only if the interface method's effect signature is `#pure` — every impl
        // is then guaranteed to satisfy the pure discipline (`satisfiesInterface` enforces
        // this at boxing time, so we don't need to inspect the actual impl here).
        ifaceVal.typ match
          case InterfaceType(_, methods) if methods(methodIdx)._4.isPure =>
            checkExpr(ifaceVal); args.foreach(checkExpr)
          case _ =>
            reject("cannot make interface-dispatch call (interface method is not declared `#pure`)")
      case TIntrinsicCall(name, args, _)   =>
        if !purePermittedBuiltins.contains(name) then reject(s"cannot call intrinsic '$name'")
        args.foreach(checkExpr)
      case TRangeCheck(e, _, _, _)         => checkExpr(e)
      case TAsmExpr(_, _)                  => reject("cannot contain asm expressions")

    def checkStmt(s: TStmt): Unit = s match
      case TVarStmt(n, _, init, _, _) =>
        checkExpr(init)
        localVars += n
      case TDestructureStmt(ns, _, init) =>
        checkExpr(init)
        localVars ++= ns
      case TDestructureAssignStmt(ns, _, init) =>
        checkExpr(init)
        for n <- ns do
          if !localVars.contains(n) then reject(s"cannot write to non-local '$n'")
      case TAssignStmt(target, value) =>
        if !localVars.contains(target) then reject(s"cannot write to non-local '$target'")
        checkExpr(value)
      case TCompoundAssignStmt(target, _, value) =>
        if !localVars.contains(target) then reject(s"cannot write to non-local '$target'")
        checkExpr(value)
      case TDerefAssignStmt(_, _) =>
        reject("cannot write through a pointer (possible non-local side effect)")
      case TIndexAssignStmt(_, _, _) =>
        reject("cannot write to an index (possible non-local side effect)")
      case TFieldAssignStmt(_, _, _) =>
        reject("cannot write to a struct field (possible non-local side effect)")
      case TFieldCompoundAssignStmt(_, _, _, _) =>
        reject("cannot compound-assign to a struct field (possible non-local side effect)")
      case TReturnStmt(v) =>
        v.foreach(checkExpr)
      case TWhileStmt(c, b, _) =>
        checkExpr(c); b.foreach(checkStmt)
      case TForStmt(init, c, u, b, _) =>
        checkStmt(init); checkExpr(c); checkStmt(u); b.foreach(checkStmt)
      case TDoWhileStmt(c, b, _) =>
        checkExpr(c); b.foreach(checkStmt)
      case TLoopStmt(b, _) =>
        b.foreach(checkStmt)
      case TBreakStmt(_) => ()
      case TContinueStmt(_) => ()
      case TDeferStmt(inner) =>
        // Defer executes on function exit — allowed if inner is pure too.
        checkStmt(inner)
      case TAsmStmt(_) =>
        reject("cannot contain asm blocks")
      case TContractCheck(_, e, _) =>
        checkExpr(e)
      case TMultiStmt(ss) =>
        ss.foreach(checkStmt)
      case TExprStmt(e) =>
        checkExpr(e)

    body match
      case TExprBody(e) => checkExpr(e)
      case TBlockBody(stmts) => stmts.foreach(checkStmt)

  /** Cache of resolved `#reads`/`#writes` effect sets, keyed by canonical (mangled)
   *  function name. Each entry is `(reads, writes)` where both are sets of *mangled*
   *  global-var names. Populated lazily by `resolveEffects` when validating bodies and
   *  call sites, so cross-function lookups don't pay quadratic resolution cost. */
  protected val resolvedEffectsCache = new mutable.HashMap[String, (Set[String], Set[String])]

  /** Resolve a function's raw `#reads`/`#writes` identifier lists to mangled global names.
   *  Validates that each name resolves to a module-level mutable var (via `globalScope`).
   *  `#pure` is treated as `#reads() #writes()` — both empty sets. Returns `None` for
   *  unannotated functions (no annotations means "effects unknown", which Rule 3 of the
   *  effects discipline rejects at call sites of annotated functions). */
  protected def resolveEffects(funInfo: FunInfo): Option[(Set[String], Set[String])] =
    if !funInfo.hasEffectAnnotations then None
    else resolvedEffectsCache.get(funInfo.name) match
      case Some(rw) => Some(rw)
      case None =>
        def resolveOne(rawName: String, attrName: String): String =
          globalScope.get(rawName) match
            case Some(sym) if sym.mutable && !sym.isConst => sym.name
            case Some(_) => throw AnalysisError(s"#$attrName on '${funInfo.name}' references '$rawName' which is not mutable")
            case None    => throw AnalysisError(s"#$attrName on '${funInfo.name}' references unknown global '$rawName'")
        val r = funInfo.reads.getOrElse(Set.empty).map(resolveOne(_, "reads"))
        val w = funInfo.writes.getOrElse(Set.empty).map(resolveOne(_, "writes"))
        val pair = (r, w)
        resolvedEffectsCache(funInfo.name) = pair
        Some(pair)

  /** Walk an annotated function body. Enforces three rules:
   *  - **Body conformance.** Every read of a module-level var V requires V ∈ R ∪ W;
   *    every write requires V ∈ W. Reads inside contract expressions count.
   *  - **Call-site subset.** A call to a function with `#reads(R')` `#writes(W')`
   *    requires R' ⊆ R ∪ W and W' ⊆ W.
   *  - **Strict closure.** The function may only call other annotated (or `#pure`) functions
   *    plus the pure builtins; indirect calls, interface dispatch, `new`, and asm are
   *    rejected (mirrors `validatePureFn`).
   *
   *  Called after body analysis, like `validatePureFn`. Skipped for unannotated functions. */
  protected def validateEffects(funcName: String, fi: FunInfo, body: TFunBody, paramNames: List[String]): Unit =
    val resolvedOpt = resolveEffects(fi)
    if resolvedOpt.isEmpty then return
    val (reads, writes) = resolvedOpt.get
    val readsOrWrites = reads ++ writes
    val localVars = mutable.HashSet.from(paramNames)

    // Index of mutable globals by mangled name — checked O(1) per TVarRef.
    val mutableGlobals: Set[String] =
      globalScope.values.collect { case s if s.mutable && !s.isConst => s.name }.toSet

    def reject(msg: String): Nothing = throw AnalysisError(s"#reads/#writes function '$funcName' $msg")

    def lookupCalleeFunInfo(callee: String): Option[FunInfo] =
      // Self-recursion: same FunInfo we're validating.
      if callee == funcName || callee == fi.name then Some(fi)
      else functions.get(callee).orElse(functions.values.find(_.name == callee))

    def isAllowedBuiltin(callee: String): Boolean =
      purePermittedBuiltins.contains(callee) || (
        builtinFunctions.contains(callee) && purePermittedBuiltins.contains(callee)
      )

    def checkCall(callee: String): Unit =
      if isAllowedBuiltin(callee) then return
      if builtinFunctions.contains(callee) then
        reject(s"cannot call impure builtin '$callee'")
      lookupCalleeFunInfo(callee) match
        case None => reject(s"cannot call unknown function '$callee'")
        case Some(calleeInfo) =>
          if !calleeInfo.hasEffectAnnotations then
            reject(s"cannot call '$callee' (no #reads/#writes annotations)")
          val (cR, cW) = resolveEffects(calleeInfo).get
          val missingW = cW.diff(writes)
          if missingW.nonEmpty then
            reject(s"calls '$callee' which writes ${missingW.mkString(", ")} not in caller's #writes")
          val missingR = cR.diff(readsOrWrites)
          if missingR.nonEmpty then
            reject(s"calls '$callee' which reads ${missingR.mkString(", ")} not in caller's #reads or #writes")

    def checkRead(name: String): Unit =
      if mutableGlobals.contains(name) && !localVars.contains(name) then
        if !readsOrWrites.contains(name) then
          reject(s"reads global '$name' not declared in #reads")

    def checkWrite(name: String): Unit =
      if mutableGlobals.contains(name) && !localVars.contains(name) then
        if !writes.contains(name) then
          reject(s"writes to global '$name' not declared in #writes")

    def checkExpr(e: TExpr): Unit = e match
      case _: TIntLit | _: TFloatLit | _: TBoolLit | _: TStringLit | _: TUnitLit => ()
      case TVarRef(name, _)                => checkRead(name)
      case TAddrOf(name, _)                => checkRead(name)
      case _: TAddrLit | _: TFuncRef | _: TSizeof | _: TArrayDecl => ()
      case TArrayLit(els, _)               => els.foreach(checkExpr)
      case TAddrOfIndex(a, i, _)           => checkExpr(a); checkExpr(i)
      case TAddrOfField(o, _, _)           => checkExpr(o)
      case TTempAddr(e, _)                 => checkExpr(e)
      case TDeref(e, _)                    => checkExpr(e)
      case TIndex(e, i, _)                 => checkExpr(e); checkExpr(i)
      case TFieldAccess(o, _, _)           => checkExpr(o)
      case TFieldPreInc(o, _, _)           => checkExpr(o)
      case TFieldPreDec(o, _, _)           => checkExpr(o)
      case TFieldPostInc(o, _, _)          => checkExpr(o)
      case TFieldPostDec(o, _, _)          => checkExpr(o)
      case _: TStructLit                   => ()
      case TStructConstruct(_, args)       => args.foreach(checkExpr)
      case TPreInc(n, _)                   => checkWrite(n); checkRead(n)
      case TPreDec(n, _)                   => checkWrite(n); checkRead(n)
      case TPostInc(n, _)                  => checkWrite(n); checkRead(n)
      case TPostDec(n, _)                  => checkWrite(n); checkRead(n)
      case TUnary(_, o, _)                 => checkExpr(o)
      case TBinary(l, _, r, _)             => checkExpr(l); checkExpr(r)
      case TCall(callee, args, _) =>
        checkCall(callee)
        args.foreach(checkExpr)
      case TIndirectCall(callee, args, _) =>
        // Indirect call permitted iff the callee's FuncType has an annotated effect
        // signature whose effects are a subset of the caller's. `#pure` callees
        // satisfy any annotated caller; `#reads(R)`/`#writes(W)` callees are checked
        // against the caller's R∪W (for reads) and W (for writes). An absent side
        // (e.g. `#writes(x)` with no `#reads`) is treated as an empty set.
        callee.typ match
          case FuncType(_, _, _, eff) if eff.isPure =>
            checkExpr(callee); args.foreach(checkExpr)
          case FuncType(_, _, _, eff) if !eff.isUnknown =>
            val cR = eff.reads.getOrElse(Set.empty)
            val cW = eff.writes.getOrElse(Set.empty)
            val missingR = cR.diff(readsOrWrites)
            if missingR.nonEmpty then
              reject(s"indirect call's #reads(${missingR.mkString(", ")}) not in caller's #reads or #writes")
            val missingW = cW.diff(writes)
            if missingW.nonEmpty then
              reject(s"indirect call's #writes(${missingW.mkString(", ")}) not in caller's #writes")
            checkExpr(callee); args.foreach(checkExpr)
          case _ =>
            reject("cannot make indirect call (callee has no effect annotation)")
      case TCast(e, _)                     => checkExpr(e)
      case TIfExpr(c, t, el, _)            => checkExpr(c); t.foreach(checkStmt); el.foreach(_.foreach(checkStmt))
      case TQuantifier(_, _, _, lo, hi, _, pred, _) =>
        checkExpr(lo); checkExpr(hi); checkExpr(pred)
      case TMatchExpr(e, arms, deflt, _) =>
        checkExpr(e)
        for arm <- arms do
          arm.guard.foreach(checkExpr)
          arm.body.foreach(checkStmt)
        deflt.foreach(_.foreach(checkStmt))
      case _: TEnumConstruct               => ()
      case _: TNew                         => reject("cannot heap-allocate (`new`) — allocation effects are not yet tracked")
      case _: TNewEnum                     => reject("cannot heap-allocate (`new`) — allocation effects are not yet tracked")
      case _: TNewArray                    => reject("cannot heap-allocate (`new`) — allocation effects are not yet tracked")
      case TLen(e, _)                      => checkExpr(e)
      case TCap(e, _)                      => checkExpr(e)
      case TSliceExpr(a, lo, hi, _)        => checkExpr(a); lo.foreach(checkExpr); hi.foreach(checkExpr)
      case TAppend(_, _, _)                => reject("cannot append to a slice (allocating side effect)")
      case TStringFromPtr(p, l, _)         => checkExpr(p); checkExpr(l)
      case TStringFromSlice(s, _)          => checkExpr(s)
      case TStr(e)                         => checkExpr(e)
      case TFmtStr(e, _)                   => checkExpr(e)
      case _: TClosure                     => reject("cannot construct closures (may capture mutable state)")
      case TInterfaceBox(e, _, _)          => checkExpr(e)
      case TInterfaceDispatch(ifaceVal, methodIdx, args, _) =>
        // Subset check against the interface method's declared effects (the impl is
        // guaranteed by `satisfiesInterface` to satisfy these at boxing time).
        ifaceVal.typ match
          case InterfaceType(_, methods) =>
            val (_, _, _, eff) = methods(methodIdx)
            if eff.isPure then
              checkExpr(ifaceVal); args.foreach(checkExpr)
            else if eff.isUnknown then
              reject("cannot make interface-dispatch call (interface method has no effect annotation)")
            else
              val cR = eff.reads.getOrElse(Set.empty)
              val cW = eff.writes.getOrElse(Set.empty)
              val missingR = cR.diff(readsOrWrites)
              if missingR.nonEmpty then
                reject(s"interface dispatch's #reads(${missingR.mkString(", ")}) not in caller's #reads or #writes")
              val missingW = cW.diff(writes)
              if missingW.nonEmpty then
                reject(s"interface dispatch's #writes(${missingW.mkString(", ")}) not in caller's #writes")
              checkExpr(ifaceVal); args.foreach(checkExpr)
          case _ => reject("interface-dispatch on non-interface type")
      case TIntrinsicCall(name, args, _)   =>
        if !purePermittedBuiltins.contains(name) then reject(s"cannot call intrinsic '$name'")
        args.foreach(checkExpr)
      case TRangeCheck(e, _, _, _)         => checkExpr(e)
      case TAsmExpr(_, _)                  => reject("cannot contain asm expressions")

    def checkStmt(s: TStmt): Unit = s match
      case TVarStmt(n, _, init, _, _) =>
        checkExpr(init)
        localVars += n
      case TDestructureStmt(ns, _, init) =>
        checkExpr(init)
        localVars ++= ns
      case TDestructureAssignStmt(ns, _, init) =>
        checkExpr(init)
        for n <- ns do checkWrite(n)
      case TAssignStmt(target, value) =>
        checkWrite(target)
        checkExpr(value)
      case TCompoundAssignStmt(target, _, value) =>
        checkWrite(target); checkRead(target)
        checkExpr(value)
      case TDerefAssignStmt(p, v) =>
        // A pointer write may target anything — too coarse to track precisely.
        // SPARK requires explicit abstract-state tying for pointer effects; reject for now.
        reject("cannot write through a pointer (effect not yet trackable)")
      case TIndexAssignStmt(a, i, v) =>
        checkExpr(a); checkExpr(i); checkExpr(v)
        // For a write into a global slice/array, treat it as a write to that global.
        a match
          case TVarRef(n, _) if mutableGlobals.contains(n) && !localVars.contains(n) => checkWrite(n)
          case _ => ()
      case TFieldAssignStmt(o, _, v) =>
        checkExpr(o); checkExpr(v)
        o match
          case TVarRef(n, _) if mutableGlobals.contains(n) && !localVars.contains(n) => checkWrite(n)
          case _ => ()
      case TFieldCompoundAssignStmt(o, _, _, v) =>
        checkExpr(o); checkExpr(v)
        o match
          case TVarRef(n, _) if mutableGlobals.contains(n) && !localVars.contains(n) => checkWrite(n)
          case _ => ()
      case TReturnStmt(v) =>
        v.foreach(checkExpr)
      case TWhileStmt(c, b, _) =>
        checkExpr(c); b.foreach(checkStmt)
      case TForStmt(init, c, u, b, _) =>
        checkStmt(init); checkExpr(c); checkStmt(u); b.foreach(checkStmt)
      case TDoWhileStmt(c, b, _) =>
        checkExpr(c); b.foreach(checkStmt)
      case TLoopStmt(b, _) =>
        b.foreach(checkStmt)
      case TBreakStmt(_) => ()
      case TContinueStmt(_) => ()
      case TDeferStmt(inner) =>
        checkStmt(inner)
      case TAsmStmt(_) =>
        reject("cannot contain asm blocks")
      case TContractCheck(_, e, _) =>
        checkExpr(e)
      case TMultiStmt(ss) =>
        ss.foreach(checkStmt)
      case TExprStmt(e) =>
        checkExpr(e)

    body match
      case TExprBody(e) => checkExpr(e)
      case TBlockBody(stmts) => stmts.foreach(checkStmt)

  /** Infer a closure's effect signature by walking its typed body. Three outcomes:
   *
   *  - **Pure** — no module-level reads or writes, no allocation, no impure calls, no
   *    writes to captured outer locals, no asm.
   *  - **RW(R, W)** — reads/writes of specific module-level mutable globals were observed,
   *    with all called functions being annotated (or pure) so their effects were inherited.
   *  - **Unknown** — the body contains a construct we can't summarize: a `new` allocation,
   *    an append, an asm block, a call to an unannotated impure function, an indirect call
   *    through an Unknown-typed callable, an interface dispatch to an unannotated method,
   *    or a write to a captured outer local (a side effect on the enclosing scope that
   *    can't be expressed as a set of module-level var names).
   *
   *  Reads of captured outer locals are permitted and do not contribute to the inferred
   *  effect sets — captures are opaque dataflow dependencies, not module-level effects. */
  protected def inferClosureEffects(body: TFunBody, paramNames: List[String]): FuncEffects =
    val locals = mutable.HashSet.from(paramNames)
    val reads  = mutable.HashSet[String]()
    val writes = mutable.HashSet[String]()
    var bailed = false
    def bail(): Unit = bailed = true

    def mutableGlobal(n: String): Option[String] =
      if locals.contains(n) then None
      else globalScope.get(n) match
        case Some(sym) if sym.mutable && !sym.isConst => Some(sym.name)
        case _ => None

    def isCapturedLocal(n: String): Boolean =
      !locals.contains(n) && !globalScope.contains(n) && !functions.contains(n)

    def absorbCallee(calleeName: String): Boolean =
      if purePermittedBuiltins.contains(calleeName) then return true
      val infoOpt = functions.get(calleeName).orElse(functions.values.find(_.name == calleeName))
      infoOpt match
        case Some(info) =>
          if info.isPure then true
          else resolveEffects(info) match
            case Some((r, w)) => reads ++= r; writes ++= w; true
            case None         => false
        case None => false

    def checkExpr(e: TExpr): Unit =
      if bailed then return
      e match
        case _: TIntLit | _: TFloatLit | _: TBoolLit | _: TStringLit | _: TUnitLit => ()
        case _: TAddrLit | _: TFuncRef | _: TSizeof | _: TArrayDecl | _: TStructLit | _: TEnumConstruct => ()
        case TVarRef(n, _) => mutableGlobal(n).foreach(reads += _)
        case TAddrOf(n, _) => mutableGlobal(n).foreach(reads += _)
        case TArrayLit(els, _)               => els.foreach(checkExpr)
        case TAddrOfIndex(a, i, _)           => checkExpr(a); checkExpr(i)
        case TAddrOfField(o, _, _)           => checkExpr(o)
        case TTempAddr(i, _)                 => checkExpr(i)
        case TDeref(i, _)                    => checkExpr(i)
        case TIndex(a, i, _)                 => checkExpr(a); checkExpr(i)
        case TFieldAccess(o, _, _)           => checkExpr(o)
        case _: TFieldPreInc | _: TFieldPreDec | _: TFieldPostInc | _: TFieldPostDec => bail()
        case TStructConstruct(_, args)       => args.foreach(checkExpr)
        case TPreInc(n, _) =>
          mutableGlobal(n) match
            case Some(mg) => reads += mg; writes += mg
            case None => if isCapturedLocal(n) then bail()
        case TPreDec(n, _) =>
          mutableGlobal(n) match
            case Some(mg) => reads += mg; writes += mg
            case None => if isCapturedLocal(n) then bail()
        case TPostInc(n, _) =>
          mutableGlobal(n) match
            case Some(mg) => reads += mg; writes += mg
            case None => if isCapturedLocal(n) then bail()
        case TPostDec(n, _) =>
          mutableGlobal(n) match
            case Some(mg) => reads += mg; writes += mg
            case None => if isCapturedLocal(n) then bail()
        case TUnary(_, o, _)                 => checkExpr(o)
        case TBinary(l, _, r, _)             => checkExpr(l); checkExpr(r)
        case TCall(callee, args, _) =>
          if !absorbCallee(callee) then bail()
          args.foreach(checkExpr)
        case TIndirectCall(callee, args, _) =>
          callee.typ match
            case FuncType(_, _, _, eff) if eff.isPure =>
              checkExpr(callee); args.foreach(checkExpr)
            case FuncType(_, _, _, eff) if !eff.isUnknown =>
              reads ++= eff.reads.getOrElse(Set.empty)
              writes ++= eff.writes.getOrElse(Set.empty)
              checkExpr(callee); args.foreach(checkExpr)
            case _ => bail()
        case TCast(inner, _)                 => checkExpr(inner)
        case TIfExpr(c, tb, eb, _)           => checkExpr(c); tb.foreach(checkStmt); eb.foreach(_.foreach(checkStmt))
        case TQuantifier(_, _, _, lo, hi, _, pred, _) => checkExpr(lo); checkExpr(hi); checkExpr(pred)
        case TMatchExpr(scr, arms, dflt, _) =>
          checkExpr(scr)
          for arm <- arms do
            arm.guard.foreach(checkExpr); arm.body.foreach(checkStmt)
          dflt.foreach(_.foreach(checkStmt))
        case _: TNew | _: TNewEnum | _: TNewArray => bail()
        case TLen(i, _)                      => checkExpr(i)
        case TCap(i, _)                      => checkExpr(i)
        case TSliceExpr(a, lo, hi, _)        => checkExpr(a); lo.foreach(checkExpr); hi.foreach(checkExpr)
        case _: TAppend                      => bail()
        case TStringFromPtr(p, l, _)         => checkExpr(p); checkExpr(l)
        case TStringFromSlice(s, _)          => checkExpr(s)
        case TStr(inner)                     => checkExpr(inner)
        case TFmtStr(inner, _)               => checkExpr(inner)
        case _: TClosure                     => bail()
        case TInterfaceBox(i, _, _)          => checkExpr(i)
        case TInterfaceDispatch(v, idx, args, _) =>
          v.typ match
            case InterfaceType(_, methods) =>
              val eff = methods(idx)._4
              if eff.isPure then { checkExpr(v); args.foreach(checkExpr) }
              else if !eff.isUnknown then
                reads ++= eff.reads.getOrElse(Set.empty)
                writes ++= eff.writes.getOrElse(Set.empty)
                checkExpr(v); args.foreach(checkExpr)
              else bail()
            case _ => bail()
        case TIntrinsicCall(name, args, _) =>
          if !purePermittedBuiltins.contains(name) then bail()
          args.foreach(checkExpr)
        case TRangeCheck(inner, _, _, _)     => checkExpr(inner)
        case _: TAsmExpr                     => bail()

    def checkStmt(s: TStmt): Unit =
      if bailed then return
      s match
        case TVarStmt(n, _, init, _, _)      => checkExpr(init); locals += n
        case TDestructureStmt(ns, _, init)   => checkExpr(init); locals ++= ns
        case TDestructureAssignStmt(ns, _, init) =>
          checkExpr(init)
          for n <- ns do
            mutableGlobal(n) match
              case Some(mg) => writes += mg
              case None => if isCapturedLocal(n) then bail()
        case TAssignStmt(t, v) =>
          mutableGlobal(t) match
            case Some(mg) => writes += mg
            case None => if isCapturedLocal(t) then bail()
          checkExpr(v)
        case TCompoundAssignStmt(t, _, v) =>
          mutableGlobal(t) match
            case Some(mg) => reads += mg; writes += mg
            case None => if isCapturedLocal(t) then bail()
          checkExpr(v)
        case TFieldAssignStmt(o, _, v) =>
          o match
            case TVarRef(n, _) => mutableGlobal(n).foreach(writes += _)
            case _ => ()
          checkExpr(o); checkExpr(v)
        case TFieldCompoundAssignStmt(o, _, _, v) =>
          o match
            case TVarRef(n, _) =>
              mutableGlobal(n) match
                case Some(mg) => reads += mg; writes += mg
                case None => ()
            case _ => ()
          checkExpr(o); checkExpr(v)
        case TIndexAssignStmt(a, i, v) =>
          a match
            case TVarRef(n, _) => mutableGlobal(n).foreach(writes += _)
            case _ => ()
          checkExpr(a); checkExpr(i); checkExpr(v)
        case _: TDerefAssignStmt => bail() // pointer write — can't track target
        case TReturnStmt(v)          => v.foreach(checkExpr)
        case TWhileStmt(c, b, _)     => checkExpr(c); b.foreach(checkStmt)
        case TForStmt(init, c, u, b, _) => checkStmt(init); checkExpr(c); checkStmt(u); b.foreach(checkStmt)
        case TDoWhileStmt(c, b, _)   => checkExpr(c); b.foreach(checkStmt)
        case TLoopStmt(b, _)         => b.foreach(checkStmt)
        case TBreakStmt(_) | TContinueStmt(_) => ()
        case TAsmStmt(_)             => bail()
        case TDeferStmt(inner)       => checkStmt(inner)
        case TContractCheck(_, e, _) => checkExpr(e)
        case TMultiStmt(ss)          => ss.foreach(checkStmt)
        case TExprStmt(e)            => checkExpr(e)

    body match
      case TExprBody(e) => checkExpr(e)
      case TBlockBody(stmts) => stmts.foreach(checkStmt)

    if bailed then FuncEffects.Unknown
    else if reads.isEmpty && writes.isEmpty then FuncEffects.Pure
    else FuncEffects(reads = Some(reads.toSet), writes = Some(writes.toSet))

  /** Build a FuncEffects from a FunInfo. The function-decl's `#pure` / `#reads` / `#writes`
   *  attributes are reflected so that taking a function reference produces a `FuncType`
   *  whose effect signature matches. The `reads`/`writes` are already mangled (resolved
   *  through globalScope by the validator), so they're directly comparable at indirect
   *  call sites. */
  protected def funInfoEffects(fi: FunInfo): FuncEffects =
    if fi.isPure then FuncEffects(isPure = true, isRealtime = fi.isRealtime)
    else if fi.reads.isDefined || fi.writes.isDefined then
      // Resolve through the cached effects table (handles mangling once, idempotent).
      resolveEffects(fi) match
        case Some((r, w)) => FuncEffects(reads = Some(r), writes = Some(w), isRealtime = fi.isRealtime)
        case None         => FuncEffects(isRealtime = fi.isRealtime)
    else FuncEffects(isRealtime = fi.isRealtime)

  /** Effect subtyping for `FuncType` compatibility. Returns true iff a function with
   *  effects `actual` can be safely placed in a slot expecting effects `slot`. The rule
   *  is "more guarantees → fewer effects": Pure (no module effects + extra pure discipline)
   *  satisfies any annotated slot; an `RW(R, W)` actual satisfies a slot iff its effects
   *  are a subset of the slot's. An unknown actual satisfies only an unknown slot.
   *
   *  Symmetric direction matters: this is *contravariant in effects* — a slot accepting
   *  an unknown callable must allow anything, but a slot demanding a pure callable must
   *  receive a pure callable. Effects unknown is the most-permissive side. */
  protected def effectsSatisfy(actual: FuncEffects, slot: FuncEffects): Boolean =
    if slot.isUnknown then true
    else if actual.isPure then true
    else if slot.isPure then false  // slot wants pure, actual is RW or unknown — reject
    else if actual.isUnknown then false  // slot wants annotated, actual is unknown — reject
    else
      // Both annotated RW. Check subset: actual's reads ⊆ slot's reads, actual's writes ⊆ slot's writes.
      val aR = actual.reads.getOrElse(Set.empty)
      val aW = actual.writes.getOrElse(Set.empty)
      val sR = slot.reads.getOrElse(Set.empty)
      val sW = slot.writes.getOrElse(Set.empty)
      aR.subsetOf(sR) && aW.subsetOf(sW)

  /** Least upper bound of two effect signatures under the effect lattice
   *  (`Pure ≤ RW(R, W) ≤ Unknown`, with `RW` ordered by subset on its sets).
   *  Returns `None` when the two are incomparable — i.e., both are `RW` but
   *  neither's read/write sets are a subset of the other's. The LUB is the
   *  *less-specific* (larger, higher) of the two when comparable, so the
   *  merged binding is wide enough that **both** original observations flow
   *  into it as actuals via `effectsSatisfy`. (Picking the smaller — the GLB —
   *  would let the merge succeed but make the larger observation fail
   *  `checkArgs` immediately afterwards.)
   */
  protected def lubEffect(e1: FuncEffects, e2: FuncEffects): Option[FuncEffects] =
    if e1 == e2 then Some(e1)
    else if e1.isUnknown || e2.isUnknown then Some(FuncEffects.Unknown)
    else if e1.isPure then Some(e2)
    else if e2.isPure then Some(e1)
    else
      val r1 = e1.reads.getOrElse(Set.empty)
      val w1 = e1.writes.getOrElse(Set.empty)
      val r2 = e2.reads.getOrElse(Set.empty)
      val w2 = e2.writes.getOrElse(Set.empty)
      if r1.subsetOf(r2) && w1.subsetOf(w2) then Some(e2)
      else if r2.subsetOf(r1) && w2.subsetOf(w1) then Some(e1)
      else None

  /** Walk a function body to enforce ghost-code discipline:
   *  - Real code cannot read ghost variables or call ghost functions. "Real code" is
   *    everything outside contract clauses, ghost var initializers, ghost-target assignment
   *    RHSes, and ghost function bodies.
   *  - A `#ghost fn` body cannot write to real (non-ghost) module-level state. Local writes
   *    are fine (the function's locals are intrinsically scoped to it).
   *
   *  Called for every function — real or ghost — because a real function may declare
   *  ghost locals whose use in real code needs to be caught here. Ghost-context tracking
   *  is per-expression: an expression is in ghost context iff (a) the enclosing function
   *  is ghost, (b) it's a contract clause expression, or (c) it's the RHS of a ghost var
   *  decl / assignment to a ghost name. */
  protected def validateGhostDiscipline(funcName: String, fi: FunInfo, body: TFunBody): Unit =
    val ghostLocals = mutable.HashSet[String]()
    def isGhostName(name: String): Boolean = ghostNames.contains(name) || ghostLocals.contains(name)
    def isGhostFn(callee: String): Boolean =
      functions.get(callee).exists(_.isGhost) ||
        functions.values.exists(f => f.name == callee && f.isGhost)
    def reject(msg: String): Nothing = throw AnalysisError(s"#ghost discipline in '$funcName': $msg")
    // A name is a real (non-ghost) module-level mutable global iff it's in globalScope,
    // not marked ghost, and not a const. Used to catch ghost-code writes to real state.
    def isRealGlobal(name: String): Boolean =
      globalScope.values.exists(s => s.name == name && !s.isGhost && s.mutable && !s.isConst)

    def checkExpr(e: TExpr, ghostCtx: Boolean): Unit = e match
      case _: TIntLit | _: TFloatLit | _: TBoolLit | _: TUnitLit | _: TStringLit | _: TArrayDecl => ()
      case _: TAddrLit | _: TFuncRef | _: TSizeof | _: TStructLit | _: TEnumConstruct => ()
      case _: TPreInc | _: TPreDec | _: TPostInc | _: TPostDec => ()
      case TVarRef(name, _) =>
        if !ghostCtx && isGhostName(name) then
          reject(s"real-code expression reads ghost variable '$name'")
      case TAddrOf(name, _) =>
        if !ghostCtx && isGhostName(name) then
          reject(s"real-code expression takes address of ghost variable '$name'")
      case TArrayLit(els, _)               => els.foreach(checkExpr(_, ghostCtx))
      case TAddrOfIndex(a, i, _)           => checkExpr(a, ghostCtx); checkExpr(i, ghostCtx)
      case TAddrOfField(o, _, _)           => checkExpr(o, ghostCtx)
      case TTempAddr(inner, _)             => checkExpr(inner, ghostCtx)
      case TDeref(inner, _)                => checkExpr(inner, ghostCtx)
      case TIndex(arr, i, _)               => checkExpr(arr, ghostCtx); checkExpr(i, ghostCtx)
      case TFieldAccess(o, _, _)           => checkExpr(o, ghostCtx)
      case TFieldPreInc(o, _, _)           => checkExpr(o, ghostCtx)
      case TFieldPreDec(o, _, _)           => checkExpr(o, ghostCtx)
      case TFieldPostInc(o, _, _)          => checkExpr(o, ghostCtx)
      case TFieldPostDec(o, _, _)          => checkExpr(o, ghostCtx)
      case TStructConstruct(st, args)      =>
        if !ghostCtx && ghostTypes.contains(st.name) then
          reject(s"real-code expression constructs ghost type '${st.name}'")
        args.foreach(checkExpr(_, ghostCtx))
      case TUnary(_, o, _)                 => checkExpr(o, ghostCtx)
      case TBinary(l, _, r, _)             => checkExpr(l, ghostCtx); checkExpr(r, ghostCtx)
      case TCall(callee, args, _) =>
        if !ghostCtx && isGhostFn(callee) then
          reject(s"real-code expression calls ghost function '$callee'")
        args.foreach(checkExpr(_, ghostCtx))
      case TIndirectCall(c, args, _)       => checkExpr(c, ghostCtx); args.foreach(checkExpr(_, ghostCtx))
      case TCast(inner, _)                 => checkExpr(inner, ghostCtx)
      case TIfExpr(c, tb, eb, _) =>
        checkExpr(c, ghostCtx)
        tb.foreach(checkStmt(_, ghostCtx))
        eb.foreach(_.foreach(checkStmt(_, ghostCtx)))
      case TQuantifier(_, _, _, lo, hi, _, pred, _) =>
        // Quantifiers are intrinsically ghost-friendly (they only ever appear in contract
        // clauses or other ghost contexts in practice), so descend with ghostCtx as-is.
        checkExpr(lo, ghostCtx); checkExpr(hi, ghostCtx); checkExpr(pred, ghostCtx)
      case TMatchExpr(scr, arms, dflt, _) =>
        checkExpr(scr, ghostCtx)
        for arm <- arms do
          arm.guard.foreach(checkExpr(_, ghostCtx))
          arm.body.foreach(checkStmt(_, ghostCtx))
        dflt.foreach(_.foreach(checkStmt(_, ghostCtx)))
      case TNew(st, args)                  =>
        if !ghostCtx && ghostTypes.contains(st.name) then
          reject(s"real-code expression constructs ghost type '${st.name}' via new")
        args.foreach(checkExpr(_, ghostCtx))
      case TNewEnum(et, _, args)           =>
        if !ghostCtx && ghostTypes.contains(et.name) then
          reject(s"real-code expression constructs ghost enum '${et.name}' via new")
        args.foreach(checkExpr(_, ghostCtx))
      case TNewArray(_, sz)                => checkExpr(sz, ghostCtx)
      case TLen(inner, _)                  => checkExpr(inner, ghostCtx)
      case TCap(inner, _)                  => checkExpr(inner, ghostCtx)
      case TSliceExpr(a, lo, hi, _)        => checkExpr(a, ghostCtx); lo.foreach(checkExpr(_, ghostCtx)); hi.foreach(checkExpr(_, ghostCtx))
      case TAppend(s, el, _)               => checkExpr(s, ghostCtx); checkExpr(el, ghostCtx)
      case TStringFromPtr(p, l, _)         => checkExpr(p, ghostCtx); checkExpr(l, ghostCtx)
      case TStringFromSlice(s, _)          => checkExpr(s, ghostCtx)
      case TStr(inner)                     => checkExpr(inner, ghostCtx)
      case TFmtStr(inner, _)               => checkExpr(inner, ghostCtx)
      case _: TClosure                     => () // Closures snapshot their environment; treat as opaque for ghost purposes.
      case TInterfaceBox(inner, _, _)      => checkExpr(inner, ghostCtx)
      case TInterfaceDispatch(v, _, args, _) => checkExpr(v, ghostCtx); args.foreach(checkExpr(_, ghostCtx))
      case TIntrinsicCall(_, args, _)      => args.foreach(checkExpr(_, ghostCtx))
      case TRangeCheck(inner, _, _, _)     => checkExpr(inner, ghostCtx)
      case _: TAsmExpr                     => ()

    def checkStmt(s: TStmt, ghostCtx: Boolean): Unit = s match
      case TVarStmt(n, _, init, _, isLocalGhost) =>
        if isLocalGhost then ghostLocals += n
        // Ghost var declared inside a ghost function: the var is ghost too. Initializer is ghost.
        val initIsGhost = ghostCtx || isLocalGhost
        checkExpr(init, initIsGhost)
      case TDestructureStmt(_, _, init) =>
        checkExpr(init, ghostCtx)
      case TDestructureAssignStmt(ns, _, init) =>
        if fi.isGhost then
          for n <- ns do if isRealGlobal(n) then reject(s"ghost function writes to real global '$n'")
        checkExpr(init, ghostCtx)
      case TAssignStmt(target, value) =>
        val targetIsGhost = isGhostName(target)
        if fi.isGhost && !targetIsGhost && isRealGlobal(target) then
          reject(s"ghost function writes to real global '$target'")
        // Real code assigning to a ghost name is implicitly a "ghost statement" — the
        // strip pass drops it. The RHS evaluates in ghost context (so it may read ghost).
        checkExpr(value, ghostCtx || targetIsGhost)
      case TCompoundAssignStmt(target, _, value) =>
        val targetIsGhost = isGhostName(target)
        if fi.isGhost && !targetIsGhost && isRealGlobal(target) then
          reject(s"ghost function writes to real global '$target'")
        checkExpr(value, ghostCtx || targetIsGhost)
      case TFieldAssignStmt(obj, _, value) =>
        // Catch the simple case: `realGlobal.field = ...` from a ghost function.
        obj match
          case TVarRef(n, _) if fi.isGhost && isRealGlobal(n) =>
            reject(s"ghost function writes to real global '$n'")
          case _ => ()
        checkExpr(obj, ghostCtx); checkExpr(value, ghostCtx)
      case TFieldCompoundAssignStmt(obj, _, _, value) =>
        obj match
          case TVarRef(n, _) if fi.isGhost && isRealGlobal(n) =>
            reject(s"ghost function writes to real global '$n'")
          case _ => ()
        checkExpr(obj, ghostCtx); checkExpr(value, ghostCtx)
      case TIndexAssignStmt(arr, idx, value) =>
        arr match
          case TVarRef(n, _) if fi.isGhost && isRealGlobal(n) =>
            reject(s"ghost function writes to real global '$n'")
          case _ => ()
        checkExpr(arr, ghostCtx); checkExpr(idx, ghostCtx); checkExpr(value, ghostCtx)
      case TDerefAssignStmt(p, v) =>
        checkExpr(p, ghostCtx); checkExpr(v, ghostCtx)
      case TReturnStmt(v) =>
        v.foreach(checkExpr(_, ghostCtx))
      case TWhileStmt(c, b, _) =>
        checkExpr(c, ghostCtx); b.foreach(checkStmt(_, ghostCtx))
      case TForStmt(init, c, u, b, _) =>
        checkStmt(init, ghostCtx); checkExpr(c, ghostCtx); checkStmt(u, ghostCtx); b.foreach(checkStmt(_, ghostCtx))
      case TDoWhileStmt(c, b, _) =>
        checkExpr(c, ghostCtx); b.foreach(checkStmt(_, ghostCtx))
      case TLoopStmt(b, _) =>
        b.foreach(checkStmt(_, ghostCtx))
      case TBreakStmt(_) | TContinueStmt(_) | TAsmStmt(_) => ()
      case TDeferStmt(inner) =>
        checkStmt(inner, ghostCtx)
      case TContractCheck(_, expr, _) =>
        // Contract clauses are intrinsically in ghost context — they may freely read both
        // real and ghost state. (Writes are already disallowed by the contract grammar.)
        checkExpr(expr, true)
      case TMultiStmt(ss) =>
        ss.foreach(checkStmt(_, ghostCtx))
      case TExprStmt(e) =>
        checkExpr(e, ghostCtx)

    val funIsGhost = fi.isGhost
    body match
      case TExprBody(e) => checkExpr(e, funIsGhost)
      case TBlockBody(stmts) => stmts.foreach(checkStmt(_, funIsGhost))

  protected def validateTestAttr(fd: FunDeclAST, info: FunInfo): Unit =
    fd.attributes.find(_.name == "test") match
      case None => ()
      case Some(attr) =>
        if fd.params.nonEmpty then
          throw AnalysisError(s"#test function '${fd.name}' must take zero parameters", fd)
        if info.returnType != UnitType then
          throw AnalysisError(s"#test function '${fd.name}' must return unit", fd)
        if fd.typeParams.nonEmpty then
          throw AnalysisError(s"#test function '${fd.name}' cannot be generic", fd)
        // Methods are registered via the StructName_methodName convention; reject those
        val underscoreIdx = fd.name.indexOf('_')
        if underscoreIdx > 0 && fd.params.nonEmpty && fd.params.head.name == "__self__" then
          throw AnalysisError(s"#test cannot be applied to a method ('${fd.name}')", fd)
