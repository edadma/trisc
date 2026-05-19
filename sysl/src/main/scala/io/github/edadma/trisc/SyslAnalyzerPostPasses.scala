package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

/** Extracted from SyslAnalyzer.scala for navigability: the post-analysis
 *  rewriters and walkers — ghost-stripping, ensure-return rewrites, generic
 *  AST walkers (`mapTExpr` / `mapTStmt`), the closure-iface-capture ownership
 *  fixup, and `lowerFunctionVariant`. Self-typed onto SyslAnalyzer so it can
 *  read shared state (`ghostNames`, `scopeStack`, `currentScope`,
 *  `variantCallCounter`, `contractsEnabled`) and call back to peers
 *  (`analyzeExpr`, `contract`, `compatible`). */
trait SyslAnalyzerPostPasses:
  self: SyslAnalyzer =>

  /** Drop ghost TFunDecl / TVarDecl from the output and rewrite every remaining function's
   *  body so no ghost state survives into codegen. Ghost functions never run (their bodies
   *  are discarded wholesale). Real functions may contain ghost locals, ghost-target
   *  assignments, and ghost-touching contracts — all stripped here. */
  protected def stripGhostDecls(decls: List[TDecl]): List[TDecl] =
    // Mangled names of ghost functions, collected from the unstripped decls. Used by
    // stripGhostStmts to recognize calls-to-ghost inside contract expressions (which get
    // the whole contract dropped).
    val ghostFnMangled: Set[String] =
      decls.collect { case f: TFunDecl if f.isGhost => f.name }.toSet
    decls.flatMap {
      case f: TFunDecl if f.isGhost => Nil
      case v: TVarDecl if v.isGhost => Nil
      case f: TFunDecl =>
        val newBody = f.body match
          case TExprBody(e) => TExprBody(e)
          case TBlockBody(stmts) => TBlockBody(stripGhostStmts(stmts, ghostFnMangled))
        List(f.copy(body = newBody))
      case other => List(other)
    }

  /** Walk a statement list, dropping ghost-only statements and contracts. `ghostLocals`
   *  accumulates names of ghost locals as we hit their `TVarStmt(..., isGhost=true)` so
   *  later writes to them are recognized and dropped. */
  protected def stripGhostStmts(stmts: List[TStmt], ghostFns: Set[String]): List[TStmt] =
    val ghostLocals = mutable.HashSet[String]()
    def isGhostVar(name: String): Boolean = ghostNames.contains(name) || ghostLocals.contains(name)

    def exprTouchesGhost(e: TExpr): Boolean =
      var touches = false
      def walk(x: TExpr): TExpr =
        x match
          case TVarRef(n, _) if isGhostVar(n)  => touches = true
          case TAddrOf(n, _) if isGhostVar(n)  => touches = true
          case TCall(c, _, _) if ghostFns.contains(c) => touches = true
          case _ => ()
        x
      mapTExpr(e)(walk)
      touches

    // Recurse through TIfExpr / TMatchExpr inside TExprStmt so contract
    // checks that `rewriteReturnsForEnsure` cloned into per-branch return
    // tails get the ghost strip applied too. Without this, ghost-touching
    // ensure clauses survive on every non-fall-through path of a
    // multi-return-point function body.
    def stripExpr(e: TExpr): TExpr = e match
      case TIfExpr(c, tb, eb, t) =>
        TIfExpr(c, tb.flatMap(stripStmt), eb.map(_.flatMap(stripStmt)), t)
      case TMatchExpr(scrut, arms, dflt, t) =>
        val newArms = arms.map(a => TMatchArm(a.patterns, a.guard, a.body.flatMap(stripStmt)))
        TMatchExpr(scrut, newArms, dflt.map(_.flatMap(stripStmt)), t)
      case other => other

    def stripStmt(s: TStmt): Option[TStmt] = s match
      case TVarStmt(n, _, _, _, true) =>
        ghostLocals += n
        None
      case TAssignStmt(t, _) if isGhostVar(t) => None
      case TCompoundAssignStmt(t, _, _) if isGhostVar(t) => None
      case TContractCheck(_, e, _) if exprTouchesGhost(e) => None
      case TWhileStmt(c, b, lbl)           => Some(TWhileStmt(c, b.flatMap(stripStmt), lbl))
      case TForStmt(init, c, u, b, lbl)    =>
        val newInit = stripStmt(init).getOrElse(TMultiStmt(Nil))
        val newUpd  = stripStmt(u).getOrElse(TMultiStmt(Nil))
        Some(TForStmt(newInit, c, newUpd, b.flatMap(stripStmt), lbl))
      case TDoWhileStmt(c, b, lbl)         => Some(TDoWhileStmt(c, b.flatMap(stripStmt), lbl))
      case TLoopStmt(b, lbl)               => Some(TLoopStmt(b.flatMap(stripStmt), lbl))
      case TDeferStmt(inner)               => stripStmt(inner).map(TDeferStmt(_))
      case TMultiStmt(ss)                  => Some(TMultiStmt(ss.flatMap(stripStmt)))
      case TExprStmt(e)                    => Some(TExprStmt(stripExpr(e)))
      case other                           => Some(other)

    stmts.flatMap(stripStmt)

  /** Zero-value expression for a return type — used to initialize the synthetic
   * `__result__` local at function entry before the body runs. */
  protected def zeroExprFor(t: SyslType): TExpr = t.underlying match
    case _: FloatType         => TFloatLit(0.0, t)
    case BoolType             => TBoolLit(false, t)
    case st: SyslType.StructType => TStructLit(st)
    case _                    => TIntLit(0, t)

  /** Recursively rewrite every `return v` inside a stmt list so that `v` is stored into
   * __result__, then the ensure checks fire, then `return __result__` runs. For void
   * functions, the assignment step is skipped. */
  protected def rewriteReturnsForEnsure(stmts: List[TStmt], returnType: SyslType, ensureChecks: List[TStmt]): List[TStmt] =
    stmts.map(s => rewriteStmtForEnsure(s, returnType, ensureChecks))

  protected def rewriteStmtForEnsure(stmt: TStmt, returnType: SyslType, ensureChecks: List[TStmt]): TStmt = stmt match
    case TReturnStmt(Some(v)) if returnType != UnitType =>
      TMultiStmt(List(TAssignStmt("__result__", v)) ++ ensureChecks ++
        List(TReturnStmt(Some(TVarRef("__result__", returnType)))))
    case TReturnStmt(None) =>
      TMultiStmt(ensureChecks ++ List(TReturnStmt(None)))
    case TReturnStmt(_) => stmt // void return with value — already rejected upstream
    case TWhileStmt(c, body, lbl)          => TWhileStmt(c, rewriteReturnsForEnsure(body, returnType, ensureChecks), lbl)
    case TForStmt(init, c, upd, body, lbl) => TForStmt(init, c, upd, rewriteReturnsForEnsure(body, returnType, ensureChecks), lbl)
    case TDoWhileStmt(c, body, lbl)        => TDoWhileStmt(c, rewriteReturnsForEnsure(body, returnType, ensureChecks), lbl)
    case TLoopStmt(body, lbl)              => TLoopStmt(rewriteReturnsForEnsure(body, returnType, ensureChecks), lbl)
    case TDeferStmt(inner)            => TDeferStmt(rewriteStmtForEnsure(inner, returnType, ensureChecks))
    case TMultiStmt(xs)               => TMultiStmt(xs.map(x => rewriteStmtForEnsure(x, returnType, ensureChecks)))
    case TExprStmt(e)                 => TExprStmt(rewriteExprForEnsure(e, returnType, ensureChecks))
    case other => other

  protected def rewriteExprForEnsure(expr: TExpr, returnType: SyslType, ensureChecks: List[TStmt]): TExpr = expr match
    case TIfExpr(c, tb, eb, t) =>
      TIfExpr(c, rewriteReturnsForEnsure(tb, returnType, ensureChecks),
              eb.map(stmts => rewriteReturnsForEnsure(stmts, returnType, ensureChecks)), t)
    case TMatchExpr(e, arms, default, t) =>
      val newArms = arms.map(a => TMatchArm(a.patterns, a.guard, rewriteReturnsForEnsure(a.body, returnType, ensureChecks)))
      TMatchExpr(e, newArms, default.map(stmts => rewriteReturnsForEnsure(stmts, returnType, ensureChecks)), t)
    case other => other

  /** If the rewritten body lacks a trailing explicit return, append one so ensure runs
   * at the implicit fall-through point. The last TExprStmt (if any) becomes the return value. */
  protected def finalizeFallThroughReturn(stmts: List[TStmt], returnType: SyslType, ensureChecks: List[TStmt]): List[TStmt] =
    if returnType == UnitType then
      // Append bare ensure + return at the end unless the last stmt is already a return
      if stmts.lastOption.exists(isTerminalReturn) then stmts
      else stmts ++ ensureChecks :+ TReturnStmt(None)
    else
      stmts.lastOption match
        case Some(s) if isTerminalReturn(s) => stmts
        case Some(TExprStmt(e)) if compatible(e.typ, returnType) =>
          stmts.init :+ TMultiStmt(List(TAssignStmt("__result__", e)) ++ ensureChecks ++
            List(TReturnStmt(Some(TVarRef("__result__", returnType)))))
        case _ =>
          // No trailing expression producing the return value; treat as void-ish or
          // let downstream catch the type mismatch. Fall back: return __result__ with zero.
          stmts ++ ensureChecks :+ TReturnStmt(Some(TVarRef("__result__", returnType)))

  protected def isTerminalReturn(s: TStmt): Boolean = s match
    case _: TReturnStmt => true
    case TMultiStmt(xs) => xs.lastOption.exists(isTerminalReturn)
    case _ => false

  /** Apply `f` to every TExpr in the typed AST tree rooted at `e`, bottom-up. Used by the
   *  function-variant lowering for two purposes: substitute parameter references in the
   *  variant expression, and locate every recursive TCall to wrap with a check. The walker
   *  is exhaustive on TExpr cases — adding a new TExpr node requires extending it. */
  protected def mapTExpr(e: TExpr)(f: TExpr => TExpr): TExpr =
    def go(x: TExpr): TExpr = f(x match
      case _: TIntLit | _: TFloatLit | _: TBoolLit | _: TStringLit => x
      case _: TArrayDecl | _: TVarRef | _: TAddrOf | _: TAddrLit | _: TFuncRef | _: TSizeof => x
      case _: TStructLit | _: TEnumConstruct => x
      case TArrayLit(els, t)               => TArrayLit(els.map(go), t)
      case TAddrOfIndex(a, i, t)           => TAddrOfIndex(go(a), go(i), t)
      case TAddrOfField(o, idx, t)         => TAddrOfField(go(o), idx, t)
      case TTempAddr(inner, t)             => TTempAddr(go(inner), t)
      case TDeref(inner, t)                => TDeref(go(inner), t)
      case TIndex(arr, i, t)               => TIndex(go(arr), go(i), t)
      case TFieldAccess(o, idx, t)         => TFieldAccess(go(o), idx, t)
      case TFieldPreInc(o, idx, t)         => TFieldPreInc(go(o), idx, t)
      case TFieldPreDec(o, idx, t)         => TFieldPreDec(go(o), idx, t)
      case TFieldPostInc(o, idx, t)        => TFieldPostInc(go(o), idx, t)
      case TFieldPostDec(o, idx, t)        => TFieldPostDec(go(o), idx, t)
      case TStructConstruct(st, args)      => TStructConstruct(st, args.map(go))
      case TPreInc(_, _) | TPreDec(_, _) | TPostInc(_, _) | TPostDec(_, _) => x
      case TUnary(op, o, t)                => TUnary(op, go(o), t)
      case TBinary(l, op, r, t)            => TBinary(go(l), op, go(r), t)
      case TCall(name, args, t)            => TCall(name, args.map(go), t)
      case TIndirectCall(callee, args, t)  => TIndirectCall(go(callee), args.map(go), t)
      case TCast(inner, t)                 => TCast(go(inner), t)
      case TIfExpr(c, tb, eb, t)           => TIfExpr(go(c), tb.map(s => mapTStmt(s)(f)), eb.map(_.map(s => mapTStmt(s)(f))), t)
      case TQuantifier(k, n, nt, lo, hi, inc, p, t) => TQuantifier(k, n, nt, go(lo), go(hi), inc, go(p), t)
      case TMatchExpr(scr, arms, dflt, t)  =>
        val newArms = arms.map(a => TMatchArm(a.patterns, a.guard.map(go), a.body.map(s => mapTStmt(s)(f))))
        TMatchExpr(go(scr), newArms, dflt.map(_.map(s => mapTStmt(s)(f))), t)
      case TNew(st, args)                  => TNew(st, args.map(go))
      case TNewEnum(et, idx, args)         => TNewEnum(et, idx, args.map(go))
      case TNewArray(et, sz)               => TNewArray(et, go(sz))
      case TLen(inner, t)                  => TLen(go(inner), t)
      case TCap(inner, t)                  => TCap(go(inner), t)
      case TSliceExpr(a, lo, hi, t)        => TSliceExpr(go(a), lo.map(go), hi.map(go), t)
      case TAppend(s, el, t)               => TAppend(go(s), go(el), t)
      case TStringFromPtr(p, l, t)         => TStringFromPtr(go(p), go(l), t)
      case TStringFromSlice(s, t)          => TStringFromSlice(go(s), t)
      case TStr(inner)                     => TStr(go(inner))
      case TFmtStr(inner, spec)            => TFmtStr(go(inner), spec)
      case _: TClosure                     => x // closures captured environments — don't descend
      case TInterfaceBox(inner, iface, owns) => TInterfaceBox(go(inner), iface, owns)
      case TInterfaceDispatch(v, m, args, rt) => TInterfaceDispatch(go(v), m, args.map(go), rt)
      case TIntrinsicCall(n, args, t)      => TIntrinsicCall(n, args.map(go), t)
      case TRangeCheck(inner, r, an, t)    => TRangeCheck(go(inner), r, an, t)
      case _: TAsmExpr                     => x
    )
    go(e)

  /** Apply `f` to every TExpr inside a statement tree, bottom-up via mapTExpr. */
  protected def mapTStmt(s: TStmt)(f: TExpr => TExpr): TStmt =
    def goS(stmt: TStmt): TStmt = stmt match
      case TVarStmt(n, t, init, vol, g)         => TVarStmt(n, t, mapTExpr(init)(f), vol, g)
      case TDestructureStmt(ns, ts, init)       => TDestructureStmt(ns, ts, mapTExpr(init)(f))
      case TDestructureAssignStmt(ns, ts, init) => TDestructureAssignStmt(ns, ts, mapTExpr(init)(f))
      case TAssignStmt(t, v)                    => TAssignStmt(t, mapTExpr(v)(f))
      case TCompoundAssignStmt(t, op, v)        => TCompoundAssignStmt(t, op, mapTExpr(v)(f))
      case TDerefAssignStmt(p, v)               => TDerefAssignStmt(mapTExpr(p)(f), mapTExpr(v)(f))
      case TIndexAssignStmt(a, i, v)            => TIndexAssignStmt(mapTExpr(a)(f), mapTExpr(i)(f), mapTExpr(v)(f))
      case TFieldAssignStmt(o, idx, v)          => TFieldAssignStmt(mapTExpr(o)(f), idx, mapTExpr(v)(f))
      case TFieldCompoundAssignStmt(o, idx, op, v) => TFieldCompoundAssignStmt(mapTExpr(o)(f), idx, op, mapTExpr(v)(f))
      case TReturnStmt(v)                       => TReturnStmt(v.map(e => mapTExpr(e)(f)))
      case TWhileStmt(c, b, lbl)                => TWhileStmt(mapTExpr(c)(f), b.map(goS), lbl)
      case TForStmt(init, c, upd, b, lbl)       => TForStmt(goS(init), mapTExpr(c)(f), goS(upd), b.map(goS), lbl)
      case TDoWhileStmt(c, b, lbl)              => TDoWhileStmt(mapTExpr(c)(f), b.map(goS), lbl)
      case TLoopStmt(b, lbl)                    => TLoopStmt(b.map(goS), lbl)
      case TBreakStmt(_) | TContinueStmt(_) | TAsmStmt(_) => stmt
      case TDeferStmt(inner)                    => TDeferStmt(goS(inner))
      case TContractCheck(k, e, m)              => TContractCheck(k, mapTExpr(e)(f), m)
      case TMultiStmt(xs)                       => TMultiStmt(xs.map(goS))
      case TExprStmt(e)                         => TExprStmt(mapTExpr(e)(f))
    goS(s)

  /** Post-pass that fixes the closure-iface UAF: when an iface-typed local is captured by
   *  an escaping closure, rewrite its `TVarStmt`'s initializer `TInterfaceBox(_,_,owns=false)`
   *  to `owns=true` so codegen heap-copies the source struct instead of pointing into the
   *  caller's stack frame (which is dead by the time the escaped closure runs).
   *
   *  Why a post-pass: at var-decl time we don't yet know whether the var will be captured
   *  later. The conservative `owns=false` default is correct for in-scope use (including
   *  mutating-self through the iface), but breaks once the iface descriptor is copied into
   *  a heap closure env and outlives the source.
   *
   *  Scope: covers DIRECT capture by an escaping closure (and indirect-via-nested-closure
   *  because scanCaptures recursively bubbles inner-body refs up into the outer closure's
   *  capture list when the inner var doesn't shadow them). Does NOT cover the case where
   *  an iface is captured by a non-escaping closure that is itself captured-by-reference via
   *  another var (a FuncType local captured by an outer escaping closure); that pattern
   *  would need additional bookkeeping and is not currently exercised by any test. */
  protected def rewriteEscapingClosureCaptureOwns(tBody: TFunBody): TFunBody =
    val capturedIfaceNames = scala.collection.mutable.Set.empty[String]
    // Collect iface var names that escape via a TReturnStmt's value — direct
    // (`return h`) OR indirect through a constructor (`return Some(h)`,
    // `return Wrapper(h)`, `return [h]`, etc.). The val-decl auto-box at
    // SyslAnalyzerStatements.scala defaults to owns=false (mutating-self
    // through the iface var requires sharing storage with the source), but
    // a var that escapes via return must own its data buffer or the
    // descriptor's data_ptr dangles when the source struct's stack frame
    // is freed. See feedback_sysl_iface_in_generic_enum_return_uaf.md.
    def collectReturnEscapes(e: TExpr): Unit = e match
      case TVarRef(n, _: SyslType.InterfaceType) => capturedIfaceNames += n
      case TInterfaceBox(inner, _, _) => collectReturnEscapes(inner)
      case TEnumConstruct(_, _, args) => args.foreach(collectReturnEscapes)
      case TNewEnum(_, _, args) => args.foreach(collectReturnEscapes)
      case TStructConstruct(_, args) => args.foreach(collectReturnEscapes)
      case TNew(_, args) => args.foreach(collectReturnEscapes)
      case TArrayLit(els, _) => els.foreach(collectReturnEscapes)
      case TIfExpr(_, tb, eb, _) =>
        tb.foreach(walkReturnStmt); eb.foreach(_.foreach(walkReturnStmt))
      case TMatchExpr(_, arms, dflt, _) =>
        arms.foreach(a => a.body.foreach(walkReturnStmt))
        dflt.foreach(_.foreach(walkReturnStmt))
      case TCast(inner, _) => collectReturnEscapes(inner)
      case _ => ()
    def walkReturnStmt(s: TStmt): Unit = s match
      case TExprStmt(e) => collectReturnEscapes(e)
      case TReturnStmt(Some(e)) => collectReturnEscapes(e)
      case _ => ()
    def walkExpr(e: TExpr): Unit = e match
      case TClosure(_, _, body, captures, _, _, _) =>
        // Conservative: rewrite captured iface ownership for ANY closure regardless
        // of the analyzer's `escapes` flag. The flag is unreliable for return-position
        // closures (FuncType.escaping defaults to false even though the closure literally
        // escapes via return). The over-approximation is safe because mutating-self via
        // iface uses a method receiver, not a closure-captured iface var.
        for (n, t) <- captures do
          t match
            case _: SyslType.InterfaceType => capturedIfaceNames += n
            case _ => ()
        body match
          case TExprBody(inner) => walkExpr(inner)
          case TBlockBody(ss)   => ss.foreach(walkStmt)
      case TBinary(l, _, r, _) => walkExpr(l); walkExpr(r)
      case TUnary(_, o, _)     => walkExpr(o)
      case TCall(_, args, _)   => args.foreach(walkExpr)
      case TIndirectCall(c, args, _) => walkExpr(c); args.foreach(walkExpr)
      case TIndex(a, i, _)     => walkExpr(a); walkExpr(i)
      case TFieldAccess(o, _, _) => walkExpr(o)
      case TDeref(e1, _)       => walkExpr(e1)
      case TCast(e1, _)        => walkExpr(e1)
      case TAddrOfIndex(a, i, _) => walkExpr(a); walkExpr(i)
      case TAddrOfField(o, _, _) => walkExpr(o)
      case TFieldPreInc(o, _, _) => walkExpr(o)
      case TFieldPreDec(o, _, _) => walkExpr(o)
      case TFieldPostInc(o, _, _) => walkExpr(o)
      case TFieldPostDec(o, _, _) => walkExpr(o)
      case TTempAddr(inner, _) => walkExpr(inner)
      case TIfExpr(c, tb, eb, _) =>
        walkExpr(c); tb.foreach(walkStmt); eb.foreach(_.foreach(walkStmt))
      case TQuantifier(_, _, _, lo, hi, _, p, _) => walkExpr(lo); walkExpr(hi); walkExpr(p)
      case TMatchExpr(scr, arms, dflt, _) =>
        walkExpr(scr)
        for a <- arms do
          a.guard.foreach(walkExpr)
          a.body.foreach(walkStmt)
        dflt.foreach(_.foreach(walkStmt))
      case TEnumConstruct(_, _, args) => args.foreach(walkExpr)
      case TStructConstruct(_, args)  => args.foreach(walkExpr)
      case TArrayLit(els, _)   => els.foreach(walkExpr)
      case TNew(_, args)       => args.foreach(walkExpr)
      case TNewEnum(_, _, args) => args.foreach(walkExpr)
      case TNewArray(_, sz)    => walkExpr(sz)
      case TLen(e1, _)         => walkExpr(e1)
      case TCap(e1, _)         => walkExpr(e1)
      case TSliceExpr(a, lo, hi, _) =>
        walkExpr(a); lo.foreach(walkExpr); hi.foreach(walkExpr)
      case TAppend(s, el, _)   => walkExpr(s); walkExpr(el)
      case TStringFromPtr(p, l, _) => walkExpr(p); walkExpr(l)
      case TStringFromSlice(s, _) => walkExpr(s)
      case TStr(e1)            => walkExpr(e1)
      case TFmtStr(e1, _)      => walkExpr(e1)
      case TInterfaceBox(inner, _, _) => walkExpr(inner)
      case TInterfaceDispatch(v, _, args, _) => walkExpr(v); args.foreach(walkExpr)
      case TIntrinsicCall(_, args, _) => args.foreach(walkExpr)
      case TRangeCheck(inner, _, _, _) => walkExpr(inner)
      case _ => ()
    def walkStmt(s: TStmt): Unit = s match
      case TVarStmt(_, _, init, _, _) => walkExpr(init)
      case TDestructureStmt(_, _, init) => walkExpr(init)
      case TDestructureAssignStmt(_, _, init) => walkExpr(init)
      case TAssignStmt(_, v) => walkExpr(v)
      case TCompoundAssignStmt(_, _, v) => walkExpr(v)
      case TDerefAssignStmt(p, v) => walkExpr(p); walkExpr(v)
      case TIndexAssignStmt(a, i, v) => walkExpr(a); walkExpr(i); walkExpr(v)
      case TFieldAssignStmt(o, _, v) => walkExpr(o); walkExpr(v)
      case TFieldCompoundAssignStmt(o, _, _, v) => walkExpr(o); walkExpr(v)
      case TReturnStmt(v) => v.foreach(walkExpr); v.foreach(collectReturnEscapes)
      case TWhileStmt(c, b, _) => walkExpr(c); b.foreach(walkStmt)
      case TForStmt(init, c, upd, b, _) =>
        walkStmt(init); walkExpr(c); walkStmt(upd); b.foreach(walkStmt)
      case TDoWhileStmt(c, b, _) => walkExpr(c); b.foreach(walkStmt)
      case TLoopStmt(b, _) => b.foreach(walkStmt)
      case TDeferStmt(inner) => walkStmt(inner)
      case TContractCheck(_, e, _) => walkExpr(e)
      case TMultiStmt(xs) => xs.foreach(walkStmt)
      case TExprStmt(e) => walkExpr(e)
      case _ => ()
    tBody match
      case TExprBody(e) =>
        // TExprBody is the function's implicit return value — treat it as
        // an escape site for iface var refs.
        walkExpr(e); collectReturnEscapes(e)
      case TBlockBody(ss) => ss.foreach(walkStmt)
    if capturedIfaceNames.isEmpty then return tBody

    def rwExpr(e: TExpr): TExpr = e match
      case TClosure(p, r, body, caps, esc, eff, sn) =>
        val nb = body match
          case TExprBody(inner) => TExprBody(rwExpr(inner))
          case TBlockBody(ss)   => TBlockBody(ss.map(rwStmt))
        TClosure(p, r, nb, caps, esc, eff, sn)
      case TBinary(l, op, r, t) => TBinary(rwExpr(l), op, rwExpr(r), t)
      case TUnary(op, o, t) => TUnary(op, rwExpr(o), t)
      case TCall(n, args, t) => TCall(n, args.map(rwExpr), t)
      case TIndirectCall(c, args, t) => TIndirectCall(rwExpr(c), args.map(rwExpr), t)
      case TIndex(a, i, t) => TIndex(rwExpr(a), rwExpr(i), t)
      case TFieldAccess(o, idx, t) => TFieldAccess(rwExpr(o), idx, t)
      case TDeref(e1, t) => TDeref(rwExpr(e1), t)
      case TCast(e1, t) => TCast(rwExpr(e1), t)
      case TAddrOfIndex(a, i, t) => TAddrOfIndex(rwExpr(a), rwExpr(i), t)
      case TAddrOfField(o, idx, t) => TAddrOfField(rwExpr(o), idx, t)
      case TFieldPreInc(o, idx, t) => TFieldPreInc(rwExpr(o), idx, t)
      case TFieldPreDec(o, idx, t) => TFieldPreDec(rwExpr(o), idx, t)
      case TFieldPostInc(o, idx, t) => TFieldPostInc(rwExpr(o), idx, t)
      case TFieldPostDec(o, idx, t) => TFieldPostDec(rwExpr(o), idx, t)
      case TTempAddr(inner, t) => TTempAddr(rwExpr(inner), t)
      case TIfExpr(c, tb, eb, t) => TIfExpr(rwExpr(c), tb.map(rwStmt), eb.map(_.map(rwStmt)), t)
      case TQuantifier(k, n, nt, lo, hi, inc, p, t) =>
        TQuantifier(k, n, nt, rwExpr(lo), rwExpr(hi), inc, rwExpr(p), t)
      case TMatchExpr(scr, arms, dflt, t) =>
        val na = arms.map(a => TMatchArm(a.patterns, a.guard.map(rwExpr), a.body.map(rwStmt)))
        TMatchExpr(rwExpr(scr), na, dflt.map(_.map(rwStmt)), t)
      case TEnumConstruct(et, idx, args) => TEnumConstruct(et, idx, args.map(rwExpr))
      case TStructConstruct(st, args) => TStructConstruct(st, args.map(rwExpr))
      case TArrayLit(els, t) => TArrayLit(els.map(rwExpr), t)
      case TNew(st, args) => TNew(st, args.map(rwExpr))
      case TNewEnum(et, idx, args) => TNewEnum(et, idx, args.map(rwExpr))
      case TNewArray(et, sz) => TNewArray(et, rwExpr(sz))
      case TLen(e1, t) => TLen(rwExpr(e1), t)
      case TCap(e1, t) => TCap(rwExpr(e1), t)
      case TSliceExpr(a, lo, hi, t) => TSliceExpr(rwExpr(a), lo.map(rwExpr), hi.map(rwExpr), t)
      case TAppend(s, el, t) => TAppend(rwExpr(s), rwExpr(el), t)
      case TStringFromPtr(p, l, t) => TStringFromPtr(rwExpr(p), rwExpr(l), t)
      case TStringFromSlice(s, t) => TStringFromSlice(rwExpr(s), t)
      case TStr(e1) => TStr(rwExpr(e1))
      case TFmtStr(e1, sp) => TFmtStr(rwExpr(e1), sp)
      case TInterfaceBox(inner, iface, owns) => TInterfaceBox(rwExpr(inner), iface, owns)
      case TInterfaceDispatch(v, m, args, rt) => TInterfaceDispatch(rwExpr(v), m, args.map(rwExpr), rt)
      case TIntrinsicCall(n, args, t) => TIntrinsicCall(n, args.map(rwExpr), t)
      case TRangeCheck(inner, r, an, t) => TRangeCheck(rwExpr(inner), r, an, t)
      case other => other
    def rwStmt(s: TStmt): TStmt = s match
      case TVarStmt(name, t, TInterfaceBox(inner, iface, false), vol, g) if capturedIfaceNames.contains(name) =>
        TVarStmt(name, t, TInterfaceBox(rwExpr(inner), iface, true), vol, g)
      case TVarStmt(n, t, init, vol, g) => TVarStmt(n, t, rwExpr(init), vol, g)
      case TDestructureStmt(ns, ts, init) => TDestructureStmt(ns, ts, rwExpr(init))
      case TDestructureAssignStmt(ns, ts, init) => TDestructureAssignStmt(ns, ts, rwExpr(init))
      case TAssignStmt(tg, v) => TAssignStmt(tg, rwExpr(v))
      case TCompoundAssignStmt(tg, op, v) => TCompoundAssignStmt(tg, op, rwExpr(v))
      case TDerefAssignStmt(p, v) => TDerefAssignStmt(rwExpr(p), rwExpr(v))
      case TIndexAssignStmt(a, i, v) => TIndexAssignStmt(rwExpr(a), rwExpr(i), rwExpr(v))
      case TFieldAssignStmt(o, idx, v) => TFieldAssignStmt(rwExpr(o), idx, rwExpr(v))
      case TFieldCompoundAssignStmt(o, idx, op, v) => TFieldCompoundAssignStmt(rwExpr(o), idx, op, rwExpr(v))
      case TReturnStmt(v) => TReturnStmt(v.map(rwExpr))
      case TWhileStmt(c, b, lbl) => TWhileStmt(rwExpr(c), b.map(rwStmt), lbl)
      case TForStmt(init, c, upd, b, lbl) => TForStmt(rwStmt(init), rwExpr(c), rwStmt(upd), b.map(rwStmt), lbl)
      case TDoWhileStmt(c, b, lbl) => TDoWhileStmt(rwExpr(c), b.map(rwStmt), lbl)
      case TLoopStmt(b, lbl) => TLoopStmt(b.map(rwStmt), lbl)
      case TDeferStmt(inner) => TDeferStmt(rwStmt(inner))
      case TContractCheck(k, e, m) => TContractCheck(k, rwExpr(e), m)
      case TMultiStmt(xs) => TMultiStmt(xs.map(rwStmt))
      case TExprStmt(e) => TExprStmt(rwExpr(e))
      case other => other
    tBody match
      case TExprBody(e) => TExprBody(rwExpr(e))
      case TBlockBody(ss) => TBlockBody(ss.map(rwStmt))

  /** Lower a function's `variant <expr>` clause: snapshot the variant at entry, then wrap
   *  every direct recursive call (TCall to `selfMangledName`) with a runtime check that
   *  the variant evaluated at the call args is strictly less than the entry snapshot AND
   *  ≥ 0. Returns `(prefixDecls, transformedBody)`. Skipped under `--no-contracts`.
   *
   *  Each recursive call is rewritten to a TIfExpr-with-statements:
   *  ```
   *  if true then
   *      var __vc_arg_N_0 = arg0; var __vc_arg_N_1 = arg1; ...
   *      var __vc_at_N : i64 = (variant with params -> __vc_arg_N_*) as i64
   *      assert(__vc_at_N < __variant_entry__ && __vc_at_N >= 0, "...")
   *      f(__vc_arg_N_0, __vc_arg_N_1, ...)
   *  ```
   *  Because TIfExpr's body's last expression is its value, the wrapper preserves the
   *  call's return value. Args are bound to temps so they're evaluated exactly once and
   *  the variant-substituted expression refers to the same evaluation. */
  protected def lowerFunctionVariant(
      selfMangledName: String,
      paramNames: List[String],
      variantClause: ContractClauseAST,
      tBody: List[TStmt],
  ): (List[TStmt], List[TStmt]) =
    if !contractsEnabled then return (Nil, tBody)
    val tVariantRaw = analyzeExpr(variantClause.expr)
    if !tVariantRaw.typ.isIntegral then
      throw AnalysisError(s"variant expression must have integer type, got ${tVariantRaw.typ}")
    val tVariantI64 = if tVariantRaw.typ == I64 then tVariantRaw else TCast(tVariantRaw, I64)
    val entryName = "__variant_entry__"
    if scopeStack != null then
      currentScope(entryName) = SymInfo(entryName, I64, mutable = false)
    val snapshotDecl = TVarStmt(entryName, I64, tVariantI64)

    val transformer: TExpr => TExpr = {
      case TCall(name, args, retType) if name == selfMangledName =>
        variantCallCounter += 1
        val id = variantCallCounter
        // Bind each call arg to a fresh local so it's evaluated once and the variant
        // expression can reference its value via the temp.
        val argTemps: List[(String, TExpr, SyslType)] = args.zipWithIndex.map { case (a, i) =>
          (s"__vc_arg_${id}_$i", a, a.typ)
        }
        val tempBinds: List[TStmt] = argTemps.map { case (n, a, t) => TVarStmt(n, t, a) }
        val paramToTemp: Map[String, TExpr] = paramNames.zip(argTemps).map {
          case (p, (tmpName, _, t)) => (p, TVarRef(tmpName, t))
        }.toMap
        val substVariant = mapTExpr(tVariantRaw) {
          case TVarRef(n, _) if paramToTemp.contains(n) => paramToTemp(n)
          case other => other
        }
        val substI64 = if substVariant.typ == I64 then substVariant else TCast(substVariant, I64)
        val atCallName = s"__vc_at_$id"
        val atCallDecl: TStmt = TVarStmt(atCallName, I64, substI64)
        val atCallRef = TVarRef(atCallName, I64)
        val entryRef = TVarRef(entryName, I64)
        val checkExpr = TBinary(
          TBinary(atCallRef, "<", entryRef, BoolType),
          "&&",
          TBinary(atCallRef, ">=", TIntLit(0L, I64), BoolType),
          BoolType,
        )
        val checkStmt: TStmt = contract("variant", checkExpr, s"$selfMangledName variant decreased fail")
        val newCall = TCall(name, argTemps.map { case (n, _, t) => TVarRef(n, t) }, retType)
        val body: List[TStmt] = tempBinds ++ List(atCallDecl, checkStmt, TExprStmt(newCall))
        TIfExpr(TBoolLit(true, BoolType), body, None, retType)
      case other => other
    }
    val transformed = tBody.map(s => mapTStmt(s)(transformer))
    (List(snapshotDecl), transformed)
