package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

class SyslAnalyzer(val contractsEnabled: Boolean = true) extends SyslAnalyzerExpressions:
  case class AnalysisError(msg: String, node: Any = null) extends RuntimeException(msg)

  /** Build a contract-check statement, or a no-op when contracts are disabled (via
   *  `--no-contracts` / `config("contracts") = "off"`). Used by every contract emission
   *  site — require/ensure/invariant/type predicate/type attribute — so the strip is
   *  centralized. When disabled, the check becomes `TMultiStmt(Nil)`: every backend
   *  already iterates zero children. The surrounding expression (e.g. `v + 1` in
   *  `__succ`) continues to execute without a guard, giving Ada-style "suppressed
   *  check" semantics: fast path, undefined on bad input. */
  protected def contract(kind: String, expr: TExpr, message: String): TStmt =
    if contractsEnabled then TContractCheck(kind, expr, message)
    else TMultiStmt(Nil)

  /** `autoIndirect = true` marks a body-scope symbol whose actual storage is a hidden
   *  pointer (Ada `out` / `inout` parameter). Every read/write goes through `*ptr`:
   *  VarRef lowers to `TDeref(TVarRef(name, *T), T)`; plain and compound assignments
   *  lower to `TDerefAssignStmt(TVarRef(name, *T), v)`. The caller passes an lvalue
   *  auto-wrapped with `TAddrOf*`. `typ` is still the underlying `T` (what the body
   *  sees); the pointer wrap is invisible to user code. */
  // `isByName`: this symbol is a call-by-name parameter. The visible-to-user
  // type is `typ` (T), but the actual storage is a `() -> T` thunk. References
  // through `VarRefAST` auto-emit a thunk call; args at by-name slots are
  // auto-wrapped at the call site. Set only on parameter symbols.
  protected case class SymInfo(name: String, typ: SyslType, mutable: Boolean, isConst: Boolean = false, autoIndirect: Boolean = false, isGhost: Boolean = false, isByName: Boolean = false)
  /** `modes` is parallel to `params`: one entry per parameter. Empty means "all In"
   *  (default, back-compat). `params` stores the call-side signature: for `Out`/`Inout`
   *  this is `*T` so `checkArgs` and codegen see the hidden-pointer type; the body-scope
   *  view is the inner `T` with `autoIndirect = true`. */
  /** `reads`/`writes` carry the *raw* (local) names from `#reads(...)` / `#writes(...)`
   *  attributes. `None` means the function is unannotated; `Some(Set.empty)` means it
   *  declared an empty set ("no module-level effects"). Resolution to the canonical
   *  mangled name + mutability/scope validation is deferred to `validateEffects`, which
   *  runs after the function body is analyzed (so all relevant globals are in scope). */
  // `byName`: indices of call-by-name params (storage type `() -> T`,
  // user-visible as `T`). Empty list (the default) means no by-name params.
  // Parallel to `params`/`modes` when non-empty.
  // `isParameterless`: declared without `()` (`f -> T = body`); referenced
  // by bare name (auto-called at every VarRefAST). Same auto-call mechanism
  // as `isDef` uses, but does not imply purity.
  protected case class FunInfo(name: String, params: List[(String, SyslType)], returnType: SyslType, isDef: Boolean = false, isPure: Boolean = false, modes: List[ParamMode] = Nil, reads: Option[Set[String]] = None, writes: Option[Set[String]] = None, isGhost: Boolean = false, byName: List[Boolean] = Nil, isParameterless: Boolean = false):
    def modeOf(i: Int): ParamMode = if modes.isEmpty then ParamMode.In else modes(i)
    def isByNameAt(i: Int): Boolean = byName.nonEmpty && i < byName.length && byName(i)
    def autoCallsBare: Boolean = isDef || isParameterless
    def hasEffectAnnotations: Boolean = reads.isDefined || writes.isDefined || isPure

  protected val globalScope = new mutable.LinkedHashMap[String, SymInfo]
  protected val functions = new mutable.LinkedHashMap[String, FunInfo]
  // Default parameter expressions: function name → list of defaults (one per param, None if no default)
  protected val functionDefaults = new mutable.LinkedHashMap[String, List[Option[TExpr]]]
  protected val structTypes = new mutable.LinkedHashMap[String, SyslType.StructType]
  // Struct name → list of invariant expressions declared in the struct body.
  // Checked at every field-assignment / field-compound-assignment on a value of that type.
  protected val structInvariants = new mutable.LinkedHashMap[String, List[ExpressionAST]]
  protected val enumTypes = new mutable.LinkedHashMap[String, Map[String, Long]]  // enum name → (member name → value)
  // Simple enums registered as EnumType so they can appear in type positions.
  // Distinct from dataEnumTypes because simple-enum `Name.Member` access still
  // lowers to TIntLit (integer constant), not TEnumConstruct.
  protected val simpleEnumTypes = new mutable.LinkedHashMap[String, SyslType.EnumType]
  protected val dataEnumTypes = new mutable.LinkedHashMap[String, SyslType.EnumType]  // data enum name → EnumType
  protected val variantToEnum = new mutable.LinkedHashMap[String, (SyslType.EnumType, Int)]  // variant name → (enum type, variant index)
  protected val interfaceTypes = new mutable.LinkedHashMap[String, SyslType.InterfaceType]  // interface name → InterfaceType
  protected val moduleNamespaces = new mutable.LinkedHashMap[String, ModuleMeta]  // short name → module meta (for qualified imports)
  // alias name → (target type AST, isNew flag, optional within-range, optional where-predicate AST)
  protected val typeAliases = new mutable.LinkedHashMap[String, (TypeAST, Boolean, Option[RangeAST], Option[ExpressionAST])]
  // name → (type params, target, isNew). `isNew=true` means each instantiation is a
  // distinct nominal type (`type Parser[A] = new (Input) -> ParseResult[A]`); `false` is
  // the historical transparent expansion. `within`/`where` remain rejected for generic
  // aliases (no scalar ordering / no operations on a bare T without trait bounds).
  protected val genericTypeAliases = new mutable.LinkedHashMap[String, (List[String], TypeAST, Boolean)]
  // Phase B — per-alias type-parameter defaults (`type Box[T = int] = ...`). Keyed
  // by alias name; missing entries mean no defaults. Carried separately from
  // `genericTypeAliases` so existing call sites stay terse.
  protected val genericTypeAliasDefaults = new mutable.LinkedHashMap[String, Map[String, TypeAST]]
  // Phase C follow-up — per-alias trait bounds on type parameters (`type Parser[I: Input, A] = ...`).
  // Keyed by alias name; missing entries mean no bounds. Mirrors `StructDeclAST.typeBounds`
  // and is consulted from `instantiateGenericNominalAlias` / the transparent-alias path.
  protected val genericTypeAliasBounds = new mutable.LinkedHashMap[String, Map[String, List[String]]]
  // Cached SyslType for each instantiation of a `new` generic alias. Keyed by (template
  // name, type args). Mirrors `genericStructInstantiations` so repeated mentions of
  // `Parser[i32]` return the same NamedType instance (object-equality dispatch).
  protected val genericAliasInstantiations = new mutable.LinkedHashMap[(String, List[SyslType]), SyslType]
  // Reverse map: mangled instantiation name → (template name, concrete args). Used by
  // the unifier to bind type vars when matching `Parser[A]` against `NamedType("Parser_i32", ...)`.
  protected val genericAliasToTemplate = new mutable.LinkedHashMap[String, (String, List[SyslType])]
  // Memoized resolved form of a named/derived/constrained alias. Plain transparent aliases
  // do not appear here — they resolve directly to their base.
  protected val resolvedNamedTypes = new mutable.LinkedHashMap[String, SyslType]
  protected val methods = new mutable.LinkedHashMap[String, mutable.Set[String]]  // struct name → set of method names
  protected val deprecations = new mutable.LinkedHashMap[String, Option[String]]  // name → optional reason
  protected val warnedDeprecations = new mutable.HashSet[String]
  protected val externalSymbols = new mutable.LinkedHashSet[String]
  protected var scopeStack: mutable.ArrayBuffer[mutable.LinkedHashMap[String, SymInfo]] = null
  protected val compileTimeConstants = new mutable.LinkedHashMap[String, Long] // val name → folded value (for constant propagation)

  /** Module-level vars tagged with `#address(N)` map to a fixed physical address — used
   *  for MMIO device registers. Reads lower to `*(N as *T)`, writes to `*(N as *T) = v`.
   *  No storage is emitted (the var is just a handle on hardware). Both the local and
   *  mangled keys are recorded so cross-module references resolve. */
  protected val fixedAddressVars = new mutable.LinkedHashMap[String, (Long, SyslType)]
  protected var loopDepth: Int = 0
  // Stack of enclosing loop labels (None for unlabeled loops). Used to validate
  // `break label` / `continue label` refers to an enclosing labeled loop.
  protected val loopLabelStack = new mutable.ArrayBuffer[Option[String]]

  /** Reject label shadowing — a labeled loop cannot be nested inside another loop with
   * the same label, because `break label` would be ambiguous. */
  protected def checkLoopLabelUnique(label: Option[String]): Unit =
    label.foreach { lbl =>
      if loopLabelStack.contains(Some(lbl)) then
        throw AnalysisError(s"duplicate loop label '$lbl': already in use by an enclosing loop")
    }

  // Counter for uniquely naming hoisted `variant` state across nested / sibling loops.
  protected var variantIdCounter: Int = 0

  /** Set of mangled names for module-level `#ghost var` declarations. Used by both the
   *  ghost-discipline post-pass (real code cannot read these) and the strip pass (writes
   *  to these are dropped from real-function bodies before codegen). */
  protected val ghostNames = mutable.HashSet[String]()

  /** Set of type names declared `#ghost` — spec-only types that real code may not
   *  construct or pattern-match. Populated during decl collection from the `#ghost`
   *  attribute on struct/enum/data-enum/type-alias decls. The discipline is enforced
   *  at construction sites in `validateGhostDiscipline` (Phase δ.5). */
  protected val ghostTypes = mutable.HashSet[String]()

  /** Type-check each invariant expression at struct declaration time. Invariants are
   *  analyzed in a scope where each field name binds to a local of the field's type, so
   *  type errors (wrong field name, non-bool result) are caught before any mutation site. */
  protected def validateStructInvariants(structName: String, fields: List[(String, SyslType)], invariants: List[ExpressionAST]): Unit =
    val savedScope = scopeStack
    scopeStack = new mutable.ArrayBuffer
    pushScope()
    for (fname, ftype) <- fields do
      currentScope(fname) = SymInfo(fname, ftype, mutable = false)
    try
      for inv <- invariants do
        val tInv = analyzeExpr(inv)
        if tInv.typ != BoolType then
          throw AnalysisError(s"struct '$structName' invariant must be bool, got ${tInv.typ}")
    finally scopeStack = savedScope

  /** Rewrite `VarRefAST(fieldName)` → `FieldAccessAST(objAst, fieldName)` for any name that
   *  is a field of the struct. Used to bind bare field names in a struct invariant expression
   *  against a concrete struct-valued sub-expression at a mutation site. */
  protected def substituteStructFieldRefs(expr: ExpressionAST, objAst: ExpressionAST, fields: Set[String]): ExpressionAST =
    def rec(e: ExpressionAST): ExpressionAST = e match
      case VarRefAST(name) if fields.contains(name) => FieldAccessAST(objAst, name)
      case BinaryAST(l, op, r)      => BinaryAST(rec(l), op, rec(r))
      case UnaryAST(op, operand)    => UnaryAST(op, rec(operand))
      case CallAST(name, args)      => CallAST(name, args.map(rec))
      case MethodCallAST(o, m, args)=> MethodCallAST(rec(o), m, args.map(rec))
      case CastAST(t, inner)        => CastAST(t, rec(inner))
      case FieldAccessAST(o, f)     => FieldAccessAST(rec(o), f)
      case IndexAST(a, i)           => IndexAST(rec(a), rec(i))
      case DerefAST(inner)          => DerefAST(rec(inner))
      case TupleLitAST(es)          => TupleLitAST(es.map(rec))
      case IfExprAST(c, tb, eb)     => IfExprAST(rec(c), tb, eb)  // stmt bodies not rewritten
      case NamedArgAST(n, v)        => NamedArgAST(n, rec(v))
      case TypeAttrAST(t, a, argO)  => TypeAttrAST(t, a, argO.map(rec))
      case other => other
    rec(expr)

  /** Build a TContractCheck stmt sequence that verifies all invariants of `structType` for
   *  the struct-valued expression `objAst`, using its struct name. Returns empty if no
   *  invariants are declared for this struct. */
  protected def buildStructInvariantChecks(objAst: ExpressionAST, structName: String): List[TStmt] =
    structInvariants.get(structName) match
      case None => Nil
      case Some(invs) =>
        val st = structTypes(structName)
        val fieldNames = st.fields.map(_._1).toSet
        invs.map { inv =>
          val rewritten = substituteStructFieldRefs(inv, objAst, fieldNames)
          val tExpr = analyzeExpr(rewritten)
          if tExpr.typ != BoolType then
            throw AnalysisError(s"struct invariant must be bool, got ${tExpr.typ}")
          contract("invariant", tExpr, s"$structName invariant")
        }

  /** Extract Ada/SPARK-style loop invariants from the leading "header" of a loop body.
   *  An invariant must appear before any non-invariant / non-variant statement; variants are
   *  allowed to interleave with invariants in the header (and are returned in `bodyOut` so
   *  `extractVariants` can still find them). Any `InvariantStmtAST` not extracted here will
   *  fall through to the analyzer's catch-all error case. */
  protected def extractLeadingInvariants(body: List[StmtAST]): (List[(ExpressionAST, Option[String])], List[StmtAST]) =
    val (header, tail) = body.span {
      case _: InvariantStmtAST | _: VariantStmtAST => true
      case _ => false
    }
    val invs = header.collect { case InvariantStmtAST(e, m) => (e, m) }
    val variantsInHeader = header.filter { case _: VariantStmtAST => true; case _ => false }
    (invs, variantsInHeader ++ tail)

  /** Type-check loop invariants and lower them to contract-check statements. Called in body
   *  scope so the invariants see for-init bindings and outer scope. Also drains any
   *  `loop_entry(expr)` snapshots accumulated during analysis — returns them as TVarStmt
   *  decls that the caller must emit before the first iteration. */
  protected def buildLoopInvariantChecks(invs: List[(ExpressionAST, Option[String])]): (List[TStmt], List[TStmt]) =
    val savedLoopMode = inLoopInvariantAnalysis
    val snapshotsBefore = loopEntrySnapshots.length
    inLoopInvariantAnalysis = true
    val checks: List[TStmt] = try invs.map { case (e, msg) =>
      val te = analyzeExpr(e)
      if te.typ != BoolType then throw AnalysisError(s"loop invariant must be bool, got ${te.typ}")
      contract("loop invariant", te, msg.getOrElse("loop invariant"))
    } finally inLoopInvariantAnalysis = savedLoopMode
    val captured = loopEntrySnapshots.drop(snapshotsBefore).toList
    loopEntrySnapshots.remove(snapshotsBefore, captured.length)
    val snapshotDecls: List[TStmt] = captured.map { (name, typ, expr) => TVarStmt(name, typ, expr) }
    (snapshotDecls, checks)

  /** Extract top-level `variant <expr>` statements from a loop body. Returns a pair of
   *  AST stmt lists: (hoisted-pre-decls, rewritten-body). The caller must analyze the
   *  pre-decls in the current scope (outside the loop) and the rewritten body in the
   *  loop's own body scope. */
  protected def extractVariants(body: List[StmtAST]): (List[StmtAST], List[StmtAST]) =
    if !contractsEnabled then
      // --no-contracts: elide loop variants entirely — no hoisted state, no per-iter check.
      return (Nil, body.filter { case _: VariantStmtAST => false; case _ => true })
    val pre = mutable.ListBuffer.empty[StmtAST]
    val newBody = body.flatMap {
      case VariantStmtAST(expr) =>
        variantIdCounter += 1
        val id = variantIdCounter
        val initName = s"__variant_init_$id"
        val prevName = s"__variant_prev_$id"
        val curName  = s"__variant_cur_$id"
        // Hoisted state: init flag + prev value (i64 is wide enough for any integer expr).
        pre += VarStmtAST(initName, Some(NamedTypeAST("bool")), BoolLitAST(false))
        pre += VarStmtAST(prevName, Some(NamedTypeAST("i64")), IntLitAST(0))
        // Inline per-iteration check and state update.
        val castExpr: ExpressionAST = CastAST(NamedTypeAST("i64"), expr)
        val okExpr: ExpressionAST = BinaryAST(
          BinaryAST(VarRefAST(curName), "<", VarRefAST(prevName)),
          "&&",
          BinaryAST(VarRefAST(curName), ">=", IntLitAST(0)),
        )
        val assertCall: ExpressionAST = CallAST("assert", List(okExpr, StringLitAST("loop variant failed")))
        val guardedCheck: ExpressionAST = IfExprAST(VarRefAST(initName), List(ExprStmtAST(assertCall)), None)
        List(
          VarStmtAST(curName, Some(NamedTypeAST("i64")), castExpr),
          ExprStmtAST(guardedCheck),
          AssignStmtAST(prevName, VarRefAST(curName)),
          AssignStmtAST(initName, BoolLitAST(true)),
        )
      case other => List(other)
    }
    (pre.toList, newBody)
  protected var currentReturnType: SyslType = UnitType

  // Module-path name mangling: set from ModuleDeclAST during analyze()
  protected var currentModule: Option[String] = None // e.g. "std_strings"

  protected def mangleName(name: String): String =
    currentModule match
      case Some(mod) => s"${mod}__$name"
      case None => name

  // Names that must never be mangled: entry point + ABI-level allocation symbols
  // Names that must not be mangled: entry points, ABI-level symbols, and OS kernel
  // functions called from boot.asm. Future: replace with #[no_mangle] attribute.
  protected val neverMangle = mutable.HashSet(
    "main", "malloc", "free", "calloc", "realloc", "sbrk",
    // OS kernel ABI (called from boot.asm):
    "kernel_init", "kernel_main", "schedule", "current_thread",
    "syscall_table", "syscall_table_6", "syscall_ssp", "irq_handlers", "ticks",
    "monotonic_ms_count",
    "thread_count", "query_thread_state", "query_thread_name", "query_thread_name_len",
    "sleep_until_current", "query_thread_ctx_switches", "query_thread_cpu_ticks",
    "query_total_ctx_switches", "kernel_set_watchdog", "kernel_panic",
    "check_stack_at", "suspend_thread", "resume_thread",
    "kernel_tls_set", "kernel_tls_get", "notify_send", "notify_wait_current",
    "notify_read", "event_wait_current", "event_set_bits", "event_clear_bits",
    "terminate_current", "query_thread_pid",
  )

  protected def shouldMangle(name: String): Boolean =
    currentModule.isDefined && !neverMangle.contains(name)

  /** Register names that must not be mangled (e.g. extern declarations from sibling files). */
  def registerNoMangle(names: Iterable[String]): Unit =
    neverMangle ++= names

  /** Pre-seed `currentModule` before `analyze()` runs. The driver uses this so
   *  `registerImport` can mangle imported sibling-impl method names with the
   *  same prefix the importing unit's own main pass would produce — necessary
   *  because mangleName/shouldMangle key off `currentModule`, which `analyze()`
   *  doesn't set until after sibling registration has already happened. */
  def preSetModule(modPath: String): Unit =
    currentModule = Some(modPath)

  /** Sibling source ASTs that the driver wants `analyze()` to re-pre-register
   *  after own type registration completes. This breaks cyclic-deps between
   *  same-module files where each sibling references the other's types AND
   *  the other's traits/impls. The first pre-register (driver-side, before
   *  analyze) sets up everything resolvable without own types in scope; this
   *  hook re-registers them once own pass 1 has populated genericTypeAliases /
   *  structTypes / etc., letting previously-skipped concrete impls resolve. */
  var siblingForwardDecls: List[ProgramAST] = Nil

  /** Strip module prefix from a mangled name to get the short name.
    * Uses indexOf (first `__`) not lastIndexOf, because function names
    * can contain `_` (e.g. `_run_atexit` → mangled `mod___run_atexit`).
    */
  protected def shortName(mangledName: String): String =
    mangledName.indexOf("__") match
      case -1 => mangledName
      case i  => mangledName.substring(i + 2)

  // Generic function support
  protected val genericTemplates = new mutable.LinkedHashMap[String, FunDeclAST]
  protected val instantiations = new mutable.LinkedHashMap[(String, List[SyslType]), String]
  protected val specializedDecls = mutable.ListBuffer.empty[TDecl]
  // Per-analyze counter for synthesized top-level fns lifted from cross-referencing
  // inner-def clusters (mutual recursion). Names look like `_inner_<N>_<defName>`.
  protected var innerDefLiftCounter: Int = 0
  protected var typeEnv: Map[String, SyslType] = Map.empty
  /** Active impl's associated-type bindings, set during impl-method
   *  monomorphization (Phase A2). Each entry is `(assoc-name, resolved-type)`
   *  taken from the impl's `assocBindings`. `resolveType` consults this map
   *  when seeing a `ProjectionTypeAST`; an empty map means "no impl context"
   *  and any projection is rejected with a clear diagnostic. The qualifier on
   *  the projection (`Self::Item`, `I::Item`, …) is sugar — resolution looks
   *  up by member name, since each impl has at most one binding per name. */
  protected var assocBindingsEnv: Map[String, SyslType] = Map.empty

  // Track which generic-template, trait, and concrete-impl entries arrived
  // via cross-unit import (registerImport / registerGenericTemplatesFrom)
  // versus being declared in this unit's own AST. The driver consults the
  // `getOwn*` accessors when building this unit's per-file ModuleMeta so the
  // sibling-mirroring path doesn't keep re-stamping imported symbols as if
  // they originated locally — that would cause every sibling to claim
  // ownership of the others' templates, then trigger duplicate registration
  // when those templates round-trip back through Step 5's sibling import.
  protected val importedTemplateNames = mutable.HashSet[String]()
  protected val importedTraitNames = mutable.HashSet[String]()
  // Concrete-impl key uses the full target list so multi-target impls
  // (e.g. `impl Combine[Box, Box, Box]`) are tracked correctly. Single-target
  // impls store a one-element list.
  protected val importedConcreteImplKeys = mutable.HashSet[(String, List[SyslType])]()
  // Mangled function names registered as stubs by the sibling pre-register
  // for concrete impl methods. Tracked so the post-pass-1 re-pre-register
  // can drop and re-add them after own struct/alias placeholders have been
  // replaced with real fields-resolved types.
  protected val importedConcreteImplStubFunctions = mutable.HashSet[String]()
  // Local-key names of non-generic free-function stubs registered by the
  // sibling pre-register. Same cleanup-and-re-register dance as concrete
  // impl stubs so cross-sibling free-fn signatures don't latch onto
  // placeholder struct types from the first pre-register pass.
  protected val importedSiblingFreeFnStubKeys = mutable.HashSet[String]()
  // Generic-impl key uses raw TypeAST patterns since the targets may carry
  // type variables that don't resolve to a SyslType at import time.
  protected val importedGenericImplKeys = mutable.HashSet[(String, List[String], List[TypeAST])]()
  protected val importedExtensionKeys = mutable.HashSet[(String, String)]() // (definingModule, mangledFnName)
  protected val importedEnumInstNames = mutable.HashSet[String]()

  // Generic struct support
  protected val genericStructs = new mutable.LinkedHashMap[String, StructDeclAST]
  protected val genericStructInstantiations = new mutable.LinkedHashMap[(String, List[SyslType]), SyslType.StructType]
  // Reverse map: mangled struct name -> (template name, concrete type args) for unification at call sites
  protected val structToTemplate = new mutable.LinkedHashMap[String, (String, List[SyslType])]

  // Generic enum support
  protected val genericEnums = new mutable.LinkedHashMap[String, DataEnumDeclAST]
  protected val genericEnumInstantiations = new mutable.LinkedHashMap[(String, List[SyslType]), SyslType.EnumType]
  // variant name -> (generic enum name, variant index) for generic enum variants
  protected val genericVariantToEnum = new mutable.LinkedHashMap[String, (String, Int)]
  // Reverse map: mangled enum name -> (template name, concrete type args) for unification at call sites
  protected val enumToTemplate = new mutable.LinkedHashMap[String, (String, List[SyslType])]

  // Expected type for bidirectional inference (used by generic variant constructors)
  protected var currentExpected: Option[SyslType] = None
  // While analyzing an `ensure` expression, `old(x)` gets intercepted and rewritten
  // into a reference to a snapshot local captured at function entry.
  protected var inEnsureAnalysis: Boolean = false
  protected var oldSnapshotCounter: Int = 0
  protected val oldSnapshots = mutable.ListBuffer.empty[(String, SyslType, TExpr)]
  // While analyzing a loop invariant, `loop_entry(x)` gets intercepted and rewritten
  // into a reference to a snapshot local captured at the moment control first reaches
  // the loop (before the first iteration).
  protected var inLoopInvariantAnalysis: Boolean = false
  protected var loopEntrySnapshotCounter: Int = 0
  protected val loopEntrySnapshots = mutable.ListBuffer.empty[(String, SyslType, TExpr)]

  /** Per-function counter for naming the temps emitted at every recursive-call site of a
   *  function with a `variant` clause. Reset at each `analyzeBlockWithContracts` entry so
   *  the names are stable per function. */
  protected var variantCallCounter: Int = 0

  /** Counter for closure params bound by `_` (discard). Each `_` slot gets a unique
   *  synthetic name so multiple discards in the same param list don't collide and the
   *  body's `_` falls through to the placeholder rule, not a var lookup.
   */
  protected var discardParamCounter: Int = 0

  // Built-in binary operator → (trait name, method name). Extensible via #operator("sym") on trait methods.
  protected val builtinBinaryOperatorTraits: Map[String, (String, String)] = Map(
    "<"  -> ("Ord", "lt"),  "<=" -> ("Ord", "le"),
    ">"  -> ("Ord", "gt"),  ">=" -> ("Ord", "ge"),
    "==" -> ("Eq",  "eq"),  "!=" -> ("Eq",  "ne"),
    "+"  -> ("Add", "add"), "-"  -> ("Sub", "sub"),
    "*"  -> ("Mul", "mul"), "/"  -> ("Div", "div"),
  )

  protected val customBinaryOperatorTraits = new mutable.LinkedHashMap[String, (String, String)]
  protected val customUnaryOperatorTraits  = new mutable.LinkedHashMap[String, (String, String)]

  protected def lookupBinaryOperatorTrait(op: String): Option[(String, String)] =
    customBinaryOperatorTraits.get(op).orElse(builtinBinaryOperatorTraits.get(op))

  protected def lookupUnaryOperatorTrait(op: String): Option[(String, String)] =
    customUnaryOperatorTraits.get(op)

  // Lvalue-mutation prefix sigils. Always reserved — `++x` / `--x` are
  // statement-shaped and the lvalue semantics make user impls trickier than
  // the parser-combinator use cases the language wants to enable. The other
  // five built-in prefix sigils (`-`, `!`, `~`, `*`, `&`) are NOT in this
  // set: a `#operator(<sigil>)` registration is allowed for them, gated at
  // *impl* time on the operand-type vs. the sigil's natural built-in domain
  // (so users can `impl Lookahead[Parser[A]]` with `#operator("&")` while
  // a stray `impl ![bool]` is still rejected).
  protected val builtinPrefixOps: Set[String] =
    Set("++", "--")

  /** Set of named-type names a `#operator(<sigil>)` impl is NOT allowed to
   *  cover, because the sigil's built-in semantics already own that type.
   *  Used by the impl-registration conflict check; checks the *raw* AST so
   *  nominal aliases (`type Meters = new int`) remain user-overloadable.
   */
  protected def builtinPrefixDomainNames(op: String): Set[String] =
    val numeric = Set("int", "uint", "long", "ulong", "short", "ushort",
      "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64",
      "float", "f32", "double", "f64", "char", "byte")
    val integral = Set("int", "uint", "long", "ulong", "short", "ushort",
      "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "char", "byte")
    op match
      case "-" => numeric
      case "!" => Set("bool")
      case "~" => integral
      case "&" => numeric + "bool"
      case "*" => Set.empty // pointer dereference collisions caught structurally below
      case _   => Set.empty

  /** Does `pat` (an impl's first target-type pattern) conflict with the
   *  built-in semantics of `op`? Walks the AST head only — generic patterns
   *  like `Parser[A]`, struct names, enum names all read as user-defined
   *  and pass; only direct references to built-in scalar / pointer shapes
   *  fail. The check uses the raw AST so that `type Meters = new int`
   *  (a NamedTypeAST head of `Meters`) is freely overloadable even though
   *  its underlying is `int`.
   */
  protected def implOperandConflictsWithBuiltinPrefix(op: String, pat: TypeAST): Boolean =
    val names = builtinPrefixDomainNames(op)
    pat match
      case NamedTypeAST(n, _) => names.contains(n)
      case PtrTypeAST(_) | PtrNonNullTypeAST(_) | RefTypeAST(_) =>
        op == "*" || op == "&"
      case _ => false

  /** Runtime counterpart of `builtinPrefixDomainNames`, applied to a resolved
   *  `SyslType` at a use site. Used by the dispatch arms (UnaryAST, AddrOfAST,
   *  DerefAST) to decide *built-in vs. user-impl-first*. Nominal aliases keep
   *  their outer NamedType so dispatch on `type Meters = new int` doesn't
   *  accidentally route through the built-in numeric path.
   */
  protected def builtinPrefixDomainContains(op: String, t: SyslType): Boolean =
    op match
      case "-" => t.isInstanceOf[IntType] || t.isInstanceOf[UIntType] || t.isInstanceOf[FloatType]
      case "!" => t == BoolType
      case "~" => t.isInstanceOf[IntType] || t.isInstanceOf[UIntType]
      case "*" => t.isInstanceOf[PtrType] || t.isInstanceOf[RefType] || t.isInstanceOf[ArrayType]
      case "&" =>
        t.isInstanceOf[IntType] || t.isInstanceOf[UIntType] || t.isInstanceOf[FloatType] ||
          t == BoolType || t.isInstanceOf[PtrType] || t.isInstanceOf[RefType]
      case _   => false

  /** Register #operator / #op attributes from trait methods. Arity routes the
   *  registration: a single-param trait method becomes a prefix operator;
   *  two-param methods become infix. Anything else is rejected.
   */
  protected def registerTraitOperatorEntries(traitName: String, methods: List[TraitMethodAST], node: Any): Unit =
    for m <- methods do
      val opAttrs = m.attributes.filter(a => a.name == "operator" || a.name == "op")
      if opAttrs.length > 1 then
        throw AnalysisError(s"trait method '${m.name}' has multiple #operator / #op attributes", m)
      opAttrs.headOption.foreach { attr =>
        val sym = extractOperatorSymbol(attr, m)
        m.params.length match
          case 2 =>
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
            customBinaryOperatorTraits(sym) = (traitName, m.name)
          case 1 =>
            if builtinPrefixOps.contains(sym) then
              throw AnalysisError(
                s"prefix operator '$sym' is reserved for built-in dispatch; cannot overload via #operator",
                m,
              )
            customUnaryOperatorTraits.get(sym) match
              case Some((t, meth)) if t != traitName || meth != m.name =>
                throw AnalysisError(
                  s"prefix operator '$sym' is already bound to trait '$t' (method '$meth')",
                  m,
                )
              case _ => ()
            customUnaryOperatorTraits(sym) = (traitName, m.name)
          case n =>
            throw AnalysisError(
              s"trait method '${m.name}' with #operator(\"$sym\") must take one (prefix) or two (infix) parameters; got $n",
              m,
            )
      }

  protected def extractOperatorSymbol(attr: Attribute, at: Any): String =
    attr.args match
      case List(AttrPositional(AttrLitString(s))) if s.nonEmpty => s
      case List(AttrNamed("sym", AttrLitString(s))) if s.nonEmpty => s
      case List(AttrNamed("symbol", AttrLitString(s))) if s.nonEmpty => s
      case _ =>
        throw AnalysisError(s"#${attr.name} requires a non-empty string literal, e.g. #operator(\"~\")", at)

  // Trait / impl support
  protected case class TraitInfo(
      name: String,
      typeParams: List[String],
      methods: List[TraitMethodAST],
      assocTypes: List[AssocTypeDeclAST] = Nil,
      /** Trait bounds on the trait's own type parameters, e.g.
       *  `trait Container[T: Ord]`. Enforced at impl registration time —
       *  each impl target type must satisfy the declared bound. */
      typeBounds: Map[String, List[String]] = Map.empty,
  )
  protected case class ImplMethodInfo(mangled: String, paramTypes: List[(String, SyslType)], retType: SyslType, body: FunBodyAST, isSynthesized: Boolean)
  /** A registered impl block — concrete or generic.
   *
   *  `targetPatterns` are TypeAST so generic impls can carry type variables (e.g. `Parser[X]`).
   *  For concrete impls (`typeParams.isEmpty`), `resolvedConcrete` carries the resolved
   *  SyslTypes — populated at registration time so dispatch sites can compare against
   *  operand types without re-resolving on every call.
   *
   *  `methodASTs` carries the raw, un-analyzed impl method bodies; only generic impls need
   *  this (concrete impls analyze their bodies once at registration into `methodInfos`).
   *
   *  `definingModule` is the module that declared the impl, used by the orphan rule
   *  (Stage F.5) to gate cross-module impls.
   */
  protected case class ImplTemplate(
      typeParams: List[String],
      targetPatterns: List[TypeAST],
      resolvedConcrete: Option[List[SyslType]],
      methods: mutable.LinkedHashMap[String, String], // methodName -> mangledFunName
      methodInfos: List[ImplMethodInfo],
      methodASTs: List[FunDeclAST],                   // raw impl bodies (generic impls only)
      definingModule: String,
      implDecl: Option[ImplDeclAST] = None,           // for source-defined impls; None when imported
      /** Associated-type bindings declared on this impl (Phase A1). Carries the
       *  raw TypeAST per binding so projection resolution (Phase A3) can resolve
       *  them on demand using the dispatch-time substitution map. */
      assocBindings: List[AssocTypeBindingAST] = Nil,
      /** Trait bounds on this impl's own type parameters (e.g.
       *  `impl[T: Ord] Get[Box[T]]`). Enforced at `instantiateImpl` time when
       *  the impl is selected for a concrete type substitution. */
      typeBounds: Map[String, List[String]] = Map.empty,
  )
  protected val traits = new mutable.LinkedHashMap[String, TraitInfo]
  // traitName -> list of registered impls (templates). Order is registration order.
  protected val implTemplates = new mutable.LinkedHashMap[String, mutable.ListBuffer[ImplTemplate]]

  // ---- Extension methods (Phase 2a: side-table for `expr.method` dispatch) ----
  // Each `extension (recv: T) { def m(...) ... }` block lowers each method into
  // a free FunDeclAST with the receiver as the first param, and registers an
  // entry here keyed on the user-facing method name. Dispatch in the
  // MethodCallAST arm consults this table whenever the normal lookup fails,
  // gated on `definingModule` visibility (Phase 2b will add the cross-module
  // gate; Phase 2a treats every entry as visible since they all live in the
  // same compilation unit).
  protected case class ExtensionEntry(
      methodName: String,         // user-facing call name, e.g. "shout"
      receiverTypeAst: TypeAST,   // for dispatch type-match (resolved lazily)
      mangledFnName: String,      // synthesized free-function name to call
      definingModule: String,     // for visibility check (Phase 2b)
  )
  protected val extensionsByMethod = new mutable.LinkedHashMap[String, mutable.ListBuffer[ExtensionEntry]]
  /** Synth ImplDeclASTs from generic-receiver operator extensions. Tracked
   *  separately so they can be carried through `meta.genericTemplates` for
   *  cross-module dispatch (Phase 2d). Concrete-receiver operator extensions
   *  go through the regular `concreteImpls` channel (their typeParams is Nil). */
  protected val extensionImplDecls = mutable.ListBuffer[ImplDeclAST]()
  /** Modules whose extensions are visible from this compilation unit: the unit's own
   *  module (set in `analyze`), every imported module path (registered via
   *  `registerImport`), and the empty-module sentinel (always visible — same-unit
   *  extensions of an unmoduled file). The Predef trick (extensions on a type T
   *  visible because T is defined in the entry's module) is handled separately at
   *  dispatch time using `typeDefiningModule` plus `primitiveDefiningModule`. */
  protected val visibleExtensionModules = mutable.HashSet[String]("")
  /** Hardwired Predef bindings for built-in types — extensions on these are visible
   *  from any unit if defined in the named "owning" module. Mirrors Scala 3's
   *  `Predef`: every program implicitly sees `string`'s extensions in `std.string`,
   *  `int`/`i32`'s in `std.int`, etc. Used in `tryExtensionDispatch`'s visibility
   *  filter alongside `typeDefiningModule`. */
  protected val primitiveDefiningModule: Map[String, String] = Map(
    "string" -> "std_string",
    "i8" -> "std_int", "i16" -> "std_int", "i32" -> "std_int", "i64" -> "std_int", "int" -> "std_int",
    "u8" -> "std_int", "u16" -> "std_int", "u32" -> "std_int", "u64" -> "std_int", "uint" -> "std_int",
    "f32" -> "std_float", "f64" -> "std_float", "float" -> "std_float",
    "bool" -> "std_bool",
  )

  /** Predef modules whose extensions are auto-visible — slash-form module
   *  paths for every primitive owner, suitable for the driver's
   *  `packageMetaCache` / `smetaCache` lookup. The driver consults this to
   *  inject a synthetic wildcard import for each Predef module that's present
   *  in the source set, so e.g. `"hi".chars` works without a literal
   *  `import std.string`. Modules not present in the source set are silently
   *  skipped — no error if std isn't around. */
  def predefModulePaths: Set[String] =
    primitiveDefiningModule.values.toSet.map(_.replaceFirst("_", "/"))

  /** Stable string key for a TypeAST, used to mangle the synthesized extension
   *  function. Best-effort — collisions only matter for cross-block conflicts,
   *  which dispatch ambiguity-checks anyway. */
  protected def extensionTypeKey(t: TypeAST): String = t match
    case NamedTypeAST(name, Nil)  => name
    case NamedTypeAST(name, args) => s"${name}_${args.map(extensionTypeKey).mkString("_")}"
    case PtrTypeAST(e)            => s"ptr_${extensionTypeKey(e)}"
    case PtrNonNullTypeAST(e)     => s"ptr_${extensionTypeKey(e)}"
    case RefTypeAST(e)            => s"ref_${extensionTypeKey(e)}"
    case ArrayTypeAST(_, e)       => s"slice_${extensionTypeKey(e)}"
    case SliceTypeAST(e)          => s"slice_${extensionTypeKey(e)}"
    case _                        => "anon"

  /** Lower extension blocks to free FunDecls + side-table entries. Runs once at
   *  the top of analyze(), before pass 0, so the synthesized funcs flow through
   *  the normal registration/analysis pipeline. Methods carrying `#operator(<sigil>)`
   *  also emit a synth trait+impl pair so the existing `customBinaryOperatorTraits`
   *  / `customUnaryOperatorTraits` dispatch fires unchanged (Phase 2c). */
  protected def lowerExtensions(decls: List[DeclAST]): (List[DeclAST], List[ExtensionEntry]) =
    val out = mutable.ListBuffer[DeclAST]()
    val entries = mutable.ListBuffer[ExtensionEntry]()
    val module = currentModule.getOrElse("")
    for decl <- decls do
      decl match
        case ExtensionDeclAST(tparams, recv, methods, _, _, extTypeBounds) =>
          val key = extensionTypeKey(recv.typ)
          for m <- methods do
            val mangled = s"__ext_${key}__${m.name}"
            // Combine extension-level tparams with method-level tparams. Order
            // matters for instantiation lookup later — extension tparams come
            // first so a generic receiver's type bindings are stable.
            val mergedTparams = tparams ++ m.typeParams
            // Forward extension-level type bounds to the synth fn. The fn-level
            // bounds carrier on FunDeclAST is `typeBounds`; method-level wins
            // on collision (rare; the method-level set is typically empty here).
            val mergedTypeBounds = extTypeBounds ++ m.typeBounds
            val synth = m.copy(
              name = mangled,
              params = recv :: m.params,
              typeParams = mergedTparams,
              typeBounds = mergedTypeBounds,
            )
            out += synth
            entries += ExtensionEntry(m.name, recv.typ, mangled, module)
            // Phase 2c: an extension method with `#operator(<sigil>)` also synthesizes
            // a trait+impl pair so the existing operator-dispatch machinery picks it
            // up. The trait is generic in the receiver position only (`T`); other
            // params and return type are concretely typed exactly as the user wrote.
            // The impl provides a one-line body that delegates to the synth function.
            val opAttrs = m.attributes.filter(a => a.name == "operator" || a.name == "op")
            if opAttrs.length > 1 then
              throw AnalysisError(s"extension method '${m.name}' has multiple #operator attributes", m)
            for opAttr <- opAttrs.headOption do
              val sigil = extractOperatorSymbol(opAttr, m)
              val traitName = s"__ExtOp_${key}_${m.name}"
              val tparam = "T"
              // Trait method uses T everywhere (receiver, other params, return).
              // For the concrete-receiver case the impl substitutes T = recv.typ
              // and signature checking ensures everything matches. For the
              // generic-receiver case (Phase 2d) the impl is generic and the
              // dispatcher uses the impl's methodAST directly for unification,
              // so this stub trait signature isn't consulted at dispatch time —
              // it just needs to be syntactically valid + carry the operator
              // attribute + have the right arity for `registerTraitOperatorEntries`.
              val recvAsT = ParamAST("__ext_self__", NamedTypeAST(tparam, Nil), None, ParamMode.In)
              val traitMethodParams =
                if tparams.isEmpty then recvAsT :: m.params
                else recvAsT :: m.params.map(p => p.copy(typ = NamedTypeAST(tparam, Nil)))
              val traitRet =
                if tparams.isEmpty then m.returnType.getOrElse(NamedTypeAST("unit", Nil))
                else NamedTypeAST(tparam, Nil)
              val traitMethod = TraitMethodAST(
                m.name,
                traitMethodParams,
                traitRet,
                None,
                List(Attribute(opAttr.name, opAttr.args)),
              )
              out += TraitDeclAST(traitName, List(tparam), List(traitMethod))
              // Impl body: delegate to the synth extension function so the lowered
              // free function carries the user's body and the impl is just a thunk.
              val implRecv = recv.copy(name = "__ext_self__")
              val implParams = implRecv :: m.params
              val callArgs: List[ExpressionAST] =
                VarRefAST("__ext_self__") :: m.params.map(p => VarRefAST(p.name))
              val implBody = ExprBodyAST(CallAST(mangled, callArgs))
              val userReturn = m.returnType.getOrElse(NamedTypeAST("unit", Nil))
              val implMethod = FunDeclAST(
                m.name,
                implParams,
                Some(userReturn),
                implBody,
              )
              // Generic-receiver extensions yield a generic impl whose tparams
              // are the user's extension-level tparams; the receiver pattern
              // (using those tparams) becomes the trait's target type.
              val implDecl = ImplDeclAST(traitName, tparams, List(recv.typ), List(implMethod), Nil, Nil, Map.empty, extTypeBounds)
              out += implDecl
              if tparams.nonEmpty then extensionImplDecls += implDecl
        case other =>
          out += other
    (out.toList, entries.toList)

  /** Walk a SyslType collecting candidate "defining module" keys for the Predef-trick
   *  visibility check. Returns every named/struct/enum/interface name *plus* the
   *  underlying primitive name (so an extension on `string` defined in `std.string`
   *  is visible without import). Pointer/ref/slice/array element types contribute
   *  too — extensions on `[]u8` defined in `std.bytes` should be visible because
   *  the element is a primitive owned by `std.int`/`std.bytes`/etc. */
  protected def collectReceiverTypeNames(t: SyslType): Set[String] = t match
    case SyslType.StringType                     => Set("string")
    case SyslType.BoolType                       => Set("bool")
    case SyslType.IntType(w)                     => Set(s"i$w", "int")
    case SyslType.UIntType(w)                    => Set(s"u$w", "uint")
    case SyslType.FloatType(w)                   => Set(s"f$w", "float")
    case SyslType.StructType(name, _, _)         => Set(name)
    case SyslType.EnumType(name, _)              => Set(name)
    case SyslType.InterfaceType(name, _)         => Set(name)
    case SyslType.NamedType(name, base, _, _, _) => Set(name) ++ collectReceiverTypeNames(base)
    case SyslType.PtrType(p)                     => collectReceiverTypeNames(p)
    case SyslType.RefType(i)                     => collectReceiverTypeNames(i)
    case SyslType.ArrayType(e, _)                => collectReceiverTypeNames(e)
    case SyslType.SliceType(e)                   => collectReceiverTypeNames(e)
    case _                                       => Set.empty

  /** Phase 2b/2d dispatch: consult `extensionsByMethod` for a visible match,
   *  gated by module visibility. An entry is visible iff its `definingModule`
   *  is empty (same-unit, no module decl), or appears in
   *  `visibleExtensionModules` (own module + every imported module), or
   *  matches the Predef rule (the receiver type is owned by that module —
   *  either via `typeDefiningModule` for user types or
   *  `primitiveDefiningModule` for built-ins).
   *
   *  Generic extensions (Phase 2d) — entries whose synth function lives in
   *  `genericTemplates` — are matched via a structural unification of the
   *  receiver pattern against the call-site type, and resolved through
   *  `instantiateGeneric` so the lowered free function is specialized for
   *  the inferred type args.
   *
   *  Returns Some(call) on a unique visible hit, throws on ambiguity, returns
   *  None when no entry matches. */
  protected def tryExtensionDispatch(method: String, tObj: TExpr, tArgs: List[TExpr]): Option[TExpr] =
    val candidates = extensionsByMethod.get(method).map(_.toList).getOrElse(Nil)
    val recvNames = collectReceiverTypeNames(tObj.typ)
    def isVisible(entry: ExtensionEntry): Boolean =
      val dm = entry.definingModule
      if dm.isEmpty then true
      else if visibleExtensionModules.contains(dm) then true
      else recvNames.exists { n =>
        typeDefiningModule.get(n).contains(dm) || primitiveDefiningModule.get(n).contains(dm)
      }

    // Try to match a candidate's receiver pattern to the call's receiver type.
    // For non-generic candidates, fall through to a direct equality check; for
    // generic candidates, run a structural unification — bindings are discarded
    // here, the real instantiation happens below via `instantiateGeneric`.
    def matchesReceiver(entry: ExtensionEntry): Boolean =
      if genericTemplates.contains(entry.mangledFnName) then
        val template = genericTemplates(entry.mangledFnName)
        val tparams = template.typeParams.toSet
        val env = mutable.Map.empty[String, SyslType]
        try
          unifyTypes(entry.receiverTypeAst, tObj.typ, tparams, env)
          // The receiver pattern's structure must match the call type's outer
          // shape; unifyTypes silently no-ops on shape mismatch (e.g. a slice
          // pattern against a non-slice arg), so verify outer shape here.
          receiverShapeMatches(entry.receiverTypeAst, tObj.typ) &&
            // Every type variable that *appears* in the receiver pattern must
            // be bound — unbound ones can still resolve via remaining args
            // during instantiateGeneric, so we only require the receiver-side
            // params to be pinned at this stage.
            tparams.forall(tp => !receiverPatternUses(entry.receiverTypeAst, tp) || env.contains(tp))
        catch case _: Throwable => false
      else
        try resolveType(entry.receiverTypeAst) == tObj.typ
        catch case _: Throwable => false

    val matches = candidates.filter(isVisible).filter(matchesReceiver)
    matches match
      case Nil => None
      case List(entry) =>
        if genericTemplates.contains(entry.mangledFnName) then
          val argTypes = tObj.typ :: tArgs.map(_.typ)
          val (mangled, funInfo) = instantiateGeneric(entry.mangledFnName, argTypes)
          val checkedArgs = checkArgs(mangled, funInfo.params.tail, tArgs, funInfo.modes.drop(1))
          Some(TCall(funInfo.name, tObj :: checkedArgs, funInfo.returnType))
        else
          val funInfo = functions(entry.mangledFnName)
          val checkedArgs = checkArgs(entry.mangledFnName, funInfo.params.tail, tArgs, funInfo.modes.drop(1))
          Some(TCall(funInfo.name, tObj :: checkedArgs, funInfo.returnType))
      case multiple =>
        throw AnalysisError(
          s"extension method '$method' is ambiguous on receiver type ${tObj.typ}: " +
            s"defined in modules ${multiple.map(_.definingModule).mkString(", ")}")

  /** Outer-shape match for receiver patterns. Each constructor must agree at the
   *  top level; deeper structure is checked recursively. NamedTypeAST that names
   *  a type parameter matches anything; named types must agree by name (their
   *  args are unified separately). */
  protected def receiverShapeMatches(pat: TypeAST, t: SyslType): Boolean = pat match
    case NamedTypeAST(_, _) => true // typevar OR named type — let unifyTypes / resolveType decide
    case PtrTypeAST(inner) => t match
      case SyslType.PtrType(p) => receiverShapeMatches(inner, p)
      case _                   => false
    case PtrNonNullTypeAST(inner) => t match
      case SyslType.PtrType(p) => receiverShapeMatches(inner, p)
      case _                   => false
    case RefTypeAST(inner) => t match
      case SyslType.RefType(i) => receiverShapeMatches(inner, i)
      case _                   => false
    case ArrayTypeAST(_, inner) => t match
      case SyslType.ArrayType(e, _) => receiverShapeMatches(inner, e)
      case _                        => false
    case SliceTypeAST(inner) => t match
      case SyslType.SliceType(e) => receiverShapeMatches(inner, e)
      case _                     => false
    case _ => true

  /** Whether a receiver TypeAST pattern syntactically uses the named type variable. */
  protected def receiverPatternUses(pat: TypeAST, tv: String): Boolean = pat match
    case NamedTypeAST(name, args) => name == tv || args.exists(receiverPatternUses(_, tv))
    case PtrTypeAST(inner)        => receiverPatternUses(inner, tv)
    case PtrNonNullTypeAST(inner) => receiverPatternUses(inner, tv)
    case RefTypeAST(inner)        => receiverPatternUses(inner, tv)
    case ArrayTypeAST(_, inner)   => receiverPatternUses(inner, tv)
    case SliceTypeAST(inner)      => receiverPatternUses(inner, tv)
    case _                        => false

  // Stage F.5 bookkeeping: which module defined each trait / named type. Populated as
  // declarations are processed; consulted by the orphan rule at impl registration.
  protected val traitDefiningModule = new mutable.LinkedHashMap[String, String]
  protected val typeDefiningModule = new mutable.LinkedHashMap[String, String]

  /** Concrete-impl lookup: find a registered impl whose first resolved target is exactly
   *  `operandType` and which has no type params. Used by built-in trait method calls,
   *  concrete operator dispatch, and generic-bound checks. */
  protected def findConcreteImpl(traitName: String, operandType: SyslType): Option[ImplTemplate] =
    implTemplates.get(traitName).flatMap { ts =>
      ts.find(t => t.typeParams.isEmpty && t.resolvedConcrete.flatMap(_.headOption).contains(operandType))
    }

  /** Iterate over all registered concrete impls (legacy shape) for cross-unit serialization
   *  and similar bookkeeping that hasn't been generalized yet. */
  protected def concreteImpls: Iterator[(String, SyslType, mutable.LinkedHashMap[String, String])] =
    implTemplates.iterator.flatMap { case (traitName, ts) =>
      ts.iterator.collect {
        // Single-target concrete impls without associated-type bindings round-trip via
        // TraitImplMeta. Impls *with* assoc bindings ride through the TEMPLATES section
        // instead (since TraitImplMeta has no slot for them) — they're picked up by the
        // driver's templates filter on `assocs.nonEmpty` and the analyzer's
        // `getTraitDecls` / generic-template registration path.
        case t if t.typeParams.isEmpty
            && t.resolvedConcrete.exists(_.length == 1)
            && t.assocBindings.isEmpty =>
          (traitName, t.resolvedConcrete.get.head, t.methods)
      }
    }

  /** Walk a TypeAST collecting every named-type reference (`NamedTypeAST` heads), used by
   *  the orphan rule. Built-in scalar names (`int`, `i64`, `string`, …) are filtered out
   *  by the caller via `typeDefiningModule.contains`. */
  protected def collectNamedTypeNames(t: TypeAST): Set[String] = t match
    case NamedTypeAST(n, args)    => Set(n) ++ args.flatMap(collectNamedTypeNames)
    case PtrTypeAST(i)            => collectNamedTypeNames(i)
    case PtrNonNullTypeAST(i)     => collectNamedTypeNames(i)
    case RefTypeAST(i)            => collectNamedTypeNames(i)
    case ArrayTypeAST(_, e)       => collectNamedTypeNames(e)
    case SliceTypeAST(e)          => collectNamedTypeNames(e)
    case FuncTypeAST(ps, r, _, _) => ps.flatMap(collectNamedTypeNames).toSet ++ collectNamedTypeNames(r)
    case TupleTypeAST(elems)      => elems.flatMap(collectNamedTypeNames).toSet
    case ByNameTypeAST(i)         => collectNamedTypeNames(i)
    case ProjectionTypeAST(_, _)  => Set.empty // qualifier is a type-param-or-Self placeholder, not a real type name

  /** Walk a `SyslType` collecting every named struct/enum/nominal-alias name. Used by the
   *  named-import path so that importing a function whose signature mentions a struct from
   *  the same module also pulls in that struct's methods — methods belong to the type, not
   *  the import scope.
   */
  protected def collectStructAndEnumNames(t: SyslType): Set[String] = t match
    case SyslType.StructType(name, _, _)        => Set(name)
    case SyslType.EnumType(name, _)             => Set(name)
    case SyslType.NamedType(name, base, _, _, _) => Set(name) ++ collectStructAndEnumNames(base)
    case SyslType.PtrType(p)                    => collectStructAndEnumNames(p)
    case SyslType.RefType(i)                    => collectStructAndEnumNames(i)
    case SyslType.ArrayType(e, _)               => collectStructAndEnumNames(e)
    case SyslType.SliceType(e)                  => collectStructAndEnumNames(e)
    case SyslType.FuncType(ps, r, _, _)         => ps.iterator.flatMap(collectStructAndEnumNames).toSet ++ collectStructAndEnumNames(r)
    case _                                      => Set.empty

  /** Stage F.5 — orphan rule. An impl is allowed iff this module owns the trait OR at
   *  least one named type appearing in the impl's target patterns. Empty `currentModule`
   *  (no `module` declaration) is treated as the implicit "root" module: orphan check
   *  is bypassed since there is no other module to conflict with.
   *
   *  This deliberately does NOT recurse through generic-instance struct names — if the
   *  user impls `Concat[Parser[Foo], Parser[Bar], Parser[(Foo, Bar)]]`, owning `Foo` or
   *  `Bar` (or `Parser` or `Concat`) is sufficient.
   */
  protected def checkOrphanRule(
      traitName: String,
      targetPatterns: List[TypeAST],
      currentMod: String,
      at: Any,
  ): Unit =
    if currentMod.isEmpty then return
    val traitOwner = traitDefiningModule.getOrElse(traitName, "")
    if traitOwner == currentMod then return
    val allNames = targetPatterns.flatMap(collectNamedTypeNames).toSet
    val ownsAnyNamedType = allNames.exists(n => typeDefiningModule.get(n).contains(currentMod))
    if !ownsAnyNamedType then
      throw AnalysisError(
        s"orphan impl: module '$currentMod' defines neither trait '$traitName' nor any named type in the impl's target patterns",
        at,
      )

  /** Stage F.5 — at-most-one coherence. Walk existing templates for `traitName`; reject
   *  if the new template's operand patterns overlap with any existing template's. The
   *  result-position pattern is functionally determined and excluded from the overlap
   *  check (per the FD positional convention).
   */
  protected def checkCoherence(
      traitName: String,
      newTemplate: ImplTemplate,
      at: Any,
  ): Unit =
    val operandCount = math.min(2, newTemplate.targetPatterns.length)
    val newOperands = newTemplate.targetPatterns.take(operandCount)
    for existing <- implTemplates.getOrElse(traitName, Nil) do
      val combinedTvars = (newTemplate.typeParams ++ existing.typeParams).toSet
      val existingOperands = existing.targetPatterns.take(operandCount)
      if patternsOverlap(newOperands, existingOperands, combinedTvars) then
        throw AnalysisError(
          s"impl of '$traitName' for [${newTemplate.targetPatterns.mkString(", ")}] overlaps with existing impl for [${existing.targetPatterns.mkString(", ")}]",
          at,
        )
  // When analyzing a synthesized default method body, rewrite unqualified calls
  // to sibling trait methods to their impl's mangled names
  protected var traitCallRewrite: Map[String, String] = Map.empty

  /** Get trait impl metadata for cross-unit serialization. Currently only concrete
   *  (non-generic, single-target) impls round-trip across units; generic impls (Stage F.5+)
   *  will need an extended IMPL line format. */
  def getTraitImplMetas: List[TraitImplMeta] =
    concreteImpls
      .filterNot { case (traitName, targetType, _) =>
        importedConcreteImplKeys.contains((traitName, List(targetType)))
      }
      .map { case (traitName, targetType, methodMap) =>
        TraitImplMeta(traitName, targetType, methodMap.toMap)
      }
      .toList

  /** Get trait declaration AST nodes for serialization in TEMPLATES section.
   *  Excludes traits that arrived via cross-unit import — only own-AST traits
   *  contribute to this unit's per-file ModuleMeta. */
  def getTraitDecls: List[TraitDeclAST] =
    traits.values
      .filterNot(t => importedTraitNames.contains(t.name))
      .map(t => TraitDeclAST(t.name, t.typeParams, t.methods, Nil, t.assocTypes))
      .toList

  /** Get generic enum instance mappings for cross-module type inference. */
  def getGenericEnumInstances: List[GenericEnumInstanceMeta] =
    enumToTemplate.iterator
      .filterNot { case (mangledName, _) => importedEnumInstNames.contains(mangledName) }
      .map { case (mangledName, (baseName, typeArgs)) =>
        GenericEnumInstanceMeta(mangledName, baseName, typeArgs)
      }
      .toList

  /** Get extension method metadata for SMETA round-trip. Resolves each entry's
   *  receiver TypeAST → SyslType (now possible since analyze is complete).
   *  Generic-receiver entries whose pattern carries free type vars cannot
   *  resolve to a concrete `SyslType`; for those the cross-module path uses
   *  `getExtensionTemplates` instead, which carries the synth FunDeclAST
   *  through `meta.genericTemplates`. Concrete entries flow through here. */
  def getExtensionMetas: List[ExtensionMeta] =
    val out = mutable.ListBuffer[ExtensionMeta]()
    for (_, buf) <- extensionsByMethod do
      for entry <- buf do
        if !importedExtensionKeys.contains((entry.definingModule, entry.mangledFnName)) then
          scala.util.Try(resolveType(entry.receiverTypeAst)).foreach { rt =>
            out += ExtensionMeta(entry.methodName, entry.definingModule, rt, entry.mangledFnName)
          }
    out.toList

  /** Generic-receiver extension synth FunDeclASTs (Phase 2d). The driver carries
   *  these through `meta.genericTemplates` so importing units can register them
   *  alongside other generic templates and instantiate per call site. The
   *  side-table EXT entry is also needed (registered separately on import via
   *  `extensionEntriesFromTemplates`) so dispatch can find them by method name. */
  def getExtensionTemplates: List[FunDeclAST] =
    genericTemplates.values
      .filter(fd => fd.name.startsWith("__ext_") && !importedTemplateNames.contains(fd.name))
      .toList

  /** Synth ImplDeclASTs for generic-receiver operator extensions (Phase 2d).
   *  Carried through `meta.genericTemplates` alongside extension synth funcs +
   *  trait decls so importing units can reconstruct the generic impl in their
   *  own `implTemplates`. Returns only impls with a `__ExtOp_` traitName so
   *  user-written generic impls aren't pulled in here (they don't yet have a
   *  cross-module path; that's a separate, larger change). */
  def getExtensionImplDecls: List[ImplDeclAST] =
    extensionImplDecls.iterator.filter(_.traitName.startsWith("__ExtOp_")).toList

  protected def pushScope(): Unit =
    scopeStack += new mutable.LinkedHashMap[String, SymInfo]

  protected def popScope(): Unit =
    scopeStack.remove(scopeStack.length - 1)

  protected def currentScope: mutable.LinkedHashMap[String, SymInfo] =
    scopeStack.last

  // Polymorphic integer arithmetic intrinsics. Currently: wrapping_* (relabels current
  // wrapping behavior, future-proofs against an overflow-checked default) and saturating_*
  // (clamps at MIN/MAX on overflow). Both signatures: (a: T, b: T) -> T for any integer T.
  protected val integerArithIntrinsics: Set[String] = Set(
    "wrapping_add", "wrapping_sub", "wrapping_mul",
    "saturating_add", "saturating_sub", "saturating_mul",
  )

  protected val builtinFunctions = Map(
    "putchar" -> FunInfo("putchar", List("c" -> U32), U32),
    "print" -> FunInfo("print", List("n" -> I32), UnitType),
    "println" -> FunInfo("println", List("n" -> I32), UnitType),
    "puts" -> FunInfo("puts", List("s" -> StringType), UnitType),
    "puti" -> FunInfo("puti", List("n" -> I32), UnitType),
    "malloc" -> FunInfo("malloc", List("size" -> I64), PtrType(I8)),
    "free" -> FunInfo("free", List("ptr" -> PtrType(I8)), UnitType),
    "calloc" -> FunInfo("calloc", List("count" -> I64, "size" -> I64), PtrType(I8)),
    "realloc" -> FunInfo("realloc", List("ptr" -> PtrType(I8), "size" -> I64), PtrType(I8)),
    "sbrk" -> FunInfo("sbrk", List("increment" -> I32), PtrType(I8)),
    "abort" -> FunInfo("abort", Nil, UnitType),
    "panic" -> FunInfo("panic", List("msg" -> StringType), UnitType),
    "assert" -> FunInfo("assert", List("cond" -> BoolType, "msg" -> StringType), UnitType),
    "expect" -> FunInfo("expect", List("actual" -> I64, "expected" -> I64, "msg" -> StringType), UnitType),
  )

  def registerImport(meta: ModuleMeta, selectors: List[ImportSelector] = List(WildcardImport), modulePath: String = ""): Unit =
    // Qualified import: import std.strings → access as strings.foo
    selectors match
      case List(QualifiedImport) =>
        val nsName = modulePath.split("/").last
        moduleNamespaces(nsName) = meta
        return
      case List(ExtensionsOnlyImport) =>
        // Predef auto-import: only register extension entries + their `__ext_*`
        // synth functions (and the generic templates that back generic
        // extensions). Skip every other public symbol so the module's regular
        // functions (e.g. `contains` in `std.string`) don't pollute the
        // importing unit's namespace and collide with same-named functions in
        // other modules. Visibility flag for the source module is still set so
        // the dispatcher's `visibleExtensionModules` check passes.
        val extFnShortNames: Set[String] = meta.extensions.map(_.mangledFnName).toSet
        val importedDefMod =
          if modulePath.nonEmpty then modulePath.replace('/', '_').replace('.', '_') else ""
        // 1. Register the synth `__ext_*` functions (concrete-receiver case).
        for sym <- meta.publicSymbols do
          val sn = shortName(sym.name)
          if extFnShortNames.contains(sn) && !functions.contains(sn) then
            sym.typ match
              case SymbolMeta.Kind.Func(params, returnType, isDef, isPure, modes, effects, isParameterless) =>
                val paramPairs = params.zipWithIndex.map((t, i) => (s"_p$i", t))
                val (reads, writes) = effects match
                  case e if e.isPure || e.isUnknown => (None, None)
                  case e => (e.reads, e.writes)
                functions(sn) = FunInfo(sym.name, paramPairs, returnType, isDef, isPure || effects.isPure, modes, reads, writes, isParameterless = isParameterless)
                if reads.isDefined || writes.isDefined then
                  resolvedEffectsCache(sym.name) = (reads.getOrElse(Set.empty), writes.getOrElse(Set.empty))
                externalSymbols += sn
              case _ => ()
        // 2. Register generic-receiver synth templates (`__ext_*` FunDeclAST in
        //    meta.genericTemplates) so cross-module generic extension dispatch
        //    sees them. Filter to ext-only — don't pull in unrelated generics.
        val extOnlyTemplates = meta.genericTemplates.collect {
          case fd: FunDeclAST if fd.name.startsWith("__ext_") => fd: DeclAST
        }
        if extOnlyTemplates.nonEmpty then
          registerGenericTemplatesFrom(ProgramAST(extOnlyTemplates), filter = None)
        // 2b. Register synth trait + generic impl decls for `__ExtOp_*` operator
        //     extensions. The trait must be registered before the impl so the
        //     impl-decl arity check passes; do trait first.
        for template <- meta.genericTemplates do
          template match
            case TraitDeclAST(name, tparams, methods, _, _, _, _) if name.startsWith("__ExtOp_") =>
              if !traits.contains(name) then
                traits(name) = TraitInfo(name, tparams, methods)
                importedTraitNames += name
                registerTraitOperatorEntries(name, methods, template)
            case _ => ()
        for template <- meta.genericTemplates do
          template match
            case impl @ ImplDeclAST(traitName, implTypeParams, targetTypes, methods, _, _, _, _)
                if traitName.startsWith("__ExtOp_") && implTypeParams.nonEmpty =>
              if !implTemplates.getOrElse(traitName, Nil).exists(t =>
                  t.typeParams == implTypeParams && t.targetPatterns == targetTypes) then
                implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += ImplTemplate(
                  typeParams = implTypeParams,
                  targetPatterns = targetTypes,
                  resolvedConcrete = None,
                  methods = mutable.LinkedHashMap.empty,
                  methodInfos = Nil,
                  methodASTs = methods,
                  definingModule = "",
                  implDecl = Some(impl),
                )
            case _ => ()
        // 3. Register concrete extension entries from meta.extensions.
        for ext <- meta.extensions do
          val entry = ExtensionEntry(
            methodName = ext.methodName,
            receiverTypeAst = syslTypeToAST(ext.receiverType),
            mangledFnName = ext.mangledFnName,
            definingModule = ext.definingModule,
          )
          val bucket = extensionsByMethod.getOrElseUpdate(ext.methodName, mutable.ListBuffer.empty)
          if !bucket.exists(e => e.definingModule == entry.definingModule && e.mangledFnName == entry.mangledFnName) then
            bucket += entry
          importedExtensionKeys += ((entry.definingModule, entry.mangledFnName))
        // 4. Reconstruct generic-receiver extension entries from synth templates.
        for template <- extOnlyTemplates do
          template match
            case fd: FunDeclAST if fd.name.startsWith("__ext_") && fd.params.nonEmpty =>
              val sep = fd.name.indexOf("__", 6)
              if sep > 0 then
                val methodName = fd.name.substring(sep + 2)
                val recv = fd.params.head
                val entry = ExtensionEntry(
                  methodName = methodName,
                  receiverTypeAst = recv.typ,
                  mangledFnName = fd.name,
                  definingModule = importedDefMod,
                )
                val bucket = extensionsByMethod.getOrElseUpdate(methodName, mutable.ListBuffer.empty)
                if !bucket.exists(e => e.definingModule == entry.definingModule && e.mangledFnName == entry.mangledFnName) then
                  bucket += entry
                importedExtensionKeys += ((entry.definingModule, entry.mangledFnName))
            case _ => ()
        if modulePath.nonEmpty then
          visibleExtensionModules += importedDefMod
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
    // Synthesized extension functions (`__ext_<typeKey>__<method>`) must be
    // pulled in alongside their EXT entry whenever an EXT is being imported
    // — even on a named-import like `import mylib.{add_one}` where the user
    // didn't name the extension's method. Without this, `tryExtensionDispatch`
    // would find the entry but fail to find the function in `functions`.
    val extensionFnShortNames: Set[String] = meta.extensions.map(_.mangledFnName).toSet
    val selectedSymbols = selectors match
      case List(WildcardImport) => dedup(meta.publicSymbols)
      case named =>
        val nameMap = named.collect { case NamedImport(n, r) => (n, r) }.toMap
        // Match selectors against short names (without module prefix).
        // When a struct or enum is imported by name, pull in its methods
        // (StructName_method). When a *function* is imported and its
        // signature mentions a struct/enum that lives in this same module,
        // also pull in that type's methods — methods belong to the type, not
        // the import scope, so `import std.builder.{new_builder}` should let
        // the user call methods on the returned `StrBuilder` without naming
        // it in the selector list.
        val directMatch = meta.publicSymbols.filter(sym => nameMap.contains(shortName(sym.name)))
        val moduleStructAndEnumNames: Set[String] =
          meta.publicSymbols.collect {
            case sym if sym.typ.isInstanceOf[SymbolMeta.Kind.Struct] => shortName(sym.name)
            case sym if sym.typ.isInstanceOf[SymbolMeta.Kind.Enum]   => shortName(sym.name)
          }.toSet
        val explicitlyImportedTypeNames = directMatch.collect {
          case sym if sym.typ.isInstanceOf[SymbolMeta.Kind.Struct] => shortName(sym.name)
          case sym if sym.typ.isInstanceOf[SymbolMeta.Kind.Enum]   => shortName(sym.name)
        }.toSet
        val typesReachableFromImportedFuncs: Set[String] =
          directMatch.iterator.collect {
            case sym if sym.typ.isInstanceOf[SymbolMeta.Kind.Func] =>
              val SymbolMeta.Kind.Func(params, ret, _, _, _, _, _) = sym.typ: @unchecked
              (params :+ ret).iterator.flatMap(collectStructAndEnumNames).toSet
          }.flatten.toSet.intersect(moduleStructAndEnumNames)
        val importedTypeNames = explicitlyImportedTypeNames ++ typesReachableFromImportedFuncs
        val withMethods = if importedTypeNames.isEmpty then directMatch
        else directMatch ++ meta.publicSymbols.filter { sym =>
          sym.typ.isInstanceOf[SymbolMeta.Kind.Func] &&
            importedTypeNames.exists(tn => shortName(sym.name).startsWith(s"${tn}_"))
        }
        val withExtensions = withMethods ++ meta.publicSymbols.filter { sym =>
          sym.typ.isInstanceOf[SymbolMeta.Kind.Func] && extensionFnShortNames.contains(shortName(sym.name))
        }
        dedup(withExtensions)
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
        case SymbolMeta.Kind.Func(params, returnType, isDef, isPure, modes, effects, isParameterless) =>
          val paramPairs = params.zipWithIndex.map((t, i) => (s"_p$i", t))
          // Even if the function itself is already known, walk its types to
          // discover any nested generic-struct instances (e.g. an imported
          // `eoi -> Box[unit]` surfaces `Box_unit`, which the importing unit
          // needs to recognize as `Box[unit]` for downstream unification).
          for p <- params do linkNestedGenericStructInstances(p)
          linkNestedGenericStructInstances(returnType)
          if functions.contains(localKey) then
            // Allow same-module sibling re-registration (same mangled name) and externs
            val existing = functions(localKey)
            if !sym.isExtern && existing.name != sym.name then
              throw AnalysisError(s"imported symbol '$localKey' conflicts with existing function")
          else
            // Carry over the imported effect signature so `funInfoEffects` and
            // `validateEffects` see the same information as if the function were
            // defined locally. Pre-resolved (mangled) names round-trip directly.
            val (reads, writes) = effects match
              case e if e.isPure => (None, None)
              case e if e.isUnknown => (None, None)
              case e => (e.reads, e.writes)
            functions(localKey) = FunInfo(sym.name, paramPairs, returnType, isDef, isPure || effects.isPure, modes, reads, writes, isParameterless = isParameterless)
            // Pre-populate the resolved-effects cache so cross-module reads use the same
            // already-mangled names without trying to look them up in this unit's globalScope.
            if reads.isDefined || writes.isDefined then
              resolvedEffectsCache(sym.name) = (reads.getOrElse(Set.empty), writes.getOrElse(Set.empty))
            externalSymbols += localKey
        case SymbolMeta.Kind.Data(dataType, isMutable) =>
          linkNestedGenericStructInstances(dataType)
          if globalScope.contains(localKey) then
            val existing = globalScope(localKey)
            if !sym.isExtern && existing.name != sym.name then
              throw AnalysisError(s"imported symbol '$localKey' conflicts with existing global")
          else
            globalScope(localKey) = SymInfo(sym.name, dataType, mutable = isMutable)
            externalSymbols += localKey
        case SymbolMeta.Kind.Const(constType, value) =>
          linkNestedGenericStructInstances(constType)
          // Cross-file `const`: register in globalScope (so VarRef name resolution
          // succeeds) AND in compileTimeConstants under both the local-key short name
          // and the fully-mangled name so the analyzer's constant-folding paths
          // (VarRef → TIntLit substitution) find the value either way.
          if globalScope.contains(localKey) then
            val existing = globalScope(localKey)
            if !sym.isExtern && existing.name != sym.name then
              throw AnalysisError(s"imported const '$localKey' conflicts with existing global")
          else
            globalScope(localKey) = SymInfo(sym.name, constType, mutable = false, isConst = true)
            externalSymbols += localKey
          compileTimeConstants(localKey) = value
          compileTimeConstants(sym.name) = value
        case SymbolMeta.Kind.Struct(st) =>
          structTypes(shortName(sym.name)) = st
          linkImportedGenericStructToTemplate(st)
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
          if findConcreteImpl(traitName, targetType).isEmpty then
            val mm = mutable.LinkedHashMap.from(methods)
            implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) +=
              ImplTemplate(
                typeParams = Nil,
                targetPatterns = List(syslTypeToAST(targetType)),
                resolvedConcrete = Some(List(targetType)),
                methods = mm,
                methodInfos = Nil,
                methodASTs = Nil,
                definingModule = "",   // imported impls — orphan check skipped on import
              )
            importedConcreteImplKeys += ((traitName, List(targetType)))

    // Register generic templates from imported module (needed for cross-module generic instantiation).
    // Selective imports (`import std.option.{Option, Some, None}`) must filter generic templates
    // by selector — otherwise generic functions like `std.option.expect[T]` leak into scope and
    // shadow the testing-builtin `expect` (or whatever else the user wants to use). Wildcard
    // imports register everything as before.
    val genericTemplateFilter: Option[Set[String]] = selectors match
      case List(WildcardImport) => None
      case named =>
        Some(named.collect { case NamedImport(n, _) => n }.toSet)
    if meta.genericTemplates.nonEmpty then
      registerGenericTemplatesFrom(ProgramAST(meta.genericTemplates), genericTemplateFilter)

    // Generic struct templates may have just been registered above; the symbol
    // loop walked imported function/data/const types before genericStructs was
    // populated, so any nested generic-struct instances (e.g. `Box_unit` inside
    // `eoi -> Box[unit]`) couldn't be linked then. Re-walk the publicSymbols'
    // types now that templates are in scope so structToTemplate has the
    // mangled-instance → template mapping for downstream unifyTypes.
    for sym <- meta.publicSymbols do
      sym.typ match
        case SymbolMeta.Kind.Func(params, returnType, _, _, _, _, _) =>
          for p <- params do linkNestedGenericStructInstances(p)
          linkNestedGenericStructInstances(returnType)
        case SymbolMeta.Kind.Data(t, _) => linkNestedGenericStructInstances(t)
        case SymbolMeta.Kind.Const(t, _) => linkNestedGenericStructInstances(t)
        case SymbolMeta.Kind.Struct(st) => linkImportedGenericStructToTemplate(st)
        case _ => ()

    // Register generic enum instance mappings for cross-module type inference
    for inst <- meta.genericEnumInstances do
      if !enumToTemplate.contains(inst.mangledName) then
        enumToTemplate(inst.mangledName) = (inst.baseName, inst.typeArgs)
      importedEnumInstNames += inst.mangledName

    // Register trait declarations from imported templates
    for template <- meta.genericTemplates do
      template match
        case TraitDeclAST(name, tparams, methods, _, assocs, _, tBounds) =>
          if !traits.contains(name) then
            traits(name) = TraitInfo(name, tparams, methods, assocs, tBounds)
            importedTraitNames += name
            registerTraitOperatorEntries(name, methods, template)
        case _ =>

    // Register trait impl mappings from imported module
    for impl <- meta.traitImpls do
      if findConcreteImpl(impl.traitName, impl.targetType).isEmpty then
        val mm = mutable.LinkedHashMap.from(impl.methods)
        implTemplates.getOrElseUpdate(impl.traitName, mutable.ListBuffer.empty) +=
          ImplTemplate(
            typeParams = Nil,
            targetPatterns = List(syslTypeToAST(impl.targetType)),
            resolvedConcrete = Some(List(impl.targetType)),
            methods = mm,
            methodInfos = Nil,
            methodASTs = Nil,
            definingModule = "",
          )
        importedConcreteImplKeys += ((impl.traitName, List(impl.targetType)))

    // Register ImplDeclASTs from imported templates. The driver puts every
    // user-written generic impl + every multi-target concrete impl in the
    // genericTemplates list — single-target concrete impls go through the
    // meta.traitImpls path above, so they're not duplicated here.
    for template <- meta.genericTemplates do
      template match
        case impl @ ImplDeclAST(traitName, implTypeParams, targetTypes, methods, _, _, _, implTBounds) =>
          val alreadyHas = implTemplates.getOrElse(traitName, Nil).exists(t =>
            t.typeParams == implTypeParams && t.targetPatterns == targetTypes)
          if !alreadyHas then
            if implTypeParams.nonEmpty then
              // Generic impl: methods are analyzed lazily at instantiation time.
              implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += ImplTemplate(
                typeParams = implTypeParams,
                targetPatterns = targetTypes,
                resolvedConcrete = None,
                methods = mutable.LinkedHashMap.empty,
                methodInfos = Nil,
                methodASTs = methods,
                definingModule = "",
                implDecl = Some(impl),
                typeBounds = implTBounds,
              )
              importedGenericImplKeys += ((traitName, implTypeParams, targetTypes))
            else
              // Multi-target concrete impl. Resolve targets so the dispatch
              // path (`enumerateImplCandidates` concrete branch) can compare
              // against them, and mangle method names with the *owning*
              // module's prefix (not the importing analyzer's `currentModule`)
              // so dispatch lands on the same name the impl's defining unit
              // emitted. For cross-module imports, modulePath identifies the
              // owning module; for same-module siblings (modulePath empty)
              // the driver pre-seeds `currentModule` to the shared module.
              // If targets can't resolve yet (rare — types are registered
              // earlier in registerImport), skip and rely on a later import
              // iteration to pick it up.
              scala.util.Try(targetTypes.map(resolveType)).toOption.foreach { resolvedTargets =>
                val owningModuleMangled =
                  if modulePath.nonEmpty then modulePath.replace('/', '_').replace('.', '_')
                  else currentModule.getOrElse("")
                val typeMangled = resolvedTargets.map(typeToMangled).mkString("_")
                val methodMap = mutable.LinkedHashMap.empty[String, String]
                for m <- methods do
                  val rawMangled = s"${traitName}_${m.name}_${typeMangled}"
                  val mangled =
                    if owningModuleMangled.nonEmpty && !neverMangle.contains(rawMangled) then
                      s"${owningModuleMangled}__${rawMangled}"
                    else rawMangled
                  methodMap(m.name) = mangled
                implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += ImplTemplate(
                  typeParams = Nil,
                  targetPatterns = targetTypes,
                  resolvedConcrete = Some(resolvedTargets),
                  methods = methodMap,
                  methodInfos = Nil,
                  methodASTs = Nil,
                  definingModule = "",
                  implDecl = Some(impl),
                )
                importedConcreteImplKeys += ((traitName, resolvedTargets))
              }
        case _ => ()

    // Register imported extensions: every extension flows into the side table
    // so dispatch can find it; visibility is gated at dispatch time. The
    // module's own key is also added to `visibleExtensionModules` so any
    // extension *defined in* this imported module is callable in this unit
    // (matches Scala 3's "import brings extensions" rule). Modules imported
    // by short name via `modulePath` are stored in their underscore-mangled
    // form to match the analyzer's `currentModule` representation.
    val importedDefiningModule =
      if modulePath.nonEmpty then modulePath.replace('/', '_').replace('.', '_') else ""
    for ext <- meta.extensions do
      val entry = ExtensionEntry(
        methodName = ext.methodName,
        receiverTypeAst = syslTypeToAST(ext.receiverType),
        mangledFnName = ext.mangledFnName,
        definingModule = ext.definingModule,
      )
      val bucket = extensionsByMethod.getOrElseUpdate(ext.methodName, mutable.ListBuffer.empty)
      // De-dup by (definingModule, mangledFnName) so siblings importing each
      // other don't double-register the same extension.
      if !bucket.exists(e => e.definingModule == entry.definingModule && e.mangledFnName == entry.mangledFnName) then
        bucket += entry
      importedExtensionKeys += ((entry.definingModule, entry.mangledFnName))
    // Phase 2d: reconstruct ExtensionEntry's for generic synth templates that
    // travelled through `meta.genericTemplates`. The synth name encodes the
    // method (`__ext_<typeKey>__<methodName>`) and the first parameter's
    // TypeAST is the original receiver pattern with its free type vars intact.
    // The `definingModule` is the module we're currently importing — it's not
    // carried in the FunDeclAST itself, so use `modulePath` (already mangled).
    for template <- meta.genericTemplates do
      template match
        case fd: FunDeclAST if fd.name.startsWith("__ext_") && fd.params.nonEmpty =>
          val sep = fd.name.indexOf("__", 6)  // skip leading "__ext_"
          if sep > 0 then
            val methodName = fd.name.substring(sep + 2)
            val recv = fd.params.head
            val entry = ExtensionEntry(
              methodName = methodName,
              receiverTypeAst = recv.typ,
              mangledFnName = fd.name,
              definingModule = importedDefiningModule,
            )
            val bucket = extensionsByMethod.getOrElseUpdate(methodName, mutable.ListBuffer.empty)
            if !bucket.exists(e => e.definingModule == entry.definingModule && e.mangledFnName == entry.mangledFnName) then
              bucket += entry
            importedExtensionKeys += ((entry.definingModule, entry.mangledFnName))
        case _ => ()
    if modulePath.nonEmpty then
      visibleExtensionModules += importedDefiningModule

  def isExternal(name: String): Boolean = externalSymbols.contains(name)
  def externals: Set[String] = externalSymbols.toSet

  /** Inverse of `typeToMangled` for a single type (used in mangled generic enum names like `ParseMaybe_i32`). */
  protected def parseMangledMonotype(s: String): Option[SyslType] =
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
        case "unit" => Some(SyslType.UnitType)
        case "f32" | "float" => Some(SyslType.F32)
        case "f64" | "double" => Some(SyslType.F64)
        case _ => None

  /** Link mangled imported enum names to generic templates for `unifyTypes` only. Do not call `instantiateGenericEnum` here — it would overwrite `variantToEnum` for shared variant names like `Got`/`Miss`. */
  protected def linkImportedDataEnumToTemplate(et: SyslType.EnumType): Unit =
    if genericEnums.isEmpty then return
    for (baseName, decl) <- genericEnums if decl.typeParams.length == 1 do
      val prefix = baseName + "_"
      if et.name.startsWith(prefix) then
        val suffix = et.name.drop(prefix.length)
        parseMangledMonotype(suffix).foreach { t =>
          enumToTemplate(et.name) = (baseName, List(t))
        }

  /** Cross-unit mirror of the local instantiation cache: when an imported
   *  symbol's type names a generic-struct *instance* (e.g. `Box_unit`),
   *  populate `structToTemplate` so subsequent `unifyTypes(Box[A], Box_unit)`
   *  in this unit can recover the type-arg binding. Without this, sibling /
   *  cross-module references to a value of generic-struct-instance type don't
   *  drive type inference at the use site. Mirrors the enum equivalent above.
   *  Single-target template only; multi-arg templates would need a richer
   *  inverse mangler. */
  protected def linkImportedGenericStructToTemplate(st: SyslType.StructType): Unit =
    if genericStructs.isEmpty then return
    if structToTemplate.contains(st.name) then return
    for (baseName, decl) <- genericStructs if decl.typeParams.length == 1 do
      val prefix = baseName + "_"
      if st.name.startsWith(prefix) then
        val suffix = st.name.drop(prefix.length)
        parseMangledMonotype(suffix).foreach { t =>
          structToTemplate(st.name) = (baseName, List(t))
        }

  /** Cross-unit mirror for generic *type aliases*: when an imported value's
   *  type is a nominal alias instance (e.g. `Box_unit` where `type Box[A] = new A`),
   *  populate `genericAliasToTemplate` so subsequent `unifyTypes(Box[A], Box_unit)`
   *  can recover the type-arg binding. Mirrors the struct equivalent, but for
   *  the `type X[A] = new ...` family. Single-target template only. */
  protected def linkImportedGenericAliasToTemplate(nt: SyslType.NamedType): Unit =
    if genericTypeAliases.isEmpty then return
    if genericAliasToTemplate.contains(nt.name) then return
    for (baseName, (tps, _, _)) <- genericTypeAliases if tps.length == 1 do
      val prefix = baseName + "_"
      if nt.name.startsWith(prefix) then
        val suffix = nt.name.drop(prefix.length)
        parseMangledMonotype(suffix).foreach { t =>
          genericAliasToTemplate(nt.name) = (baseName, List(t))
        }

  /** Walk a SyslType and link any nested generic-struct or generic-alias
   *  instance to its template. Used when registering imported function
   *  signatures whose param/return types may surface instance types
   *  (e.g. `Box_unit`) that aren't themselves imported as standalone symbols. */
  protected def linkNestedGenericStructInstances(t: SyslType): Unit = t match
    case st: SyslType.StructType =>
      linkImportedGenericStructToTemplate(st)
      for (_, ft) <- st.fields do linkNestedGenericStructInstances(ft)
    case nt @ SyslType.NamedType(_, base, true, _, _) =>
      linkImportedGenericAliasToTemplate(nt)
      linkNestedGenericStructInstances(base)
    case SyslType.NamedType(_, base, _, _, _) => linkNestedGenericStructInstances(base)
    case SyslType.PtrType(p) => linkNestedGenericStructInstances(p)
    case SyslType.RefType(p) => linkNestedGenericStructInstances(p)
    case SyslType.ArrayType(e, _) => linkNestedGenericStructInstances(e)
    case SyslType.SliceType(e) => linkNestedGenericStructInstances(e)
    case SyslType.FuncType(ps, r, _, _) =>
      for p <- ps do linkNestedGenericStructInstances(p)
      linkNestedGenericStructInstances(r)
    case SyslType.EnumType(_, variants) =>
      for (_, fs) <- variants; (_, ft) <- fs do linkNestedGenericStructInstances(ft)
    case _ => ()

  /** Generic templates are omitted from `ModuleMeta` / typed `TProgram`; same-package siblings need the raw AST templates to resolve calls like `alt(...)`. */
  /** Register generic templates (functions, structs, data enums) for the current
   *  unit or for a cross-module import. When `filter` is `None` (wildcard / own
   *  module), every template is registered.
   *
   *  When `filter` is `Some(set)`, only **functions** whose name is in the set are
   *  registered — this is what makes selective imports actually selective for
   *  generic functions, which is where shadowing bugs surface (the canonical
   *  case: `import std.option.{Option, Some, None}` should NOT pull in the
   *  generic `expect[T]`). Generic structs / data enums are still registered
   *  unconditionally because their names appear in user-written types and the
   *  analyzer needs them resolvable; the symbol-table import path already
   *  filters them through publicSymbols. */
  def registerGenericTemplatesFrom(
      program: ProgramAST,
      filter: Option[Set[String]] = None,
  ): Unit =
    def funcSelected(name: String): Boolean = filter.forall(_.contains(name))
    for decl <- program.decls do
      decl match
        case fd @ FunDeclAST(name, _, _, _, _, tps, _, _, _, _, _) if tps.nonEmpty =>
          if funcSelected(name) && !genericTemplates.contains(name) && !functions.contains(name) then
            genericTemplates(name) = fd
            importedTemplateNames += name
        case sd @ StructDeclAST(name, _, tps, _, _, _, _) if tps.nonEmpty =>
          if !genericStructs.contains(name) then
            genericStructs(name) = sd
            importedTemplateNames += name
        case de @ DataEnumDeclAST(name, variants, tps, _, _, _) if tps.nonEmpty =>
          if !genericEnums.contains(name) then
            genericEnums(name) = de
            importedTemplateNames += name
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
        case TypeAliasDeclAST(name, target, tps, _, isNew, _, _, defs, bounds) if tps.nonEmpty =>
          if !genericTypeAliases.contains(name) && !typeAliases.contains(name) then
            genericTypeAliases(name) = (tps, target, isNew)
            if defs.nonEmpty then genericTypeAliasDefaults(name) = defs
            if bounds.nonEmpty then genericTypeAliasBounds(name) = bounds
            importedTemplateNames += name
        case _ => ()

  /** Sibling forward-decl pass: registers structs/enums/aliases/traits/impls
   *  from a same-module sibling file's source AST. Distinct from
   *  registerGenericTemplatesFrom — that one is called from registerImport's
   *  cross-module path and must NOT mangle with the importing module's
   *  prefix. This method is only safe to call when the analyzer's
   *  currentModule equals the sibling's owning module (i.e. they're in the
   *  same module). The driver pre-seeds currentModule and calls this on
   *  every sibling AST during Step 4b; the analyzer re-calls it after own
   *  pass 1 so sibling concrete impls referencing own types can resolve.
   *  Idempotent — every clause guards on existence. */
  def registerSiblingForwardDeclsFrom(program: ProgramAST): Unit =
    // Pre-pass: register generic templates first so cross-sibling generic
    // types are visible to the type-checks in subsequent clauses.
    registerGenericTemplatesFrom(program)
    for decl <- program.decls do
      decl match
        case td @ TraitDeclAST(name, tparams, methods, _, assocs, _, tBounds) =>
          if !traits.contains(name) then
            traits(name) = TraitInfo(name, tparams, methods, assocs, tBounds)
            importedTraitNames += name
            registerTraitOperatorEntries(name, methods, td)
        case StructDeclAST(name, fields, typeParams, _, _, _, _) if typeParams.isEmpty =>
          // Best-effort: register the struct with resolved fields so cross-
          // sibling field accesses (e.g. atoms.lsysl reading `inp.source`
          // when Input is in parsyl.lsysl) work during body analysis.
          // Falls back to a Nil-fields placeholder if any field type can't
          // resolve yet — the post-pass-1 hook re-runs and may complete it.
          val existing = structTypes.get(name)
          val needsFill = existing.forall(_.fields.isEmpty)
          if needsFill then
            val resolvedFields = scala.util.Try(fields.map { case (fn, ft, _) =>
              (fn, resolveType(ft))
            }).getOrElse(Nil)
            structTypes(name) = SyslType.StructType(name, resolvedFields)
        case DataEnumDeclAST(name, variants, typeParams, _, _, _) if typeParams.isEmpty =>
          // Best-effort: resolve variant fields too. Same fallback as struct.
          val existing = dataEnumTypes.get(name)
          val needsFill = existing.forall(_.variants.forall(_._2.isEmpty))
          if needsFill then
            val resolvedVariants = variants.map { case EnumVariantAST(vname, vfields) =>
              val resolved = scala.util.Try(vfields.map { case (fn, ft) =>
                (fn, resolveType(ft))
              }).getOrElse(Nil)
              (vname, resolved)
            }
            dataEnumTypes(name) = SyslType.EnumType(name, resolvedVariants)
            for (EnumVariantAST(vname, _), idx) <- variants.zipWithIndex do
              if !variantToEnum.contains(vname) && !genericVariantToEnum.contains(vname) then
                variantToEnum(vname) = (dataEnumTypes(name), idx)
        case TypeAliasDeclAST(name, target, typeParams, _, isNew, range, predicate, _, _) if typeParams.isEmpty =>
          if !typeAliases.contains(name) && !genericTypeAliases.contains(name) then
            typeAliases(name) = (target, isNew, range, predicate)
        case fd @ FunDeclAST(name, params, returnType, _, _, tps, _, attrs, isDef, isParameterless, _)
            if tps.isEmpty && !name.startsWith("__") =>
          // Non-generic free functions (including struct methods, parsed as
          // `Input_at_end` etc.) — pre-register a stub FunInfo so cross-
          // sibling references like `inp.at_end()` (which looks up
          // `functions(Input_at_end)`) and direct calls (like `literal(s)`
          // from operators.lsysl into atoms.lsysl) resolve. Param/return
          // types may still be placeholder-typed at first pre-register; the
          // post-pass-1 hook re-registers with real types after own pass 1
          // populates struct fields. Skip if the name conflicts with an
          // already-known function/template, or if signature resolution
          // fails (sibling types not yet registered — retry next iteration).
          if !functions.contains(name) && !genericTemplates.contains(name) then
            scala.util.Try {
              val paramTypes = params.map(p => (p.name, resolveType(p.typ)))
              val retType = returnType.map(resolveType).getOrElse(UnitType)
              val mangled = if shouldMangle(name) then mangleName(name) else name
              val isPure = attrs.exists(_.name == "pure")
              functions(name) = FunInfo(
                mangled, paramTypes, retType, isDef, isPure,
                isParameterless = isParameterless,
              )
              externalSymbols += name
              importedSiblingFreeFnStubKeys += name
            }
        case impl @ ImplDeclAST(traitName, implTypeParams, targetTypes, methods, _, _, _, implTBounds) =>
          val alreadyHas = implTemplates.getOrElse(traitName, Nil).exists(t =>
            t.typeParams == implTypeParams && t.targetPatterns == targetTypes)
          if !alreadyHas then
            if implTypeParams.nonEmpty then
              implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += ImplTemplate(
                typeParams = implTypeParams,
                targetPatterns = targetTypes,
                resolvedConcrete = None,
                methods = mutable.LinkedHashMap.empty,
                methodInfos = Nil,
                methodASTs = methods,
                definingModule = "",
                implDecl = Some(impl),
                typeBounds = implTBounds,
              )
              importedGenericImplKeys += ((traitName, implTypeParams, targetTypes))
            else
              // Concrete: best-effort resolve. Targets that don't resolve yet
              // (sibling hasn't pre-collected its type decls) are skipped; a
              // later call retries with more state. Build methods map by
              // mangling so enumerateImplCandidates can look up paramTypes
              // via `functions(mangled)` — empty methods map breaks dispatch.
              scala.util.Try(targetTypes.map(resolveType)).toOption.foreach { resolvedTargets =>
                val owningModuleMangled = currentModule.getOrElse("")
                val typeMangled = resolvedTargets.map(typeToMangled).mkString("_")
                val methodMap = mutable.LinkedHashMap.empty[String, String]
                // Also register a stub FunInfo per impl method using the trait
                // method's signature substituted with the impl's targets — so
                // enumerateImplCandidates' concrete branch can fetch paramTypes
                // from `functions(mangled)` and the dispatch comparison works.
                // The function body is owned by the sibling's compilation unit;
                // here we only need the type shape for dispatch.
                val trait_ = traits.get(traitName)
                for m <- methods do
                  val rawMangled = s"${traitName}_${m.name}_${typeMangled}"
                  val mangled =
                    if owningModuleMangled.nonEmpty && !neverMangle.contains(rawMangled) then
                      s"${owningModuleMangled}__${rawMangled}"
                    else rawMangled
                  methodMap(m.name) = mangled
                  if !functions.contains(mangled) then
                    trait_.foreach { ti =>
                      ti.methods.find(_.name == m.name).foreach { tm =>
                        scala.util.Try {
                          val savedEnv = typeEnv
                          typeEnv = typeEnv ++ ti.typeParams.zip(resolvedTargets).toMap
                          try
                            val paramTypes = tm.params.map(p => (p.name, resolveType(p.typ)))
                            val retType = resolveType(tm.returnType)
                            functions(mangled) = FunInfo(mangled, paramTypes, retType)
                            externalSymbols += mangled
                            importedConcreteImplStubFunctions += mangled
                          finally typeEnv = savedEnv
                        }
                      }
                    }
                implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += ImplTemplate(
                  typeParams = Nil,
                  targetPatterns = targetTypes,
                  resolvedConcrete = Some(resolvedTargets),
                  methods = methodMap,
                  methodInfos = Nil,
                  methodASTs = Nil,
                  definingModule = "",
                  implDecl = Some(impl),
                  typeBounds = implTBounds,
                )
                importedConcreteImplKeys += ((traitName, resolvedTargets))
              }
        case VarDeclAST(name, typOpt, init, _, isMutable, attrs, _, isConst) =>
          // Module-level `val` / `const`: register a forward stub in globalScope
          // so cross-sibling `VarRefAST(name)` lookups don't throw during
          // pre-collection. Without this, two siblings that each define a const
          // (or `val literal`) the other references form an unbreakable mutual-
          // undefined cycle in Step 4b's fix-point loop — convergence stalls,
          // both files miss the packageMetaCache, and Step 5 surfaces a
          // misleading `undefined variable` from a third file that depends on
          // the cascade victim.
          //
          // Type is computed conservatively: explicit annotation wins; otherwise
          // we inspect the initializer and only register if we can pin the type
          // exactly (literal int → I32/I64 by the same rule as IntLitAST in
          // analyzeExpr; literal bool → BoolType; const-fold over already-known
          // forward consts). Anything else is left for the own-file pass — a
          // wrong stub type poisons type-checking far away (e.g. an int param
          // mistakenly fed an i64 forward-stub fails with "expects int, got i64").
          if !globalScope.contains(name) && !attrs.exists(_.name == "address") then
            scala.util.Try {
              val stubInfo: Option[(SyslType, Option[Long])] = typOpt match
                case Some(t) =>
                  val rt = resolveType(t)
                  Some((rt, tryConstEvalInit(init)))
                case None =>
                  initStubType(init).map(t => (t, tryConstEvalInit(init)))
              stubInfo.foreach { case (resolvedType, foldedOpt) =>
                val mangledName = if shouldMangle(name) then mangleName(name) else name
                val isGhost = attrs.exists(_.name == "ghost")
                globalScope(name) = SymInfo(mangledName, resolvedType, mutable = isMutable, isConst = isConst, isGhost = isGhost)
                externalSymbols += name
                foldedOpt.foreach { v =>
                  compileTimeConstants(name) = v
                  compileTimeConstants(mangledName) = v
                }
              }
            }
        case _ => ()

  /** Type stub for sibling forward-decl: only return a type when we can pin it
   *  exactly from the AST. Returns None for anything we'd have to guess at. */
  private def initStubType(init: ExpressionAST): Option[SyslType] =
    init match
      case IntLitAST(n) =>
        Some(if n > 0xFFFFFFFFL || n < -0x80000000L then IntType(64) else IntType(32))
      case TypedIntLitAST(_, typeName) =>
        scala.util.Try(resolveType(NamedTypeAST(typeName))).toOption
      case BoolLitAST(_) => Some(BoolType)
      case UnaryAST("-", inner) => initStubType(inner)
      case BinaryAST(l, _, r) =>
        // Conservative: only return a type when both sides agree.
        (initStubType(l), initStubType(r)) match
          case (Some(lt), Some(rt)) if lt == rt => Some(lt)
          case _ => None
      case _ => None

  /** Best-effort literal evaluation for sibling forward-decl pre-registration. */
  private def tryConstEvalInit(init: ExpressionAST): Option[Long] =
    init match
      case IntLitAST(n) => Some(n)
      case BoolLitAST(b) => Some(if b then 1L else 0L)
      case UnaryAST("-", IntLitAST(n)) => Some(-n)
      case BinaryAST(l, op, r) =>
        for li <- tryConstEvalInit(l); ri <- tryConstEvalInit(r) yield op match
          case "+" => li + ri
          case "-" => li - ri
          case "*" => li * ri
          case "/" => if ri != 0 then li / ri else 0L
          case "%" => if ri != 0 then li % ri else 0L
          case "<<" => li << ri
          case ">>" => li >> ri
          case "&" => li & ri
          case "|" => li | ri
          case "^" => li ^ ri
          case _ => 0L
      case VarRefAST(n) if compileTimeConstants.contains(n) => Some(compileTimeConstants(n))
      case _ => None

  def analyze(programIn: ProgramAST): TProgram =
    // Pre-pass 0: extract module name, then lower extension blocks. Module
    // extraction has to come first so each ExtensionEntry gets the right
    // `definingModule` for visibility checks. Lowering replaces every
    // ExtensionDeclAST with one synthesized free FunDeclAST per method, so
    // the rest of the pipeline can register/analyze them as if the user had
    // written `__ext_<recvType>__<method>(recv: T, ...) -> R = body` by hand.
    for decl <- programIn.decls do
      decl match
        case ModuleDeclAST(path) => currentModule = Some(path.mkString("_"))
        case _ =>
    // Own module's extensions are always visible. Imported modules are added
    // by `registerImport`; the empty sentinel handles unmoduled compilation
    // units where same-unit extensions still need to dispatch.
    currentModule.foreach(visibleExtensionModules.add)
    val (loweredDecls, extEntries) = lowerExtensions(programIn.decls)
    for entry <- extEntries do
      val bucket = extensionsByMethod.getOrElseUpdate(entry.methodName, mutable.ListBuffer.empty)
      // Dedup against entries already mirrored from same-module siblings
      // (driver passes meta.extensions through to siblings now). Without this,
      // a sibling-file extension would already be in the bucket and the local
      // lowering would re-add the same (definingModule, mangledFnName) pair,
      // making tryExtensionDispatch report it as ambiguous.
      if !bucket.exists(e => e.definingModule == entry.definingModule && e.mangledFnName == entry.mangledFnName) then
        bucket += entry
    val program = ProgramAST(loweredDecls)

    // Pass 0: forward-declare all type names so recursive references resolve.
    // Struct and enum names are registered as placeholder types; fields are
    // resolved in the next pass once all names are visible.
    //
    // Same-file duplicate struct/enum declarations are still rejected
    // (`pass0*` set tracks own-file decls). Cross-sibling pre-registration
    // may have already placed a placeholder in `structTypes`/`dataEnumTypes`;
    // that's allowed — the own file's declaration wins and pass 0.5 fills
    // the real fields. Without this tolerance, two-file modules where each
    // sibling pre-registers the other's types throw spurious duplicates.
    val pass0Structs = mutable.HashSet[String]()
    val pass0Enums = mutable.HashSet[String]()
    for decl <- program.decls do
      decl match
        case sd @ StructDeclAST(name, _, typeParams, attrs, _, _, _) if typeParams.isEmpty =>
          if pass0Structs.contains(name) then
            throw AnalysisError(s"duplicate struct: '$name'", decl)
          pass0Structs += name
          structTypes(name) = SyslType.StructType(name, Nil) // placeholder — fields filled below
          // δ.5: track ghost-marked struct types so the discipline check (in
          // validateGhostDiscipline) can reject real-code construction.
          if attrs.exists(_.name == "ghost") then ghostTypes += name
        case ed @ DataEnumDeclAST(name, _, typeParams, attrs, _, _) if typeParams.isEmpty =>
          if pass0Enums.contains(name) then
            throw AnalysisError(s"duplicate enum: '$name'", decl)
          pass0Enums += name
          dataEnumTypes(name) = SyslType.EnumType(name, Nil) // placeholder — variants filled below
          if attrs.exists(_.name == "ghost") then ghostTypes += name
        case ee @ EnumDeclAST(name, _, attrs) =>
          if attrs.exists(_.name == "ghost") then ghostTypes += name
        case _ => ()

    // Extract module path for name mangling
    for decl <- program.decls do
      decl match
        case ModuleDeclAST(path) =>
          currentModule = Some(path.mkString("_"))
        case _ =>

    // Stage F.5 — record locally-defined types for the orphan rule. Done eagerly so the
    // impl-registration pass can consult these maps without ordering surprises.
    val curMod = currentModule.getOrElse("")
    if curMod.nonEmpty then
      for decl <- program.decls do
        decl match
          case StructDeclAST(name, _, _, _, _, _, _)        => typeDefiningModule(name) = curMod
          case DataEnumDeclAST(name, _, _, _, _, _)         => typeDefiningModule(name) = curMod
          case EnumDeclAST(name, _, _)                => typeDefiningModule(name) = curMod
          case InterfaceDeclAST(name, _, _, _)        => typeDefiningModule(name) = curMod
          case TypeAliasDeclAST(name, _, _, _, _, _, _, _, _) => typeDefiningModule(name) = curMod
          case TraitDeclAST(name, _, _, _, _, _, _)      => traitDefiningModule(name) = curMod
          case _ => ()

    // Pass 0.5: resolve struct and data-enum FIELDS before any function signature.
    // Functions declared before their referenced structs/enums would otherwise capture
    // a placeholder with empty fields (sizeOf = 0), breaking backends that read typ.sizeOf
    // on a TCall's return-typed temp. Two iterations: first pass resolves each declaration's
    // fields in source order (forward struct-to-struct refs still see placeholders); second
    // pass re-resolves so captured references inside struct fields point to fully-filled types.
    // Pre-register simple (no-payload) enums so data-enum variant fields and
    // struct fields can reference them during the resolveStructsAndEnums passes
    // below. The main declaration pass (~line 782) repopulates simpleEnumTypes
    // with the same value plus the variant→enum bindings; this just gets the
    // type known to `resolveType` before any field resolution runs.
    //
    // Same logic for generic type aliases: a struct field whose declared type
    // is `Alias[T]` would otherwise hit "'Alias' is not a generic type"
    // because the alias is only registered in `genericTypeAliases` during the
    // main first pass. The main pass re-registers idempotently; this just
    // gets the alias visible to `resolveType` during struct/enum field
    // resolution. The duplicate-check in the main pass tolerates the
    // pre-seeded entry (see ~line 2197).
    for decl <- program.decls do
      decl match
        case EnumDeclAST(name, members, _) =>
          if !simpleEnumTypes.contains(name) then
            val variants = members.map((vname, _) => (vname, Nil: List[(String, SyslType)]))
            simpleEnumTypes(name) = SyslType.EnumType(name, variants)
        case TypeAliasDeclAST(name, target, tparams, _, isNew, _, _, defs, bounds) if tparams.nonEmpty =>
          if !genericTypeAliases.contains(name) && !typeAliases.contains(name) then
            genericTypeAliases(name) = (tparams, target, isNew)
            if defs.nonEmpty then genericTypeAliasDefaults(name) = defs
            if bounds.nonEmpty then genericTypeAliasBounds(name) = bounds
        // Pre-seed interface placeholders so a struct field declared as
        // `field: SomeInterface` resolves during `resolveStructsAndEnums`
        // below. The main declaration pass repopulates with the real method
        // list; the duplicate-check there tolerates a placeholder entry.
        case InterfaceDeclAST(name, _, _, _) =>
          if !interfaceTypes.contains(name) then
            interfaceTypes(name) = SyslType.InterfaceType(name, Nil)
        case _ => ()

    def resolveStructsAndEnums(): Unit =
      for decl <- program.decls do
        decl match
          case StructDeclAST(name, fields, typeParams, _, invariants, _, _) if typeParams.isEmpty =>
            if !genericStructs.contains(name) then
              val resolvedFields = fields.map((n, t, _) => (n, resolveType(t)))
              val volSet = fields.zipWithIndex.collect { case ((_, _, true), i) => i }.toSet
              structTypes(name) = SyslType.StructType(name, resolvedFields, volSet)
              if invariants.nonEmpty then structInvariants(name) = invariants
          case DataEnumDeclAST(name, variants, typeParams, _, _, _) if typeParams.isEmpty =>
            val resolvedVariants = variants.map { case EnumVariantAST(vname, fields) =>
              val resolvedFields = fields.map((fname, ftype) => (fname, resolveType(ftype)))
              (vname, resolvedFields)
            }
            val et: SyslType.EnumType = SyslType.EnumType(name, resolvedVariants)
            dataEnumTypes(name) = et
            for ((vname, _), idx) <- resolvedVariants.zipWithIndex do
              variantToEnum(vname) = (et, idx)
          case _ => ()
    resolveStructsAndEnums()
    resolveStructsAndEnums() // second pass: fix forward struct-to-struct refs in field types

    // First pass: register all functions and globals
    for decl <- program.decls do
      decl match
        case _: ModuleDeclAST => // metadata only
        case _: ImportDeclAST => // handled later
        case ExternFuncDeclAST(name, params, returnType, _) =>
          if !functions.contains(name) && !builtinFunctions.contains(name) then
            val paramTypes = params.map(p => (p.name, resolveType(p.typ)))
            val retType = returnType.map(resolveType).getOrElse(UnitType)
            functions(name) = FunInfo(name, paramTypes, retType)
            externalSymbols += name
          // else: already registered from same-module sibling or import — skip
        case ExternVarDeclAST(name, typ, _) =>
          if !globalScope.contains(name) then
            val resolved = resolveType(typ)
            globalScope(name) = SymInfo(name, resolved, mutable = false)
            externalSymbols += name
          // else: already registered from same-module sibling or import — skip
        case sd @ StructDeclAST(name, fields, typeParams, _, invariants, _, _) =>
          if typeParams.nonEmpty then
            // Generic struct: store as template, don't resolve fields yet
            if genericStructs.contains(name) then
              throw AnalysisError(s"duplicate struct: '$name'", decl)
            genericStructs(name) = sd
          else
            if genericStructs.contains(name) then throw AnalysisError(s"duplicate struct: '$name'", decl)
            // Fields already resolved by pass 0.5 (resolveStructsAndEnums).
            // Do not re-assign structTypes here — that would invalidate references captured
            // by function signatures processed later in this same source-order loop.
        case fd @ FunDeclAST(name, params, returnType, _, _, typeParams, _, _, isDef, _, _) =>
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
          // Validate mode constraints: no defaults on out/inout; no out/inout on self.
          for p <- params do
            if p.mode != ParamMode.In && p.default.isDefined then
              throw AnalysisError(s"parameter '${p.name}' of '$name' is ${p.mode.toString.toLowerCase} and cannot have a default value", decl)
            if p.mode != ParamMode.In && p.name == "__self__" then
              throw AnalysisError(s"method receiver 'self' of '$name' cannot be '${p.mode.toString.toLowerCase}'", decl)
          if typeParams.nonEmpty then
            // Generic function: store as template, don't resolve types yet
            if genericTemplates.contains(name) || functions.contains(name) then
              throw AnalysisError(s"duplicate function: '$name'", decl)
            if params.exists(_.default.isDefined) then
              throw AnalysisError(s"generic function '$name' cannot have default parameter values (not yet supported)", decl)
            if params.exists(_.mode != ParamMode.In) then
              throw AnalysisError(s"generic function '$name' cannot have 'out'/'inout' parameters (not yet supported)", decl)
            genericTemplates(name) = fd
          else
            // For Out/Inout params, the call-side signature carries `*T` (hidden pointer);
            // the body sees the inner `T` with autoIndirect via SymInfo. Store modes parallel
            // to params so checkArgs and SMETA can consult them.
            // For `=> T` (call-by-name) params, the storage type is `() -> T`. Mode
            // must be `In` — by-name is incompatible with out/inout (the thunk has
            // no lvalue to write back to). Track by-name positions in `byNameFlags`
            // so the call-site auto-wrap and body auto-eval paths can find them.
            val byNameFlags = params.map(_.typ.isInstanceOf[ByNameTypeAST])
            val anyByName = byNameFlags.exists(identity)
            for (p <- params) do
              if p.typ.isInstanceOf[ByNameTypeAST] then
                if p.mode != ParamMode.In then
                  throw AnalysisError(s"parameter '${p.name}' of '$name' is by-name (`=> T`) and cannot also be '${p.mode.toString.toLowerCase}'", decl)
                if p.default.isDefined then
                  throw AnalysisError(s"parameter '${p.name}' of '$name' is by-name (`=> T`) and cannot have a default value", decl)
            val paramTypes = params.map { p =>
              val (effectiveAst, isByName) = p.typ match
                case ByNameTypeAST(inner) => (FuncTypeAST(Nil, inner), true)
                case other => (other, false)
              val inner = resolveType(effectiveAst)
              val sigType = p.mode match
                case ParamMode.In => inner
                case ParamMode.Out | ParamMode.Inout => PtrType(inner)
              (p.name, sigType)
            }
            val paramModes = params.map(_.mode)
            val retType = returnType.map(resolveType).getOrElse(UnitType)
            if functions.contains(name) || genericTemplates.contains(name) then
              throw AnalysisError(s"duplicate function: '$name'", decl)
            val mangledName = if shouldMangle(name) then mangleName(name) else name
            val isPureAttr = fd.attributes.exists(_.name == "pure")
            val isGhost = fd.attributes.exists(_.name == "ghost")
            // Extract `#reads(a, b)` / `#writes(c)` raw identifier lists. Validation that
            // each name resolves to a module-level mutable var is deferred to validateEffects
            // (run after the body is analyzed, so all relevant globals are in scope).
            // Each attribute may appear multiple times; results are unioned. `#pure` cannot
            // be combined with explicit `#reads`/`#writes` (it already implies both empty).
            def extractIdentList(attrName: String): Option[Set[String]] =
              val matching = fd.attributes.filter(_.name == attrName)
              if matching.isEmpty then None
              else Some(matching.flatMap { attr =>
                attr.args.map {
                  case AttrPositional(AttrLitIdent(n)) => n
                  case other => throw AnalysisError(s"#$attrName on '$name' expects identifier arguments, got $other", fd)
                }
              }.toSet)
            val readsSet = extractIdentList("reads")
            val writesSet = extractIdentList("writes")
            // Expression functions (`def ...`) are implicitly `#pure` — they exist to serve
            // as proof-friendly abstraction predicates, so side effects and global mutation
            // are never appropriate. `#ghost def` stays on the ghost track (ghost is already
            // restricted and is stripped before codegen).
            if isDef && (readsSet.isDefined || writesSet.isDefined) then
              throw AnalysisError(s"'def $name' cannot carry #reads/#writes — def functions are implicitly pure", fd)
            val isPure = isPureAttr || (isDef && !isGhost)
            if isPureAttr && (readsSet.isDefined || writesSet.isDefined) then
              throw AnalysisError(s"#pure on '$name' cannot be combined with #reads/#writes (it already implies both empty)", fd)
            if isGhost && isPureAttr then
              throw AnalysisError(s"#ghost on '$name' is incompatible with #pure (ghost code is removed before codegen, so #pure is meaningless)", fd)
            if isGhost && (readsSet.isDefined || writesSet.isDefined) then
              throw AnalysisError(s"#ghost on '$name' is incompatible with #reads/#writes (ghost code is removed before codegen)", fd)
            // Collision check: a parameterless decl and a zero-arg decl with the
            // same name are ambiguous at the call site (`foo` could mean either),
            // so reject. (Two zero-arg or two parameterless decls with the same
            // name are caught by the regular duplicate-function check above.)
            if fd.isParameterless && fd.typeParams.nonEmpty then
              throw AnalysisError(s"parameterless function '$name' cannot be generic", decl)
            functions(name) = FunInfo(mangledName, paramTypes, retType, isDef && params.isEmpty, isPure, paramModes, readsSet, writesSet, isGhost, if anyByName then byNameFlags else Nil, fd.isParameterless)
            // Record #deprecated info. The lexer's StringLit carrier is
            // byte-form (each Char is one UTF-8 byte); decode for the host
            // diagnostic so Unicode reasons render correctly when printed.
            for attr <- fd.attributes if attr.name == "deprecated" do
              val reason = attr.args.collectFirst { case AttrPositional(AttrLitString(s)) =>
                new String(s.getBytes("ISO-8859-1"), "UTF-8")
              }
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
        case de @ DataEnumDeclAST(name, variants, typeParams, _, _, _) =>
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
            // Variants already resolved by pass 0.5 (resolveStructsAndEnums).
            // Do not re-assign dataEnumTypes here — same reason as StructDeclAST above.
        case TypeAliasDeclAST(name, target, tparams, _, isNew, range, predicate, defaults, bounds) =>
          // The struct-field pre-pass (~line 1976) seeds generic type aliases
          // into `genericTypeAliases` so struct fields can name them. When the
          // main pass revisits the SAME decl, skip the duplicate diagnostic so
          // the pre-seeded entry doesn't trip it.
          val preSeededGeneric = tparams.nonEmpty && genericTypeAliases.get(name)
            .exists(_ == ((tparams, target, isNew)))
          if !preSeededGeneric && (typeAliases.contains(name) || genericTypeAliases.contains(name)) then
            throw AnalysisError(s"duplicate type alias: '$name'", decl)
          if tparams.nonEmpty then
            // `within` and `where` need scalar ordering / operations on T; both are
            // unavailable on a bare type parameter without trait bounds. `new` does NOT
            // need either — it's just nominal identity per instantiation, fully supported
            // by the existing monomorphization machinery.
            if range.nonEmpty || predicate.nonEmpty then
              throw AnalysisError(s"generic type aliases cannot use 'within' or 'where': '$name'", decl)
            genericTypeAliases(name) = (tparams, target, isNew)
            if defaults.nonEmpty then genericTypeAliasDefaults(name) = defaults
            if bounds.nonEmpty then genericTypeAliasBounds(name) = bounds
          else
            typeAliases(name) = (target, isNew, range, predicate)
        case TraitDeclAST(name, tparams, methods, _, assocs, _, tBounds) =>
          // Tolerate sibling-pre-registered traits (importedTraitNames) — those
          // came from this same trait decl via cross-sibling forward-decl pass
          // and are structurally identical. Without this, two-file modules
          // where each sibling pre-registers the other's traits throw a
          // spurious duplicate when the defining file's main pass reaches the
          // decl. A real same-file duplicate (`importedTraitNames` doesn't
          // contain it) is still rejected.
          if traits.contains(name) && !importedTraitNames.contains(name) then
            throw AnalysisError(s"duplicate trait: '$name'", decl)
          if !traits.contains(name) then
            // Check no duplicate method names within the trait
            val methodNames = methods.map(_.name)
            if methodNames.distinct.length != methodNames.length then
              throw AnalysisError(s"duplicate method names in trait '$name'")
            if tparams.distinct.length != tparams.length then
              throw AnalysisError(s"duplicate type parameter names in trait '$name'")
            // Validate associated-type declarations: unique names, no clash with
            // type parameters, and no clash with the reserved `T::Attr` names used
            // for enum / within-int introspection (First, Last, Range, Image,
            // Value, Valid, Pos, Val, Succ, Pred). Until Phase A3 wires up
            // projection resolution, bounds are accepted but not enforced.
            val assocNames = assocs.map(_.name)
            if assocNames.distinct.length != assocNames.length then
              throw AnalysisError(s"duplicate associated-type names in trait '$name'")
            for a <- assocs do
              if tparams.contains(a.name) then
                throw AnalysisError(s"associated type '${a.name}' shadows trait type parameter in '$name'", a)
              if methodNames.contains(a.name) then
                throw AnalysisError(s"associated type '${a.name}' shadows method name in trait '$name'", a)
              if SyslAnalyzer.ReservedTypeAttrNames.contains(a.name) then
                throw AnalysisError(s"associated type '${a.name}' uses a reserved attribute name in trait '$name'; rename to avoid clashing with the built-in T::${a.name} introspection attribute", a)
            // Validate that every name on the LHS of typeBounds is one of the
            // declared type parameters; flag-but-don't-fail if a bound names an
            // unknown trait (resolution happens at impl registration to avoid
            // ordering pitfalls between trait + impl decls).
            for (bn, _) <- tBounds do
              if !tparams.contains(bn) then
                throw AnalysisError(s"trait '$name' declares bound on unknown type parameter '$bn'", decl)
            traits(name) = TraitInfo(name, tparams, methods, assocs, tBounds)
            registerTraitOperatorEntries(name, methods, decl)
          else
            // Already registered (sibling pre-collect); the trait now owns
            // this unit (this is the defining unit's main pass) so clear the
            // imported flag so getTraitDecls includes it in this unit's
            // perFileMeta.
            importedTraitNames -= name
        case InterfaceDeclAST(name, methodASTs, embeddedNames, _) =>
          // Tolerate a placeholder pre-seeded by pass 0.5 (empty methods list)
          // — pass 0.5 pre-registers interface names so struct fields typed by
          // them resolve before this pass runs. A real duplicate (non-empty
          // methods list) still throws.
          val preSeededInterface = interfaceTypes.get(name).exists(_.methods.isEmpty)
          if interfaceTypes.contains(name) && !preSeededInterface then throw AnalysisError(s"duplicate interface: '$name'", decl)
          // Resolve embedded interfaces and flatten methods
          val embeddedMethods = embeddedNames.flatMap { en =>
            interfaceTypes.getOrElse(en, throw AnalysisError(s"embedded interface '$en' not found", decl)).methods
          }
          val ownMethods = methodASTs.map { m =>
            val paramTypes = m.params.map(p => resolveType(p.typ))
            val retType = resolveType(m.returnType)
            // Resolve raw names in #reads/#writes through globalScope so subset checks at
            // boxing/dispatch sites compare mangled names directly.
            val resolvedEff = if m.effects.isPure || (m.effects.reads.isEmpty && m.effects.writes.isEmpty) then m.effects
              else
                def resolveOne(n: String, kind: String): String =
                  globalScope.get(n) match
                    case Some(sym) if sym.mutable && !sym.isConst => sym.name
                    case Some(_) => throw AnalysisError(s"#$kind on interface '$name' method '${m.name}' references '$n' which is not mutable")
                    case None    => throw AnalysisError(s"#$kind on interface '$name' method '${m.name}' references unknown global '$n'")
                FuncEffects(m.effects.isPure, m.effects.reads.map(_.map(resolveOne(_, "reads"))), m.effects.writes.map(_.map(resolveOne(_, "writes"))))
            (m.name, paramTypes, retType, resolvedEff)
          }
          val allMethods = embeddedMethods ++ ownMethods
          // Check for duplicate method names
          val names = allMethods.map(_._1)
          if names.distinct.length != names.length then
            throw AnalysisError(s"duplicate method names in interface '$name'")
          interfaceTypes(name) = SyslType.InterfaceType(name, allMethods)
        case _: ImplDeclAST =>
          // Deferred to registerImpls after all traits are known
          ()
        case VarDeclAST(name, _, _, _, isMutable, attrs, _, isConst) =>
          if globalScope.contains(name) then
            throw AnalysisError(s"duplicate global: '$name'", decl)
          // Pre-register the name so effect signatures on same-unit interface methods /
          // function types / callbacks can resolve it during Pass 1. The real SymInfo
          // (with correct type) is written by `analyzeRegularVarDecl` in Pass 2 and
          // overwrites this placeholder. Fields used by effect-name resolution (`mutable`
          // and `isConst`) are set correctly from the AST here.
          val mangled = if shouldMangle(name) then mangleName(name) else name
          val isGhost = attrs.exists(_.name == "ghost")
          globalScope(name) = SymInfo(mangled, SyslType.UnitType, isMutable, isConst = isConst, isGhost = isGhost)
        case _: StaticAssertDeclAST => // evaluated in pass 2
        case _ => // other decls (e.g. CondDeclAST) handled elsewhere

    // Pre-pass: evaluate module-level `const` initializers eagerly so they are available
    // to `within` range bounds and other contexts that resolve types before function bodies.
    for decl <- program.decls do
      decl match
        case VarDeclAST(name, typOpt, init, _, _, _, _, true) =>
          val declType = typOpt.map(resolveType).getOrElse(I32)
          if !declType.isIntegral then
            throw AnalysisError(s"const '$name' must have an integer type (found $declType); float/string/aggregate const is not yet supported", decl)
          evalConstExprAST(init) match
            case Some(v) =>
              val masked = maskToType(v, declType)
              compileTimeConstants(name) = masked
              val mangled = if shouldMangle(name) then mangleName(name) else name
              compileTimeConstants(mangled) = masked
              // Also publish the const in globalScope so name-based lookups during this pass
              // (e.g. struct invariant validation) can find it.
              if !globalScope.contains(name) then
                globalScope(name) = SymInfo(mangled, declType, mutable = false, isConst = true)
            case None =>
              throw AnalysisError(s"const '$name' initializer is not compile-time evaluable", decl)
        case _ =>

    // Validate all struct invariants now that constants are registered — catches wrong field
    // names, unknown identifiers, and non-bool result types before any mutation site sees them.
    for (structName, invariants) <- structInvariants do
      val st = structTypes(structName)
      validateStructInvariants(structName, st.fields, invariants)

    // Re-register sibling forward-decls now that own types/aliases are in
    // scope. Sibling concrete impls that reference own generic aliases
    // (e.g. operators.lsysl declares `impl Peek[string, Parser[unit]]`
    // where Parser is in parsyl.lsysl) failed to resolve in the driver's
    // pre-pass; with own pass 1 done, they can now register. Idempotent —
    // skip if already registered.
    // Drop ONLY the stale impl entries that were registered by the sibling
    // pre-register against type-placeholders (tracked via the stub-functions
    // set). The post-hook re-registers them with real types now that own
    // pass 1 has filled in struct fields. Cross-module imports (which use
    // the proper symbol path) leave importedConcreteImplStubFunctions empty
    // for their entries — they're untouched here.
    val staleStubs = importedConcreteImplStubFunctions.toList
    if staleStubs.nonEmpty then
      // Locate impl entries whose mangled methods are stale stubs and drop them.
      for (traitName, buf) <- implTemplates do
        buf.filterInPlace(t =>
          t.typeParams.nonEmpty || !t.methods.values.exists(staleStubs.contains))
      // Also drop the stale FunInfo stubs and their imported-key entries.
      for fn <- staleStubs do
        functions.remove(fn)
        externalSymbols -= fn
      importedConcreteImplStubFunctions.clear()
      // Drop concrete impl keys whose mangled name was a stale stub (others
      // — proper cross-module imports — keep their entries).
      val stubSet = staleStubs.toSet
      importedConcreteImplKeys.filterInPlace { case (traitName, _) =>
        // Conservative: drop a key only if the corresponding impl is gone.
        // Re-registration adds a fresh key.
        implTemplates.getOrElse(traitName, Nil).exists(_.methods.values.exists(stubSet.contains))
      }
    // Same cleanup for free-fn stubs: drop so the re-pre re-registers with
    // real types in scope (sibling pre-register may have latched onto
    // placeholder struct types in the first pass).
    val staleFreeFnStubs = importedSiblingFreeFnStubKeys.toList
    if staleFreeFnStubs.nonEmpty then
      for k <- staleFreeFnStubs do
        functions.remove(k)
        externalSymbols -= k
      importedSiblingFreeFnStubKeys.clear()
    for sib <- siblingForwardDecls do
      registerSiblingForwardDeclsFrom(sib)

    // Intermediate pass: register impl blocks (traits now known; signatures may reference traits)
    for decl <- program.decls do
      decl match
        case impl @ ImplDeclAST(traitName, implTypeParams, targetTypes, methods, _, assocBindings, _, implTypeBounds) =>
          val trait_ = traits.getOrElse(traitName,
            throw AnalysisError(s"impl references unknown trait '$traitName'", decl))
          if targetTypes.length != trait_.typeParams.length then
            throw AnalysisError(s"impl of '$traitName' has ${targetTypes.length} target type(s) but the trait has ${trait_.typeParams.length} type parameter(s)", decl)
          // Check required methods are all provided
          val providedNames = methods.map(_.name).toSet
          val missing = trait_.methods.filter(m => m.body.isEmpty && !providedNames.contains(m.name))
          if missing.nonEmpty then
            throw AnalysisError(s"impl ${traitName}[${targetTypes.mkString(", ")}] missing required method(s): ${missing.map(_.name).mkString(", ")}")
          // Check each impl method exists in trait
          for m <- methods do
            if !trait_.methods.exists(_.name == m.name) then
              throw AnalysisError(s"impl method '${m.name}' is not declared in trait '$traitName'")
          // Validate associated-type bindings (Phase A1):
          //   - every assoc type the trait declared must be bound;
          //   - every binding must name a declared assoc;
          //   - no duplicate binding names.
          val bindingNames = assocBindings.map(_.name)
          if bindingNames.distinct.length != bindingNames.length then
            throw AnalysisError(s"duplicate associated-type binding(s) in impl of '$traitName'", decl)
          val declaredAssocs = trait_.assocTypes.map(_.name).toSet
          for b <- assocBindings do
            if !declaredAssocs.contains(b.name) then
              throw AnalysisError(s"impl of '$traitName' binds undeclared associated type 'type ${b.name}'", b)
          val missingAssocs = trait_.assocTypes.map(_.name).filterNot(bindingNames.contains)
          if missingAssocs.nonEmpty then
            throw AnalysisError(s"impl of '$traitName' missing associated-type binding(s): ${missingAssocs.map("type " + _).mkString(", ")}", decl)
          // Phase C — enforce trait bounds declared on associated types.
          // For each assoc binding, look up the trait's declaration of that
          // assoc; for each declared bound trait, validate that the binding
          // target satisfies the bound (an impl exists). For now we enforce
          // this only on concrete impls; generic impls would need
          // instantiation-time deferral (their binding targets may reference
          // impl tvars) and are tracked as Phase C.2.
          if implTypeParams.isEmpty then
            val assocBoundsByName = trait_.assocTypes.map(a => (a.name, a.bounds)).toMap
            for b <- assocBindings do
              val bounds = assocBoundsByName.getOrElse(b.name, Nil)
              if bounds.nonEmpty then
                val resolvedBindingTarget = resolveType(b.target)
                for boundTrait <- bounds do
                  if !traits.contains(boundTrait) then
                    throw AnalysisError(s"associated type 'type ${b.name}' on trait '$traitName' references unknown trait '$boundTrait'", b)
                  val matched = implTemplates.getOrElse(boundTrait, Nil).exists { t =>
                    if t.typeParams.isEmpty then
                      t.resolvedConcrete.flatMap(_.headOption).contains(resolvedBindingTarget)
                    else
                      tryUnifyAll(t.targetPatterns.headOption.toList, List(resolvedBindingTarget), t.typeParams.toSet).isDefined
                  }
                  if !matched then
                    throw AnalysisError(s"impl of '$traitName' binds 'type ${b.name} = $resolvedBindingTarget', which does not satisfy bound '$boundTrait' declared on the associated type", b)
          // Phase C follow-up — enforce trait bounds declared on the trait's
          // own type parameters (e.g. `trait Container[T: Ord]`). For each
          // bounded trait tparam, the corresponding impl target must satisfy
          // each bound trait. Concrete impls check their resolved targets
          // directly; generic impls defer to `instantiateImpl` because a
          // target pattern may reference impl tvars whose substitution isn't
          // known until dispatch.
          //
          // Validate that each bound names a known trait up-front, regardless
          // of impl-tparam-ness, so a typo on `trait Container[T: Ord]` fails
          // at the first impl rather than at every dispatch site.
          for (tp, tBounds) <- trait_.typeBounds do
            for boundTrait <- tBounds do
              if !traits.contains(boundTrait) then
                throw AnalysisError(s"bound '$boundTrait' on trait '$traitName' type parameter '$tp' refers to unknown trait", decl)
          if implTypeParams.isEmpty then
            val resolvedTargetsForBoundCheck = targetTypes.map(resolveType)
            for ((tp, tgt) <- trait_.typeParams.zip(resolvedTargetsForBoundCheck)) do
              val tBounds = trait_.typeBounds.getOrElse(tp, Nil)
              for boundTrait <- tBounds do
                val matched = implTemplates.getOrElse(boundTrait, Nil).exists { t =>
                  if t.typeParams.isEmpty then
                    t.resolvedConcrete.flatMap(_.headOption).contains(tgt)
                  else
                    tryUnifyAll(t.targetPatterns.headOption.toList, List(tgt), t.typeParams.toSet).isDefined
                }
                if !matched then
                  throw AnalysisError(s"impl of '$traitName' for $tgt does not satisfy bound '$boundTrait' declared on type parameter '$tp'", decl)
          // Validate that any impl-level type-bound LHS names a declared impl
          // tparam, and that each bound names a known trait. Defer the
          // dispatch-time check itself to `instantiateImpl`.
          for (bn, bs) <- implTypeBounds do
            if !implTypeParams.contains(bn) then
              throw AnalysisError(s"impl declares bound on unknown type parameter '$bn'", decl)
            for bt <- bs do
              if !traits.contains(bt) then
                throw AnalysisError(s"bound '$bt' on impl type parameter '$bn' refers to unknown trait", decl)
          // Built-in prefix-sigil collision: when a trait method carries
          // `#operator(<sigil>)` for one of `-`, `!`, `~`, `*`, `&`, the impl's
          // first target pattern must NOT cover the sigil's natural built-in
          // domain (e.g. `impl Neg[bool]` for `#operator("!")` would steal
          // `!true`). Generic patterns (`Parser[A]`) and nominal aliases
          // (`type Meters = new int`) read as user-defined and pass freely;
          // only direct references to built-in scalar / pointer shapes fail.
          if targetTypes.nonEmpty then
            for sigilTraitMethod <- trait_.methods do
              val opAttrs = sigilTraitMethod.attributes.filter(a => a.name == "operator" || a.name == "op")
              for attr <- opAttrs.headOption do
                val sigil = extractOperatorSymbol(attr, sigilTraitMethod)
                if sigilTraitMethod.params.length == 1 && implOperandConflictsWithBuiltinPrefix(sigil, targetTypes.head) then
                  throw AnalysisError(
                    s"impl of '$traitName' for ${targetTypes.head} conflicts with built-in prefix '$sigil' on its natural type; pick a different operand type",
                    decl,
                  )
          if implTypeParams.nonEmpty then
            // Generic impl: defer signature checking + mangling to specialization time.
            // Every declared impl tvar must appear in at least one target pattern,
            // otherwise it's never bindable at dispatch time and the impl can't match
            // anything — fail fast with a clear diagnostic.
            val patternTvars = targetTypes.flatMap(collectNamedTypeNames).toSet
            val unused = implTypeParams.filterNot(patternTvars.contains)
            if unused.nonEmpty then
              throw AnalysisError(s"impl of '$traitName' declares unused type parameter(s) ${unused.mkString(", ")} (must appear in target patterns)", decl)
            // Coherence check (Stage F.5) walks here before we add to implTemplates.
            val newTemplate = ImplTemplate(
              typeParams = implTypeParams,
              targetPatterns = targetTypes,
              resolvedConcrete = None,
              methods = mutable.LinkedHashMap.empty,
              methodInfos = Nil,
              methodASTs = methods,
              definingModule = currentModule.getOrElse(""),
              implDecl = Some(impl),
              assocBindings = assocBindings,
              typeBounds = implTypeBounds,
            )
            checkOrphanRule(traitName, targetTypes, currentModule.getOrElse(""), decl)
            checkCoherence(traitName, newTemplate, decl)
            implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += newTemplate
          else
            // Concrete impl: resolve targets, register one mangled function per trait method.
            val resolvedTargets = targetTypes.map(resolveType)
            if findConcreteImpl(traitName, resolvedTargets.head).exists(t =>
                t.resolvedConcrete.contains(resolvedTargets)) then
              throw AnalysisError(s"duplicate impl: trait '$traitName' already implemented for ${resolvedTargets.mkString(", ")}", decl)
            // Mangle suffix: join all resolved targets with `_` so multi-param impls don't collide
            val typeMangled = resolvedTargets.map(typeToMangled).mkString("_")
            val methodMap = mutable.LinkedHashMap.empty[String, String]
            val infos = mutable.ListBuffer.empty[ImplMethodInfo]
            val savedEnv = typeEnv
            val savedAssocs = assocBindingsEnv
            typeEnv = trait_.typeParams.zip(resolvedTargets).toMap
            // Resolve impl's assoc bindings using the trait-param-substituted
            // typeEnv so a binding like `type Item = T` (referring to a generic
            // impl tvar) lands as the substituted concrete type. For non-generic
            // impls, this is just direct resolution.
            assocBindingsEnv = assocBindings.map(b => (b.name, resolveType(b.target))).toMap
            try
              for traitMethod <- trait_.methods do
                val rawMangled = s"${traitName}_${traitMethod.name}_${typeMangled}"
                val mangled = if shouldMangle(rawMangled) then mangleName(rawMangled) else rawMangled
                if functions.contains(mangled) then
                  throw AnalysisError(s"impl method collides with existing function '$mangled'", decl)
                val expectedParams = traitMethod.params.map(p => (p.name, resolveType(p.typ)))
                val expectedRet = resolveType(traitMethod.returnType)
                val providedOpt = methods.find(_.name == traitMethod.name)
                val (paramTypes, retType, body, synthesized) = providedOpt match
                  case Some(implMethod) =>
                    val pTypes = implMethod.params.map(p => (p.name, resolveType(p.typ)))
                    val r = implMethod.returnType.map(resolveType).getOrElse(UnitType)
                    if pTypes.map(_._2) != expectedParams.map(_._2) then
                      throw AnalysisError(s"impl method '${implMethod.name}' parameter types don't match trait: expected ${expectedParams.map(_._2).mkString("(", ", ", ")")}, got ${pTypes.map(_._2).mkString("(", ", ", ")")}", decl)
                    if r != expectedRet then
                      throw AnalysisError(s"impl method '${implMethod.name}' return type doesn't match trait: expected $expectedRet, got $r", decl)
                    (pTypes, r, implMethod.body, false)
                  case None =>
                    (expectedParams, expectedRet, traitMethod.body.get, true)
                functions(mangled) = FunInfo(mangled, paramTypes, retType)
                methodMap(traitMethod.name) = mangled
                infos += ImplMethodInfo(mangled, paramTypes, retType, body, isSynthesized = synthesized)
            finally
              typeEnv = savedEnv
              assocBindingsEnv = savedAssocs
            val newTemplate = ImplTemplate(
              typeParams = Nil,
              targetPatterns = targetTypes,
              resolvedConcrete = Some(resolvedTargets),
              methods = methodMap,
              methodInfos = infos.toList,
              methodASTs = Nil,
              definingModule = currentModule.getOrElse(""),
              implDecl = Some(impl),
              assocBindings = assocBindings,
              typeBounds = implTypeBounds,
            )
            checkOrphanRule(traitName, targetTypes, currentModule.getOrElse(""), decl)
            checkCoherence(traitName, newTemplate, decl)
            implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += newTemplate
        case _ =>

    // Re-resolve struct/enum fields one more time so any field captured at
    // pass 0.5 against a placeholder interface (empty methods) picks up the
    // now-fully-populated InterfaceType. Without this, `struct S { f: I }`
    // would dispatch through a zero-method `I` and fail with "interface I has
    // no method 'foo'" at the first call site.
    resolveStructsAndEnums()

    // Second pass: produce typed AST (skip generic templates; they're instantiated on demand)
    val tDecls = program.decls.flatMap {
      case f: FunDeclAST if f.typeParams.nonEmpty => Nil
      case s: StructDeclAST if s.typeParams.nonEmpty => Nil
      case e: DataEnumDeclAST if e.typeParams.nonEmpty => Nil
      case _: TraitDeclAST => Nil // traits emit nothing; only impls do
      case _: ExtensionDeclAST => Nil // Phase 1 stub: parsed but not yet analyzed
      case i: InterfaceDeclAST => List(TInterfaceDecl(i.name, interfaceTypes(i.name)))
      case impl: ImplDeclAST   => analyzeImplMethods(impl)
      case sa: StaticAssertDeclAST =>
        evalStaticAssert(sa)
        Nil
      case mi: ModuleInvariantDeclAST =>
        // Spec-only: validate that the expression is a bool reference over module-level
        // state, then drop. The WhyML backend reads the original AST directly. Other
        // backends never see this decl (analyzer returns no TDecl).
        scopeStack = new mutable.ArrayBuffer
        pushScope()
        try
          val tExpr = analyzeExpr(mi.expr)
          if tExpr.typ != BoolType then
            throw AnalysisError(s"module_invariant must be a bool expression, got ${tExpr.typ}")
        finally
          scopeStack = null
        Nil
      case d => List(analyzeDecl(d))
    }
    val allDecls = tDecls ++ specializedDecls.toList
    // Ghost strip pass: remove `#ghost var`/`#ghost fn` declarations from codegen output,
    // and inside every real function body, drop ghost-local decls, ghost-target assignments,
    // and contract checks that reference ghost state. Ghost discipline has already been
    // enforced in analyzeDecl, so here the pass is a pure rewrite with no error checks.
    val strippedDecls = stripGhostDecls(allDecls)
    TProgram(strippedDecls)

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

  /** Evaluate a `static_assert` at compile time and throw if the condition is false. */
  protected def evalStaticAssert(sa: StaticAssertDeclAST): Unit =
    // Analyze in a blank function scope so params/locals are inaccessible (module-scope only).
    val savedScope = scopeStack
    scopeStack = null
    val te = try analyzeExpr(sa.cond) finally scopeStack = savedScope
    if te.typ != BoolType then
      throw AnalysisError(s"static_assert condition must be bool, got ${te.typ}", sa)
    tryConstEval(te) match
      case Some(n) =>
        if n == 0 then
          val msg = sa.message.map(m => s": $m").getOrElse("")
          throw AnalysisError(s"static_assert failed$msg", sa)
      case None =>
        throw AnalysisError(s"static_assert condition is not compile-time evaluable", sa)

  protected def analyzeDecl(decl: DeclAST): TDecl =
    decl match
      case ModuleDeclAST(path) =>
        TModuleDecl(path)

      case ImportDeclAST(modulePath, _) =>
        TImportDecl(modulePath)

      case ExternFuncDeclAST(name, params, returnType, _) =>
        val paramTypes = params.map(p => resolveType(p.typ))
        val retType = returnType.map(resolveType).getOrElse(UnitType)
        TExternFuncDecl(name, paramTypes, retType)

      case ExternVarDeclAST(name, typ, _) =>
        TExternVarDecl(name, resolveType(typ))

      case StructDeclAST(name, _, _, _, _, _, _) =>
        val st = structTypes(name)
        TStructDecl(name, st.fields, st.volatileFields)

      case EnumDeclAST(name, _, _) =>
        val members = enumTypes(name).toList.sortBy(_._2)
        TEnumDecl(name, members)

      case DataEnumDeclAST(name, _, _, _, _, _) =>
        TDataEnumDecl(name, dataEnumTypes(name))

      case TypeAliasDeclAST(name, _, tparams, _, _, _, _, _, _) =>
        if tparams.nonEmpty then TTypeAliasDecl(name, UnitType) // generic alias: type-only, no codegen
        else TTypeAliasDecl(name, resolveType(NamedTypeAST(name))) // force resolution (and range validation)

      case fdAst @ FunDeclAST(name, params, _, body, isPrivate, _, _, attrs, _, _, _) =>
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
        for (((paramName, paramType), idx) <- funInfo.params.zipWithIndex) do
          val mode = funInfo.modeOf(idx)
          // For Out/Inout, the body-scope symbol sees the inner type with autoIndirect,
          // while the actual parameter storage is a `*T` the body auto-dereferences.
          val (bodyType, autoInd) = mode match
            case ParamMode.In => (paramType, false)
            case ParamMode.Out | ParamMode.Inout => paramType match
              case PtrType(inner) => (inner, true)
              case other => (other, false) // shouldn't happen — collectDecls wrapped it
          // For by-name params, the actual storage is `() -> T`, but the body sees
          // the symbol with type `T` (and isByName=true). VarRefAST resolution
          // handles the auto-call.
          val isByN = funInfo.isByNameAt(idx)
          val visibleType = bodyType match
            case FuncType(Nil, ret, _, _) if isByN => ret
            case other => other
          currentScope(paramName) = SymInfo(paramName, visibleType, true, autoIndirect = autoInd, isByName = isByN)
          // Auto-alias the implicit method receiver: `self` -> `__self__`
          // so method bodies can write `self.x` while the actual parameter
          // is named `__self__` to avoid conflicting with user-declared names.
          if paramName == "__self__" then
            currentScope("self") = SymInfo(paramName, visibleType, true, autoIndirect = autoInd, isByName = isByN)
        val savedExp = currentExpected
        currentExpected = if funInfo.returnType == UnitType then None else Some(funInfo.returnType)
        val tBody = try body match
          case ExprBodyAST(expr) =>
            val tExpr = analyzeExpr(expr)
            // Apply return-type range check for expression-body functions.
            val checked = if funInfo.returnType != UnitType then applyTargetType(tExpr, funInfo.returnType) else tExpr
            TExprBody(checked)
          case BlockBodyAST(stmts, contracts) =>
            analyzeBlockWithContracts(stmts, contracts, funInfo.returnType, funInfo.name, funInfo.params.map(_._1))
        finally currentExpected = savedExp
        // For def functions with no explicit return type, infer from body
        val retType = if funInfo.isDef && funInfo.returnType == UnitType then
          val inferred = tBody match
            case TExprBody(expr) => expr.typ
            case _ => UnitType
          // Update FunInfo so other references see the correct type
          functions(name) = funInfo.copy(returnType = inferred)
          inferred
        else funInfo.returnType
        val tParams = funInfo.params.zipWithIndex.map { case ((n, t), i) => TParam(n, t, None, funInfo.modeOf(i)) }
        currentReturnType = savedReturnType
        scopeStack = null
        validateTestAttr(fdAst, funInfo)
        if funInfo.isPure then
          val isDefFn = fdAst match { case FunDeclAST(_, _, _, _, _, _, _, _, d, _, _) => d }
          validatePureFn(name, tBody, funInfo.params.map(_._1), isDefFn)
        if funInfo.reads.isDefined || funInfo.writes.isDefined then
          validateEffects(name, funInfo, tBody, funInfo.params.map(_._1))
        validateGhostDiscipline(name, funInfo, tBody)
        TFunDecl(funInfo.name, tParams, retType, tBody, isPrivate, attrs, funInfo.isDef, isGhost = funInfo.isGhost, effects = funInfoEffects(funInfo), isParameterless = funInfo.isParameterless)

      case VarDeclAST(name, typOpt, init, isPrivate, isMutable, attrs, isVolatile, isConst) =>
        scopeStack = new mutable.ArrayBuffer
        pushScope()
        val isGhost = attrs.exists(_.name == "ghost")
        if isGhost && attrs.exists(_.name == "address") then
          throw AnalysisError(s"#ghost on '$name' is incompatible with #address (ghost vars have no runtime storage)")
        if isGhost && isConst then
          throw AnalysisError(s"#ghost on '$name' is incompatible with const (ghost decls are stripped from codegen)")
        // #address(N): MMIO var. No storage; reads/writes become direct loads/stores
        // through a literal pointer. Must have an explicit type.
        attrs.find(_.name == "address") match
          case Some(addrAttr) =>
            if isConst then throw AnalysisError(s"'#address' cannot be combined with 'const' on '$name'")
            val addr = addrAttr.args match
              case List(AttrPositional(AttrLitInt(n))) => n
              case _ => throw AnalysisError(s"#address on '$name' expects a single integer address, e.g. #address(0xFF000000)")
            val declTypeAST = typOpt.getOrElse(
              throw AnalysisError(s"#address var '$name' must have an explicit type"))
            val resolvedType = resolveType(declTypeAST)
            val mangledName = if shouldMangle(name) then mangleName(name) else name
            fixedAddressVars(name) = (addr, resolvedType)
            fixedAddressVars(mangledName) = (addr, resolvedType)
            globalScope(name) = SymInfo(mangledName, resolvedType, isMutable)
            scopeStack = null
            // #address vars don't carry a foldable value at compile time —
            // 0 is a placeholder, not used by anything (caller emits MMIO loads/stores).
            TConstDecl(mangledName, resolvedType, 0L) // no storage emitted
          case None =>
            analyzeRegularVarDecl(name, typOpt, init, isPrivate, isMutable, isVolatile, isConst, isGhost)

  /** Factored path for the non-#address module-level var decl. Same logic as before — pulled
   *  into its own method so the #address branch can early-return cleanly. */
  protected def analyzeRegularVarDecl(name: String, typOpt: Option[TypeAST], init: ExpressionAST,
      isPrivate: Boolean, isMutable: Boolean, isVolatile: Boolean, isConst: Boolean, isGhost: Boolean = false): TDecl =
    val tInit0 = analyzeExpr(init)
    val declType = typOpt.map(resolveType).getOrElse(tInit0.typ)
    val tInit1 = coerceLiteral(tInit0, declType)
    // `const` requires a compile-time-evaluable initializer.
    if isConst then
      if !declType.isIntegral then
        throw AnalysisError(s"const '$name' must have an integer type (found $declType); float/string/aggregate const is not yet supported")
      tryConstEval(tInit1) match
        case Some(n) =>
          val masked = maskToType(n, declType)
          val mangledName = if shouldMangle(name) then mangleName(name) else name
          compileTimeConstants(name) = masked
          compileTimeConstants(mangledName) = masked
        case None =>
          throw AnalysisError(s"const '$name' initializer is not compile-time evaluable")
    // Constant folding: immutable vals with constant initializers become compile-time constants
    val tInit = if !isMutable && !isGhost then
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
    globalScope(name) = SymInfo(mangledVarName, declType, isMutable, isConst = isConst, isGhost = isGhost)
    if isGhost then ghostNames += mangledVarName
    scopeStack = null
    // `const` declarations do not generate a storage slot — callers inline the folded value
    // via compileTimeConstants lookup during VarRef analysis. The value is also carried on
    // the typed decl so cross-file ModuleMeta serialization can publish it to sibling files.
    if isConst then TConstDecl(mangledVarName, declType, compileTimeConstants(mangledVarName))
    else TVarDecl(mangledVarName, declType, tInit, isPrivate, isVolatile, isGhost = isGhost, isMutable = isMutable)

  protected def warnDeprecated(name: String): Unit =
    if deprecations.contains(name) && !warnedDeprecations.contains(name) then
      warnedDeprecations += name
      val suffix = deprecations(name).map(r => s": $r").getOrElse("")
      System.err.println(s"warning: '$name' is deprecated$suffix")

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
      case TInterfaceBox(e, _)             => checkExpr(e)
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
      case TInterfaceBox(e, _)             => checkExpr(e)
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
        case TInterfaceBox(i, _)             => checkExpr(i)
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
    if fi.isPure then FuncEffects.Pure
    else if fi.reads.isDefined || fi.writes.isDefined then
      // Resolve through the cached effects table (handles mangling once, idempotent).
      resolveEffects(fi) match
        case Some((r, w)) => FuncEffects(reads = Some(r), writes = Some(w))
        case None         => FuncEffects.Unknown
    else FuncEffects.Unknown

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
      case TInterfaceBox(inner, _)         => checkExpr(inner, ghostCtx)
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
      functions.values.find(f => SyslAnalyzer.this.shortName(f.name) == shortName)
    }

  protected def satisfiesInterface(st: SyslType.StructType, iface: SyslType.InterfaceType): Boolean =
    val structName = st.name
    iface.methods.forall { (methodName, paramTypes, retType, ifaceEffects) =>
      lookupMethod(structName, methodName) match
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
    case _ => None

  protected def lookup(name: String): SymInfo =
    if scopeStack != null then
      var i = scopeStack.length - 1
      while i >= 0 do
        if scopeStack(i).contains(name) then return scopeStack(i)(name)
        i -= 1
    if globalScope.contains(name) then globalScope(name)
    else throw AnalysisError(s"undefined variable: '$name'")

  protected def tryLookup(name: String): Option[SymInfo] =
    if scopeStack != null then
      var i = scopeStack.length - 1
      while i >= 0 do
        if scopeStack(i).contains(name) then return Some(scopeStack(i)(name))
        i -= 1
    if globalScope.contains(name) then Some(globalScope(name))
    else None

  /** Like `tryLookup` but stops at the function boundary — returns only bindings from
   *  scopes pushed inside the current function (parameters, locals, match-bound names,
   *  destructuring binders). Used by the call-site resolver to honor local-shadows-
   *  global semantics for callable values: a pattern-bound `f: (string) -> int` must
   *  shadow a top-level `f(int) -> int` even though the global is registered earlier. */
  protected def lookupLocal(name: String): Option[SymInfo] =
    if scopeStack != null then
      var i = scopeStack.length - 1
      while i >= 0 do
        if scopeStack(i).contains(name) then return Some(scopeStack(i)(name))
        i -= 1
    None

  protected def lookupOrCreate(name: String, typ: SyslType): SymInfo =
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

  protected def lookupFun(name: String): FunInfo =
    functions.getOrElse(name,
      builtinFunctions.getOrElse(name,
        throw AnalysisError(s"undefined function: '$name'")))

  // ===== Generic function support =====

  // Mangle a type to a name-safe identifier for use in instantiated function names
  protected def typeToMangled(t: SyslType): String = t match
    case IntType(w)      => s"i$w"
    case UIntType(w)     => s"u$w"
    case BoolType        => "bool"
    case FloatType(w)    => s"f$w"
    case StringType      => "string"
    case UnitType        => "unit"
    case PtrType(i)      => "ptr" + typeToMangled(i)
    case RefType(i)      => "ref" + typeToMangled(i)
    case ArrayType(e, n) => s"arr${n}${typeToMangled(e)}"
    case SliceType(e)    => "slice" + typeToMangled(e)
    case FuncType(ps, r, _, _) => "fn" + ps.map(typeToMangled).mkString("") + "Ret" + typeToMangled(r)
    case StructType(n, _, _)    => n
    case EnumType(n, _)      => n
    case InterfaceType(n, _) => n
    case NamedType(n, _, _, _, _) => n

  protected def mangleGenericName(base: String, typeArgs: List[SyslType]): String =
    base + "_" + typeArgs.map(typeToMangled).mkString("_")

  /** Inverse-ish of `resolveType`: lift a fully-resolved SyslType back into a TypeAST that
   *  re-resolves to the same type. Used when storing impl `targetPatterns` for cross-unit
   *  imports (their patterns arrive as SyslTypes; the unifier consumes TypeASTs).
   *
   *  This is a structural lift — it does not preserve the original surface syntax (e.g.
   *  generic-instance struct names are emitted as `NamedTypeAST(mangledName, Nil)`, not as
   *  `NamedTypeAST(base, args)` — both resolve to the same StructType because the mangled
   *  name is already registered in `structTypes`).
   */
  protected def syslTypeToAST(t: SyslType): TypeAST = t match
    case IntType(8)        => NamedTypeAST("i8", Nil)
    case IntType(16)       => NamedTypeAST("i16", Nil)
    case IntType(32)       => NamedTypeAST("i32", Nil)
    case IntType(64)       => NamedTypeAST("i64", Nil)
    case UIntType(8)       => NamedTypeAST("u8", Nil)
    case UIntType(16)      => NamedTypeAST("u16", Nil)
    case UIntType(32)      => NamedTypeAST("u32", Nil)
    case UIntType(64)      => NamedTypeAST("u64", Nil)
    case BoolType          => NamedTypeAST("bool", Nil)
    case FloatType(32)     => NamedTypeAST("f32", Nil)
    case FloatType(64)     => NamedTypeAST("f64", Nil)
    case FloatType(w)      => NamedTypeAST(s"f$w", Nil)
    case IntType(w)        => NamedTypeAST(s"i$w", Nil)
    case UIntType(w)       => NamedTypeAST(s"u$w", Nil)
    case StringType        => NamedTypeAST("string", Nil)
    case UnitType          => NamedTypeAST("unit", Nil)
    case PtrType(i)        => PtrTypeAST(syslTypeToAST(i))
    case RefType(i)        => RefTypeAST(syslTypeToAST(i))
    case ArrayType(e, n)   => ArrayTypeAST(n, syslTypeToAST(e))
    case SliceType(e)      => SliceTypeAST(syslTypeToAST(e))
    case FuncType(ps, r, esc, eff) =>
      FuncTypeAST(ps.map(syslTypeToAST), syslTypeToAST(r), esc, eff)
    case StructType(n, _, _)        => NamedTypeAST(n, Nil)
    case EnumType(n, _)             => NamedTypeAST(n, Nil)
    case InterfaceType(n, _)        => NamedTypeAST(n, Nil)
    case NamedType(n, _, _, _, _)   => NamedTypeAST(n, Nil)

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

  /** Merge two concrete-type observations of the same generic type variable,
   *  taking the lattice LUB on `FuncType` effects rather than demanding
   *  structural equality. Recurses through container types (slice / array /
   *  ptr / ref / struct / named) so an embedded `FuncType` anywhere in the
   *  shape uses the lattice. Returns `None` when the two types are
   *  structurally incompatible or carry incomparable effect annotations.
   *
   *  This is the inference-engine analogue of `latticeEqual`: that one decides
   *  *whether* a single observation flows into a slot; this one decides *what*
   *  binding to pick when several observations of the same type variable
   *  appear at different use sites. The merged binding is wide enough that
   *  every original observation still satisfies it as a slot.
   */
  protected def mergeBindings(t1: SyslType, t2: SyslType): Option[SyslType] =
    if t1 == t2 then Some(t1)
    else
      (t1, t2) match
        case (FuncType(p1, r1, esc1, eff1), FuncType(p2, r2, _, eff2)) if p1.length == p2.length =>
          val mergedParams = p1.zip(p2).map((a, b) => mergeBindings(a, b))
          if mergedParams.exists(_.isEmpty) then None
          else
            mergeBindings(r1, r2).flatMap { mr =>
              lubEffect(eff1, eff2).map { eff =>
                FuncType(mergedParams.map(_.get), mr, esc1, eff)
              }
            }
        case (SliceType(a), SliceType(b)) => mergeBindings(a, b).map(SliceType.apply)
        case (ArrayType(a, n1), ArrayType(b, n2)) if n1 == n2 =>
          mergeBindings(a, b).map(ArrayType(_, n1))
        case (PtrType(a), PtrType(b)) => mergeBindings(a, b).map(PtrType.apply)
        case (RefType(a), RefType(b)) => mergeBindings(a, b).map(RefType.apply)
        case (StructType(n1, f1, v1), StructType(n2, f2, v2))
            if n1 == n2 && v1 == v2 && f1.length == f2.length &&
              f1.zip(f2).forall { case ((fn1, _), (fn2, _)) => fn1 == fn2 } =>
          val merged = f1.zip(f2).map { case ((fn, ft1), (_, ft2)) => mergeBindings(ft1, ft2).map((fn, _)) }
          if merged.exists(_.isEmpty) then None
          else Some(StructType(n1, merged.map(_.get), v1))
        case (NamedType(n1, u1, nom1, ro1, pr1), NamedType(n2, u2, nom2, ro2, pr2))
            if n1 == n2 && nom1 == nom2 && ro1 == ro2 && pr1 == pr2 =>
          mergeBindings(u1, u2).map(NamedType(n1, _, nom1, ro1, pr1))
        case _ => None

  // Unify a parameter TypeAST (which may contain type variables) against a concrete SyslType,
  // recording type variable bindings. Returns true if unification succeeded structurally.
  protected def unifyTypes(param: TypeAST, arg: SyslType, typeParams: Set[String], env: mutable.Map[String, SyslType]): Unit =
    param match
      case NamedTypeAST(name, _) if typeParams.contains(name) =>
        env.get(name) match
          case Some(existing) if existing == arg => ()
          case Some(existing) =>
            // Lattice merge: two concrete observations of the same type variable
            // are compatible iff one is ≤ the other under the effect/structure
            // lattice. The merged binding is the GLB (more-specific). Without
            // this, any combinator-library call where the user supplies a
            // literal closure (auto-`#pure`) and the same type variable is also
            // constrained by an unannotated context is rejected.
            mergeBindings(existing, arg) match
              case Some(merged) => env(name) = merged
              case None =>
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
      case FuncTypeAST(paramTypes, ret, _, _) => arg match
        case FuncType(argParams, argRet, _, _) =>
          if paramTypes.length == argParams.length then
            for (pt, at) <- paramTypes.zip(argParams) do unifyTypes(pt, at, typeParams, env)
          unifyTypes(ret, argRet, typeParams, env)
        case _ => ()
      case ByNameTypeAST(inner) => arg match
        // `=> T` desugars to `() -> T`. Treat it identically here so impl
        // patterns with by-name slots unify against call-site args that have
        // already been auto-wrapped to a zero-arg thunk.
        case FuncType(Nil, argRet, _, _) => unifyTypes(inner, argRet, typeParams, env)
        case _ => () // structural mismatch handled later by tryUnifyAll's post-check
      case TupleTypeAST(elems) => arg match
        case StructType(_, fields, _) if elems.length == fields.length =>
          for (e, (_, ft)) <- elems.zip(fields) do unifyTypes(e, ft, typeParams, env)
        case _ => ()
      case NamedTypeAST(name, tArgs) if tArgs.nonEmpty =>
        // If this is a generic type alias, expand it and unify the expanded type
        if genericTypeAliases.contains(name) then
          val (tparams, target, isNew) = genericTypeAliases(name)
          if tArgs.length == tparams.length then
            if isNew then
              // Nominal generic alias: gate on `genericAliasToTemplate` so the
              // actual's mangled name still has to resolve back to *this* template
              // (otherwise `Parser2[A]` would unify against a `Parser[T]`-shaped
              // underlying).
              //
              // Two binding sources are consulted, in this order:
              //   1. Cached `concreteArgs` from `genericAliasToTemplate`. Works
              //      directly even for phantom type parameters that don't appear
              //      in the alias's `target` (e.g. `type Box[T] = new int`).
              //   2. Refinement via expand-and-unify against `arg.underlying`.
              //      This catches the case where `typeToMangled` collides two
              //      distinct instantiations (e.g. `Parser[(int)->int]` and
              //      `Parser[(int)->int #pure]` both mangle to `Parser_fni32Retfni32Reti32`),
              //      so the cached `concreteArgs` may have been overwritten by a
              //      different instantiation. Pulling the per-instance bindings
              //      from the underlying recovers the correct effect annotations.
              //
              // `unifyTypes` already merges conflicting bindings via the effect
              // lattice, so doing both is safe — the underlying-unification
              // either confirms the cache or refines it.
              arg match
                case SyslType.NamedType(argName, argUnderlying, true, _, _) =>
                  genericAliasToTemplate.get(argName) match
                    case Some((templateName, concreteArgs)) if templateName == name && concreteArgs.length == tArgs.length =>
                      for (p, a) <- tArgs.zip(concreteArgs) do unifyTypes(p, a, typeParams, env)
                      val subst = tparams.zip(tArgs).toMap
                      val expandedTarget = substituteTypeAST(target, subst)
                      unifyTypes(expandedTarget, argUnderlying, typeParams, env)
                    case _ => () // structural mismatch — caught by post-validation
                case _ => ()
            else
              // Transparent: substitute alias type params with the call's type args in
              // the target TypeAST, then unify the expanded structure against the actual.
              // e.g., type Parser[T] = (string, int) -> Result[T, string]
              //   Parser[A] → substitute T→A in target → (string, int) -> Result[A, string]
              val subst = tparams.zip(tArgs).toMap
              val expanded = substituteTypeAST(target, subst)
              unifyTypes(expanded, arg, typeParams, env)
        else arg match
          case SyslType.StructType(argName, argFields, _) =>
            // Same lossy-cache caveat as the nominal-alias branch above:
            // `typeToMangled` drops effect annotations on `FuncType`, so two
            // distinct generic-struct instantiations can collide on the same
            // mangled name and clobber each other in `structToTemplate`. The
            // cache lookup gates the template identity (so `Foo[T]` doesn't
            // match an unrelated `Bar[T]`), but the per-instance bindings are
            // refined by walking the actual struct's fields and unifying each
            // field's TypeAST (with the pattern's tArgs substituted in) against
            // the field's resolved type. `unifyTypes` merges any conflicting
            // binding via the effect lattice — so doing both is safe.
            structToTemplate.get(argName) match
              case Some((templateName, concreteArgs)) if templateName == name && concreteArgs.length == tArgs.length =>
                for (p, a) <- tArgs.zip(concreteArgs) do unifyTypes(p, a, typeParams, env)
                genericStructs.get(name).foreach { template =>
                  if template.typeParams.length == tArgs.length && template.fields.length == argFields.length then
                    val subst = template.typeParams.zip(tArgs).toMap
                    for ((fnTpl, ftAst, _), (fnAct, ftActual)) <- template.fields.zip(argFields) do
                      if fnTpl == fnAct then
                        val expanded = substituteTypeAST(ftAst, subst)
                        unifyTypes(expanded, ftActual, typeParams, env)
                }
              case _ => ()
          case SyslType.EnumType(argName, argVariants) =>
            // Same lossy-cache caveat as above. The variants-walk refinement
            // is what makes nested function-typed enum payloads (e.g.
            // `enum PR[A] { Ok(value: A, ...) }` with `A = (...) -> int`)
            // recover their per-instance effect annotations even when the
            // shared mangled name has clobbered the cache.
            enumToTemplate.get(argName) match
              case Some((templateName, concreteArgs)) if templateName == name && concreteArgs.length == tArgs.length =>
                for (p, a) <- tArgs.zip(concreteArgs) do unifyTypes(p, a, typeParams, env)
                genericEnums.get(name).foreach { template =>
                  if template.typeParams.length == tArgs.length && template.variants.length == argVariants.length then
                    val subst = template.typeParams.zip(tArgs).toMap
                    for (vAst, (vName, vFields)) <- template.variants.zip(argVariants) do
                      if vAst.name == vName && vAst.fields.length == vFields.length then
                        for ((_, ftAst), (_, ftActual)) <- vAst.fields.zip(vFields) do
                          val expanded = substituteTypeAST(ftAst, subst)
                          unifyTypes(expanded, ftActual, typeParams, env)
                }
              case _ => ()
          case _ => ()
      case _ => () // concrete parameter type, nothing to infer

  /** Substitute named types in a TypeAST. Used to expand generic type alias params before unification. */
  protected def substituteTypeAST(t: TypeAST, subst: Map[String, TypeAST]): TypeAST = t match
    case NamedTypeAST(name, Nil) if subst.contains(name) => subst(name)
    case NamedTypeAST(name, args) => NamedTypeAST(name, args.map(substituteTypeAST(_, subst)))
    case PtrTypeAST(inner) => PtrTypeAST(substituteTypeAST(inner, subst))
    case PtrNonNullTypeAST(inner) => PtrNonNullTypeAST(substituteTypeAST(inner, subst))
    case ArrayTypeAST(size, elem) => ArrayTypeAST(size, substituteTypeAST(elem, subst))
    case SliceTypeAST(elem) => SliceTypeAST(substituteTypeAST(elem, subst))
    case FuncTypeAST(params, ret, esc, eff) => FuncTypeAST(params.map(substituteTypeAST(_, subst)), substituteTypeAST(ret, subst), esc, eff)
    case TupleTypeAST(elems) => TupleTypeAST(elems.map(substituteTypeAST(_, subst)))
    case RefTypeAST(inner) => RefTypeAST(substituteTypeAST(inner, subst))
    case ByNameTypeAST(inner) => ByNameTypeAST(substituteTypeAST(inner, subst))
    case ProjectionTypeAST(qualifier, member) =>
      // If the qualifier is being substituted to a NamedTypeAST whose name we
      // can use as the new qualifier, rewrite. Otherwise pass through unchanged
      // — projection resolution happens at resolveType time against the
      // active impl context. (Pre-A4 substitution semantics.)
      subst.get(qualifier) match
        case Some(NamedTypeAST(n, _)) => ProjectionTypeAST(n, member)
        case _ => t

  /** Strict structural equality with effect-lattice tolerance for FuncType.
   *  Used for impl-dispatch post-validation. `slot` is the impl pattern resolved
   *  with the unifier-bound env; `actual` is the call-site type. They must be
   *  structurally identical *except* that nested FuncType effects compare by
   *  `effectsSatisfy(actualEff, slotEff)` rather than `==`. This lets a `#pure`
   *  closure (auto-inferred for any side-effect-free body) flow into an
   *  unannotated higher-order parameter — the common shape for combinator
   *  libraries that haven't yet opted into the effect discipline.
   *
   *  Containers (slice/array/ref/ptr/tuple/struct/enum) recurse component-wise
   *  so the lattice rule fires on FuncType anywhere in the tree.
   */
  protected def latticeEqual(slot: SyslType, actual: SyslType): Boolean =
    (slot, actual) match
      case (FuncType(p1, r1, _, eff1), FuncType(p2, r2, _, eff2)) =>
        // Parameters and return types match by lattice (recursive). Effects
        // checked one-way: actual must satisfy slot. Escape flag ignored — it's
        // an optimization hint, not a type distinction (mirrors `compatible`).
        p1.length == p2.length &&
          p1.zip(p2).forall((a, b) => latticeEqual(a, b)) &&
          latticeEqual(r1, r2) &&
          effectsSatisfy(eff2, eff1)
      case (SliceType(a), SliceType(b)) => latticeEqual(a, b)
      case (ArrayType(a, n1), ArrayType(b, n2)) => n1 == n2 && latticeEqual(a, b)
      case (PtrType(a), PtrType(b)) => latticeEqual(a, b)
      case (RefType(a), RefType(b)) => latticeEqual(a, b)
      case (StructType(n1, f1, v1), StructType(n2, f2, v2)) if n1 == n2 && v1 == v2 && f1.length == f2.length =>
        // Same nominal struct — recurse on fields so an embedded FuncType still uses
        // the lattice (anonymous tuple structs land here too — they share generated names).
        f1.zip(f2).forall { case ((fn1, ft1), (fn2, ft2)) => fn1 == fn2 && latticeEqual(ft1, ft2) }
      case (NamedType(n1, u1, nom1, r1, p1), NamedType(n2, u2, nom2, r2, p2)) =>
        n1 == n2 && nom1 == nom2 && r1 == r2 && p1 == p2 && latticeEqual(u1, u2)
      // Default: strict equality. Covers primitives (int, float, bool, string, unit),
      // enums, interfaces — anywhere effects don't appear in the shape.
      case (s, a) => s == a

  /** Stage F.3 entry point — try to unify a list of TypeAST patterns against a list of
   *  concrete SyslType actuals, returning the inferred binding map on success or None on
   *  any failure. Failure modes captured: arity mismatch, type-var conflict (caught as
   *  AnalysisError from the underlying `unifyTypes`), unbound type parameters, or
   *  structural mismatch that the recursive walker silently no-ops past.
   *
   *  The post-validation step substitutes the inferred env into each pattern, re-resolves
   *  it via `resolveType`, and demands `latticeEqual` against the actual. The lattice
   *  rule lets a candidate FuncType with stricter effects (e.g. `#pure`) flow into an
   *  unannotated slot — the common shape for combinator-library impls. Without this, a
   *  literal closure (always inferred `#pure` for side-effect-free bodies) would never
   *  match an `(A) -> B` impl pattern.
   */
  protected def tryUnifyAll(
      patterns: List[TypeAST],
      actuals: List[SyslType],
      tvars: Set[String],
  ): Option[Map[String, SyslType]] =
    if patterns.length != actuals.length then None
    else
      val env = mutable.Map.empty[String, SyslType]
      val unifyOk =
        try
          for (p, a) <- patterns.zip(actuals) do
            unifyTypes(p, a, tvars, env)
          true
        catch case _: AnalysisError => false
      if !unifyOk then None
      else
        val savedEnv = typeEnv
        typeEnv = typeEnv ++ env
        val structuralOk =
          try
            patterns.zip(actuals).forall { (p, a) =>
              val resolved =
                try Some(resolveType(p))
                catch case _: Throwable => None
              resolved.exists(r => latticeEqual(r, a))
            }
          finally typeEnv = savedEnv
        if !structuralOk then None
        else if !tvars.forall(env.contains) then None
        else Some(env.toMap)

  /** Stage F.5 helper — pattern-to-pattern overlap. Two impl templates overlap iff there
   *  exists a concrete substitution that satisfies both pattern lists simultaneously.
   *
   *  Approach: walk both patterns side-by-side, unifying type-var-bearing structure into
   *  a single combined env (`tvarsA ∪ tvarsB`). When both sides hit the same concrete
   *  shape they must agree structurally; when one is a tvar it binds.
   *
   *  This is symmetric, conservative, and good enough for the at-most-one coherence rule.
   *  False positives (rejecting non-overlapping templates) are preferred to false
   *  negatives (admitting actual ambiguity).
   */
  protected def patternsOverlap(
      a: List[TypeAST],
      b: List[TypeAST],
      tvars: Set[String],
  ): Boolean =
    if a.length != b.length then return false
    val env = mutable.Map.empty[String, TypeAST]
    def bindOrEqual(name: String, t: TypeAST): Boolean =
      env.get(name) match
        case Some(prev) => prev == t
        case None => env(name) = t; true
    def overlap(x: TypeAST, y: TypeAST): Boolean = (x, y) match
      case (NamedTypeAST(nx, Nil), _) if tvars.contains(nx) => bindOrEqual(nx, y)
      case (_, NamedTypeAST(ny, Nil)) if tvars.contains(ny) => bindOrEqual(ny, x)
      case (NamedTypeAST(nx, ax), NamedTypeAST(ny, ay)) =>
        nx == ny && ax.length == ay.length && ax.zip(ay).forall((p, q) => overlap(p, q))
      case (PtrTypeAST(ix), PtrTypeAST(iy)) => overlap(ix, iy)
      case (RefTypeAST(ix), RefTypeAST(iy)) => overlap(ix, iy)
      case (SliceTypeAST(ex), SliceTypeAST(ey)) => overlap(ex, ey)
      case (ArrayTypeAST(_, ex), ArrayTypeAST(_, ey)) => overlap(ex, ey)
      case (FuncTypeAST(px, rx, _, _), FuncTypeAST(py, ry, _, _)) =>
        px.length == py.length && px.zip(py).forall((p, q) => overlap(p, q)) && overlap(rx, ry)
      case (TupleTypeAST(ex), TupleTypeAST(ey)) =>
        ex.length == ey.length && ex.zip(ey).forall((p, q) => overlap(p, q))
      case _ => false
    a.zip(b).forall((p, q) => overlap(p, q))

  // Instantiate a generic function with inferred type arguments, returning the mangled name
  // and FunInfo of the instantiated function. Reuses cached instantiations.
  // If an operator has a user-defined struct/enum operand, desugar to the corresponding trait call.
  // Returns None if no desugaring applies (use built-in dispatch).
  //
  // `strict` controls behaviour when no impl matches:
  //   - strict=true: throw "no impl of trait for operator on operands" — used when an
  //     operand is non-arithmetic (struct/enum/nominal alias of non-numeric) and built-in
  //     fallback can't possibly succeed; the better diagnostic names trait + operands.
  //   - strict=false: return None silently — used when both operands are numeric (or
  //     nominal aliases of numerics) and the built-in arithmetic path is a valid fallback,
  //     so `Meters + Meters` with no `impl Add[Meters]` still does plain int arithmetic.
  protected def tryOperatorDispatch(op: String, tLeft: TExpr, tRight: TExpr, strict: Boolean): Option[TExpr] =
    lookupBinaryOperatorTrait(op) match
      case None => None
      case Some((traitName, methodName)) =>
        val operands = List(tLeft.typ, tRight.typ)
        // For built-in operators (`+`, `-`, `==`, …), only fire user-defined
        // dispatch when at least one operand is a user type. This preserves the
        // friendly "unknown operator on i32" path and keeps `1 + 2` from going
        // through trait machinery. Custom operators (registered via #operator)
        // have no built-in fallback, so the gate is wrong for them: e.g. with
        // `impl[B] MapTo[string, B, Parser[B]]`, the dispatch
        // `"+" ^^^ (_ + _)` has operand types `(string, fn)` — both built-in —
        // but it must still go through trait dispatch to be meaningful.
        val isCustomOp = customBinaryOperatorTraits.contains(op)
        if !isCustomOp then
          val hasUserType = operands.exists {
            case _: SyslType.StructType | _: SyslType.EnumType | _: SyslType.NamedType => true
            case _ => false
          }
          if !hasUserType then return None
        if !traits.contains(traitName) then
          if strict then
            throw AnalysisError(s"operator '$op' on ${operands.mkString(", ")} requires trait '$traitName' but it is not defined")
          else return None
        val candidates = enumerateImplCandidates(traitName, methodName, operands)
        candidates match
          case Nil =>
            if strict then
              throw AnalysisError(s"no impl of '$traitName' for operator '$op' on ${operands.mkString(", ")}")
            else None
          case (template, subst) :: Nil =>
            val (mangled, funInfo) = instantiateImpl(template, traitName, methodName, subst)
            val checkedArgs = checkArgs(mangled, funInfo.params, List(tLeft, tRight))
            Some(TCall(mangled, checkedArgs, funInfo.returnType))
          case multi =>
            throw AnalysisError(s"ambiguous: ${multi.length} impls of '$traitName' match operator '$op' on ${operands.mkString(", ")}")

  /** Prefix-operator dispatch. Caller is expected to have already gated on
   *  `customUnaryOperatorTraits.contains(op)`. Throws when the bound trait
   *  is missing or no impl matches; throws on ambiguity. Use
   *  `tryUnaryOperatorDispatchOpt` instead at sites that want to fall back
   *  to a different lowering when no impl matches (e.g. the `&` arm where
   *  built-in address-of is the fallback for true lvalues).
   */
  protected def tryUnaryOperatorDispatch(op: String, tOperand: TExpr): TExpr =
    tryUnaryOperatorDispatchOpt(op, tOperand).getOrElse {
      val (traitName, _) = customUnaryOperatorTraits(op)
      if !traits.contains(traitName) then
        throw AnalysisError(
          s"prefix operator '$op' on ${tOperand.typ} requires trait '$traitName' but it is not defined")
      throw AnalysisError(s"no impl of '$traitName' for prefix operator '$op' on ${tOperand.typ}")
    }

  /** Same as `tryUnaryOperatorDispatch` but returns `None` instead of
   *  throwing when zero impls match. Ambiguity (>1 impl) still throws.
   */
  protected def tryUnaryOperatorDispatchOpt(op: String, tOperand: TExpr): Option[TExpr] =
    val (traitName, methodName) = customUnaryOperatorTraits(op)
    if !traits.contains(traitName) then return None
    val candidates = enumerateImplCandidates(traitName, methodName, List(tOperand.typ))
    candidates match
      case Nil => None
      case (template, subst) :: Nil =>
        val (mangled, funInfo) = instantiateImpl(template, traitName, methodName, subst)
        val checkedArgs = checkArgs(mangled, funInfo.params, List(tOperand))
        Some(TCall(mangled, checkedArgs, funInfo.returnType))
      case multi =>
        throw AnalysisError(
          s"ambiguous: ${multi.length} impls of '$traitName' match prefix operator '$op' on ${tOperand.typ}")

  /** Lookahead helper for binary-operator expected-type forwarding. Given the
   *  trait+method that `op` resolves to, the LHS operand's already-resolved
   *  type, and an optional outer expected type for the result, find the unique
   *  impl that matches the LHS at param 0 (and the result if expected is given),
   *  then resolve and return the type of the second formal parameter under the
   *  inferred type-param bindings. Returns `None` if zero or many impls match,
   *  or if any required type variable is still unbound after the partial
   *  unification.
   *
   *  This lets a closure-literal RHS (`lhs ^^^ (_ + _)`) typecheck with the
   *  expected type the dispatcher would use *after* dispatch — exactly the same
   *  service ordinary call sites already provide for closure-literal args.
   */
  protected def expectedTypeForBinaryOpRhs(
      traitName: String,
      methodName: String,
      leftType: SyslType,
      expectedReturnType: Option[SyslType],
  ): Option[SyslType] =
    if !traits.contains(traitName) then return None
    val results = implTemplates.getOrElse(traitName, Nil).iterator.flatMap { t =>
      // Recover the impl method's pattern types — explicit AST when present,
      // else derived from the trait method by substituting impl's targetPatterns.
      val (paramPatterns, retPattern): (List[TypeAST], TypeAST) =
        t.methodASTs.find(_.name == methodName) match
          case Some(im) =>
            (im.params.map(_.typ), im.returnType.getOrElse(NamedTypeAST("unit", Nil)))
          case None =>
            traits(traitName).methods.find(_.name == methodName) match
              case Some(tm) =>
                val traitToImpl = traits(traitName).typeParams.zip(t.targetPatterns).toMap
                (tm.params.map(p => substituteTypeAST(p.typ, traitToImpl)),
                 substituteTypeAST(tm.returnType, traitToImpl))
              case None => (Nil, NamedTypeAST("unit", Nil))
      if paramPatterns.length != 2 then None
      else
        val tvars = t.typeParams.toSet
        val env = mutable.Map.empty[String, SyslType]
        try
          unifyTypes(paramPatterns.head, leftType, tvars, env)
          expectedReturnType.foreach(rt => unifyTypes(retPattern, rt, tvars, env))
          // `unifyTypes` silently no-ops on shape mismatches (its callers post-
          // validate via `latticeEqual`). Ordinary dispatch (`tryUnifyAll`) does
          // the post-check; this lookahead must do the same, otherwise a
          // non-matching impl (e.g. `MapTo[Parser[A], …]` against `string` LHS)
          // sneaks through and `expectedTypeForBinaryOpRhs` returns None
          // (ambiguous). The placeholder closure then has nothing to resolve
          // against. Match the dispatcher's structural check 1:1.
          val savedEnv = typeEnv
          typeEnv = typeEnv ++ env.toMap
          val structuralOk =
            try
              latticeEqual(resolveType(paramPatterns.head), leftType) &&
                expectedReturnType.forall(rt =>
                  try latticeEqual(resolveType(retPattern), rt)
                  catch case _: Throwable => false)
            catch case _: Throwable => false
          val out =
            if !structuralOk then None
            else
              try Some(resolveType(paramPatterns(1)))
              catch case _: Throwable => None
          typeEnv = savedEnv
          out
        catch case _: AnalysisError => None
    }.toList
    results match
      case single :: Nil => Some(single)
      case _ => None

  /** Enumerate every registered impl template of `traitName` whose `methodName` parameter
   *  patterns unify with `argTypes`. Concrete impls fast-path through `==` checks;
   *  generic impls go through `tryUnifyAll` against the raw impl-method param TypeAST.
   *
   *  Returned in registration order. The dispatcher above expects: 0 → no impl, 1 → use,
   *  N>1 → ambiguous.
   */
  protected def enumerateImplCandidates(
      traitName: String,
      methodName: String,
      argTypes: List[SyslType],
  ): List[(ImplTemplate, Map[String, SyslType])] =
    implTemplates.getOrElse(traitName, Nil).iterator.flatMap { t =>
      if t.typeParams.isEmpty then
        // Concrete: compare argTypes against this impl method's bound parameter types.
        // Use either the recorded methodInfo (source-defined impl) or, for cross-unit
        // imports without methodInfos, fall back to `functions(mangled).params`.
        val infoOpt = t.methodInfos.find(_.mangled.endsWith("_" + methodName) || true).find(i =>
          // We index by name embedded in `mangled`; safest is to look up the mangled name first
          t.methods.get(methodName).contains(i.mangled)
        )
        val paramTypes = infoOpt.map(_.paramTypes.map(_._2))
          .orElse(t.methods.get(methodName).flatMap(m => functions.get(m).map(_.params.map(_._2))))
          .orElse(t.methods.get(methodName).flatMap(m => functions.get(shortName(m)).map(_.params.map(_._2))))
        // Compare via `latticeEqual` so a `(T) -> U #pure` actual matches a
        // `(T) -> U` impl-pattern slot — the same lattice rule the generic
        // path already uses in `tryUnifyAll`'s post-validation. Without this,
        // a closure literal (always inferred `#pure` for side-effect-free
        // bodies) would never dispatch through a concrete operator-trait impl.
        paramTypes match
          case Some(ps) if ps.length == argTypes.length &&
              ps.zip(argTypes).forall((p, a) => latticeEqual(p, a)) =>
            Some((t, Map.empty[String, SyslType]))
          case _ => None
      else
        // Generic: unify raw impl method param TypeAST against arg types.
        val implMethod = t.methodASTs.find(_.name == methodName)
        implMethod match
          case Some(im) =>
            val patternTypes = im.params.map(_.typ)
            tryUnifyAll(patternTypes, argTypes, t.typeParams.toSet).map(s => (t, s))
          case None =>
            // Generic impl with synthesized default — derive trait-method param patterns
            // by substituting impl's targetPatterns into the trait method's params, then
            // unify against arg types.
            val trait_ = traits(traitName)
            trait_.methods.find(_.name == methodName).flatMap { tm =>
              // Build a tparam→TypeAST substitution: traitParam → impl's targetPattern at that index
              val traitToImpl = trait_.typeParams.zip(t.targetPatterns).toMap
              val patternTypes = tm.params.map(p => substituteTypeAST(p.typ, traitToImpl))
              tryUnifyAll(patternTypes, argTypes, t.typeParams.toSet).map(s => (t, s))
            }
    }.toList

  // Instantiate a generic struct with concrete type arguments, returning its StructType
  /** Instantiate a `new` generic alias (`type Parser[A] = new (Input) -> ParseResult[A]`)
   *  for a specific list of type args. Each instantiation gets a unique mangled name
   *  (e.g. `Parser_i32`) and is wrapped as a nominal `NamedType` so it is distinct from
   *  both its underlying base and from other instantiations.
   *
   *  Cached so that two mentions of `Parser[i32]` produce object-equal `SyslType` values —
   *  this is what makes trait/impl dispatch see them as the same type.
   */
  /** Phase B — fill missing trailing type-arg slots from declared defaults.
   *  Defaults resolve under the partial env of already-pinned slots so a later
   *  default may reference an earlier param (e.g. `[I, O = I]`). The returned
   *  list always has length `tparams.length`. Errors if `typeArgs.length` is
   *  greater than `tparams.length`, or if a missing slot has no default.
   *  `kind` is just for the diagnostic ("struct" / "enum" / "type alias"). */
  protected def fillTypeArgsFromDefaults(
      name: String,
      kind: String,
      tparams: List[String],
      defaults: Map[String, TypeAST],
      typeArgs: List[SyslType],
  ): List[SyslType] =
    if typeArgs.length == tparams.length then return typeArgs
    if typeArgs.length > tparams.length then
      throw AnalysisError(s"generic $kind '$name' expects ${tparams.length} type arg(s), got ${typeArgs.length}")
    val pinned = mutable.Map.empty[String, SyslType]
    for (tp, ty) <- tparams.zip(typeArgs) do pinned(tp) = ty
    for tp <- tparams.drop(typeArgs.length) do
      defaults.get(tp) match
        case Some(defAst) =>
          val savedEnv = typeEnv
          typeEnv = typeEnv ++ pinned.toMap
          try pinned(tp) = resolveType(defAst)
          finally typeEnv = savedEnv
        case None =>
          throw AnalysisError(s"generic $kind '$name' expects ${tparams.length} type arg(s), got ${typeArgs.length} (no default for type parameter '$tp')")
    tparams.map(pinned)

  /** Validate that `typeArgs` satisfy the trait bounds declared on
   *  `typeParams`, mirroring the bound check in `instantiateGeneric`. Returns
   *  Left(msg) on failure, Right(()) on success. Multi-bound disambiguation
   *  (when two bounds bind the same assoc name to different types) is detected
   *  here too. The companion `buildAssocBindingsEnv` constructs the matching
   *  assocBindingsEnv map. Both are used by struct/enum/alias instantiation. */
  protected def enforceBoundsAndBuildAssocs(
      what: String,
      typeParams: List[String],
      typeArgs: List[SyslType],
      typeBounds: Map[String, List[String]],
  ): Either[String, Unit] =
    val tpIter = typeParams.iterator
    while tpIter.hasNext do
      val tp = tpIter.next()
      val bounds = typeBounds.getOrElse(tp, Nil)
      val concreteType = typeArgs(typeParams.indexOf(tp))
      val tnIter = bounds.iterator
      while tnIter.hasNext do
        val traitName = tnIter.next()
        if !traits.contains(traitName) then
          return Left(s"bound '$traitName' on type parameter '$tp' of $what refers to unknown trait")
        val matched = implTemplates.getOrElse(traitName, Nil).exists { t =>
          if t.typeParams.isEmpty then
            t.resolvedConcrete.flatMap(_.headOption).contains(concreteType)
          else
            tryUnifyAll(t.targetPatterns.headOption.toList, List(concreteType), t.typeParams.toSet).isDefined
        }
        if !matched then
          return Left(s"type $concreteType does not satisfy bound '$traitName' for type parameter '$tp' in $what")
    Right(())

  /** Build the assoc-bindings env for `typeParams` instantiated to `typeArgs`,
   *  by locating each bounded type parameter's matching impl and pulling its
   *  assocBindings into the env. Mirrors the same logic in `instantiateGeneric`,
   *  including multi-bound ambiguity rejection. */
  protected def buildAssocBindingsEnv(
      typeParams: List[String],
      typeArgs: List[SyslType],
      typeBounds: Map[String, List[String]],
  ): Map[String, SyslType] =
    val newAssocs = mutable.Map.empty[String, SyslType]
    val newAssocSources = mutable.Map.empty[String, String]
    for (tp, concreteType) <- typeParams.zip(typeArgs) do
      val bounds = typeBounds.getOrElse(tp, Nil)
      for traitName <- bounds do
        val matched = implTemplates.getOrElse(traitName, Nil).iterator.flatMap { t =>
          if t.typeParams.isEmpty then
            if t.resolvedConcrete.flatMap(_.headOption).contains(concreteType)
            then Some((t, Map.empty[String, SyslType]))
            else None
          else
            tryUnifyAll(t.targetPatterns.headOption.toList, List(concreteType), t.typeParams.toSet)
              .map(s => (t, s))
        }.nextOption()
        for (impl, implSubst) <- matched do
          val savedEnv = typeEnv
          typeEnv = typeEnv ++ implSubst
          try
            for binding <- impl.assocBindings do
              val resolved = resolveType(binding.target)
              newAssocs.get(binding.name) match
                case Some(prior) if prior != resolved =>
                  val priorTrait = newAssocSources.getOrElse(binding.name, "?")
                  throw AnalysisError(
                    s"projection '${tp}::${binding.name}' is ambiguous: " +
                      s"bound '$priorTrait' binds it to $prior but bound '$traitName' binds it to $resolved",
                  )
                case _ =>
                  newAssocs(binding.name) = resolved
                  newAssocSources(binding.name) = traitName
          finally typeEnv = savedEnv
    newAssocs.toMap

  protected def instantiateGenericNominalAlias(
      name: String,
      tparams: List[String],
      target: TypeAST,
      typeArgs: List[SyslType],
  ): SyslType =
    val defaults = genericTypeAliasDefaults.getOrElse(name, Map.empty)
    val bounds = genericTypeAliasBounds.getOrElse(name, Map.empty)
    val filledArgs = fillTypeArgsFromDefaults(name, "type alias", tparams, defaults, typeArgs)
    val cacheKey = (name, filledArgs)
    genericAliasInstantiations.get(cacheKey) match
      case Some(t) => t
      case None =>
        val typeArgs = filledArgs
        // Enforce trait bounds declared on the alias's type parameters (Phase C
        // follow-up) and bring projection assocs into scope while resolving the
        // alias body, so `Parser[I: Input, A] = (I) -> ParseResult[I, A]` can
        // resolve `I::Elem` references inside the target type.
        enforceBoundsAndBuildAssocs(s"type alias '$name'", tparams, typeArgs, bounds) match
          case Left(err) => throw AnalysisError(err)
          case Right(_)  => ()
        val mangled = name + "_" + typeArgs.map(typeToMangled).mkString("_")
        val savedEnv = typeEnv
        val savedAssocs = assocBindingsEnv
        typeEnv = typeEnv ++ tparams.zip(typeArgs).toMap
        assocBindingsEnv = buildAssocBindingsEnv(tparams, typeArgs, bounds)
        val base =
          try resolveType(target)
          finally
            typeEnv = savedEnv
            assocBindingsEnv = savedAssocs
        val nt = SyslType.NamedType(mangled, base, nominal = true, range = None, predicateFunc = None)
        genericAliasInstantiations(cacheKey) = nt
        genericAliasToTemplate(mangled) = (name, typeArgs)
        nt

  protected def instantiateGenericStruct(name: String, typeArgs: List[SyslType]): SyslType.StructType =
    val template0 = genericStructs.getOrElse(name,
      throw AnalysisError(s"'$name' is not a generic struct"))
    val filledArgs = fillTypeArgsFromDefaults(name, "struct", template0.typeParams, template0.typeParamDefaults, typeArgs)
    val cacheKey = (name, filledArgs)
    genericStructInstantiations.get(cacheKey) match
      case Some(st) => st
      case None =>
        val template = template0
        val typeArgs = filledArgs
        // Enforce trait bounds declared on the struct's type parameters and
        // build the assoc-bindings env so projection-typed fields like
        // `pulled: T::Item` resolve via the matching impl.
        enforceBoundsAndBuildAssocs(s"struct '$name'", template.typeParams, typeArgs, template.typeBounds) match
          case Left(err) => throw AnalysisError(err)
          case Right(_)  => ()
        val mangled = name + "_" + typeArgs.map(typeToMangled).mkString("_")
        // Insert a placeholder StructType to handle recursive field types
        val placeholder: SyslType.StructType = SyslType.StructType(mangled, Nil)
        genericStructInstantiations(cacheKey) = placeholder
        structTypes(mangled) = placeholder
        structToTemplate(mangled) = (name, typeArgs)
        val savedEnv = typeEnv
        val savedAssocs = assocBindingsEnv
        typeEnv = typeEnv ++ template.typeParams.zip(typeArgs).toMap
        assocBindingsEnv = buildAssocBindingsEnv(template.typeParams, typeArgs, template.typeBounds)
        try
          val resolvedFields = template.fields.map((n, t, _) => (n, resolveType(t)))
          val st: SyslType.StructType = SyslType.StructType(mangled, resolvedFields)
          genericStructInstantiations(cacheKey) = st
          structTypes(mangled) = st
          specializedDecls += TStructDecl(mangled, resolvedFields)
          st
        finally
          typeEnv = savedEnv
          assocBindingsEnv = savedAssocs

  // Instantiate a generic enum with concrete type arguments, returning its EnumType
  protected def instantiateGenericEnum(name: String, typeArgs: List[SyslType]): SyslType.EnumType =
    val template0 = genericEnums(name)
    val filledArgs = fillTypeArgsFromDefaults(name, "enum", template0.typeParams, template0.typeParamDefaults, typeArgs)
    val cacheKey = (name, filledArgs)
    genericEnumInstantiations.get(cacheKey) match
      case Some(et) => et
      case None =>
        val template = template0
        val typeArgs = filledArgs
        // Enforce trait bounds declared on the enum's type parameters (Phase C
        // follow-up) and build the assoc-bindings env so projection-typed variant
        // fields like `pulled: T::Item` resolve via the matching impl.
        enforceBoundsAndBuildAssocs(s"enum '$name'", template.typeParams, typeArgs, template.typeBounds) match
          case Left(err) => throw AnalysisError(err)
          case Right(_)  => ()
        val mangled = name + "_" + typeArgs.map(typeToMangled).mkString("_")
        val savedEnv = typeEnv
        val savedAssocs = assocBindingsEnv
        typeEnv = typeEnv ++ template.typeParams.zip(typeArgs).toMap
        assocBindingsEnv = buildAssocBindingsEnv(template.typeParams, typeArgs, template.typeBounds)
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
        finally
          typeEnv = savedEnv
          assocBindingsEnv = savedAssocs

  // Analyze each impl method (including synthesized defaults) as a mangled top-level function
  protected def analyzeImplMethods(impl: ImplDeclAST): List[TDecl] =
    // Generic impls (typeParams.nonEmpty) are NOT analyzed at registration time —
    // they're specialized on demand at each dispatch site by `instantiateImpl`.
    if impl.typeParams.nonEmpty then return Nil
    val resolvedTargets = impl.targetTypes.map(resolveType)
    val template = implTemplates.getOrElse(impl.traitName, Nil).find(t =>
      t.typeParams.isEmpty && t.resolvedConcrete.contains(resolvedTargets)
    ).getOrElse(
      throw AnalysisError(s"internal: impl of '${impl.traitName}' for ${resolvedTargets.mkString(", ")} not registered"))
    val methodMap = template.methods
    val infos = template.methodInfos
    val trait_ = traits(impl.traitName)
    val savedEnv = typeEnv
    val savedRewrite = traitCallRewrite
    val savedAssocs = assocBindingsEnv
    // Activate the impl's assoc bindings so projection types in trait method
    // bodies (`var t: Self::Item = ...`, casts, etc.) resolve correctly when
    // analyzing an inherited default body. The set is empty for impls without
    // assoc types, in which case projection use is rejected as before.
    assocBindingsEnv = template.assocBindings.map(b => (b.name, resolveType(b.target))).toMap
    try
      // For synthesized defaults, set typeEnv + traitCallRewrite so trait params resolve
      // and unqualified calls to sibling trait methods route to the impl's mangled
      // functions. Multi-param traits get a multi-entry env; the convention scales.
      infos.map { info =>
        if info.isSynthesized then
          typeEnv = trait_.typeParams.zip(resolvedTargets).toMap
          traitCallRewrite = methodMap.toMap
        else
          typeEnv = savedEnv
          traitCallRewrite = savedRewrite
        scopeStack = new mutable.ArrayBuffer
        pushScope()
        // Determine which params are by-name from the original impl-method AST
        // (or trait method, for synthesized defaults). Body references to
        // by-name params must auto-call; that's enforced via SymInfo.isByName.
        // ImplMethodInfo has no `name` field; reverse-lookup via methodMap.
        val originalName: Option[String] =
          methodMap.collectFirst { case (n, m) if m == info.mangled => n }
        val sourceParams: List[ParamAST] =
          originalName match
            case Some(n) =>
              impl.methods.find(_.name == n).map(_.params)
                .orElse(trait_.methods.find(_.name == n).map(_.params))
                .getOrElse(Nil)
            case None => Nil
        val byNameFlags: List[Boolean] =
          if sourceParams.length == info.paramTypes.length then
            sourceParams.map(_.typ.isInstanceOf[ByNameTypeAST])
          else List.fill(info.paramTypes.length)(false)
        for (((paramName, paramType), idx) <- info.paramTypes.zipWithIndex) do
          val isByN = idx < byNameFlags.length && byNameFlags(idx)
          val visibleType = paramType match
            case FuncType(Nil, ret, _, _) if isByN => ret
            case other => other
          currentScope(paramName) = SymInfo(paramName, visibleType, true, isByName = isByN)
        // Persist by-name flags onto the impl method's FunInfo so direct CallAST
        // dispatch (Trait.method(..) routed through traitCallRewrite, or direct
        // mangled-name calls) auto-wraps args at by-name slots.
        if byNameFlags.exists(identity) then
          functions.get(info.mangled).foreach { fi =>
            functions(info.mangled) = fi.copy(byName = byNameFlags)
          }
        val tBody = info.body match
          case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
          case BlockBodyAST(stmts, _) => TBlockBody(analyzeBlock(stmts))
        val tParams = info.paramTypes.map((n, t) => TParam(n, t))
        scopeStack = null
        TFunDecl(info.mangled, tParams, info.retType, tBody, isPrivate = false)
      }
    finally
      typeEnv = savedEnv
      traitCallRewrite = savedRewrite
      assocBindingsEnv = savedAssocs

  /** Stage G — specialize a generic impl method at a use site, or fast-path return for a
   *  concrete impl. Cached by `(template-identity, methodName, sortedSubst)` so repeated
   *  dispatches with the same operand types reuse one mangled function.
   *
   *  For concrete impls (`subst.isEmpty`), looks up the pre-mangled function. For generic
   *  impls, builds a typeEnv from `trait.typeParams ++ impl.typeParams` (the impl's vars
   *  also need to be in scope — patterns like `Bar[X]` mention `X`), resolves param/return
   *  types, mangles by hash of the substitution, registers a TFunDecl, and emits it via
   *  `specializedDecls`.
   */
  protected def instantiateImpl(
      template: ImplTemplate,
      traitName: String,
      methodName: String,
      subst: Map[String, SyslType],
  ): (String, FunInfo) =
    if template.typeParams.isEmpty then
      // Concrete: pre-registered at impl-declaration time
      val mangled = template.methods.getOrElse(methodName,
        throw AnalysisError(s"trait method '$traitName.$methodName' not implemented in this impl"))
      val funInfo = functions.getOrElse(mangled,
        functions.getOrElse(shortName(mangled),
          throw AnalysisError(s"trait method '$traitName.$methodName' resolved to '$mangled' but function not found")))
      (mangled, funInfo)
    else
      // Generic: specialize on demand. Cache key includes the substitution applied to
      // every type variable (sorted for determinism) plus the template identity.
      val sortedSubst = template.typeParams.map(tp => tp -> subst(tp))
      val cacheKey = (System.identityHashCode(template), methodName, sortedSubst)
      genericImplInstantiations.get(cacheKey) match
        case Some((m, fi)) => (m, fi)
        case None =>
          val trait_ = traits(traitName)
          // Phase C follow-up — enforce trait bounds on the impl's type
          // parameters at the moment a concrete substitution is selected. For
          // each impl tparam with bounds, verify that subst(tparam) is implemented
          // by some impl of every bound trait. Mirrors `enforceBoundsAndBuildAssocs`
          // shape but without assoc-binding env construction — those flow through
          // the impl's own assocBindings.
          for (tp, bounds) <- template.typeBounds do
            val concrete = subst.getOrElse(tp,
              throw AnalysisError(s"impl bound check: type parameter '$tp' has no substitution"))
            for boundTrait <- bounds do
              val matched = implTemplates.getOrElse(boundTrait, Nil).exists { t =>
                if t.typeParams.isEmpty then
                  t.resolvedConcrete.flatMap(_.headOption).contains(concrete)
                else
                  tryUnifyAll(t.targetPatterns.headOption.toList, List(concrete), t.typeParams.toSet).isDefined
              }
              if !matched then
                throw AnalysisError(s"type $concrete does not satisfy bound '$boundTrait' for impl type parameter '$tp' of trait '$traitName'")
          // Phase C follow-up — enforce trait bounds on the trait's own type
          // parameters at the moment the impl is selected. For each trait
          // tparam with bounds, the corresponding resolved target (under the
          // impl's substitution) must satisfy each bound. Concrete impls
          // checked this at registration; generic impls defer here because
          // their target patterns may reference impl tvars whose substitution
          // wasn't known until now.
          if trait_.typeBounds.nonEmpty then
            val savedEnvForTraitBounds = typeEnv
            typeEnv = typeEnv ++ subst
            try
              for ((tp, pat) <- trait_.typeParams.zip(template.targetPatterns)) do
                val tBounds = trait_.typeBounds.getOrElse(tp, Nil)
                if tBounds.nonEmpty then
                  val resolvedPat = resolveType(pat)
                  for boundTrait <- tBounds do
                    val matched = implTemplates.getOrElse(boundTrait, Nil).exists { t =>
                      if t.typeParams.isEmpty then
                        t.resolvedConcrete.flatMap(_.headOption).contains(resolvedPat)
                      else
                        tryUnifyAll(t.targetPatterns.headOption.toList, List(resolvedPat), t.typeParams.toSet).isDefined
                    }
                    if !matched then
                      throw AnalysisError(s"impl of '$traitName' for $resolvedPat does not satisfy bound '$boundTrait' declared on type parameter '$tp'")
            finally typeEnv = savedEnvForTraitBounds
          // Phase C.2 — enforce trait bounds on associated-type bindings for
          // generic impls. The concrete-impl path checks at registration; for
          // generic impls the binding target may reference impl tvars (e.g.
          // `type Token = T`), so we defer to here where `subst` is known.
          // Resolve each binding under the substitution and validate against
          // the trait's declared assoc bounds.
          if template.assocBindings.nonEmpty && trait_.assocTypes.exists(_.bounds.nonEmpty) then
            val assocBoundsByName = trait_.assocTypes.map(a => (a.name, a.bounds)).toMap
            val savedEnvForAssocBounds = typeEnv
            typeEnv = typeEnv ++ subst
            try
              for b <- template.assocBindings do
                val bounds = assocBoundsByName.getOrElse(b.name, Nil)
                if bounds.nonEmpty then
                  val resolvedBindingTarget = resolveType(b.target)
                  for boundTrait <- bounds do
                    if !traits.contains(boundTrait) then
                      throw AnalysisError(s"associated type 'type ${b.name}' on trait '$traitName' references unknown trait '$boundTrait'")
                    val matched = implTemplates.getOrElse(boundTrait, Nil).exists { t =>
                      if t.typeParams.isEmpty then
                        t.resolvedConcrete.flatMap(_.headOption).contains(resolvedBindingTarget)
                      else
                        tryUnifyAll(t.targetPatterns.headOption.toList, List(resolvedBindingTarget), t.typeParams.toSet).isDefined
                    }
                    if !matched then
                      throw AnalysisError(s"generic impl of '$traitName' binds 'type ${b.name} = $resolvedBindingTarget' (under substitution), which does not satisfy bound '$boundTrait' declared on the associated type")
            finally typeEnv = savedEnvForAssocBounds
          // Resolve the trait's targetPatterns under the new substitution to obtain the
          // concrete trait-level types — these become the trait typeParam → concrete map.
          val savedEnv = typeEnv
          typeEnv = typeEnv ++ subst
          val resolvedTargets =
            try template.targetPatterns.map(resolveType)
            finally typeEnv = savedEnv
          val typeMangled = resolvedTargets.map(typeToMangled).mkString("_")
          val implMethod = template.methodASTs.find(_.name == methodName).getOrElse {
            // Method not provided by impl — must be a synthesized default from the trait
            val tm = trait_.methods.find(_.name == methodName).getOrElse(
              throw AnalysisError(s"trait '$traitName' has no method '$methodName'"))
            // Synthesize a FunDeclAST from the trait method's default body
            val body = tm.body.getOrElse(
              throw AnalysisError(s"impl missing required method '$methodName' (no default in trait)"))
            FunDeclAST(tm.name, tm.params, Some(tm.returnType), body)
          }
          val rawMangled = s"${traitName}_${methodName}_${typeMangled}"
          val mangled = if shouldMangle(rawMangled) then mangleName(rawMangled) else rawMangled
          // Build the full typeEnv: trait type params bound to resolved targets, plus
          // impl type params bound to subst (so patterns like `Bar[X]` referenced inside
          // the method resolve correctly).
          val fullEnv = trait_.typeParams.zip(resolvedTargets).toMap ++ subst
          typeEnv = typeEnv ++ fullEnv
          // Activate the impl's assoc bindings — resolved under the substituted typeEnv
          // so generic-impl bindings like `type Item = T` land as the substituted type.
          val savedAssocs = assocBindingsEnv
          assocBindingsEnv = template.assocBindings.map(b => (b.name, resolveType(b.target))).toMap
          val (paramTypes, retType, body, isSynthesized) =
            try
              val pTypes = implMethod.params.map(p => (p.name, resolveType(p.typ)))
              val r = implMethod.returnType.map(resolveType).getOrElse(UnitType)
              val provided = template.methodASTs.exists(_.name == methodName)
              (pTypes, r, implMethod.body, !provided)
            finally typeEnv = savedEnv
          val byNameFlags: List[Boolean] = implMethod.params.map(_.typ.isInstanceOf[ByNameTypeAST])
          val funInfo = FunInfo(mangled, paramTypes, retType, byName = if byNameFlags.exists(identity) then byNameFlags else Nil)
          functions(mangled) = funInfo
          // Save into template.methods so cross-method dispatch (e.g. default methods that
          // call sibling methods) finds the same specialization.
          template.methods(methodName) = mangled
          genericImplInstantiations(cacheKey) = (mangled, funInfo)
          // Analyze the body in the substituted env, with traitCallRewrite set so default
          // methods route sibling trait-method calls to this impl's mangled functions.
          val savedRewrite = traitCallRewrite
          val savedScope = scopeStack
          val savedLoopDepth = loopDepth
          typeEnv = typeEnv ++ fullEnv
          if isSynthesized then traitCallRewrite = template.methods.toMap
          scopeStack = new mutable.ArrayBuffer
          loopDepth = 0
          pushScope()
          for (((paramName, paramType), idx) <- paramTypes.zipWithIndex) do
            val isByN = idx < byNameFlags.length && byNameFlags(idx)
            val visibleType = paramType match
              case FuncType(Nil, ret, _, _) if isByN => ret
              case other => other
            currentScope(paramName) = SymInfo(paramName, visibleType, true, isByName = isByN)
          val tBody =
            try body match
              case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
              case BlockBodyAST(stmts, _) => TBlockBody(analyzeBlock(stmts))
            finally
              typeEnv = savedEnv
              traitCallRewrite = savedRewrite
              scopeStack = savedScope
              loopDepth = savedLoopDepth
              assocBindingsEnv = savedAssocs
          val tParams = paramTypes.map((n, t) => TParam(n, t))
          specializedDecls += TFunDecl(mangled, tParams, retType, tBody, isPrivate = false)
          (mangled, funInfo)

  protected val genericImplInstantiations = new mutable.HashMap[(Int, String, List[(String, SyslType)]), (String, FunInfo)]

  // Resolve a trait method call like Ord.cmp(a, b) to the appropriate impl's mangled function.
  // Stage G: enumerates candidates by unifying each impl's method param patterns directly
  // against the call's arg types. Result-position trait params (e.g. R in Concat[A, B, R])
  // don't need to be inferable from arg types — they're determined by which impl matches.
  protected def analyzeTraitCall(traitName: String, methodName: String, tArgs: List[TExpr]): (String, FunInfo) =
    val trait_ = traits(traitName)
    val method = trait_.methods.find(_.name == methodName).getOrElse(
      throw AnalysisError(s"trait '$traitName' has no method '$methodName'"))
    if method.params.length != tArgs.length then
      throw AnalysisError(s"trait method '$traitName.$methodName' expects ${method.params.length} argument(s), got ${tArgs.length}")
    val argTypes = tArgs.map(_.typ)
    val candidates = enumerateImplCandidates(traitName, methodName, argTypes)
    candidates match
      case Nil =>
        throw AnalysisError(s"no impl of trait '$traitName.$methodName' matches arg type(s) ${argTypes.mkString(", ")}")
      case (template, subst) :: Nil =>
        instantiateImpl(template, traitName, methodName, subst)
      case multi =>
        throw AnalysisError(s"ambiguous: ${multi.length} impls of '$traitName' match $methodName(${argTypes.mkString(", ")})")

  protected def instantiateGeneric(
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
    // Phase B — fill any still-unbound type parameters from declared defaults.
    // Defaults resolve under the partial env so a later default may reference an
    // earlier param (e.g. `[I, O = I]`). Iterate in declaration order so
    // earlier-pinned params are visible to later defaults.
    // Phase B.2 — defaults may also reference the projections of already-pinned
    // bounded params (e.g. `[I: Reader, T = I::Token]`). Before resolving each
    // default, build a partial assocBindingsEnv from the params pinned so far.
    for tp <- typeParams if !env.contains(tp) do
      template.typeParamDefaults.get(tp) match
        case Some(defAst) =>
          val savedEnvB = typeEnv
          val savedAssocsB = assocBindingsEnv
          typeEnv = typeEnv ++ env.toMap
          val pinnedSoFar = typeParams.takeWhile(env.contains)
          assocBindingsEnv = buildAssocBindingsEnv(
            pinnedSoFar,
            pinnedSoFar.map(env(_)),
            template.typeBounds,
          )
          try env(tp) = resolveType(defAst)
          finally
            typeEnv = savedEnvB
            assocBindingsEnv = savedAssocsB
        case None => // leave unbound; the next pass produces the diagnostic
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
        // Bound is satisfied if the trait has any registered impl whose first target
        // pattern unifies with `concreteType` (concrete fast-path or generic impl).
        val trait_ = traits(traitName)
        val matched = implTemplates.getOrElse(traitName, Nil).exists { t =>
          if t.typeParams.isEmpty then
            t.resolvedConcrete.flatMap(_.headOption).contains(concreteType)
          else
            tryUnifyAll(t.targetPatterns.headOption.toList, List(concreteType), t.typeParams.toSet).isDefined
        }
        if !matched then
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
        val savedAssocsInst = assocBindingsEnv
        typeEnv = typeParams.zip(inferredArgs).toMap
        // Phase A3 — for each bounded type parameter, locate the matching impl
        // and pull its associated-type bindings into `assocBindingsEnv` so that
        // projections like `T::Item` in the generic body resolve to the impl's
        // declared type. The bound check above already validated that a matching
        // impl exists; we re-find it here to capture both the impl template and
        // (for generic impls) the substitution that pins its type parameters.
        // Multi-bound disambiguation: if two distinct bound traits both bind the
        // same assoc name to *different* resolved types, the projection is
        // ambiguous and we reject the call. Repeated identical bindings (same
        // name, same resolved type) are fine.
        val newAssocs = mutable.Map.empty[String, SyslType]
        val newAssocSources = mutable.Map.empty[String, String]
        for tp <- typeParams do
          val bounds = template.typeBounds.getOrElse(tp, Nil)
          val concreteType = env(tp)
          for traitName <- bounds do
            val matched = implTemplates.getOrElse(traitName, Nil).iterator.flatMap { t =>
              if t.typeParams.isEmpty then
                if t.resolvedConcrete.flatMap(_.headOption).contains(concreteType)
                then Some((t, Map.empty[String, SyslType]))
                else None
              else
                tryUnifyAll(t.targetPatterns.headOption.toList, List(concreteType), t.typeParams.toSet)
                  .map(s => (t, s))
            }.nextOption()
            for (impl, implSubst) <- matched do
              val savedEnv2 = typeEnv
              typeEnv = typeEnv ++ implSubst
              try
                for binding <- impl.assocBindings do
                  val resolved = resolveType(binding.target)
                  newAssocs.get(binding.name) match
                    case Some(prior) if prior != resolved =>
                      val priorTrait = newAssocSources.getOrElse(binding.name, "?")
                      throw AnalysisError(
                        s"projection '${tp}::${binding.name}' is ambiguous in '$name': " +
                          s"bound '$priorTrait' binds it to $prior but bound '$traitName' binds it to $resolved",
                      )
                    case _ =>
                      newAssocs(binding.name) = resolved
                      newAssocSources(binding.name) = traitName
              finally typeEnv = savedEnv2
        assocBindingsEnv = newAssocs.toMap
        try
          // Resolve param/return types in the new env
          val paramTypes = template.params.map(p => (p.name, resolveType(p.typ)))
          val retType = template.returnType.map(resolveType).getOrElse(UnitType)
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
              case BlockBodyAST(stmts, _) => TBlockBody(analyzeBlock(stmts))
          finally
            currentExpected = savedExpectedInst
          scopeStack = savedScopeStack
          loopDepth = savedLoopDepth
          val tParams = paramTypes.map((n, t) => TParam(n, t))
          specializedDecls += TFunDecl(mangled, tParams, retType, tBody, template.isPrivate)
          (mangled, funInfo)
        finally
          typeEnv = savedEnv
          assocBindingsEnv = savedAssocsInst

  /** Build a typed positional arg list from a mix of positional and named args.
    * Rules:
    *   - Positional args must come before named args.
    *   - Each named arg must match a parameter name (once).
    *   - Missing slots are filled with typed defaults from `functionDefaults`.
    *   - If a missing slot has no default, it is left uncovered — `checkArgs`
    *     will error on the mismatched length.
    *
    * `paramTypes` provides expected types for type inference during analysis.
    */
  protected def resolveNamedArgsTyped(
    fnName: String,
    paramNames: List[String],
    paramTypes: List[SyslType],
    args: List[ExpressionAST],
  ): List[TExpr] =
    // Validate ordering: no positional after named.
    var seenNamed = false
    for arg <- args do
      arg match
        case _: NamedArgAST => seenNamed = true
        case _ =>
          if seenNamed then
            throw AnalysisError(s"positional argument after named argument in call to '$fnName'")

    val slots = Array.fill[Option[TExpr]](paramNames.length)(None)
    val namedSeen = mutable.HashSet[String]()
    var pos = 0
    for arg <- args do
      arg match
        case NamedArgAST(argName, value) =>
          val idx = paramNames.indexOf(argName)
          if idx < 0 then
            throw AnalysisError(s"unknown parameter '$argName' in call to '$fnName'")
          if slots(idx).isDefined then
            throw AnalysisError(s"parameter '$argName' of '$fnName' already has a value (duplicate or positional conflict)")
          if !namedSeen.add(argName) then
            throw AnalysisError(s"duplicate named argument '$argName' in call to '$fnName'")
          val savedExp = currentExpected
          currentExpected = Some(paramTypes(idx))
          slots(idx) = Some(try analyzeExpr(value) finally currentExpected = savedExp)
        case other =>
          if pos >= paramNames.length then
            throw AnalysisError(s"too many positional arguments in call to '$fnName'")
          val savedExp = currentExpected
          currentExpected = Some(paramTypes(pos))
          slots(pos) = Some(try analyzeExpr(other) finally currentExpected = savedExp)
          pos += 1

    // Fill missing slots from typed defaults.
    val defaults = functionDefaults.getOrElse(fnName, Nil)
    val result = new scala.collection.mutable.ListBuffer[TExpr]
    for i <- paramNames.indices do
      slots(i) match
        case Some(e) => result += e
        case None =>
          if i < defaults.length && defaults(i).isDefined then
            result += defaults(i).get
          // else: leave unfilled — checkArgs will report an arity error
    result.toList

  /** For an `out`/`inout` argument, take an already-analyzed arg (seen as plain T) and
   *  wrap it as a pointer so it matches the hidden `*T` param slot. Accepts any simple
   *  lvalue: a variable reference, a field access, an index, or an already-dereferenced
   *  pointer (`&*p` collapses to `p`). Anything else is rejected — literals, temporaries,
   *  and calls have no address. */
  protected def lvalueToAddr(name: String, pName: String, mode: ParamMode, arg: TExpr): TExpr =
    val inner = arg.typ
    arg match
      case TVarRef(n, t)              => TAddrOf(n, PtrType(t))
      case TFieldAccess(obj, idx, t)  => TAddrOfField(obj, idx, PtrType(t))
      case TIndex(arr, ix, t)         => TAddrOfIndex(arr, ix, PtrType(t))
      case TDeref(p, _)               => p // &*p = p
      case _ =>
        throw AnalysisError(s"argument '$pName' of '$name' is '${mode.toString.toLowerCase}' and requires an lvalue (variable, field, or index), got ${arg.getClass.getSimpleName}")

  protected def checkArgs(name: String, params: List[(String, SyslType)], args: List[TExpr], modes: List[ParamMode] = Nil): List[TExpr] =
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
    filledArgs.zip(params).zipWithIndex.map { case ((arg, (pName, pType)), idx) =>
      val mode = if modes.isEmpty then ParamMode.In else modes(idx)
      mode match
        case ParamMode.Out | ParamMode.Inout =>
          // pType is PtrType(inner). Require arg to be T-valued lvalue, wrap with TAddrOf*.
          val innerT = pType match
            case PtrType(t) => t
            case _          => pType // defensive — collectDecls wrapped it
          if !compatible(arg.typ, innerT) then
            throw AnalysisError(s"argument '$pName' of '$name' is '${mode.toString.toLowerCase}' expecting $innerT, got ${arg.typ}")
          lvalueToAddr(name, pName, mode, arg)
        case ParamMode.In =>
          val coerced = coerceLiteral(arg, pType)
          // Special case: `self` (always *T inside methods, mangled as __self__) auto-derefs
          // when passed to a T param. This is the ONLY implicit *T → T allowed; elsewhere write `*ptr`.
          val selfDeref = (coerced, pType) match
            case (TVarRef("__self__", PtrType(st: StructType)), pSt: StructType) if st.name == pSt.name =>
              Some(TDeref(coerced, pSt))
            case _ => None
          if selfDeref.isEmpty && !compatible(coerced.typ, pType) then
            throw AnalysisError(s"argument '$pName' of '$name' expects $pType, got ${coerced.typ}")
          // Insert explicit conversions for codegen
          val converted = selfDeref.getOrElse {
            (coerced.typ, pType) match
              case (StringType, PtrType(I8 | U8)) => TCast(coerced, pType)
              case (_: FuncType, IntType(64) | UIntType(64)) => TCast(coerced, pType)
              case (_, iface: InterfaceType) if !coerced.typ.isInstanceOf[InterfaceType] =>
                TInterfaceBox(coerced, iface)
              case (ArrayType(_, _), st: SliceType) =>
                // Array → slice arg: backends need a concrete slice descriptor at the call
                // site. Without this, TRISC pushes the array's address as a single 8-byte
                // scalar and the callee reads slice fields from arbitrary memory. Modeling
                // the conversion as `arg[:]` keeps codegen in one place.
                TSliceExpr(coerced, None, None, st)
              case _ => coerced
          }
          applyTargetType(converted, pType)
    }

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
  private def analyzeContractedInnerBody(
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
      case TInterfaceBox(inner, iface)     => TInterfaceBox(go(inner), iface)
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
        TIndexAssignStmt(tArray, tIndex, tValue)

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
        val assign = TFieldAssignStmt(resolvedObj, idx, tValue)
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
          applyTargetType(tv, currentReturnType)
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
      case et: SyslType.EnumType =>
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
      // a struct or variant name (of the scrutinee's enum type) becomes a
      // fresh binding capturing the whole scrutinee. Mirrors the destructure
      // field-binding convention (see `analyzeFieldPattern` at line 6923):
      // names bind freely; module-level consts referenced as patterns must
      // be written `MOD.OK` or destructured explicitly.
      case ValuePatternAST(VarRefAST(name))
          if scopeStack != null
            && !structTypes.contains(name)
            && resolveVariant(name, scrutineeType).isEmpty =>
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

object SyslAnalyzer:
  /** Reserved attribute names usable as `T::Attr` for enum and within-int
   *  introspection. Associated-type declarations may not reuse these names —
   *  doing so would cause ambiguity at projection sites once Phase A3 lands.
   *  Listed in the language reference under "Type Attributes". */
  val ReservedTypeAttrNames: Set[String] =
    Set("First", "Last", "Range", "Image", "Value", "Valid", "Pos", "Val", "Succ", "Pred")

