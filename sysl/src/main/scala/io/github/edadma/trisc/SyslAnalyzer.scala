package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

class SyslAnalyzer(val contractsEnabled: Boolean = true) extends SyslAnalyzerContracts, SyslAnalyzerPostPasses, SyslAnalyzerStatements, SyslAnalyzerTypes, SyslAnalyzerUnification, SyslAnalyzerGenerics, SyslAnalyzerExtensions, SyslAnalyzerImports, SyslAnalyzerOperators, SyslAnalyzerExpressions:
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
        val tBodyFixed = rewriteEscapingClosureCaptureOwns(tBody)
        TFunDecl(funInfo.name, tParams, retType, tBodyFixed, isPrivate, attrs, funInfo.isDef, isGhost = funInfo.isGhost, effects = funInfoEffects(funInfo), isParameterless = funInfo.isParameterless)

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


object SyslAnalyzer:
  /** Reserved attribute names usable as `T::Attr` for enum and within-int
   *  introspection. Associated-type declarations may not reuse these names —
   *  doing so would cause ambiguity at projection sites once Phase A3 lands.
   *  Listed in the language reference under "Type Attributes". */
  val ReservedTypeAttrNames: Set[String] =
    Set("First", "Last", "Range", "Image", "Value", "Valid", "Pos", "Val", "Succ", "Pred")

