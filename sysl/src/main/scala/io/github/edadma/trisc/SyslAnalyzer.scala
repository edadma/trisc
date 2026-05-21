package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

class SyslAnalyzer(val contractsEnabled: Boolean = true) extends SyslAnalyzerContracts, SyslAnalyzerPostPasses, SyslAnalyzerStatements, SyslAnalyzerTypes, SyslAnalyzerUnification, SyslAnalyzerGenerics, SyslAnalyzerExtensions, SyslAnalyzerImports, SyslAnalyzerOperators, SyslAnalyzerCore, SyslAnalyzerExpressions:
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
  protected case class FunInfo(name: String, params: List[(String, SyslType)], returnType: SyslType, isDef: Boolean = false, isPure: Boolean = false, modes: List[ParamMode] = Nil, reads: Option[Set[String]] = None, writes: Option[Set[String]] = None, isGhost: Boolean = false, byName: List[Boolean] = Nil, isParameterless: Boolean = false, isRealtime: Boolean = false):
    def modeOf(i: Int): ParamMode = if modes.isEmpty then ParamMode.In else modes(i)
    def isByNameAt(i: Int): Boolean = byName.nonEmpty && i < byName.length && byName(i)
    def autoCallsBare: Boolean = isDef || isParameterless
    def hasEffectAnnotations: Boolean = reads.isDefined || writes.isDefined || isPure || isRealtime

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

