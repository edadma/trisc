package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

class SyslAnalyzer(val contractsEnabled: Boolean = true):
  case class AnalysisError(msg: String, node: Any = null) extends RuntimeException(msg)

  /** Build a contract-check statement, or a no-op when contracts are disabled (via
   *  `--no-contracts` / `config("contracts") = "off"`). Used by every contract emission
   *  site — require/ensure/invariant/type predicate/type attribute — so the strip is
   *  centralized. When disabled, the check becomes `TMultiStmt(Nil)`: every backend
   *  already iterates zero children. The surrounding expression (e.g. `v + 1` in
   *  `__succ`) continues to execute without a guard, giving Ada-style "suppressed
   *  check" semantics: fast path, undefined on bad input. */
  private def contract(kind: String, expr: TExpr, message: String): TStmt =
    if contractsEnabled then TContractCheck(kind, expr, message)
    else TMultiStmt(Nil)

  /** `autoIndirect = true` marks a body-scope symbol whose actual storage is a hidden
   *  pointer (Ada `out` / `inout` parameter). Every read/write goes through `*ptr`:
   *  VarRef lowers to `TDeref(TVarRef(name, *T), T)`; plain and compound assignments
   *  lower to `TDerefAssignStmt(TVarRef(name, *T), v)`. The caller passes an lvalue
   *  auto-wrapped with `TAddrOf*`. `typ` is still the underlying `T` (what the body
   *  sees); the pointer wrap is invisible to user code. */
  private case class SymInfo(name: String, typ: SyslType, mutable: Boolean, isConst: Boolean = false, autoIndirect: Boolean = false, isGhost: Boolean = false)
  /** `modes` is parallel to `params`: one entry per parameter. Empty means "all In"
   *  (default, back-compat). `params` stores the call-side signature: for `Out`/`Inout`
   *  this is `*T` so `checkArgs` and codegen see the hidden-pointer type; the body-scope
   *  view is the inner `T` with `autoIndirect = true`. */
  /** `reads`/`writes` carry the *raw* (local) names from `#reads(...)` / `#writes(...)`
   *  attributes. `None` means the function is unannotated; `Some(Set.empty)` means it
   *  declared an empty set ("no module-level effects"). Resolution to the canonical
   *  mangled name + mutability/scope validation is deferred to `validateEffects`, which
   *  runs after the function body is analyzed (so all relevant globals are in scope). */
  private case class FunInfo(name: String, params: List[(String, SyslType)], returnType: SyslType, isDef: Boolean = false, isPure: Boolean = false, modes: List[ParamMode] = Nil, reads: Option[Set[String]] = None, writes: Option[Set[String]] = None, isGhost: Boolean = false):
    def modeOf(i: Int): ParamMode = if modes.isEmpty then ParamMode.In else modes(i)
    def hasEffectAnnotations: Boolean = reads.isDefined || writes.isDefined || isPure

  private val globalScope = new mutable.LinkedHashMap[String, SymInfo]
  private val functions = new mutable.LinkedHashMap[String, FunInfo]
  // Default parameter expressions: function name → list of defaults (one per param, None if no default)
  private val functionDefaults = new mutable.LinkedHashMap[String, List[Option[TExpr]]]
  private val structTypes = new mutable.LinkedHashMap[String, SyslType.StructType]
  // Struct name → list of invariant expressions declared in the struct body.
  // Checked at every field-assignment / field-compound-assignment on a value of that type.
  private val structInvariants = new mutable.LinkedHashMap[String, List[ExpressionAST]]
  private val enumTypes = new mutable.LinkedHashMap[String, Map[String, Long]]  // enum name → (member name → value)
  // Simple enums registered as EnumType so they can appear in type positions.
  // Distinct from dataEnumTypes because simple-enum `Name.Member` access still
  // lowers to TIntLit (integer constant), not TEnumConstruct.
  private val simpleEnumTypes = new mutable.LinkedHashMap[String, SyslType.EnumType]
  private val dataEnumTypes = new mutable.LinkedHashMap[String, SyslType.EnumType]  // data enum name → EnumType
  private val variantToEnum = new mutable.LinkedHashMap[String, (SyslType.EnumType, Int)]  // variant name → (enum type, variant index)
  private val interfaceTypes = new mutable.LinkedHashMap[String, SyslType.InterfaceType]  // interface name → InterfaceType
  private val moduleNamespaces = new mutable.LinkedHashMap[String, ModuleMeta]  // short name → module meta (for qualified imports)
  // alias name → (target type AST, isNew flag, optional within-range, optional where-predicate AST)
  private val typeAliases = new mutable.LinkedHashMap[String, (TypeAST, Boolean, Option[RangeAST], Option[ExpressionAST])]
  // name → (type params, target, isNew). `isNew=true` means each instantiation is a
  // distinct nominal type (`type Parser[A] = new (Input) -> ParseResult[A]`); `false` is
  // the historical transparent expansion. `within`/`where` remain rejected for generic
  // aliases (no scalar ordering / no operations on a bare T without trait bounds).
  private val genericTypeAliases = new mutable.LinkedHashMap[String, (List[String], TypeAST, Boolean)]
  // Cached SyslType for each instantiation of a `new` generic alias. Keyed by (template
  // name, type args). Mirrors `genericStructInstantiations` so repeated mentions of
  // `Parser[i32]` return the same NamedType instance (object-equality dispatch).
  private val genericAliasInstantiations = new mutable.LinkedHashMap[(String, List[SyslType]), SyslType]
  // Reverse map: mangled instantiation name → (template name, concrete args). Used by
  // the unifier to bind type vars when matching `Parser[A]` against `NamedType("Parser_i32", ...)`.
  private val genericAliasToTemplate = new mutable.LinkedHashMap[String, (String, List[SyslType])]
  // Memoized resolved form of a named/derived/constrained alias. Plain transparent aliases
  // do not appear here — they resolve directly to their base.
  private val resolvedNamedTypes = new mutable.LinkedHashMap[String, SyslType]
  private val methods = new mutable.LinkedHashMap[String, mutable.Set[String]]  // struct name → set of method names
  private val deprecations = new mutable.LinkedHashMap[String, Option[String]]  // name → optional reason
  private val warnedDeprecations = new mutable.HashSet[String]
  private val externalSymbols = new mutable.LinkedHashSet[String]
  private var scopeStack: mutable.ArrayBuffer[mutable.LinkedHashMap[String, SymInfo]] = null
  private val compileTimeConstants = new mutable.LinkedHashMap[String, Long] // val name → folded value (for constant propagation)

  /** Module-level vars tagged with `#address(N)` map to a fixed physical address — used
   *  for MMIO device registers. Reads lower to `*(N as *T)`, writes to `*(N as *T) = v`.
   *  No storage is emitted (the var is just a handle on hardware). Both the local and
   *  mangled keys are recorded so cross-module references resolve. */
  private val fixedAddressVars = new mutable.LinkedHashMap[String, (Long, SyslType)]
  private var loopDepth: Int = 0
  // Stack of enclosing loop labels (None for unlabeled loops). Used to validate
  // `break label` / `continue label` refers to an enclosing labeled loop.
  private val loopLabelStack = new mutable.ArrayBuffer[Option[String]]

  /** Reject label shadowing — a labeled loop cannot be nested inside another loop with
   * the same label, because `break label` would be ambiguous. */
  private def checkLoopLabelUnique(label: Option[String]): Unit =
    label.foreach { lbl =>
      if loopLabelStack.contains(Some(lbl)) then
        throw AnalysisError(s"duplicate loop label '$lbl': already in use by an enclosing loop")
    }

  // Counter for uniquely naming hoisted `variant` state across nested / sibling loops.
  private var variantIdCounter: Int = 0

  /** Set of mangled names for module-level `#ghost var` declarations. Used by both the
   *  ghost-discipline post-pass (real code cannot read these) and the strip pass (writes
   *  to these are dropped from real-function bodies before codegen). */
  private val ghostNames = mutable.HashSet[String]()

  /** Type-check each invariant expression at struct declaration time. Invariants are
   *  analyzed in a scope where each field name binds to a local of the field's type, so
   *  type errors (wrong field name, non-bool result) are caught before any mutation site. */
  private def validateStructInvariants(structName: String, fields: List[(String, SyslType)], invariants: List[ExpressionAST]): Unit =
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
  private def substituteStructFieldRefs(expr: ExpressionAST, objAst: ExpressionAST, fields: Set[String]): ExpressionAST =
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
  private def buildStructInvariantChecks(objAst: ExpressionAST, structName: String): List[TStmt] =
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
  private def extractLeadingInvariants(body: List[StmtAST]): (List[(ExpressionAST, Option[String])], List[StmtAST]) =
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
  private def buildLoopInvariantChecks(invs: List[(ExpressionAST, Option[String])]): (List[TStmt], List[TStmt]) =
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
  private def extractVariants(body: List[StmtAST]): (List[StmtAST], List[StmtAST]) =
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
  private var currentReturnType: SyslType = UnitType

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
    "syscall_table", "syscall_table_6", "syscall_ssp", "irq_handlers", "ticks",
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
  // While analyzing an `ensure` expression, `old(x)` gets intercepted and rewritten
  // into a reference to a snapshot local captured at function entry.
  private var inEnsureAnalysis: Boolean = false
  private var oldSnapshotCounter: Int = 0
  private val oldSnapshots = mutable.ListBuffer.empty[(String, SyslType, TExpr)]
  // While analyzing a loop invariant, `loop_entry(x)` gets intercepted and rewritten
  // into a reference to a snapshot local captured at the moment control first reaches
  // the loop (before the first iteration).
  private var inLoopInvariantAnalysis: Boolean = false
  private var loopEntrySnapshotCounter: Int = 0
  private val loopEntrySnapshots = mutable.ListBuffer.empty[(String, SyslType, TExpr)]

  /** Per-function counter for naming the temps emitted at every recursive-call site of a
   *  function with a `variant` clause. Reset at each `analyzeBlockWithContracts` entry so
   *  the names are stable per function. */
  private var variantCallCounter: Int = 0

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
  private case class TraitInfo(name: String, typeParams: List[String], methods: List[TraitMethodAST])
  private case class ImplMethodInfo(mangled: String, paramTypes: List[(String, SyslType)], retType: SyslType, body: FunBodyAST, isSynthesized: Boolean)
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
  private case class ImplTemplate(
      typeParams: List[String],
      targetPatterns: List[TypeAST],
      resolvedConcrete: Option[List[SyslType]],
      methods: mutable.LinkedHashMap[String, String], // methodName -> mangledFunName
      methodInfos: List[ImplMethodInfo],
      methodASTs: List[FunDeclAST],                   // raw impl bodies (generic impls only)
      definingModule: String,
      implDecl: Option[ImplDeclAST] = None,           // for source-defined impls; None when imported
  )
  private val traits = new mutable.LinkedHashMap[String, TraitInfo]
  // traitName -> list of registered impls (templates). Order is registration order.
  private val implTemplates = new mutable.LinkedHashMap[String, mutable.ListBuffer[ImplTemplate]]

  // Stage F.5 bookkeeping: which module defined each trait / named type. Populated as
  // declarations are processed; consulted by the orphan rule at impl registration.
  private val traitDefiningModule = new mutable.LinkedHashMap[String, String]
  private val typeDefiningModule = new mutable.LinkedHashMap[String, String]

  /** Concrete-impl lookup: find a registered impl whose first resolved target is exactly
   *  `operandType` and which has no type params. Used by built-in trait method calls,
   *  concrete operator dispatch, and generic-bound checks. */
  private def findConcreteImpl(traitName: String, operandType: SyslType): Option[ImplTemplate] =
    implTemplates.get(traitName).flatMap { ts =>
      ts.find(t => t.typeParams.isEmpty && t.resolvedConcrete.flatMap(_.headOption).contains(operandType))
    }

  /** Iterate over all registered concrete impls (legacy shape) for cross-unit serialization
   *  and similar bookkeeping that hasn't been generalized yet. */
  private def concreteImpls: Iterator[(String, SyslType, mutable.LinkedHashMap[String, String])] =
    implTemplates.iterator.flatMap { case (traitName, ts) =>
      ts.iterator.collect {
        case t if t.typeParams.isEmpty && t.resolvedConcrete.exists(_.length == 1) =>
          (traitName, t.resolvedConcrete.get.head, t.methods)
      }
    }

  /** Walk a TypeAST collecting every named-type reference (`NamedTypeAST` heads), used by
   *  the orphan rule. Built-in scalar names (`int`, `i64`, `string`, …) are filtered out
   *  by the caller via `typeDefiningModule.contains`. */
  private def collectNamedTypeNames(t: TypeAST): Set[String] = t match
    case NamedTypeAST(n, args)    => Set(n) ++ args.flatMap(collectNamedTypeNames)
    case PtrTypeAST(i)            => collectNamedTypeNames(i)
    case PtrNonNullTypeAST(i)     => collectNamedTypeNames(i)
    case RefTypeAST(i)            => collectNamedTypeNames(i)
    case ArrayTypeAST(_, e)       => collectNamedTypeNames(e)
    case SliceTypeAST(e)          => collectNamedTypeNames(e)
    case FuncTypeAST(ps, r, _, _) => ps.flatMap(collectNamedTypeNames).toSet ++ collectNamedTypeNames(r)
    case TupleTypeAST(elems)      => elems.flatMap(collectNamedTypeNames).toSet

  /** Stage F.5 — orphan rule. An impl is allowed iff this module owns the trait OR at
   *  least one named type appearing in the impl's target patterns. Empty `currentModule`
   *  (no `module` declaration) is treated as the implicit "root" module: orphan check
   *  is bypassed since there is no other module to conflict with.
   *
   *  This deliberately does NOT recurse through generic-instance struct names — if the
   *  user impls `Concat[Parser[Foo], Parser[Bar], Parser[(Foo, Bar)]]`, owning `Foo` or
   *  `Bar` (or `Parser` or `Concat`) is sufficient.
   */
  private def checkOrphanRule(
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
  private def checkCoherence(
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
  private var traitCallRewrite: Map[String, String] = Map.empty

  /** Get trait impl metadata for cross-unit serialization. Currently only concrete
   *  (non-generic, single-target) impls round-trip across units; generic impls (Stage F.5+)
   *  will need an extended IMPL line format. */
  def getTraitImplMetas: List[TraitImplMeta] =
    concreteImpls.map { case (traitName, targetType, methodMap) =>
      TraitImplMeta(traitName, targetType, methodMap.toMap)
    }.toList

  /** Get trait declaration AST nodes for serialization in TEMPLATES section. */
  def getTraitDecls: List[TraitDeclAST] =
    traits.values.map(t => TraitDeclAST(t.name, t.typeParams, t.methods)).toList

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

  // Polymorphic integer arithmetic intrinsics. Currently: wrapping_* (relabels current
  // wrapping behavior, future-proofs against an overflow-checked default) and saturating_*
  // (clamps at MIN/MAX on overflow). Both signatures: (a: T, b: T) -> T for any integer T.
  private val integerArithIntrinsics: Set[String] = Set(
    "wrapping_add", "wrapping_sub", "wrapping_mul",
    "saturating_add", "saturating_sub", "saturating_mul",
  )

  private val builtinFunctions = Map(
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
        case SymbolMeta.Kind.Func(params, returnType, isDef, isPure, modes, effects) =>
          val paramPairs = params.zipWithIndex.map((t, i) => (s"_p$i", t))
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
            functions(localKey) = FunInfo(sym.name, paramPairs, returnType, isDef, isPure || effects.isPure, modes, reads, writes)
            // Pre-populate the resolved-effects cache so cross-module reads use the same
            // already-mangled names without trying to look them up in this unit's globalScope.
            if reads.isDefined || writes.isDefined then
              resolvedEffectsCache(sym.name) = (reads.getOrElse(Set.empty), writes.getOrElse(Set.empty))
            externalSymbols += localKey
        case SymbolMeta.Kind.Data(dataType, isMutable) =>
          if globalScope.contains(localKey) then
            val existing = globalScope(localKey)
            if !sym.isExtern && existing.name != sym.name then
              throw AnalysisError(s"imported symbol '$localKey' conflicts with existing global")
          else
            globalScope(localKey) = SymInfo(sym.name, dataType, mutable = isMutable)
            externalSymbols += localKey
        case SymbolMeta.Kind.Const(constType, value) =>
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

    // Register generic enum instance mappings for cross-module type inference
    for inst <- meta.genericEnumInstances do
      if !enumToTemplate.contains(inst.mangledName) then
        enumToTemplate(inst.mangledName) = (inst.baseName, inst.typeArgs)

    // Register trait declarations from imported templates
    for template <- meta.genericTemplates do
      template match
        case TraitDeclAST(name, tparams, methods, _) =>
          if !traits.contains(name) then
            traits(name) = TraitInfo(name, tparams, methods)
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
        case "unit" => Some(SyslType.UnitType)
        case "f32" | "float" => Some(SyslType.F32)
        case "f64" | "double" => Some(SyslType.F64)
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
        case fd @ FunDeclAST(name, _, _, _, _, tps, _, _, _) if tps.nonEmpty =>
          if funcSelected(name) && !genericTemplates.contains(name) && !functions.contains(name) then
            genericTemplates(name) = fd
        case sd @ StructDeclAST(name, _, tps, _, _) if tps.nonEmpty =>
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
        case StructDeclAST(name, _, typeParams, _, _) if typeParams.isEmpty =>
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

    // Stage F.5 — record locally-defined types for the orphan rule. Done eagerly so the
    // impl-registration pass can consult these maps without ordering surprises.
    val curMod = currentModule.getOrElse("")
    if curMod.nonEmpty then
      for decl <- program.decls do
        decl match
          case StructDeclAST(name, _, _, _, _)        => typeDefiningModule(name) = curMod
          case DataEnumDeclAST(name, _, _, _)         => typeDefiningModule(name) = curMod
          case EnumDeclAST(name, _, _)                => typeDefiningModule(name) = curMod
          case InterfaceDeclAST(name, _, _, _)        => typeDefiningModule(name) = curMod
          case TypeAliasDeclAST(name, _, _, _, _, _, _) => typeDefiningModule(name) = curMod
          case TraitDeclAST(name, _, _, _)            => traitDefiningModule(name) = curMod
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
    for decl <- program.decls do
      decl match
        case EnumDeclAST(name, members, _) =>
          if !simpleEnumTypes.contains(name) then
            val variants = members.map((vname, _) => (vname, Nil: List[(String, SyslType)]))
            simpleEnumTypes(name) = SyslType.EnumType(name, variants)
        case _ => ()

    def resolveStructsAndEnums(): Unit =
      for decl <- program.decls do
        decl match
          case StructDeclAST(name, fields, typeParams, _, invariants) if typeParams.isEmpty =>
            if !genericStructs.contains(name) then
              val resolvedFields = fields.map((n, t, _) => (n, resolveType(t)))
              val volSet = fields.zipWithIndex.collect { case ((_, _, true), i) => i }.toSet
              structTypes(name) = SyslType.StructType(name, resolvedFields, volSet)
              if invariants.nonEmpty then structInvariants(name) = invariants
          case DataEnumDeclAST(name, variants, typeParams, _) if typeParams.isEmpty =>
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
        case sd @ StructDeclAST(name, fields, typeParams, _, invariants) =>
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
            val paramTypes = params.map { p =>
              val inner = resolveType(p.typ)
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
            functions(name) = FunInfo(mangledName, paramTypes, retType, isDef && params.isEmpty, isPure, paramModes, readsSet, writesSet, isGhost)
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
            // Variants already resolved by pass 0.5 (resolveStructsAndEnums).
            // Do not re-assign dataEnumTypes here — same reason as StructDeclAST above.
        case TypeAliasDeclAST(name, target, tparams, _, isNew, range, predicate) =>
          if typeAliases.contains(name) || genericTypeAliases.contains(name) then
            throw AnalysisError(s"duplicate type alias: '$name'", decl)
          if tparams.nonEmpty then
            // `within` and `where` need scalar ordering / operations on T; both are
            // unavailable on a bare type parameter without trait bounds. `new` does NOT
            // need either — it's just nominal identity per instantiation, fully supported
            // by the existing monomorphization machinery.
            if range.nonEmpty || predicate.nonEmpty then
              throw AnalysisError(s"generic type aliases cannot use 'within' or 'where': '$name'", decl)
            genericTypeAliases(name) = (tparams, target, isNew)
          else
            typeAliases(name) = (target, isNew, range, predicate)
        case TraitDeclAST(name, tparams, methods, _) =>
          if traits.contains(name) then throw AnalysisError(s"duplicate trait: '$name'", decl)
          // Check no duplicate method names within the trait
          val methodNames = methods.map(_.name)
          if methodNames.distinct.length != methodNames.length then
            throw AnalysisError(s"duplicate method names in trait '$name'")
          if tparams.distinct.length != tparams.length then
            throw AnalysisError(s"duplicate type parameter names in trait '$name'")
          traits(name) = TraitInfo(name, tparams, methods)
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

    // Intermediate pass: register impl blocks (traits now known; signatures may reference traits)
    for decl <- program.decls do
      decl match
        case impl @ ImplDeclAST(traitName, implTypeParams, targetTypes, methods, _) =>
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
            typeEnv = trait_.typeParams.zip(resolvedTargets).toMap
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
            finally typeEnv = savedEnv
            val newTemplate = ImplTemplate(
              typeParams = Nil,
              targetPatterns = targetTypes,
              resolvedConcrete = Some(resolvedTargets),
              methods = methodMap,
              methodInfos = infos.toList,
              methodASTs = Nil,
              definingModule = currentModule.getOrElse(""),
              implDecl = Some(impl),
            )
            checkOrphanRule(traitName, targetTypes, currentModule.getOrElse(""), decl)
            checkCoherence(traitName, newTemplate, decl)
            implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += newTemplate
        case _ =>

    // Second pass: produce typed AST (skip generic templates; they're instantiated on demand)
    val tDecls = program.decls.flatMap {
      case f: FunDeclAST if f.typeParams.nonEmpty => Nil
      case s: StructDeclAST if s.typeParams.nonEmpty => Nil
      case e: DataEnumDeclAST if e.typeParams.nonEmpty => Nil
      case _: TraitDeclAST => Nil // traits emit nothing; only impls do
      case i: InterfaceDeclAST => List(TInterfaceDecl(i.name, interfaceTypes(i.name)))
      case impl: ImplDeclAST   => analyzeImplMethods(impl)
      case sa: StaticAssertDeclAST =>
        evalStaticAssert(sa)
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
  private def stripGhostDecls(decls: List[TDecl]): List[TDecl] =
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
  private def stripGhostStmts(stmts: List[TStmt], ghostFns: Set[String]): List[TStmt] =
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
      case other                           => Some(other)

    stmts.flatMap(stripStmt)

  /** Evaluate a `static_assert` at compile time and throw if the condition is false. */
  private def evalStaticAssert(sa: StaticAssertDeclAST): Unit =
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

  private def analyzeDecl(decl: DeclAST): TDecl =
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

      case StructDeclAST(name, _, _, _, _) =>
        val st = structTypes(name)
        TStructDecl(name, st.fields, st.volatileFields)

      case EnumDeclAST(name, _, _) =>
        val members = enumTypes(name).toList.sortBy(_._2)
        TEnumDecl(name, members)

      case DataEnumDeclAST(name, _, _, _) =>
        TDataEnumDecl(name, dataEnumTypes(name))

      case TypeAliasDeclAST(name, _, tparams, _, _, _, _) =>
        if tparams.nonEmpty then TTypeAliasDecl(name, UnitType) // generic alias: type-only, no codegen
        else TTypeAliasDecl(name, resolveType(NamedTypeAST(name))) // force resolution (and range validation)

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
        for (((paramName, paramType), idx) <- funInfo.params.zipWithIndex) do
          val mode = funInfo.modeOf(idx)
          // For Out/Inout, the body-scope symbol sees the inner type with autoIndirect,
          // while the actual parameter storage is a `*T` the body auto-dereferences.
          val (bodyType, autoInd) = mode match
            case ParamMode.In => (paramType, false)
            case ParamMode.Out | ParamMode.Inout => paramType match
              case PtrType(inner) => (inner, true)
              case other => (other, false) // shouldn't happen — collectDecls wrapped it
          currentScope(paramName) = SymInfo(paramName, bodyType, true, autoIndirect = autoInd)
          // Auto-alias the implicit method receiver: `self` -> `__self__`
          // so method bodies can write `self.x` while the actual parameter
          // is named `__self__` to avoid conflicting with user-declared names.
          if paramName == "__self__" then
            currentScope("self") = SymInfo(paramName, bodyType, true, autoIndirect = autoInd)
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
          val isDefFn = fdAst match { case FunDeclAST(_, _, _, _, _, _, _, _, d) => d }
          validatePureFn(name, tBody, funInfo.params.map(_._1), isDefFn)
        if funInfo.reads.isDefined || funInfo.writes.isDefined then
          validateEffects(name, funInfo, tBody, funInfo.params.map(_._1))
        validateGhostDiscipline(name, funInfo, tBody)
        TFunDecl(funInfo.name, tParams, retType, tBody, isPrivate, attrs, funInfo.isDef, isGhost = funInfo.isGhost, effects = funInfoEffects(funInfo))

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
  private def analyzeRegularVarDecl(name: String, typOpt: Option[TypeAST], init: ExpressionAST,
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

  private def warnDeprecated(name: String): Unit =
    if deprecations.contains(name) && !warnedDeprecations.contains(name) then
      warnedDeprecations += name
      val suffix = deprecations(name).map(r => s": $r").getOrElse("")
      System.err.println(s"warning: '$name' is deprecated$suffix")

  /** Names of builtins that are safe to call from a #pure function. Arithmetic/comparison
   *  intrinsics aren't calls (they lower to TBinary/TUnary) so they don't need listing.
   *  `assert` is allowed because its only observable effect is termination — consistent
   *  with Ada's policy of letting pragma Assert live in pure functions. IO, allocation
   *  (malloc/free/…), and side-effecting traps (panic/abort/expect) are *not* listed. */
  private val purePermittedBuiltins: Set[String] = Set("assert")

  /** Analyze a #pure function body. Reject any construct that could have observable
   *  side effects on state outside the function: writes to non-local vars, writes
   *  through pointers/fields, calls to non-pure user functions, indirect calls, asm,
   *  and IO/allocation builtins. Local-variable mutation is fine — it cannot escape.
   *  Called after body analysis so the typed AST is complete; purity of callees is
   *  read from their FunInfo, which was populated in the pre-collection pass. */
  private def validatePureFn(funcName: String, body: TFunBody, paramNames: List[String], isDefFn: Boolean = false): Unit =
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
  private val resolvedEffectsCache = new mutable.HashMap[String, (Set[String], Set[String])]

  /** Resolve a function's raw `#reads`/`#writes` identifier lists to mangled global names.
   *  Validates that each name resolves to a module-level mutable var (via `globalScope`).
   *  `#pure` is treated as `#reads() #writes()` — both empty sets. Returns `None` for
   *  unannotated functions (no annotations means "effects unknown", which Rule 3 of the
   *  effects discipline rejects at call sites of annotated functions). */
  private def resolveEffects(funInfo: FunInfo): Option[(Set[String], Set[String])] =
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
  private def validateEffects(funcName: String, fi: FunInfo, body: TFunBody, paramNames: List[String]): Unit =
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
  private def inferClosureEffects(body: TFunBody, paramNames: List[String]): FuncEffects =
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
  private def funInfoEffects(fi: FunInfo): FuncEffects =
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
  private def effectsSatisfy(actual: FuncEffects, slot: FuncEffects): Boolean =
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
  private def validateGhostDiscipline(funcName: String, fi: FunInfo, body: TFunBody): Unit =
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
      case TStructConstruct(_, args)       => args.foreach(checkExpr(_, ghostCtx))
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
      case TNew(_, args)                   => args.foreach(checkExpr(_, ghostCtx))
      case TNewEnum(_, _, args)            => args.foreach(checkExpr(_, ghostCtx))
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

  private def validateTestAttr(fd: FunDeclAST, info: FunInfo): Unit =
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

  private def resolveType(t: TypeAST): SyslType = t match
    case NamedTypeAST(name, typeArgs) if typeArgs.nonEmpty =>
      val resolved = typeArgs.map(resolveType)
      if genericTypeAliases.contains(name) then
        val (tparams, target, isNew) = genericTypeAliases(name)
        if resolved.length != tparams.length then
          throw AnalysisError(s"type alias '$name' expects ${tparams.length} type argument(s), got ${resolved.length}")
        if isNew then instantiateGenericNominalAlias(name, tparams, target, resolved)
        else
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
  private def evalConstExprAST(e: ExpressionAST): Option[Long] = e match
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
  private def evalRangeBound(aliasName: String, ra: RangeAST, base: SyslType): TypeRange =
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
  private def materializeEnumStrFunc(et: SyslType.EnumType): String =
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
  private def materializePredicateFunc(aliasName: String, predExpr: ExpressionAST, base: SyslType): String =
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
  private def materializeEnumImageFunc(et: SyslType.EnumType): String =
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
  private def materializeEnumPosFunc(et: SyslType.EnumType): String =
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
  private def materializeEnumValFunc(et: SyslType.EnumType): String =
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
  private def materializeEnumValidFunc(et: SyslType.EnumType): String =
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
  private def materializeEnumValueFunc(et: SyslType.EnumType): String =
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
  private def materializeEnumSuccFunc(et: SyslType.EnumType): String =
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
  private def materializeEnumPredFunc(et: SyslType.EnumType): String =
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
  private def materializeWithinSuccFunc(aliasName: String, nt: NamedType, base: SyslType, range: IntRange): String =
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
  private def materializeWithinPredFunc(aliasName: String, nt: NamedType, base: SyslType, range: IntRange): String =
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
  private def analyzeTypeFirstLast(typeName: String, typ: SyslType, isFirst: Boolean): TExpr =
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
  private def analyzeTypeImage(typeName: String, typ: SyslType, argAst: ExpressionAST): TExpr =
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
  private def analyzeTypePos(typeName: String, typ: SyslType, argAst: ExpressionAST): TExpr =
    val tArg = analyzeExpr(argAst)
    typ match
      case et @ EnumType(name, _) if simpleEnumTypes.contains(name) =>
        val asInt = if tArg.typ == I32 then tArg else TCast(tArg, I32)
        TCall(materializeEnumPosFunc(et), List(asInt), I32)
      case other =>
        throw AnalysisError(s"$typeName::Pos requires a simple enum, got $other")

  /** Resolve a `Type::Val(n)` attribute — returns the enum value at position n. */
  private def analyzeTypeVal(typeName: String, typ: SyslType, argAst: ExpressionAST): TExpr =
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
  private def analyzeTypeValid(typeName: String, typ: SyslType, argAst: ExpressionAST): TExpr =
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
  private def analyzeTypeValueString(typeName: String, typ: SyslType, argAst: ExpressionAST): TExpr =
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
  private def analyzeTypeSuccPred(typeName: String, typ: SyslType, argAst: ExpressionAST, isSucc: Boolean): TExpr =
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
  private def latestStruct(st: SyslType.StructType): SyslType.StructType =
    structTypes.getOrElse(st.name, st)

  /** Convert an expression AST to a type AST (for explicit type args parsed as index expressions).
   *  The expression-position grammar parses generic type args as expressions, so this maps
   *  the relevant shapes back to types: `A` (VarRef), `Parser[A]` (Index of VarRef),
   *  `(A, B)` (TupleLit), and `[]A` / `[5]A` (TypeRefExprAST wrappers emitted by the parser
   *  for slice / array type literals appearing in expression position). */
  private def exprToTypeAST(expr: ExpressionAST): TypeAST = expr match
    case TypeRefExprAST(t) => t
    case VarRefAST(name) => NamedTypeAST(name)
    case TupleLitAST(elems) => TupleTypeAST(elems.map(exprToTypeAST))
    case IndexAST(VarRefAST(name), arg) => NamedTypeAST(name, List(exprToTypeAST(arg)))
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
  private def nominallyEqual(a: SyslType, b: SyslType): Boolean = (a, b) match
    case _ if a == b => true
    case (StructType(n1, _, _), StructType(n2, _, _)) => n1 == n2
    case (EnumType(n1, _), EnumType(n2, _))           => n1 == n2
    case (SliceType(e1), SliceType(e2))               => nominallyEqual(e1, e2)
    case (ArrayType(e1, n1), ArrayType(e2, n2))       => n1 == n2 && nominallyEqual(e1, e2)
    case (RefType(e1), RefType(e2))                   => nominallyEqual(e1, e2)
    case (PtrType(e1), PtrType(e2))                   => nominallyEqual(e1, e2)
    case _                                            => false

  private def compatible(from: SyslType, to: SyslType): Boolean =
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
  private def coerceLiteral(expr: TExpr, target: SyslType): TExpr =
    // Don't auto-promote an untyped literal to a nominal NamedType — a cast is required.
    target match
      case NamedType(_, _, true, _, _) => return expr
      case _ =>
    expr match
      case TIntLit(value, _) if target.isIntegral => TIntLit(value, target)
      case TIntLit(0, _) if target.isInstanceOf[PtrType] => TIntLit(0, target) // null pointer
      // Float literal → narrower float type (untyped float literal coercion)
      case TFloatLit(value, _) if target.isFloat => TFloatLit(value, target)
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

  // Apply a target type at a produce-site (var init, assign, param bind, return, cast).
  // For a NamedType target with a `within` range, validates literal values at compile-time and
  // inserts a runtime TRangeCheck for non-literal values. Also re-wraps static type so downstream
  // code sees the NamedType.
  private def applyTargetType(expr: TExpr, target: SyslType): TExpr =
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

  /** Like `tryLookup` but stops at the function boundary — returns only bindings from
   *  scopes pushed inside the current function (parameters, locals, match-bound names,
   *  destructuring binders). Used by the call-site resolver to honor local-shadows-
   *  global semantics for callable values: a pattern-bound `f: (string) -> int` must
   *  shadow a top-level `f(int) -> int` even though the global is registered earlier. */
  private def lookupLocal(name: String): Option[SymInfo] =
    if scopeStack != null then
      var i = scopeStack.length - 1
      while i >= 0 do
        if scopeStack(i).contains(name) then return Some(scopeStack(i)(name))
        i -= 1
    None

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

  private def mangleGenericName(base: String, typeArgs: List[SyslType]): String =
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
  private def syslTypeToAST(t: SyslType): TypeAST = t match
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
      case FuncTypeAST(paramTypes, ret, _, _) => arg match
        case FuncType(argParams, argRet, _, _) =>
          if paramTypes.length == argParams.length then
            for (pt, at) <- paramTypes.zip(argParams) do unifyTypes(pt, at, typeParams, env)
          unifyTypes(ret, argRet, typeParams, env)
        case _ => ()
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
              // Nominal generic alias: the actual must be a NamedType whose mangled name
              // resolves back through `genericAliasToTemplate` to this template.
              arg match
                case SyslType.NamedType(argName, _, true, _, _) =>
                  genericAliasToTemplate.get(argName) match
                    case Some((templateName, concreteArgs)) if templateName == name && concreteArgs.length == tArgs.length =>
                      for (p, a) <- tArgs.zip(concreteArgs) do unifyTypes(p, a, typeParams, env)
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
          case SyslType.StructType(argName, _, _) =>
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
    case PtrNonNullTypeAST(inner) => PtrNonNullTypeAST(substituteTypeAST(inner, subst))
    case ArrayTypeAST(size, elem) => ArrayTypeAST(size, substituteTypeAST(elem, subst))
    case SliceTypeAST(elem) => SliceTypeAST(substituteTypeAST(elem, subst))
    case FuncTypeAST(params, ret, esc, eff) => FuncTypeAST(params.map(substituteTypeAST(_, subst)), substituteTypeAST(ret, subst), esc, eff)
    case TupleTypeAST(elems) => TupleTypeAST(elems.map(substituteTypeAST(_, subst)))
    case RefTypeAST(inner) => RefTypeAST(substituteTypeAST(inner, subst))

  /** Stage F.3 entry point — try to unify a list of TypeAST patterns against a list of
   *  concrete SyslType actuals, returning the inferred binding map on success or None on
   *  any failure. Failure modes captured: arity mismatch, type-var conflict (caught as
   *  AnalysisError from the underlying `unifyTypes`), unbound type parameters, or
   *  structural mismatch that the recursive walker silently no-ops past.
   *
   *  The post-validation step substitutes the inferred env into each pattern, re-resolves
   *  it via `resolveType`, and demands `==` against the actual. Without this, a pattern
   *  like `NamedTypeAST("Foo_int")` against `StructType("Foo_int", ...)` would silently
   *  bind nothing and pass — wrong for dispatch.
   */
  private def tryUnifyAll(
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
              resolved.contains(a)
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
  private def patternsOverlap(
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
  private def tryOperatorDispatch(op: String, tLeft: TExpr, tRight: TExpr): Option[TExpr] =
    lookupBinaryOperatorTrait(op) match
      case None => None
      case Some((traitName, methodName)) =>
        val operands = List(tLeft.typ, tRight.typ)
        // Only fire user-defined operator dispatch when at least one operand is a
        // user-defined type. This preserves the user-friendly "unknown operator on i32"
        // error path for built-in operands.
        val hasUserType = operands.exists {
          case _: SyslType.StructType | _: SyslType.EnumType | _: SyslType.NamedType => true
          case _ => false
        }
        if !hasUserType then return None
        if !traits.contains(traitName) then
          throw AnalysisError(s"operator '$op' on ${operands.mkString(", ")} requires trait '$traitName' but it is not defined")
        val candidates = enumerateImplCandidates(traitName, methodName, operands)
        candidates match
          case Nil =>
            throw AnalysisError(s"no impl of '$traitName' for operator '$op' on ${operands.mkString(", ")}")
          case (template, subst) :: Nil =>
            val (mangled, funInfo) = instantiateImpl(template, traitName, methodName, subst)
            val checkedArgs = checkArgs(mangled, funInfo.params, List(tLeft, tRight))
            Some(TCall(mangled, checkedArgs, funInfo.returnType))
          case multi =>
            throw AnalysisError(s"ambiguous: ${multi.length} impls of '$traitName' match operator '$op' on ${operands.mkString(", ")}")

  /** Enumerate every registered impl template of `traitName` whose `methodName` parameter
   *  patterns unify with `argTypes`. Concrete impls fast-path through `==` checks;
   *  generic impls go through `tryUnifyAll` against the raw impl-method param TypeAST.
   *
   *  Returned in registration order. The dispatcher above expects: 0 → no impl, 1 → use,
   *  N>1 → ambiguous.
   */
  private def enumerateImplCandidates(
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
        paramTypes match
          case Some(ps) if ps == argTypes => Some((t, Map.empty[String, SyslType]))
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
  private def instantiateGenericNominalAlias(
      name: String,
      tparams: List[String],
      target: TypeAST,
      typeArgs: List[SyslType],
  ): SyslType =
    val cacheKey = (name, typeArgs)
    genericAliasInstantiations.get(cacheKey) match
      case Some(t) => t
      case None =>
        val mangled = name + "_" + typeArgs.map(typeToMangled).mkString("_")
        val savedEnv = typeEnv
        typeEnv = typeEnv ++ tparams.zip(typeArgs).toMap
        val base =
          try resolveType(target)
          finally typeEnv = savedEnv
        val nt = SyslType.NamedType(mangled, base, nominal = true, range = None, predicateFunc = None)
        genericAliasInstantiations(cacheKey) = nt
        genericAliasToTemplate(mangled) = (name, typeArgs)
        nt

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
          val resolvedFields = template.fields.map((n, t, _) => (n, resolveType(t)))
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
        for (paramName, paramType) <- info.paramTypes do
          currentScope(paramName) = SymInfo(paramName, paramType, true)
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
  private def instantiateImpl(
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
          val (paramTypes, retType, body, isSynthesized) =
            try
              val pTypes = implMethod.params.map(p => (p.name, resolveType(p.typ)))
              val r = implMethod.returnType.map(resolveType).getOrElse(UnitType)
              val provided = template.methodASTs.exists(_.name == methodName)
              (pTypes, r, implMethod.body, !provided)
            finally typeEnv = savedEnv
          val funInfo = FunInfo(mangled, paramTypes, retType)
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
          for (paramName, paramType) <- paramTypes do
            currentScope(paramName) = SymInfo(paramName, paramType, true)
          val tBody =
            try body match
              case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
              case BlockBodyAST(stmts, _) => TBlockBody(analyzeBlock(stmts))
            finally
              typeEnv = savedEnv
              traitCallRewrite = savedRewrite
              scopeStack = savedScope
              loopDepth = savedLoopDepth
          val tParams = paramTypes.map((n, t) => TParam(n, t))
          specializedDecls += TFunDecl(mangled, tParams, retType, tBody, isPrivate = false)
          (mangled, funInfo)

  private val genericImplInstantiations = new mutable.HashMap[(Int, String, List[(String, SyslType)]), (String, FunInfo)]

  // Resolve a trait method call like Ord.cmp(a, b) to the appropriate impl's mangled function.
  // Stage G: enumerates candidates by unifying each impl's method param patterns directly
  // against the call's arg types. Result-position trait params (e.g. R in Concat[A, B, R])
  // don't need to be inferable from arg types — they're determined by which impl matches.
  private def analyzeTraitCall(traitName: String, methodName: String, tArgs: List[TExpr]): (String, FunInfo) =
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
        typeEnv = typeParams.zip(inferredArgs).toMap
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
  private def resolveNamedArgsTyped(
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
  private def lvalueToAddr(name: String, pName: String, mode: ParamMode, arg: TExpr): TExpr =
    val inner = arg.typ
    arg match
      case TVarRef(n, t)              => TAddrOf(n, PtrType(t))
      case TFieldAccess(obj, idx, t)  => TAddrOfField(obj, idx, PtrType(t))
      case TIndex(arr, ix, t)         => TAddrOfIndex(arr, ix, PtrType(t))
      case TDeref(p, _)               => p // &*p = p
      case _ =>
        throw AnalysisError(s"argument '$pName' of '$name' is '${mode.toString.toLowerCase}' and requires an lvalue (variable, field, or index), got ${arg.getClass.getSimpleName}")

  private def checkArgs(name: String, params: List[(String, SyslType)], args: List[TExpr], modes: List[ParamMode] = Nil): List[TExpr] =
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
              case _ => coerced
          }
          applyTargetType(converted, pType)
    }

  private def analyzeBlock(stmts: List[StmtAST]): List[TStmt] =
    stmts.map(analyzeStmt)

  /** Analyze a function block body together with its `require` / `ensure` contract clauses.
   * Generates require checks at entry, injects a `__result__` local, and rewrites every
   * `return v` so it stores v into `__result__`, runs ensure checks, then returns. */
  private def analyzeBlockWithContracts(
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
    val (variantPrefix, variantBody) = variantClauses.headOption match
      case Some(vc) if selfMangledName.nonEmpty =>
        lowerFunctionVariant(selfMangledName, paramNames, vc, tStmts)
      case _ => (Nil, tStmts)
    val rewritten = rewriteReturnsForEnsure(variantBody, returnType, ensureChecks)
    val finalized = finalizeFallThroughReturn(rewritten, returnType, ensureChecks)
    val resultDecl: List[TStmt] =
      if hasResult then List(TVarStmt("__result__", returnType, zeroExprFor(returnType)))
      else Nil
    TBlockBody(snapshotDecls ++ variantPrefix ++ resultDecl ++ requireChecks ++ finalized)

  /** Zero-value expression for a scalar/pointer return type. */
  private def zeroExprFor(t: SyslType): TExpr = t.underlying match
    case _: FloatType => TFloatLit(0.0, t)
    case BoolType     => TBoolLit(false, t)
    case _            => TIntLit(0, t)

  /** Recursively rewrite every `return v` inside a stmt list so that `v` is stored into
   * __result__, then the ensure checks fire, then `return __result__` runs. For void
   * functions, the assignment step is skipped. */
  private def rewriteReturnsForEnsure(stmts: List[TStmt], returnType: SyslType, ensureChecks: List[TStmt]): List[TStmt] =
    stmts.map(s => rewriteStmtForEnsure(s, returnType, ensureChecks))

  private def rewriteStmtForEnsure(stmt: TStmt, returnType: SyslType, ensureChecks: List[TStmt]): TStmt = stmt match
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

  private def rewriteExprForEnsure(expr: TExpr, returnType: SyslType, ensureChecks: List[TStmt]): TExpr = expr match
    case TIfExpr(c, tb, eb, t) =>
      TIfExpr(c, rewriteReturnsForEnsure(tb, returnType, ensureChecks),
              eb.map(stmts => rewriteReturnsForEnsure(stmts, returnType, ensureChecks)), t)
    case TMatchExpr(e, arms, default, t) =>
      val newArms = arms.map(a => TMatchArm(a.patterns, a.guard, rewriteReturnsForEnsure(a.body, returnType, ensureChecks)))
      TMatchExpr(e, newArms, default.map(stmts => rewriteReturnsForEnsure(stmts, returnType, ensureChecks)), t)
    case other => other

  /** If the rewritten body lacks a trailing explicit return, append one so ensure runs
   * at the implicit fall-through point. The last TExprStmt (if any) becomes the return value. */
  private def finalizeFallThroughReturn(stmts: List[TStmt], returnType: SyslType, ensureChecks: List[TStmt]): List[TStmt] =
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

  private def isTerminalReturn(s: TStmt): Boolean = s match
    case _: TReturnStmt => true
    case TMultiStmt(xs) => xs.lastOption.exists(isTerminalReturn)
    case _ => false

  /** Apply `f` to every TExpr in the typed AST tree rooted at `e`, bottom-up. Used by the
   *  function-variant lowering for two purposes: substitute parameter references in the
   *  variant expression, and locate every recursive TCall to wrap with a check. The walker
   *  is exhaustive on TExpr cases — adding a new TExpr node requires extending it. */
  private def mapTExpr(e: TExpr)(f: TExpr => TExpr): TExpr =
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
  private def mapTStmt(s: TStmt)(f: TExpr => TExpr): TStmt =
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
  private def lowerFunctionVariant(
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

  private def analyzeStmt(stmt: StmtAST): TStmt =
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
        val tValue0 = analyzeExpr(value)
        // Did this name already exist (param, prior decl, or global), or are we about
        // to implicitly create a fresh local? Capture this BEFORE `lookupOrCreate` so the
        // newly-bound case can be distinguished. Bare `name = expr` (no `var`/`val`)
        // inside a function body is sysl's implicit-local syntax — when the analyzer
        // creates a fresh local, downstream passes need to see it as a binding (TVarStmt),
        // not a write to an existing variable (TAssignStmt). Closure capture-detection
        // walks TAssignStmt as an assignment to an outer name, so emitting TAssignStmt
        // here would incorrectly mark a freshly-created inner local as a captured outer.
        val existedBefore = tryLookup(target).isDefined
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
        val tValue = analyzeExpr(value)
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
  private def resolveVariant(name: String, scrutineeType: SyslType): Option[(SyslType.EnumType, Int)] =
    scrutineeType match
      case et: SyslType.EnumType =>
        val idx = et.variants.indexWhere(_._1 == name)
        if idx >= 0 then Some((et, idx))
        else variantToEnum.get(name)
      case _ => variantToEnum.get(name)

  // Counter for synthesizing unique outer-bindings for nested tuple patterns.
  private var tuplePatternCounter: Int = 0
  private def freshTupleBindName(): String =
    tuplePatternCounter += 1
    s"_match_tup_$tuplePatternCounter"

  private def isTupleStructType(t: SyslType): Boolean = t.underlying match
    case st: SyslType.StructType => st.name.startsWith("_Tuple")
    case _ => false

  /** Analyze a field pattern inside a destructure (variant or struct). For most shapes
   *  this returns the user-visible binding name (or None for wildcard / literal). For a
   *  nested tuple pattern `(a, b)` the helper introduces a fresh synthetic outer name
   *  bound to the field, then appends `val a = sym._0; val b = sym._1` to `prelude` —
   *  these are prepended to the arm body before it's analyzed. */
  private def analyzeFieldPattern(
      fieldPat: MatchPatternAST,
      fieldType: SyslType,
      prelude: scala.collection.mutable.ListBuffer[StmtAST],
  ): Option[String] = fieldPat match
    case WildcardPatternAST => None
    case ValuePatternAST(VarRefAST(bindName)) =>
      if scopeStack != null then
        currentScope(bindName) = SymInfo(bindName, fieldType, false)
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
          Some(syn)
        case other =>
          throw AnalysisError(s"tuple pattern requires tuple type, got $other")
    case ValuePatternAST(_) => None
    case _ => throw AnalysisError(s"unsupported pattern in destructure")

  private def analyzePattern(
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
          val bindings = fields.zip(variantFields).map { case (fieldPat, (_, fieldType)) =>
            analyzeFieldPattern(fieldPat, fieldType, prelude)
          }
          TVariantPattern(et, variantIdx, bindings, variantFields.map(_._2))
        else
          val st = structTypes.getOrElse(name, throw AnalysisError(s"unknown struct or variant '$name' in match pattern"))
          if fields.length != st.fields.length then
            throw AnalysisError(s"struct '$name' has ${st.fields.length} fields, pattern has ${fields.length}")
          val bindings = fields.zip(st.fields).map { case (fieldPat, (_, fieldType)) =>
            analyzeFieldPattern(fieldPat, fieldType, prelude)
          }
          TDestructurePattern(st, bindings, st.fields.map(_._2))

  private def analyzeExpr(expr: ExpressionAST): TExpr =
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
            case Some(t) if t != UnitType => t
            case _ => UnitType
        )
        // Push scope with closure params
        pushScope()
        for p <- typedParams do
          currentScope(p.name) = SymInfo(p.name, p.typ, mutable = false)
        // Analyze body
        val savedExp = currentExpected
        currentExpected = if expectedRet == UnitType then None else Some(expectedRet)
        val tBody = try body match
          case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
          case BlockBodyAST(stmts, _) => TBlockBody(analyzeBlock(stmts))
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
          case Some(sym) if sym.autoIndirect =>
            return TDeref(TVarRef(sym.name, PtrType(sym.typ)), sym.typ)
          case Some(sym) =>
            return TVarRef(sym.name, sym.typ)
          case None => ()
        // Check if name is a function (used as a value = function pointer)
        if functions.contains(name) then
          val f = functions(name)
          if f.isDef then
            // Auto-call: bare reference to a def function emits a call
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
        val resultType = tInner.typ.underlying match
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
          case SymbolMeta.Kind.Func(params, retType, _, _, _, eff) => TFuncRef(sym.name, SyslType.FuncType(params, retType, effects = eff))
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
          case PtrType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
          case RefType(st: StructType) => (TDeref(tObj, latestStruct(st)), st)
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
        val tLeft00 = analyzeExpr(left)
        val tRight00 = analyzeExpr(right)
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
              case (FloatType(a), FloatType(b)) => FloatType(a max b)
              case (_: FloatType, _) => tLeft.typ
              case (_, _: FloatType) => tRight.typ
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
                  s"operator '$op' is bound to trait '$traitName', but neither operand is a struct/enum that impls it (got ${tLeft.typ} $op ${tRight.typ})",
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
        val tArgs =
          if genericTypeAliases.contains(name) && args.length == 1 then
            val target = resolveType(NamedTypeAST(name, List(exprToTypeAST(typeExpr))))
            val savedExp = currentExpected
            currentExpected = Some(target.underlying)
            try args.map(analyzeExpr) finally currentExpected = savedExp
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

      case MethodCallAST(VarRefAST(nsName), method, args) if moduleNamespaces.contains(nsName) =>
        // Qualified import call: strings.has_prefix(s, prefix)
        val meta = moduleNamespaces(nsName)
        val tArgs = args.map(analyzeExpr)
        // Find the function in the module's symbols
        val funcSym = meta.publicSymbols.find(s => shortName(s.name) == method)
          .getOrElse(throw AnalysisError(s"module '$nsName' has no function '$method'"))
        funcSym.typ match
          case SymbolMeta.Kind.Func(params, returnType, _, _, modes, _) =>
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
              throw AnalysisError(s"struct $structName has no method or field '$method'")

      case CallAST(name, args) =>
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
                return TIndirectCall(TVarRef(name, sym.typ), checkedArgs, ft.returnType)
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
          if funInfo.isDef && funInfo.params.isEmpty && tArgs.nonEmpty then
            // Auto-call def, then indirect-call the result with the provided args
            val autoCall = TCall(funInfo.name, Nil, funInfo.returnType)
            funInfo.returnType match
              case FuncType(fParams, fRet, _, _) =>
                val paramPairs = fParams.zipWithIndex.map((t, i) => (s"_p$i", t))
                val checkedArgs = checkArgs(name, paramPairs, tArgs)
                TIndirectCall(autoCall, checkedArgs, fRet)
              case _ => throw AnalysisError(s"def '$name' returns ${funInfo.returnType}, not a callable type")
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
                case TVariantPattern(_, idx, _, _) => coveredVariants += idx
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
              case t if t.isNumeric || t == SyslType.BoolType => TStr(analyzed)
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
        else if expr.typ.isNumeric || expr.typ == SyslType.BoolType then
          if spec.width == 0 && !spec.leftAlign then TStr(expr) else TFmtStr(TStr(expr), spec)
        else throw AnalysisError(s"cannot format value of type ${expr.typ} with %s")
      case 'c' =>
        if !expr.typ.isNumeric then
          throw AnalysisError(s"format verb '%c' requires numeric type, got ${expr.typ}")
        TFmtStr(expr, spec)
      case _ => throw AnalysisError(s"unknown format verb '%${spec.verb}'")
