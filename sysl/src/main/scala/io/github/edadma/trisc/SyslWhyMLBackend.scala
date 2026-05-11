package io.github.edadma.trisc

/** Phase 1 sysl → WhyML translator. Walks the parser AST (NOT the typed AST) so that
 *  `require` / `ensure` / `variant` clauses are still attached to functions as separate
 *  declarative pieces — by the time the analyzer runs they have been woven into the
 *  function body around `__result__` and a return-rewrite pass.
 *
 *  Phase 1 scope: pure functions returning int / bool, integer arithmetic, comparisons,
 *  bool ops, if-expressions, `require` / `ensure` / `variant`, `old(e)` inside `ensure`,
 *  and recursion. Anything outside that surface is rejected with a clear message naming
 *  the unsupported construct so the user knows it's a translator gap, not a sysl error. */
class SyslWhyMLBackend(moduleName: String = "M"):
  private val out = new StringBuilder
  private var indentLevel = 0
  /** Set of enum (no-payload) type names declared in this program. Used by `formatExpr` to
   *  recognize `EnumName.Variant` field access and rewrite to just `Variant` (WhyML
   *  constructors live in the module-level namespace, not under the type). */
  private var enumNames: Set[String] = Set.empty
  /** Map of struct name → ordered field names. Used by `formatExpr` to translate struct
   *  construction calls `Point(1, 2)` into WhyML record literals `{ x = 1; y = 2 }` (need
   *  field names since records are name-keyed) and to recognize struct types in `typeOf`. */
  private var structFields: Map[String, List[String]] = Map.empty
  /** Map of data-enum type name → list of `(variantName, fields)` (fields are positional
   *  WhyML constructor arguments). Drives data-enum type emission, constructor-call
   *  recognition in `formatExpr`, and destructure-pattern emission in `formatPattern`. */
  private var dataEnumNames: Map[String, List[(String, List[(String, TypeAST)])]] = Map.empty
  /** Reverse-lookup: variant name → enclosing data-enum type name. Used to recognize bare
   *  (no-payload) variant references (`None`) and constructor calls (`Some(42)`) without
   *  the user qualifying them. Sysl variant names are globally unique across data enums. */
  private var dataEnumVariantOf: Map[String, String] = Map.empty
  /** Type parameters currently in scope for the function being emitted. Recognized in
   *  `typeOf` and rendered as WhyML type variables (`T` → `'t`, `U` → `'u`). Pushed when
   *  we begin emitting a generic function and cleared after — module-level scope has no
   *  active type variables, only declared inside functions or generic types. */
  private var currentTypeParams: Set[String] = Set.empty
  /** Return type of the function being emitted. Used by `?` (TryAST) to know which
   *  data-enum failure variant to emit when the operator early-exits. Sysl convention
   *  (enforced by the analyzer): `?` requires the enclosing function's return type to
   *  match the inner expression's enum, so this is the canonical source of "what does
   *  the failure value look like?". */
  private var currentFunctionReturnType: Option[TypeAST] = None
  /** Names currently bound as WhyML `ref`s in scope. Reads of these get `!name`; assignments
   *  get `name := expr`. Populated during `formatBlockBody` when a sysl `var` is detected to
   *  be reassigned later in the same scope; popped on the way out. Phase 4a does not handle
   *  shadowing — a function with two same-named locals in disjoint scopes would conflate them. */
  private val refScope: scala.collection.mutable.Set[String] = scala.collection.mutable.Set.empty
  /** Module-invariant decls collected per `generate(program)`. The WhyML backend
   *  emits them as a single `predicate module_inv ()` whose body conjoins every
   *  invariant, then implicitly attaches `requires { module_inv () }` /
   *  `ensures { module_inv () }` to every public function. Phase δ.3. */
  private var moduleInvariants: List[ModuleInvariantDeclAST] = Nil

  private def indent: String = "  " * indentLevel
  private def line(s: String): Unit =
    out.append(indent).append(s).append('\n')
  private def blank(): Unit = out.append('\n')

  private class Unsupported(val msg: String) extends RuntimeException(msg)
  private def unsupported(what: String, ctx: String = ""): Nothing =
    val suffix = if ctx.isEmpty then "" else s" ($ctx)"
    throw new Unsupported(s"WhyML translator: unsupported $what$suffix")

  /** Translate a parsed sysl program to a WhyML module string. */
  def generate(program: ProgramAST): String =
    out.clear()
    indentLevel = 0
    refScope.clear()
    val enums = program.decls.collect { case e: EnumDeclAST => e }
    enumNames = enums.map(_.name).toSet
    val structs = program.decls.collect { case s: StructDeclAST => s }
    structFields = structs.map(s => s.name -> s.fields.map(_._1)).toMap
    val dataEnums = program.decls.collect { case d: DataEnumDeclAST => d }
    dataEnumNames = dataEnums.map(d => d.name -> d.variants.map(v => (v.name, v.fields))).toMap
    dataEnumVariantOf = (for d <- dataEnums; v <- d.variants yield v.name -> d.name).toMap
    val constants = program.decls.collect { case v: VarDeclAST => v }
    val fns = program.decls.collect { case f: FunDeclAST => f }
    moduleInvariants = program.decls.collect { case mi: ModuleInvariantDeclAST => mi }
    line(s"module $moduleName")
    indentLevel += 1
    line("use int.Int")
    // ComputerDivision provides truncating `div` and `mod` (C-style). int.Int does not
    // define `/` or `%` because their semantics are debatable; we pin to the C-style choice
    // since it matches sysl's interpreter and codegen behavior. Always-imported is harmless.
    line("use int.ComputerDivision")
    // ref.Ref provides the `ref` constructor, `(!)` deref, and `(:=)` assignment used by
    // Phase 4a. Always-imported is harmless: programs that don't use refs simply never
    // reference these symbols. Without this import Why3 reports "unbound symbol 'ref'".
    line("use ref.Ref")
    // string.String provides the built-in `string` type. Only imported when a program
    // actually mentions `string` — keeps the preamble minimal for the common case
    // (verification rarely involves strings since they're opaque to the prover).
    if usesString(program) then line("use string.String")
    blank()
    var first = true
    for e <- enums do
      if !first then blank()
      first = false
      emitEnum(e)
    for d <- dataEnums do
      if !first then blank()
      first = false
      emitDataEnum(d)
    for s <- structs do
      if !first then blank()
      first = false
      emitStruct(s)
    for c <- constants do
      if !first then blank()
      first = false
      emitConstant(c)
    // Phase δ.3: emit a `predicate module_inv ()` after constants/refs are in scope so
    // the body can reference them. Each `module_invariant` decl contributes one
    // conjunct; `module_inv ()` evaluates to `true` when the program has none, which
    // is harmless to include in requires/ensures.
    if moduleInvariants.nonEmpty then
      if !first then blank()
      first = false
      emitModuleInvariantPredicate()
    for fn <- fns do
      if !first then blank()
      first = false
      emitFunction(fn)
    indentLevel -= 1
    line("end")
    out.toString

  /** Module-level `val NAME : T = expr` (or `const NAME : T = expr`) becomes a WhyML
   *  `constant`. Mutable `var` at module scope would need WhyML refs and is deferred —
   *  most proof-relevant module-level data is naturally immutable (limits, sentinels,
   *  shared math constants), so `val` covers the common case. */
  /** Emit `predicate module_inv () = c1 /\ c2 /\ ...` where each `cN` is one of the
   *  collected module_invariant clauses. The `()` parameter list is empty because the
   *  predicate closes over module-level state via Why3's normal scoping rules — refs
   *  read via `!name` etc. The predicate is logic-mode (formula); we emit the clauses
   *  joined with `/\` (formula AND), not `&&` (program-bool AND), because Why3 expects
   *  predicate bodies to be formulas. */
  private def emitModuleInvariantPredicate(): Unit =
    val clauses = moduleInvariants.map(mi => stripOuterParens(formatExpr(mi.expr)))
    line(s"predicate module_inv () = ${clauses.mkString(" /\\ ")}")

  private def emitConstant(v: VarDeclAST): Unit =
    // Type annotation is optional: when sysl omits it, drop the `: t` clause and
    // let WhyML's type inference unify against the init expression (mirrors the
    // function-without-return-type fix in γ.3).
    val typeStr = v.typ.map(t => s" : ${typeOf(t)}").getOrElse("")
    val refTypeStr = v.typ.map(t => s" : ref ${typeOf(t)}").getOrElse("")
    val sname = sanitizeName(v.name)
    if v.isMutable then
      // Module-level `var` → WhyML `val name : ref type = ref initial`. Permanently
      // add to refScope so every function body sees it as a ref (`!name` to read,
      // `name := value` to assign). Drivers and kernel state live here in real code;
      // this gates a large slice of OS verification.
      refScope += v.name
      line(s"val $sname$refTypeStr = ref ${formatExpr(v.init)}")
    else
      // `let constant` (program-level) — usable from both contracts and code bodies. Without
      // `let`, the constant is logic-only and Why3 reports "logical symbol used in a non-ghost
      // context" when a `let function` body references it.
      line(s"let constant $sname$typeStr = ${formatExpr(v.init)}")

  /** sysl `struct Point { x: int; y: int }` → WhyML `type point = { x: int; y: int }`.
   *  WhyML records are immutable by default; field updates are functional (`{ p with x = 5 }`).
   *  Generic structs (with type params) are rejected.
   *
   *  Struct invariants — sysl `invariant <expr>` clauses translate to WhyML
   *  `invariant { <expr> }` immediately after the field list. WhyML requires a non-empty
   *  witness for any record-with-invariant (the type must be inhabited), provided via a
   *  `by { f = default }` clause. We synthesize defaults from field types: `int` → 0,
   *  `bool` → false. Default-derivable types only — exotic field types in an invariant-bearing
   *  struct are rejected (the user could split: keep the data struct invariant-free, layer the
   *  invariant on a wrapper).
   *
   *  Multiple invariants are joined with `&&` (WhyML accepts conjunction in invariant
   *  clauses). The generated `by` witness must satisfy all of them simultaneously — for
   *  typical numeric invariants like `lo <= hi` or `balance >= -limit`, the all-zeros
   *  witness works. If a user invariant rejects the all-zeros witness, Why3 will report
   *  the failed witness goal and the user can refactor. */
  private def emitStruct(s: StructDeclAST): Unit =
    if s.fields.isEmpty then unsupported("empty struct", s.name)
    val typeName = s"${s.name.head.toLower}${s.name.tail}"
    val savedTypeParams = currentTypeParams
    currentTypeParams = s.typeParams.toSet
    try
      // Generic structs emit parametric WhyML records:
      //   `struct Pair[T] { x: T; y: T }` → `type pair 'a = { x: 'a; y: 'a }`.
      // Sysl type-param naming convention (single-letter uppercase) maps to WhyML
      // type variables (lowercase, prefixed with `'`).
      val typeParamsStr =
        if s.typeParams.isEmpty then ""
        else " " + s.typeParams.map(p => s"'${p.toLowerCase}").mkString(" ")
      val fieldStr = s.fields.map { case (fname, ftyp, _) =>
        s"$fname: ${typeOf(ftyp)}"
      }.mkString("; ")
      if s.invariants.isEmpty then
        line(s"type $typeName$typeParamsStr = { $fieldStr }")
      else
        // Generic structs with invariants: the `by { ... }` witness needs concrete
        // values for every field, but defaultValue currently only handles int / bool.
        // For a type-parameterized field, there's no canonical default — reject with
        // a clear escape-hatch message.
        if s.typeParams.nonEmpty then
          unsupported(
            "generic struct with invariant",
            s"${s.name} — the `by { ... }` witness needs a concrete value for every " +
              "type-parameterized field; extract the invariant to a non-generic wrapper " +
              "(e.g. `struct PairInt { inner: Pair[int]; invariant ... }`) so the witness " +
              "is well-defined",
          )
        val invStr = s.invariants
          .map(e => stripOuterParens(formatExpr(e)))
          .mkString(" && ")
        val witness = s.fields.map { case (fname, ftyp, _) =>
          s"$fname = ${defaultValue(ftyp, s.name, fname)}"
        }.mkString("; ")
        line(s"type $typeName = { $fieldStr }")
        indentLevel += 1
        line(s"invariant { $invStr }")
        line(s"by { $witness }")
        indentLevel -= 1
    finally currentTypeParams = savedTypeParams

  /** Default value for a field's type, used to synthesize a `by { ... }` witness for
   *  invariant-bearing records. Only the trivially-derivable types are supported; anything
   *  else fails fast with a useful message. */
  private def defaultValue(t: TypeAST, structName: String, fieldName: String): String = t match
    case NamedTypeAST("bool", Nil) => "false"
    case NamedTypeAST(name, Nil) if isInteger(name) => "0"
    case other =>
      unsupported(
        "default-value synthesis for invariant witness",
        s"$structName.$fieldName has type $other; only int / bool fields are supported in invariant-bearing structs")

  private def isInteger(n: String): Boolean = n match
    case "int" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" |
         "byte" | "char" | "rune" => true
    case _ => false

  /** Quick scan: does any function signature or expression mention sysl's `string`?
   *  Used to gate `use string.String` import — most verification code never touches
   *  strings, and importing it unconditionally bloats the preamble for tests/snapshots. */
  private def usesString(program: ProgramAST): Boolean =
    def hasStringInType(t: TypeAST): Boolean = t match
      case NamedTypeAST("string", _) => true
      case NamedTypeAST(_, args)     => args.exists(hasStringInType)
      case _                         => false
    program.decls.exists {
      case fn: FunDeclAST =>
        fn.params.exists(p => hasStringInType(p.typ)) ||
          fn.returnType.exists(hasStringInType) ||
          containsStringLit(fn.body)
      case v: VarDeclAST  => v.typ.exists(hasStringInType)
      case _              => false
    }

  private def containsStringLit(b: FunBodyAST): Boolean = b match
    case ExprBodyAST(e)         => containsStringLitExpr(e)
    case BlockBodyAST(stmts, _) => stmts.exists(containsStringLitStmt)

  private def containsStringLitExpr(e: ExpressionAST): Boolean = e match
    case _: StringLitAST | _: StringLitExprAST => true
    case BinaryAST(l, _, r) => containsStringLitExpr(l) || containsStringLitExpr(r)
    case UnaryAST(_, x)     => containsStringLitExpr(x)
    case CallAST(_, args)   => args.exists(containsStringLitExpr)
    case IfExprAST(c, t, e) =>
      containsStringLitExpr(c) ||
        t.exists(containsStringLitStmt) ||
        e.exists(_.exists(containsStringLitStmt))
    case TryAST(inner)      => containsStringLitExpr(inner)
    case _                  => false

  private def containsStringLitStmt(s: StmtAST): Boolean = s match
    case ExprStmtAST(e)             => containsStringLitExpr(e)
    case ReturnStmtAST(Some(e))     => containsStringLitExpr(e)
    case VarStmtAST(_, _, init, _, _, _, _) => containsStringLitExpr(init)
    case AssignStmtAST(_, v)        => containsStringLitExpr(v)
    case CompoundAssignStmtAST(_, _, v) => containsStringLitExpr(v)
    case WhileStmtAST(c, body, _)   => containsStringLitExpr(c) || body.exists(containsStringLitStmt)
    case ForStmtAST(init, c, u, body, _) =>
      containsStringLitStmt(init) || containsStringLitExpr(c) ||
        containsStringLitStmt(u) || body.exists(containsStringLitStmt)
    case _ => false

  /** sysl `enum Color { Red, Green, Blue }` → WhyML `type color = Red | Green | Blue`.
   *  The integer values that sysl assigns (auto-incrementing or explicit) are dropped —
   *  WhyML algebraic types don't expose a numeric tag, and proofs typically reason about
   *  variant identity rather than its underlying int. WhyML type names are conventionally
   *  lowercase; constructor names stay as-written (sysl convention is also uppercase). */
  private def emitEnum(e: EnumDeclAST): Unit =
    val typeName = s"${e.name.head.toLower}${e.name.tail}"
    val ctors = e.members.map { case (name, _) => name }
    if ctors.isEmpty then unsupported("empty enum", e.name)
    line(s"type $typeName = ${ctors.mkString(" | ")}")

  /** sysl `enum Option[T] { Some(value: T); None }` → WhyML
   *  `type option 'a = Some 'a | None`. WhyML data-enum variants are positional, so we
   *  drop the sysl field names (`value:`) and emit each field's translated type in order.
   *  No-payload variants (`None`) emit as bare constructors with no trailing types.
   *
   *  Type parameters are pushed into `currentTypeParams` so `typeOf` can recognize them
   *  and emit `'a` / `'b` / etc. inside the variant payload types. They are popped after
   *  emission so other declarations don't accidentally see them. */
  private def emitDataEnum(d: DataEnumDeclAST): Unit =
    if d.variants.isEmpty then unsupported("empty data enum", d.name)
    val typeName = s"${d.name.head.toLower}${d.name.tail}"
    val savedTypeParams = currentTypeParams
    currentTypeParams = d.typeParams.toSet
    try
      val typeParamsStr =
        if d.typeParams.isEmpty then ""
        else " " + d.typeParams.map(p => s"'${p.toLowerCase}").mkString(" ")
      val variantStrs = d.variants.map { v =>
        if v.fields.isEmpty then v.name
        else s"${v.name} " + v.fields.map((_, t) => typeOf(t)).mkString(" ")
      }
      line(s"type $typeName$typeParamsStr = ${variantStrs.mkString(" | ")}")
    finally currentTypeParams = savedTypeParams

  /** True iff `fn` calls itself by name anywhere in its body (Phase 1 detects only direct
   *  self-recursion — mutual recursion would need a cross-decl scan and `with` syntax in WhyML). */
  private def isRecursive(fn: FunDeclAST): Boolean =
    fn.body match
      case ExprBodyAST(e)        => callsName(e, fn.name)
      case BlockBodyAST(stmts, _) => stmts.exists(stmtCallsName(_, fn.name))

  private def callsName(e: ExpressionAST, name: String): Boolean = e match
    case CallAST(n, args) if n == name => true
    case CallAST(_, args)              => args.exists(callsName(_, name))
    case BinaryAST(l, _, r)            => callsName(l, name) || callsName(r, name)
    case UnaryAST(_, x)                => callsName(x, name)
    case IfExprAST(c, tb, eb) =>
      callsName(c, name) || tb.exists(stmtCallsName(_, name)) ||
        eb.exists(_.exists(stmtCallsName(_, name)))
    case _ => false

  private def stmtCallsName(s: StmtAST, name: String): Boolean = s match
    case ReturnStmtAST(Some(e)) => callsName(e, name)
    case ReturnStmtAST(None)    => false
    case ExprStmtAST(e)         => callsName(e, name)
    case VarStmtAST(_, _, init, _, _, _, _) => callsName(init, name)
    case _                      => false

  private def emitFunction(fn: FunDeclAST): Unit =
    if fn.attributes.exists(_.name == "test") then return
    val savedTypeParams = currentTypeParams
    val savedReturnType = currentFunctionReturnType
    if fn.typeParams.nonEmpty then currentTypeParams = fn.typeParams.toSet
    currentFunctionReturnType = fn.returnType
    try emitFunctionImpl(fn)
    finally
      currentTypeParams = savedTypeParams
      currentFunctionReturnType = savedReturnType

  private def emitFunctionImpl(fn: FunDeclAST): Unit =
    val name = sanitizeName(fn.name)
    val params =
      if fn.params.isEmpty then "()"
      else fn.params.map(p => s"(${sanitizeName(p.name)}: ${typeOf(p.typ)})").mkString(" ")
    val (contracts, bodyStmts) = splitBody(fn)
    val isGhost = fn.attributes.exists(_.name == "ghost")
    val bodyStr = formatBlockBody(bodyStmts)
    // For predicate-shape detection we need the trailing expression of a single-stmt body.
    // Multi-statement bodies can never be predicate-shaped (a let-chain isn't a prop).
    val singleBodyExpr: Option[ExpressionAST] = bodyStmts match
      case List(ExprStmtAST(e))         => Some(e)
      case List(ReturnStmtAST(Some(e))) => Some(e)
      case _                            => None

    // Lift to a logic-level `predicate` when the shape is right: `def f(...) -> bool` whose
    // body IS a quantifier and which carries no contracts. Why3's `forall`/`exists` are
    // formula-level (return `prop`), so they cannot appear in `let function`'s value-typed
    // body — only in contract positions or as the body of `predicate` / formula-valued
    // logic definitions. `predicate` is exactly the right WhyML construct here, and matches
    // sysl's runtime semantics for `def`-with-quantifier (a pure boolean-valued query).
    // `#ghost` is redundant on predicates (they're inherently logic-level), so we drop it.
    val returnsBool = fn.returnType match
      case Some(NamedTypeAST("bool", Nil)) => true
      case _                               => false
    val isPredicateShape = fn.isDef && returnsBool && contracts.isEmpty &&
      singleBodyExpr.exists(isFormula)
    if isPredicateShape then
      line(s"predicate $name $params = ${stripOuterParens(formatExpr(singleBodyExpr.get))}")
      return

    val recKw = if isRecursive(fn) then "rec " else ""
    // WhyML's `ghost` qualifier marks a function as proof-only — it is checked but erased
    // before extraction. Maps 1:1 from sysl's `#ghost`. The required keyword order is
    // `let [rec] [ghost] function f ...` — ghost must follow rec, not precede it.
    val ghostKw = if isGhost then "ghost " else ""
    // `let function` is reflected to the logic and must be a pure expression — it cannot use
    // mutable refs, sequenced assignments, or loops. When the body uses any of those (Phase 4),
    // drop the `function` keyword and emit a plain `let`. The function is still verified
    // against its contracts; it just isn't usable inside contract expressions of OTHER
    // functions. Pure functions (Phase 1–3) keep `function` so they can be called from
    // contracts as well.
    // `let function` is pure (logical-mode); it cannot carry writes/reads clauses
    // and cannot read or write a `ref`. If the user annotated #writes / #reads, they
    // are declaring effects — force the impure emit (`let f ...`) so the clauses
    // can be attached. Otherwise body-shape inference picks the form.
    val hasFrameAttr = fn.attributes.exists(a => a.name == "writes" || a.name == "reads")
    val funKw = if isImpure(bodyStmts) || hasFrameAttr then "" else "function "
    // sysl `def` functions often omit the return type and rely on body inference.
    // WhyML's type inference can fill in the gap — emit `let function f x = body`
    // without a `: ret` clause and let Why3 unify. Annotated return types still
    // emit explicitly (they document intent + give Why3 a fixed point for
    // contract-side `result` typing).
    val ret = fn.returnType match
      case None    => ""
      case Some(t) => s" : ${typeOf(t)}"
    line(s"let $recKw$ghostKw" + funKw + s"$name $params$ret")
    indentLevel += 1
    // Frame conditions (δ.1). Sysl's `#writes(g1, g2)` / `#reads(g1, g2)` annotate
    // which module-level mutable vars an impure function may modify / read. They
    // map directly to Why3's `writes { v1; v2 }` / `reads { v1; v2 }` clauses on
    // `let`. Only emitted for impure functions (funKw is empty); for pure
    // `let function` Why3 already enforces no side effects, so writes/reads are
    // redundant and would actually be rejected.
    if funKw.isEmpty then
      for attr <- fn.attributes do attr.name match
        case "writes" =>
          val names = attr.args.collect { case AttrPositional(AttrLitIdent(n)) => sanitizeName(n) }
          if names.nonEmpty then line(s"writes { ${names.mkString("; ")} }")
        case "reads" =>
          val names = attr.args.collect { case AttrPositional(AttrLitIdent(n)) => sanitizeName(n) }
          if names.nonEmpty then line(s"reads { ${names.mkString("; ")} }")
        case _ =>
    // Phase δ.3: implicit module-invariant clauses on every public, non-ghost
    // function. Public/external entry points must preserve invariants; private
    // helpers can rely on them too but are checked transitively. Ghost fns are
    // proof-only and don't need to carry the invariant. The `module_inv ()`
    // predicate is in scope (emitted before all functions).
    if moduleInvariants.nonEmpty && !fn.isPrivate && !isGhost then
      line("requires { module_inv () }")
      line("ensures  { module_inv () }")
    for c <- contracts do emitContract(c)
    line(s"= $bodyStr")
    indentLevel -= 1

  /** True if `e` is a formula-shaped expression — currently just a top-level quantifier.
   *  Extended in later phases to recognize boolean connectives joining quantifiers. */
  private def isFormula(e: ExpressionAST): Boolean = e match
    case _: QuantifierAST => true
    case _                => false

  private def splitBody(fn: FunDeclAST): (List[ContractClauseAST], List[StmtAST]) =
    fn.body match
      case ExprBodyAST(e) => (Nil, List(ExprStmtAST(e)))
      case BlockBodyAST(stmts, contracts) => (contracts, stmts)

  /** Lower a sequence of body statements to a single WhyML expression string.
   *
   *  Supported mid-body forms (Phase 3 + 4a/b):
   *    - `val name = expr`                  → `let name = expr in <rest>`
   *    - `var name = expr` (immutable use)  → `let name = expr in <rest>`
   *    - `var name = expr` (later reassigned) → `let name = ref expr in <rest>` + add to refScope
   *    - `name = expr`                      → `name := expr; <rest>` (must be in refScope)
   *    - `name op= expr`                    → `name := (!name op expr); <rest>`
   *    - `if cond then return e` (no else)  → `(if cond then e else <rest>)`
   *    - `while cond do body done`          → `(while ... done); <rest>` (with invariant/variant)
   */
  private def formatBlockBody(stmts: List[StmtAST]): String =
    stmts match
      case Nil => unsupported("empty function body", "must have a trailing expression or return")
      case List(s) => stmtAsTrailingExpr(s)
      case head :: rest =>
        head match
          // `val name = e?` — propagate failure or bind success. Lower the whole tail
          // (this binding + everything after) into a single match against `e`. The success
          // arm binds the unwrapped value to `name` and recurses on `rest`; the failure
          // arm reconstructs the failure variant of the enclosing function's return type.
          case VarStmtAST(name, _, TryAST(inner), _, _, _, isGhost) =>
            val sname = sanitizeName(name)
            val ghostKw = if isGhost then "ghost " else ""
            val (succPat, succExpr, failArm) = tryDesugarPieces(bindName = Some(name))
            s"(match ${formatExpr(inner)} with " +
              s"| $succPat -> let $ghostKw$sname = $succExpr in ${formatBlockBody(rest)} " +
              s"| $failArm end)"
          case VarStmtAST(name, _, init, isMutable, _, _, isGhost) =>
            val sname = sanitizeName(name)
            val ghostKw = if isGhost then "ghost " else ""
            // Detect ref-shape: declared mutable AND actually reassigned later in scope.
            // A `var` that's never reassigned can stay as an immutable `let` — it's pure
            // and Why3 prefers pure where possible.
            val mutableUse = isMutable && isReassigned(name, rest)
            if mutableUse then
              refScope += name
              try
                s"let $ghostKw$sname = ref ${formatExpr(init)} in ${formatBlockBody(rest)}"
              finally refScope -= name
            else
              s"let $ghostKw$sname = ${formatExpr(init)} in ${formatBlockBody(rest)}"

          // Bare `e?` as a statement — evaluates `e`, ignores the success value, and
          // propagates failure. Lower to a match where the success arm continues with
          // `rest` (unwrapped value discarded with `_`) and the failure arm exits.
          case ExprStmtAST(TryAST(inner)) =>
            val (succPat, _, failArm) = tryDesugarPieces(bindName = None)
            s"(match ${formatExpr(inner)} with " +
              s"| $succPat -> ${formatBlockBody(rest)} " +
              s"| $failArm end)"

          case AssignStmtAST(target, value) =>
            if !refScope(target) then
              unsupported("assignment to non-mutable binding", target)
            s"${sanitizeName(target)} := ${formatExpr(value)}; ${formatBlockBody(rest)}"

          case CompoundAssignStmtAST(target, op, value) =>
            if !refScope(target) then
              unsupported("compound assignment to non-mutable binding", target)
            s"${formatCompoundAssign(target, op, value)}; ${formatBlockBody(rest)}"

          case WhileStmtAST(cond, body, _) =>
            s"${formatWhile(cond, body)}; ${formatBlockBody(rest)}"

          // Sysl `for i in lo..<hi <body>` parses to a C-style ForStmtAST(init, cond, update, body).
          // When the shape is canonical (immutable counter, increment-by-1, `<`/`<=` cond, body
          // doesn't reassign the counter), emit a native WhyML `for i = lo to hi do ... done`,
          // whose termination is by construction (no `variant` needed). Otherwise lower to a
          // while-equivalent and recurse — the init's VarStmt naturally goes through ref-form.
          case fs: ForStmtAST =>
            canonicalForLoop(fs) match
              case Some((counterName, lo, hi, body)) =>
                val (annotations, realBody) = body.span {
                  // WhyML `for` accepts only `invariant`; user `variant` is redundant for native
                  // for-loops (loop bound is finite by construction) and silently dropped.
                  case _: InvariantStmtAST | _: VariantStmtAST => true
                  case _                                        => false
                }
                val invariants = annotations.collect {
                  case InvariantStmtAST(e, _) => s"invariant { ${stripOuterParens(formatExpr(e))} }"
                }.mkString(" ")
                val bodyStr = formatLoopBody(realBody)
                val sep = if invariants.isEmpty || bodyStr.isEmpty then "" else " "
                val sname = sanitizeName(counterName)
                s"(for $sname = ${formatExpr(lo)} to ${formatExpr(hi)} do $invariants$sep$bodyStr done); ${formatBlockBody(rest)}"
              case None =>
                val whileEquivalent = fs.init :: WhileStmtAST(fs.cond, fs.body :+ fs.update, None) :: rest
                formatBlockBody(whileEquivalent)

          // Bare expression-statements that we know how to lower to a unit-typed step:
          // `i++` / `i--` are sugar for the corresponding compound assignment.
          case ExprStmtAST(PostIncAST(name)) =>
            if !refScope(name) then unsupported("post-increment of non-mutable binding", name)
            s"${sanitizeName(name)} := (!${sanitizeName(name)} + 1); ${formatBlockBody(rest)}"
          case ExprStmtAST(PostDecAST(name)) =>
            if !refScope(name) then unsupported("post-decrement of non-mutable binding", name)
            s"${sanitizeName(name)} := (!${sanitizeName(name)} - 1); ${formatBlockBody(rest)}"

          // `assert(cond, msg)` in statement position → WhyML `assert { cond }; rest`.
          // The message is dropped — Why3's assert reports the source location and the
          // failed condition, which is enough to locate the violation. The condition is
          // a sysl bool; WhyML accepts bool in assert via implicit coercion to prop.
          case ExprStmtAST(CallAST("assert", List(cond, _))) =>
            s"assert { ${stripOuterParens(formatExpr(cond))} }; ${formatBlockBody(rest)}"
          case ExprStmtAST(CallAST("assert", List(cond))) =>
            s"assert { ${stripOuterParens(formatExpr(cond))} }; ${formatBlockBody(rest)}"

          // `panic(...)` in statement position is no-return — anything after it is dead
          // code in sysl semantics. Emit `absurd` and drop the rest entirely.
          case ExprStmtAST(CallAST("panic", _)) => "absurd"

          // Mid-body early-exit: `if cond then return e` (no else) followed by more stmts
          // lowers to `if cond then <e> else <rest>`. The then-branch must end in a return
          // (otherwise it would fall through into the rest, which has different semantics).
          case ExprStmtAST(IfExprAST(cond, thenStmts, None)) if thenStmts.exists(isReturn) =>
            val thenExpr = stmtsAsExpr(thenStmts)
            s"(if ${formatExpr(cond)} then $thenExpr else ${formatBlockBody(rest)})"

          case other =>
            unsupported(
              "non-binding statement in function body",
              s"supported mid-body forms are val/var bindings, assignments, while loops, and `if cond then return expr`; got ${other.getClass.getSimpleName}")

  /** Format a compound-assignment `target op= value` as `target := (!target op value)`.
   *  `/` and `%` route through int.ComputerDivision's prefix `div` / `mod`; everything else is
   *  infix. The `op` string carries the trailing `=`, e.g. `+=`, which we strip before mapping. */
  private def formatCompoundAssign(target: String, op: String, value: ExpressionAST): String =
    val tname = sanitizeName(target)
    val baseOp = if op.endsWith("=") then op.dropRight(1) else op
    val rhs = baseOp match
      case "/" => s"(div !$tname ${formatExpr(value)})"
      case "%" => s"(mod !$tname ${formatExpr(value)})"
      case _   => s"(!$tname ${mapBinaryOp(baseOp)} ${formatExpr(value)})"
    s"$tname := $rhs"

  /** Format a sysl `while cond <body>` as a WhyML `while cond do invariant{} variant{} body done`.
   *  Sysl interleaves `invariant` and `variant` statements with the loop body; WhyML wants them
   *  immediately after `do`. The split is positional: leading invariant/variant stmts become
   *  WhyML annotations, the rest becomes the loop body proper. WhyML allows multiple `invariant`
   *  clauses but at most one `variant` — Phase 4b doesn't enforce that, leaving it to Why3 to
   *  reject. The body sequences with `;`. The whole `while ... done` is unit-typed. */
  private def formatWhile(cond: ExpressionAST, body: List[StmtAST]): String =
    val (annotations, realBody) = body.span {
      case _: InvariantStmtAST | _: VariantStmtAST => true
      case _                                        => false
    }
    val annoStr = annotations.map {
      case InvariantStmtAST(e, _) => s"invariant { ${stripOuterParens(formatExpr(e))} }"
      case VariantStmtAST(e)      => s"variant { ${stripOuterParens(formatExpr(e))} }"
      case other                  => unsupported("loop annotation", other.getClass.getSimpleName)
    }.mkString(" ")
    val bodyStr = formatLoopBody(realBody)
    val sep = if annoStr.isEmpty || bodyStr.isEmpty then "" else " "
    s"(while ${formatExpr(cond)} do $annoStr$sep$bodyStr done)"

  /** Format the *body* of a loop — a sequence of unit-typed statements joined with `;`.
   *  Unlike `formatBlockBody`, there is no trailing expression: every statement contributes to
   *  the body's side effects and the loop itself is unit-typed. Supported: assignment, compound
   *  assignment, nested while. */
  private def formatLoopBody(stmts: List[StmtAST]): String =
    stmts.map(formatLoopStmt).mkString("; ")

  private def formatLoopStmt(s: StmtAST): String = s match
    case AssignStmtAST(target, value) =>
      if !refScope(target) then unsupported("assignment to non-mutable binding in loop", target)
      s"${sanitizeName(target)} := ${formatExpr(value)}"
    case CompoundAssignStmtAST(target, op, value) =>
      if !refScope(target) then unsupported("compound assignment in loop", target)
      formatCompoundAssign(target, op, value)
    case ExprStmtAST(CallAST("assert", List(cond, _))) =>
      s"assert { ${stripOuterParens(formatExpr(cond))} }"
    case ExprStmtAST(CallAST("assert", List(cond))) =>
      s"assert { ${stripOuterParens(formatExpr(cond))} }"
    case WhileStmtAST(cond, body, _) => formatWhile(cond, body)
    case ExprStmtAST(PostIncAST(name)) =>
      if !refScope(name) then unsupported("post-increment of non-mutable binding in loop", name)
      val n = sanitizeName(name); s"$n := (!$n + 1)"
    case ExprStmtAST(PostDecAST(name)) =>
      if !refScope(name) then unsupported("post-decrement of non-mutable binding in loop", name)
      val n = sanitizeName(name); s"$n := (!$n - 1)"
    case ExprStmtAST(e)              => formatExpr(e)
    case other =>
      unsupported(
        "loop body statement",
        s"loops support assignments, compound assignments, nested while, and expression stmts; got ${other.getClass.getSimpleName}")

  /** Recognize a sysl ForStmtAST shape that maps cleanly to WhyML's native `for i = lo to hi`:
   *    - init  is `var i = lo` (mutable)
   *    - update is `i++` / `i = i + 1` / `i += 1` (increment by 1)
   *    - cond  is `i < hi` (effective hi = `hi - 1`) or `i <= hi` (effective hi = `hi`)
   *    - body  doesn't reassign the counter (WhyML for-loop counters are immutable in body)
   *  Returns (counterName, lo, effectiveHi, body) for emission, or None to fall back to while. */
  private def canonicalForLoop(fs: ForStmtAST): Option[(String, ExpressionAST, ExpressionAST, List[StmtAST])] =
    fs.init match
      case VarStmtAST(name, _, lo, true, _, _, _) =>
        val incBy1 = fs.update match
          case ExprStmtAST(PostIncAST(n)) if n == name => true
          case ExprStmtAST(PreIncAST(n))  if n == name => true
          case CompoundAssignStmtAST(n, "+", IntLitAST(1)) if n == name => true
          case AssignStmtAST(n, BinaryAST(VarRefAST(m), "+", IntLitAST(1))) if n == name && m == name => true
          case _ => false
        if !incBy1 then None
        else fs.cond match
          case BinaryAST(VarRefAST(n1), op, hi) if n1 == name =>
            val effectiveHi = op match
              case "<"  => Some(BinaryAST(hi, "-", IntLitAST(1)))
              case "<=" => Some(hi)
              case _    => None
            effectiveHi.flatMap { ehi =>
              if isReassigned(name, fs.body) then None
              else Some((name, lo, ehi, fs.body))
            }
          case _ => None
      case _ => None

  /** True iff any statement in `stmts` (or any of its nested loops/branches/matches) reassigns
   *  the variable named `name`. Used by `formatBlockBody` to decide whether a `var` should be
   *  emitted as a WhyML ref or as an immutable let. */
  private def isReassigned(name: String, stmts: List[StmtAST]): Boolean =
    stmts.exists(stmtAssignsTo(name, _))

  private def stmtAssignsTo(name: String, s: StmtAST): Boolean = s match
    case AssignStmtAST(t, _) if t == name           => true
    case CompoundAssignStmtAST(t, _, _) if t == name => true
    case ExprStmtAST(PostIncAST(t)) if t == name    => true
    case ExprStmtAST(PostDecAST(t)) if t == name    => true
    case ExprStmtAST(PreIncAST(t))  if t == name    => true
    case ExprStmtAST(PreDecAST(t))  if t == name    => true
    case WhileStmtAST(_, body, _)                   => isReassigned(name, body)
    case ForStmtAST(init, _, update, body, _) =>
      stmtAssignsTo(name, init) || stmtAssignsTo(name, update) || isReassigned(name, body)
    case DoWhileStmtAST(_, body, _)                 => isReassigned(name, body)
    case LoopStmtAST(body, _)                       => isReassigned(name, body)
    case ExprStmtAST(IfExprAST(_, t, e)) =>
      isReassigned(name, t) || e.exists(isReassigned(name, _))
    case ExprStmtAST(MatchExprAST(_, arms, default)) =>
      arms.exists(a => isReassigned(name, a.body)) || default.exists(isReassigned(name, _))
    case _ => false

  /** True iff the function body uses any imperative construct (loops, assignment) that prevents
   *  it from being declared as `let function` (which Why3 reflects to logic). Pure function
   *  bodies — even those with local `let` bindings, conditionals, or matches — keep `function`. */
  private def isImpure(stmts: List[StmtAST]): Boolean = stmts.exists(stmtIsImpure)

  private def stmtIsImpure(s: StmtAST): Boolean = s match
    case _: AssignStmtAST | _: CompoundAssignStmtAST              => true
    case _: WhileStmtAST | _: ForStmtAST | _: DoWhileStmtAST | _: LoopStmtAST => true
    case ExprStmtAST(_: PostIncAST | _: PostDecAST | _: PreIncAST | _: PreDecAST) => true
    case ExprStmtAST(IfExprAST(_, t, e)) =>
      isImpure(t) || e.exists(isImpure)
    case ExprStmtAST(MatchExprAST(_, arms, default)) =>
      arms.exists(a => isImpure(a.body)) || default.exists(isImpure)
    case _ => false

  private def isReturn(s: StmtAST): Boolean = s match
    case _: ReturnStmtAST => true
    case _                => false

  private def stmtAsTrailingExpr(s: StmtAST): String = s match
    case ReturnStmtAST(Some(e)) => formatExpr(e)
    case ExprStmtAST(e)         => formatExpr(e)
    case other =>
      unsupported(
        "non-expression trailing statement",
        s"function body must end with `return <expr>` or a bare expression; got ${other.getClass.getSimpleName}")

  private def emitContract(c: ContractClauseAST): Unit =
    val keyword = c.kind match
      case ContractRequire => "requires"
      case ContractEnsure  => "ensures "
      case ContractVariant => "variant "
    // δ.2: lex-tuple variant — `variant { e1, e2 }` parsed as TupleLitAST. Why3
    // supports lexicographic measures via `variant { e1; e2; ... }` natively.
    c.expr match
      case TupleLitAST(es) if c.kind == ContractVariant =>
        val parts = es.map(e => stripOuterParens(formatExpr(e))).mkString("; ")
        line(s"$keyword { $parts }")
      case _ =>
        line(s"$keyword { ${stripOuterParens(formatExpr(c.expr))} }")

  /** Strip one matched outer paren pair if it wraps the entire string. The formatter
   *  conservatively wraps every binary expression in parens; outermost wrapping inside a
   *  brace-delimited contract clause is redundant and noisy, so peel one layer when safe. */
  private def stripOuterParens(s: String): String =
    if s.length >= 2 && s.startsWith("(") && s.endsWith(")") then
      var depth = 0
      var i = 0
      var allMatched = true
      while i < s.length - 1 && allMatched do
        if s.charAt(i) == '(' then depth += 1
        else if s.charAt(i) == ')' then
          depth -= 1
          if depth == 0 then allMatched = false
        i += 1
      if allMatched then s.substring(1, s.length - 1) else s
    else s

  /** Map a sysl type AST to a WhyML type. Phase 1 collapses every signed/unsigned int width
   *  to mathematical `int` — overflow is a separate verification problem we layer on later
   *  via Why3's `Int32` / `Int64` modules. `bool` maps directly. Type parameters in scope
   *  (e.g. `T` inside `def f[T](...)`) lower to WhyML type variables (`T` → `'t`). Generic
   *  data enums applied to type args (`Option[int]` → `option int`) emit positionally. */
  private def typeOf(t: TypeAST): String = t match
    case NamedTypeAST(name, Nil) => name match
      case "int" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" |
           "byte" | "char" | "rune" => "int"
      case "bool" => "bool"
      case "string" => "string"
      case n if currentTypeParams(n) =>
        // Sysl convention: single-letter or short uppercase type params (T, U, K, V).
        // WhyML type variables must start with `'` and be lowercase: `'t`, `'u`, …
        s"'${n.toLowerCase}"
      case n if enumNames(n) || structFields.contains(n) || dataEnumNames.contains(n) =>
        // Lowercase the first letter to match the lowered type name (enum, struct, data enum).
        s"${n.head.toLower}${n.tail}"
      case other  => unsupported("type",
        s"'$other' is not a recognized sysl type — supported scalars are int/i8..i64/u8..u64/" +
          "byte/char/rune/bool/string, plus any struct, enum, or data-enum declared in this " +
          "module (and type parameters of the enclosing decl). Pointer/slice/array types are " +
          "out of scope for the current verification surface.")
    case NamedTypeAST(name, args) if dataEnumNames.contains(name) =>
      // Generic data-enum application: `Option[int]` → `option int`. Type args are
      // emitted positionally and parenthesized when complex (multi-token).
      val typeName = s"${name.head.toLower}${name.tail}"
      val argStrs = args.map { a =>
        val s = typeOf(a)
        if s.contains(' ') then s"($s)" else s
      }
      s"$typeName ${argStrs.mkString(" ")}"
    case NamedTypeAST(name, args) if structFields.contains(name) =>
      // Generic struct application: `Pair[int]` → `pair int`. Mirrors the data-enum
      // path — positional type args, parenthesized when multi-token. The struct's
      // type-parameter declaration is rendered by emitStruct via currentTypeParams.
      val typeName = s"${name.head.toLower}${name.tail}"
      val argStrs = args.map { a =>
        val s = typeOf(a)
        if s.contains(' ') then s"($s)" else s
      }
      s"$typeName ${argStrs.mkString(" ")}"
    case other => unsupported("type form",
      s"non-named type ${other.getClass.getSimpleName} — function types, slice/array types, " +
        "and pointer types are out of scope for the current verification surface (they map to " +
        "Phase ε in the finish roadmap; until then, model the data as a plain record/enum)")

  /** Build the three pieces needed to desugar a `?` operator: success pattern, success
   *  expression (the value bound), and the failure arm (`pattern -> reconstruct`).
   *
   *  Conventions follow the analyzer (`SyslAnalyzer.scala::TryAST`):
   *    - Variant 0 of the data enum is the success variant (`Some`, `Ok`).
   *    - Variant 1 is the failure variant (`None`, `Err`).
   *    - Failure-variant fields are bound with fresh names and re-emitted as the
   *      failure value (so `Err(e)?` reconstructs `Err e`).
   *
   *  Single-field success unwraps directly; multi-field success would lower to a
   *  tuple, which we don't support yet (the analyzer hasn't built one for std.option /
   *  std.result, the only realistic Phase 4-data+ targets). */
  private def tryDesugarPieces(bindName: Option[String]): (String, String, String) =
    val retType = currentFunctionReturnType.getOrElse(
      unsupported("`?` operator", "enclosing function has no declared return type"))
    val enumName = retType match
      case NamedTypeAST(n, _) if dataEnumNames.contains(n) => n
      case other => unsupported(
        "`?` operator",
        s"enclosing function returns $other, not a generic data enum (Option/Result)")
    val variants = dataEnumNames(enumName)
    if variants.length != 2 then unsupported(
      "`?` operator", s"data enum $enumName has ${variants.length} variants; `?` requires exactly 2")
    val (succName, succFields) = variants.head
    val (failName, failFields) = variants(1)
    if succFields.length != 1 then unsupported(
      "`?` operator", s"success variant $succName has ${succFields.length} fields; only 1-field success supported")
    val succBindName = bindName match
      case Some(n) => s"_try_v_${sanitizeName(n)}"
      case None    => "_"
    val succPat = s"$succName $succBindName"
    val succExpr = if bindName.isEmpty then "()" else succBindName
    val failBindNames = failFields.indices.map(i => s"_try_e$i").toList
    val failPat = if failFields.isEmpty then failName else s"$failName " + failBindNames.mkString(" ")
    val failReconstruct =
      if failFields.isEmpty then failName
      else s"($failName " + failBindNames.mkString(" ") + ")"
    (succPat, succExpr, s"$failPat -> $failReconstruct")

  /** Format an expression as a WhyML expression string. Operator translation is identical
   *  in code and contract positions for the Phase 1 subset (= and <>); we don't switch
   *  between `&&`/`/\` because either form is accepted in both positions in WhyML. */
  private def formatExpr(e: ExpressionAST): String = e match
    case IntLitAST(v) =>
      if v < 0 then s"(- ${-v})" else v.toString
    case BoolLitAST(v) => v.toString
    case StringLitAST(v) =>
      // Sysl strings are opaque to verification — they only flow into panic/assert
      // (where we drop them) or unused parameters. We emit them as Why3 string literals
      // so the type lines up; the actual content is irrelevant to the proof.
      s"\"${v.replace("\\", "\\\\").replace("\"", "\\\"")}\""
    case StringLitExprAST(v) =>
      s"\"${v.replace("\\", "\\\\").replace("\"", "\\\"")}\""
    case VarRefAST(name) =>
      // `result` is WhyML's reserved name for a function's return value (only valid inside
      // ensures clauses). A bare reference to a no-payload data-enum variant (`None`) is a
      // nullary constructor — emit the constructor name as-is. Otherwise: deref if the
      // name names a WhyML ref, else plain.
      if name == "result" then "result"
      else if dataEnumVariantOf.contains(name) then name
      else if refScope(name) then s"!${sanitizeName(name)}"
      else sanitizeName(name)
    case BinaryAST(l, op, r) =>
      // `div` and `mod` from int.ComputerDivision are plain prefix functions in WhyML,
      // not infix operators — `a div b` would parse as `a` applied to `div b`. Emit them
      // as `(div a b)`. All other ops (arithmetic, comparison, logical) are infix.
      op match
        case "/"          => s"(div ${formatExpr(l)} ${formatExpr(r)})"
        case "%" | "mod"  => s"(mod ${formatExpr(l)} ${formatExpr(r)})"
        case _ =>
          val opStr = mapBinaryOp(op)
          s"(${formatExpr(l)} $opStr ${formatExpr(r)})"
    case UnaryAST(op, x) =>
      // The space after `-` and `not` is significant: `-x` would lex as the binary minus,
      // and `notx` as an identifier. Always render with a separator.
      val opStr = op match
        case "-"   => "- "
        case "not" => "not "
        case "!"   => "not "
        case other => unsupported("unary operator",
          s"sysl unary `$other` has no WhyML equivalent in this translator. Bitwise NOT (`~`) " +
            "would need a bitvector model (`use bv.BV32` etc.) — out of scope for the current " +
            "verification surface. Pointer ops (`*`, `&`) are similarly out of scope.")
      s"($opStr${formatExpr(x)})"
    case CallAST("old", List(arg)) =>
      s"(old ${formatExpr(arg)})"
    case CallAST("panic", _) =>
      // sysl `panic("msg")` is no-return. WhyML's `absurd` claims unreachability and
      // emits a `false` proof obligation — the user must prove via preconditions that
      // execution never reaches this point. Exactly the SPARK-style discipline we want
      // (e.g. `unwrap` panics on None ⇒ caller must `requires { is_some o }`). The
      // message argument is dropped — verification cares about reachability, not text.
      "absurd"
    case _: TryAST =>
      // `?` only desugars cleanly when it's the init of a `val name = e?` binding or a
      // bare `e?` statement (handled in `formatBlockBody`). Nested usage like `f(e?)`
      // would need CPS lowering — flag it so the user knows this is a translator gap.
      unsupported(
        "`?` operator outside top-level binding/statement",
        "supported forms: `val name = e?` and bare `e?` as a statement")
    case FieldAccessAST(VarRefAST(t), member) if enumNames(t) =>
      // sysl `EnumName.Variant` → WhyML bare `Variant`. WhyML constructors live at module
      // scope, not under their type, so we just drop the type prefix.
      member
    case FieldAccessAST(obj, field) =>
      // Struct field access. WhyML records use the same `.` syntax: `p.x`. Sanitize the
      // field name for keyword collisions; the obj formats recursively (handles parenthesized
      // sub-expressions, deref of refs, etc).
      s"${formatExpr(obj)}.${sanitizeName(field)}"
    case CallAST(n, args) if structFields.contains(n) =>
      // Struct construction: `Point(1, 2)` → WhyML record literal `{ x = 1; y = 2 }`. Sysl
      // also allows named-arg construction (`Point(x=1, y=2)`); the parser already binds
      // those positionally by this point. Arg count must match field count exactly.
      val fields = structFields(n)
      if args.length != fields.length then
        unsupported("struct construction arity mismatch",
                    s"$n expects ${fields.length} fields, got ${args.length} args")
      val pairs = fields.zip(args).map { case (fname, av) =>
        s"$fname = ${formatExpr(av)}"
      }.mkString("; ")
      s"{ $pairs }"
    case CallAST(n, args) if dataEnumVariantOf.contains(n) =>
      // Data-enum constructor application: `Some(42)` → `(Some 42)`. WhyML constructors are
      // curried-style — arguments follow the constructor name with spaces, no parens at the
      // call site. Outer parens group the whole constructor application as a single value.
      if args.isEmpty then n
      else s"($n ${args.map(formatExpr).mkString(" ")})"
    case CallAST(n, args) =>
      val argStr = if args.isEmpty then "" else args.map(formatExpr).mkString(" ", " ", "")
      s"(${sanitizeName(n)}$argStr)"
    case IfExprAST(c, tb, eb) =>
      // WhyML: `if c then e1 else e2` requires both branches to unify in type.
      // Sysl `if cond then body` (no else) at expression position is genuinely
      // ambiguous — for unit-typed body it's fine ("do this if cond, else
      // nothing"), for value-typed body the missing else has no good answer.
      // We emit `else ()` to match the unit case; if the body is value-typed
      // Why3 reports a clear type-mismatch error at verification time (much
      // better than the translator silently giving up).
      val tExpr = stmtsAsExpr(tb)
      val eExpr = eb match
        case Some(stmts) => stmtsAsExpr(stmts)
        case None        => "()"
      s"(if ${formatExpr(c)} then $tExpr else $eExpr)"
    case MatchExprAST(scrutinee, arms, default) =>
      // WhyML: `match e with | pat -> body | ... end`. Each arm's body must be a single
      // expression — multi-statement arm bodies (let-bindings, sequencing) come with the
      // broader multi-stmt support in a later piece. The default arm (sysl `else`)
      // becomes a wildcard `_ -> ...`.
      //
      // WhyML restricts match patterns to ADT constructors and `_`/variables — integer
      // and bool literal patterns are NOT allowed. Sysl `n match { 0 -> a; 1 -> b;
      // else -> c }` therefore lowers to an if-chain `(if n = 0 then a else if n = 1
      // then b else c)` when the scrutinee is not an enum value. Detection is
      // syntactic: if every non-default arm's pattern is an ADT constructor, emit
      // `match`; otherwise lower to if-chain.
      //
      // Phase β supports:
      //   - Multi-pattern arms (`| A | B -> body`) in both ADT and literal paths.
      //   - Range patterns (`1..10`) in the literal path (lower to a guard).
      //   - Wildcard patterns before the default in the literal path (`true`).
      //   - Struct destructure patterns (`Point { x, y }`) in the ADT path.
      //
      // WhyML logic-mode `match` does NOT have `when` guards — guards on individual
      // arms still error out, with a clearer message pointing at the workaround
      // (rewrite as if-chain manually, or push the guard into the arm body when
      // there's a clean fall-through).
      if arms.exists(_.guard.isDefined) then unsupported(
        "match arm with `when` guard",
        "Why3 logic-mode `match` has no native guards — rewrite as an if-chain " +
          "(`if pat-matches /\\ guard then body else next`) or move the condition into " +
          "the arm body when there's a single fall-through path",
      )
      val allCtorArms = arms.forall { a =>
        a.patterns.forall {
          case ValuePatternAST(FieldAccessAST(VarRefAST(t), _)) if enumNames(t) => true
          case ValuePatternAST(VarRefAST(n)) if dataEnumVariantOf.contains(n) => true
          case DestructurePatternAST(n, _) if dataEnumVariantOf.contains(n) => true
          case DestructurePatternAST(n, _) if structFields.contains(n)      => true
          case WildcardPatternAST => true
          case _                  => false
        }
      }
      if allCtorArms then
        val sb = new StringBuilder
        sb.append(s"(match ${formatExpr(scrutinee)} with")
        for arm <- arms do
          val patStr = arm.patterns.map(formatPattern).mkString(" | ")
          sb.append(s" | $patStr -> ${stmtsAsExpr(arm.body)}")
        default match
          case Some(stmts) => sb.append(s" | _ -> ${stmtsAsExpr(stmts)}")
          case None        =>
        sb.append(" end)")
        sb.toString
      else
        // Literal-pattern path — lower to a chain of `if` tests. The scrutinee is
        // evaluated once and each pattern becomes a condition against it. Multi-pattern
        // arms OR-join their conditions; range patterns become `lo <= x /\ x <= hi`;
        // wildcards become unconditional `true` (matches anything).
        val scr = formatExpr(scrutinee)
        val defaultExpr = default match
          case Some(stmts) => stmtsAsExpr(stmts)
          case None        => unsupported("literal-pattern match without `else` default",
                                          "WhyML cannot pattern-match int/bool literals — needs an exhaustive else")
        def patternCond(p: MatchPatternAST): String = p match
          case ValuePatternAST(e)         => s"$scr = ${formatExpr(e)}"
          case RangePatternAST(lo, hi)    =>
            // Sysl ranges in match are inclusive: `1..10` matches 1..=10.
            s"($scr >= ${formatExpr(lo)} /\\ $scr <= ${formatExpr(hi)})"
          case WildcardPatternAST         => "true"
          case other                      => unsupported("literal-pattern shape", other.getClass.getSimpleName)
        val sb = new StringBuilder
        sb.append("(")
        for arm <- arms do
          val cond = arm.patterns.map(patternCond).mkString(" \\/ ")
          sb.append(s"if $cond then ${stmtsAsExpr(arm.body)} else ")
        sb.append(s"$defaultExpr)")
        sb.toString
    case QuantifierAST(kind, name, lo, hi, inclusive, pred) =>
      // sysl `for all x in lo..hi => P`  → `forall x: int. lo <= x <= hi -> P`
      // sysl `for all x in lo..<hi => P` → `forall x: int. lo <= x <  hi -> P`
      // sysl `for some` mirrors with `exists` and conjunction (/\) instead of implication.
      // The bound is `int` because Phase 1 collapses every sysl int width to mathematical int.
      val cmp = if inclusive then "<=" else "<"
      val v = sanitizeName(name)
      val loS = formatExpr(lo)
      val hiS = formatExpr(hi)
      val predS = formatExpr(pred)
      kind match
        case "all"  => s"(forall $v: int. $loS <= $v $cmp $hiS -> $predS)"
        case "some" => s"(exists $v: int. $loS <= $v $cmp $hiS /\\ $predS)"
        case other  => unsupported("quantifier kind", other)
    case other => unsupported("expression",
      s"sysl ${other.getClass.getSimpleName} has no WhyML translation in this backend yet. " +
        "Likely candidates not yet covered: SliceAST, IndexAST, ArrayLitAST (need slice/array " +
        "model — Phase ε), AddrOfAST/DerefAST (pointer model — Phase ε), TryAST in non-top-" +
        "level position (needs CPS lowering). If your code hits this in a verification " +
        "context, the path forward is to model the data without slices/pointers.")

  private def stmtsAsExpr(stmts: List[StmtAST]): String = stmts match
    case List(ReturnStmtAST(Some(e))) => formatExpr(e)
    case List(ExprStmtAST(e))         => formatExpr(e)
    case _ =>
      unsupported(
        "if-branch with non-trivial body",
        "Phase 1 supports only a single expression or single `return <expr>` per branch")

  /** Format a sysl match pattern as a WhyML pattern. Covers the wildcard, integer
   *  literal patterns, simple enum constructor patterns, bare no-payload data-enum
   *  variant references (`None`), destructured data-enum variant patterns
   *  (`Some(v)`, `Some(_)`), and struct destructure patterns (`Point { x, y }`).
   *
   *  Range patterns are NOT formatted here — WhyML `match` has no range syntax;
   *  ranges only appear in the literal-if-chain path (handled in MatchExprAST emit). */
  private def formatPattern(p: MatchPatternAST): String = p match
    case WildcardPatternAST => "_"
    case ValuePatternAST(IntLitAST(v))   => if v < 0 then s"(- ${-v})" else v.toString
    case ValuePatternAST(BoolLitAST(v))  => v.toString
    case ValuePatternAST(FieldAccessAST(VarRefAST(t), member)) if enumNames(t) => member
    case ValuePatternAST(VarRefAST(n)) if dataEnumVariantOf.contains(n) => n
    case ValuePatternAST(VarRefAST(name)) => sanitizeName(name)
    case ValuePatternAST(other) => unsupported("match value pattern", other.getClass.getSimpleName)
    case _: RangePatternAST     => unsupported("range match pattern in ADT context",
      "WhyML `match` has no range syntax — ranges work only in the literal-if-chain " +
        "path (when the scrutinee is a number, not an ADT)")
    case DestructurePatternAST(n, fields) if dataEnumVariantOf.contains(n) =>
      // `Some(v)` → `Some v`, `Some(_)` → `Some _`. Each sub-pattern formats recursively
      // (today only wildcards and bare names; nested destructuring works the same way).
      if fields.isEmpty then n
      else s"$n " + fields.map(formatPattern).mkString(" ")
    case DestructurePatternAST(n, fields) if structFields.contains(n) =>
      // Struct destructure → WhyML record pattern `{ field1 = pat; field2 = pat; ... }`.
      // Sysl positional destructure (`Point(x, y)`) zips with the struct's field
      // declaration order; named destructure (parser-side syntax) would arrive with the
      // same shape since the pattern is positional in the AST.
      val fieldNames = structFields(n)
      if fields.length != fieldNames.length then
        unsupported("struct destructure arity mismatch",
          s"$n expects ${fieldNames.length} field${if fieldNames.length == 1 then "" else "s"}, got ${fields.length}")
      val parts = fieldNames.zip(fields).map { (fname, fpat) => s"$fname = ${formatPattern(fpat)}" }
      s"{ ${parts.mkString("; ")} }"
    case _: DestructurePatternAST => unsupported("destructuring match pattern",
      "name not recognized as a data-enum variant or struct in scope")

  private def mapBinaryOp(op: String): String = op match
    case "==" => "="
    case "!=" => "<>"
    // WhyML: `&&` / `||` work in both program (bool) and contract (prop) positions; Why3
    // coerces bool to prop in formula contexts. Formula-only `/\` / `\/` are emitted directly
    // by the quantifier translator (where the surrounding context is guaranteed to be a prop).
    case "&&" => "&&"
    case "||" => "||"
    // `/` and `%` route through int.ComputerDivision's `div` / `mod` — the only sense in
    // which integer division is total in WhyML. The lexer treats `mod` as an identifier;
    // it is recognized as the operator only because we imported ComputerDivision.
    case "/"   => "div"
    case "%"   => "mod"
    case "mod" => "mod"
    case "+" | "-" | "*" | "<" | ">" | "<=" | ">=" => op
    case other => unsupported("binary operator",
      s"sysl binary `$other` has no WhyML equivalent. Bitwise ops (`& | ^ << >>`) need a " +
        "bitvector model (`use bv.BV32`); arithmetic on bytes/floats may need width-specific " +
        "imports. Out of scope for the current verification surface.")

  /** Strip module-qualified prefixes; lowercase the first letter (WhyML reserves
   *  uppercase-first identifiers for constructors / modules); map sysl identifiers that
   *  collide with WhyML keywords to safe names. Constructor names go through a separate
   *  path (formatPattern / FieldAccessAST handling), so they retain their original case. */
  private def sanitizeName(n: String): String =
    val bare = n.indexOf("__") match
      case -1 => n
      case i  => n.substring(i + 2)
    // WhyML rejects uppercase-first identifiers in value position (those are constructors).
    // For all-uppercase identifiers like `MAX_AGE`, lowercase the entire string to avoid
    // ugly half-cased names like `mAX_AGE`. Mixed-case identifiers (`MyValue`) only need
    // the first letter lowered (`myValue`). The transformation is consistent across decl
    // and use sites because it's the same function.
    val lc =
      if bare.nonEmpty && bare.head.isUpper then
        if bare.forall(c => c.isUpper || c == '_' || c.isDigit) then bare.toLowerCase
        else s"${bare.head.toLower}${bare.tail}"
      else bare
    lc match
      case "function" | "let" | "in" | "with" | "match" | "end" | "module" | "use" |
           "type" | "begin" | "rec" | "and" | "or" | "fun" | "if" | "then" | "else" |
           "for" | "to" | "do" | "done" | "while" | "result" | "old" | "lemma" |
           "axiom" | "theory" | "predicate" | "exception" | "assert" | "assume" |
           "check" | "ghost" | "pure" | "absurd" | "raise" | "any" | "ref" |
           "writes" | "reads" | "requires" | "ensures" | "variant" | "invariant" =>
        lc + "_"
      case _ => lc
