package io.github.edadma.trisc

import io.github.edadma.indentation.IndentationLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharSequenceReader

class SyslParser extends StandardTokenParsers {

  override val lexical: SyslLexical = new SyslLexical

  import lexical.{Newline, Indent, Dedent}

  // --- Entry point ---

  def parseProgram(source: String): Either[String, ProgramAST] =
    phrase(program)(lexical.read(new CharSequenceReader(source))) match
      case Success(result, _) => Right(result)
      case ns: NoSuccess      => Left(ns.toString)

  def parseExpression(source: String): Either[String, ExpressionAST] =
    // Use logicalOr (not expr) to avoid match/if which need indentation context.
    // Accept optional trailing Newline that the IndentationLexical may insert.
    phrase(logicalOr <~ opt(Newline))(lexical.read(new CharSequenceReader(source))) match
      case Success(result, _) => Right(result)
      case ns: NoSuccess      => Left(ns.toString)

  // --- Program ---

  lazy val program: Parser[ProgramAST] =
    opt(moduleDecl <~ rep1(Newline)) ~ repsep(decl, rep1(Newline)) <~ opt(rep(Newline)) ^^ {
      case mod ~ decls => ProgramAST(mod.toList ::: decls)
    }

  lazy val moduleDecl: Parser[ModuleDeclAST] =
    "module" ~> rep1sep(importIdent, ".") ^^ ModuleDeclAST.apply

  // --- Declarations ---

  lazy val decl: Parser[DeclAST] =
    rep(positioned(attribute) <~ rep1(Newline)) ~ declBare ^^ {
      case attrs ~ d => if attrs.isEmpty then d else attachAttrs(d, attrs)
    }

  lazy val declBare: Parser[DeclAST] =
    condDecl | importDecl | externDecl | structDecl | enumDecl | traitDecl | implDecl | interfaceDecl | typeAliasDecl | staticAssertDecl | "private" ~> "def" ~> defDecl(true) | "private" ~> declBody(true) | "def" ~> defDecl(false) | declBody(false)

  lazy val staticAssertDecl: Parser[StaticAssertDeclAST] =
    "static_assert" ~> "(" ~> expr ~ opt("," ~> stringLit) <~ ")" ^^ {
      case cond ~ msg => StaticAssertDeclAST(cond, msg)
    }

  // --- Attributes ---

  lazy val attribute: Parser[Attribute] =
    "#" ~> ident ~ opt("(" ~> repsep(attrArg, ",") <~ ")") ^^ {
      case name ~ args => Attribute(name, args.getOrElse(Nil))
    }

  lazy val attrArg: Parser[AttrArg] =
    ident ~ opt(":" ~> attrLiteral) ^^ {
      case name ~ Some(v) => AttrNamed(name, v)
      case name ~ None    => AttrPositional(AttrLitIdent(name))
    } |
    attrLiteral ^^ AttrPositional.apply

  lazy val attrLiteral: Parser[AttrLiteral] =
    stringLit ^^ AttrLitString.apply |
    numericLit ^^ (s => AttrLitInt(s.toLong)) |
    "true" ^^^ AttrLitBool(true) |
    "false" ^^^ AttrLitBool(false)

  private def attachAttrs(d: DeclAST, attrs: List[Attribute]): DeclAST = d match
    case f: FunDeclAST        => f.copy(attributes = attrs ++ f.attributes)
    case s: StructDeclAST     => s.copy(attributes = attrs ++ s.attributes)
    case e: EnumDeclAST       => e.copy(attributes = attrs ++ e.attributes)
    case e: DataEnumDeclAST   => e.copy(attributes = attrs ++ e.attributes)
    case v: VarDeclAST        => v.copy(attributes = attrs ++ v.attributes)
    case t: TraitDeclAST      => t.copy(attributes = attrs ++ t.attributes)
    case i: ImplDeclAST       => i.copy(attributes = attrs ++ i.attributes)
    case t: TypeAliasDeclAST  => t.copy(attributes = attrs ++ t.attributes)
    case e: ExternFuncDeclAST => e.copy(attributes = attrs ++ e.attributes)
    case e: ExternVarDeclAST  => e.copy(attributes = attrs ++ e.attributes)
    case other                => other

  // --- Conditional compilation ---

  lazy val condDecl: Parser[CondDeclAST] =
    "#" ~> "if" ~> condExpr ~ (rep1(Newline) ~> rep1sep(decl, rep1(Newline))) ~
      opt(rep1(Newline) ~> "#" ~> "else" ~> rep1(Newline) ~> rep1sep(decl, rep1(Newline))) <~
      rep1(Newline) <~ "#" <~ "endif" ^^ {
      case cond ~ thenDecls ~ elseDecls => CondDeclAST(cond, thenDecls, elseDecls)
    }

  lazy val condExpr: Parser[CondExpr] =
    "!" ~> ident ^^ (name => CondNot(CondSymbol(name))) |
      ident ~ ("==" ~> condValue) ^^ { case name ~ value => CondEq(name, value) } |
      ident ~ ("!=" ~> condValue) ^^ { case name ~ value => CondNeq(name, value) } |
      ident ^^ CondSymbol.apply

  lazy val condValue: Parser[String] =
    stringLit |
      numericLit |
      "true" ^^^ "true" |
      "false" ^^^ "false"

  lazy val structDecl: Parser[StructDeclAST] =
    "struct" ~> ident ~ typeParamList ~ (Newline ~> Indent ~> rep1sep(structMember, rep1(Newline)) <~ opt(Newline) <~ Dedent) <~ opt(endMarker("struct")) ^^ {
      case name ~ tps ~ members =>
        val fields = members.collect { case Left(f) => f }
        val invariants = members.collect { case Right(e) => e }
        StructDeclAST(name, fields, tps, Nil, invariants)
    }

  // A struct body member is either a field declaration or an `invariant <expr>` clause.
  lazy val structMember: Parser[Either[(String, TypeAST, Boolean), ExpressionAST]] =
    "invariant" ~> expr ^^ (e => Right(e)) |
    structField ^^ (f => Left(f))

  lazy val structField: Parser[(String, TypeAST, Boolean)] =
    opt("volatile") ~ ident ~ (":" ~> typeRef) ^^ { case vol ~ name ~ typ => (name, typ, vol.isDefined) }

  lazy val enumDecl: Parser[DeclAST] =
    "enum" ~> ident ~ typeParamList ~ (Newline ~> Indent ~> rep1sep(enumVariantOrMember, rep1(Newline)) <~ opt(Newline) <~ Dedent) <~ opt(endMarker("enum")) ^^ {
      case name ~ tps ~ members =>
        // If any member has fields, it's a data enum
        val hasData = members.exists(_.isInstanceOf[Right[?, ?]]) || tps.nonEmpty
        if hasData then
          val variants = members.map {
            case Right(v) => v
            case Left((n, _)) => EnumVariantAST(n, Nil) // plain member in a data enum = no-arg variant
          }
          DataEnumDeclAST(name, variants, tps)
        else
          EnumDeclAST(name, members.map { case Left(m) => m; case _ => ??? })
    }

  // Returns Left for simple members, Right for data variants
  lazy val enumVariantOrMember: Parser[Either[(String, Option[Long]), EnumVariantAST]] =
    ident ~ ("(" ~> repsep(structField, ",") <~ ")") ^^ { case name ~ fields => Right(EnumVariantAST(name, fields.map((n, t, _) => (n, t)))) } |
      ident ~ ("=" ~> numericLit) ^^ { case name ~ value => Left((name, Some(value.toLong))) } |
      ident ^^ (name => Left((name, None)))

  lazy val typeAliasDecl: Parser[TypeAliasDeclAST] =
    "type" ~> ident ~ opt("[" ~> rep1sep(ident, ",") <~ "]") ~ ("=" ~> opt("new")) ~ typeRef ~ opt(withinClause) ~ opt(whereClause) ^^ {
      case name ~ tparams ~ isNew ~ target ~ range ~ predicate =>
        TypeAliasDeclAST(name, target, tparams.getOrElse(Nil), Nil, isNew.isDefined, range, predicate)
    }

  lazy val withinClause: Parser[RangeAST] =
    "within" ~> unary ~ (("..<" | "..") ~ unary) ^^ {
      case lo ~ (op ~ hi) => RangeAST(lo, hi, op == "..<")
    }

  lazy val whereClause: Parser[ExpressionAST] =
    "where" ~> logicalOr

  lazy val traitDecl: Parser[TraitDeclAST] =
    "trait" ~> ident ~ ("[" ~> rep1sep(ident, ",") <~ "]") ~
      (Newline ~> Indent ~> rep1sep(traitMethod, rep1(Newline)) <~ opt(Newline) <~ Dedent) <~ opt(endMarker("trait")) ^^ {
        case name ~ tparams ~ methods => TraitDeclAST(name, tparams, methods)
      }

  lazy val traitMethod: Parser[TraitMethodAST] =
    rep(positioned(attribute) <~ rep1(Newline)) ~ ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ ("->" ~> typeRef) ~ opt(traitMethodBody) ^^ {
      case attrs ~ name ~ params ~ rt ~ body => TraitMethodAST(name, params, rt, body, attrs)
    }

  lazy val traitMethodBody: Parser[FunBodyAST] =
    "=" ~> bodyExprOrBlock |
      funBlockBody

  lazy val implDecl: Parser[ImplDeclAST] =
    "impl" ~> opt("[" ~> rep1sep(ident, ",") <~ "]") ~ ident ~ ("[" ~> rep1sep(typeRef, ",") <~ "]") ~
      (Newline ~> Indent ~> rep1sep(implMethod, rep1(Newline)) <~ opt(Newline) <~ Dedent) <~ opt(endMarker("impl")) ^^ {
        case tparams ~ name ~ targets ~ methods =>
          ImplDeclAST(name, tparams.getOrElse(Nil), targets, methods)
      }

  lazy val implMethod: Parser[FunDeclAST] =
    ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ funRest ^^ {
      case name ~ params ~ ((rt, body)) => FunDeclAST(name, params, rt, body)
    }

  lazy val interfaceDecl: Parser[InterfaceDeclAST] =
    "interface" ~> ident ~
      (Newline ~> Indent ~> rep1sep(interfaceMember, rep1(Newline)) <~ opt(Newline) <~ Dedent) <~ opt(endMarker("interface")) ^^ {
        case name ~ members =>
          val methods = members.collect { case Right(m) => m }
          val embedded = members.collect { case Left(n) => n }
          InterfaceDeclAST(name, methods, embedded)
      }

  lazy val interfaceMember: Parser[Either[String, InterfaceMethodAST]] =
    ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ opt("->" ~> typeRef) ~ funcTypeEffects ^^ {
      case name ~ params ~ rt ~ eff => Right(InterfaceMethodAST(name, params, rt.getOrElse(NamedTypeAST("unit")), eff))
    } |
    ident ^^ (name => Left(name))


  // Accept identifiers and type keywords (e.g., "string") in import paths
  private lazy val importIdent: Parser[String] =
    ident | "int" | "uint" | "long" | "ulong" | "short" | "ushort" | "char" | "byte" | "bool" | "unit" | "string" |
      "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" |
      "float" | "f32" | "double" | "f64"

  lazy val importDecl: Parser[ImportDeclAST] =
    "import" ~> rep1sep(importIdent, ".") ~ opt("." ~> importTail) ^^ {
      case path ~ Some(selectors) => ImportDeclAST(path.mkString("/"), selectors)
      case path ~ None =>
        if path.length < 2 then sys.error(s"import requires selector: use 'import ${path.head}.*' or 'import ${path.head}.name'")
        // Ambiguous: could be qualified module import (import std.strings)
        // or single symbol import (import math.add). Mark as QualifiedImport;
        // the driver resolves by checking if the full path is a known module.
        ImportDeclAST(path.mkString("/"), List(QualifiedImport))
    }

  lazy val importTail: Parser[List[ImportSelector]] =
    "*" ^^^ List(WildcardImport) |
      "{" ~> rep1sep(importItem, ",") <~ "}"

  lazy val importItem: Parser[NamedImport] =
    importIdent ~ ("=>" ~> importIdent) ^^ { case name ~ alias => NamedImport(name, Some(alias)) } |
      importIdent ^^ (name => NamedImport(name))

  lazy val externDecl: Parser[DeclAST] =
    "extern" ~> ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ opt("->" ~> typeRef) ^^ {
      case name ~ params ~ rt => ExternFuncDeclAST(name, params, rt)
    } |
      "extern" ~> ident ~ (":" ~> typeRef) ^^ {
        case name ~ t => ExternVarDeclAST(name, t)
      }

  private case class Mut(isMutable: Boolean, isConst: Boolean)
  private def mutability: Parser[Mut] =
    "var" ^^^ Mut(true, false) | "val" ^^^ Mut(false, false) | "const" ^^^ Mut(false, true)

  def declBody(priv: Boolean): Parser[DeclAST] =
    ident ~ typeParamListWithBounds ~ ("." ~> ident) ~ ("(" ~> repsep(param, ",") <~ ")") ~ funRest ^^ {
      case typeName ~ tps ~ methodName ~ params ~ ((rt, body)) =>
        // Sem.wait(params) -> ret { body } desugars to Sem_wait(__self__: *Sem, params) -> ret { body }
        // MinHeap[T].push(v: T) desugars to MinHeap_push[T](__self__: *MinHeap[T], v: T) -> ret { body }
        // The parameter is named `__self__` to avoid collisions with user-declared
        // params named `self`. The analyzer auto-aliases `self` -> `__self__` in
        // method bodies, so users still write `self.x`.
        val names = tps.map(_._1)
        val bounds = tps.collect { case (n, bs) if bs.nonEmpty => (n, bs) }.toMap
        val typeArgs = names.map(n => NamedTypeAST(n): TypeAST)
        val selfParam = ParamAST("__self__", PtrTypeAST(NamedTypeAST(typeName, typeArgs)))
        FunDeclAST(s"${typeName}_$methodName", selfParam :: params, rt, body, priv, names, bounds)
    } |
    ident ~ typeParamListWithBounds ~ ("(" ~> repsep(param, ",") <~ ")") ~ funRest ^^ {
      case name ~ tps ~ params ~ ((rt, body)) =>
        val names = tps.map(_._1)
        val bounds = tps.collect { case (n, bs) if bs.nonEmpty => (n, bs) }.toMap
        FunDeclAST(name, params, rt, body, priv, names, bounds)
    } |
      // Parameterless function: `name -> RetType = body` or
      // `name -> RetType <indented block>`. Disambiguates from typed val
      // (which uses `:`) by the `->` token. Cannot have type parameters
      // (a generic parameterless makes no sense — there's nothing at the
      // call site to fix the type args). Auto-called at every reference.
      ident ~ ("->" ~> typeRef) ~ ("=" ~> bodyExprOrBlock) ^^ {
        case name ~ rt ~ body => FunDeclAST(name, Nil, Some(rt), body, priv, isParameterless = true)
      } |
      ident ~ ("->" ~> typeRef) ~ funBlockBody ^^ {
        case name ~ rt ~ body => FunDeclAST(name, Nil, Some(rt), body, priv, isParameterless = true)
      } |
      opt("volatile") ~ opt(mutability) ~ ident ~ (":" ~> typeExpr) ~ ("=" ~> expr) ^^ {
        case vol ~ mut ~ name ~ t ~ e =>
          val m = mut.getOrElse(Mut(true, false))
          VarDeclAST(name, Some(t), e, priv, m.isMutable, isVolatile = vol.isDefined, isConst = m.isConst)
      } |
      opt("volatile") ~ opt(mutability) ~ ident ~ (":" ~> typeExpr) ^^ {
        case vol ~ mut ~ name ~ t =>
          val m = mut.getOrElse(Mut(true, false))
          val size = t match { case ArrayTypeAST(s, _) => s; case _ => 0 }
          VarDeclAST(name, Some(t), ArrayDeclAST(size, t), priv, m.isMutable, isVolatile = vol.isDefined, isConst = m.isConst)
      } |
      opt("volatile") ~ opt(mutability) ~ ident ~ (":" ~> typeRef) ~ not("=") ^^ {
        case vol ~ mut ~ name ~ t ~ _ =>
          val m = mut.getOrElse(Mut(true, false))
          VarDeclAST(name, Some(t), UninitDeclAST(t), priv, m.isMutable, isVolatile = vol.isDefined, isConst = m.isConst)
      } |
      opt("volatile") ~ opt(mutability) ~ ident ~ (":" ~> typeRef) ~ ("=" ~> expr) ^^ {
        case vol ~ mut ~ name ~ t ~ e =>
          val m = mut.getOrElse(Mut(true, false))
          VarDeclAST(name, Some(t), e, priv, m.isMutable, isVolatile = vol.isDefined, isConst = m.isConst)
      } |
      opt("volatile") ~ opt(mutability) ~ ident ~ ("=" ~> expr) ^^ {
        case vol ~ mut ~ name ~ e =>
          val m = mut.getOrElse(Mut(true, false))
          VarDeclAST(name, None, e, priv, m.isMutable, isVolatile = vol.isDefined, isConst = m.isConst)
      }

  /** `def name = expr` (zero-arg auto-call) or `def name(params) -> ret body` (documentary). */
  def defDecl(priv: Boolean): Parser[FunDeclAST] =
    // def name(params) -> ret body — parametric, isDef is documentary
    ident ~ typeParamListWithBounds ~ ("(" ~> repsep(param, ",") <~ ")") ~ funRest ^^ {
      case name ~ tps ~ params ~ ((rt, body)) =>
        val names = tps.map(_._1)
        val bounds = tps.collect { case (n, bs) if bs.nonEmpty => (n, bs) }.toMap
        FunDeclAST(name, params, rt, body, priv, names, bounds, isDef = true)
    } |
    // def name -> RetType body — zero-arg with explicit return type
    ident ~ ("->" ~> typeRef) ~ ("=" ~> bodyExprOrBlock) ^^ {
      case name ~ rt ~ body => FunDeclAST(name, Nil, Some(rt), body, priv, isDef = true)
    } |
    ident ~ ("->" ~> typeRef) ~ funBlockBody ^^ {
      case name ~ rt ~ body => FunDeclAST(name, Nil, Some(rt), body, priv, isDef = true)
    } |
    // def name = expr — zero-arg, inferred return type
    ident ~ ("=" ~> bodyExprOrBlock) ^^ {
      case name ~ body => FunDeclAST(name, Nil, None, body, priv, isDef = true)
    }

  // Type parameter with optional trait bounds: T, T: Ord, T: Ord + Eq
  lazy val typeParamWithBounds: Parser[(String, List[String])] =
    ident ~ opt(":" ~> rep1sep(ident, "+")) ^^ {
      case name ~ bounds => (name, bounds.getOrElse(Nil))
    }

  // Optional type parameter list for generic functions: [T], [T, U], [T: Ord], or absent
  lazy val typeParamList: Parser[List[String]] =
    opt("[" ~> rep1sep(ident, ",") <~ "]") ^^ (_.getOrElse(Nil))

  // Type parameter list that captures bounds: [T: Ord], [T: Ord + Eq, U: Eq]
  lazy val typeParamListWithBounds: Parser[List[(String, List[String])]] =
    opt("[" ~> rep1sep(typeParamWithBounds, ",") <~ "]") ^^ (_.getOrElse(Nil))

  lazy val funRest: Parser[(Option[TypeAST], FunBodyAST)] =
    "->" ~> typeRef ~ ("=" ~> bodyExprOrBlock) ^^ { case rt ~ body => (Some(rt), body) } |
      "->" ~> typeRef ~ funBlockBody ^^ { case rt ~ body => (Some(rt), body) } |
      "=" ~> bodyExprOrBlock ^^ { body => (None, body) } |
      funBlockBody ^^ { body => (None, body) }

  /** One parsed contract clause, possibly expanded into multiple clauses (e.g. `ensure cases`
   *  desugars to one require-OR for completeness + N ensures for per-case implication). */
  lazy val contractClause: Parser[List[ContractClauseAST]] =
    ensureCasesBlock |
    "require" ~> expr ~ opt("," ~> stringLit) ^^ { case e ~ msg => List(ContractClauseAST(ContractRequire, e, msg)) } |
    "ensure" ~> expr ~ opt("," ~> stringLit) ^^ { case e ~ msg => List(ContractClauseAST(ContractEnsure, e, msg)) } |
    "variant" ~> expr ^^ { case e => List(ContractClauseAST(ContractVariant, e, None)) }

  /** `ensure cases` block: `guard => postcondition [, "msg"]`, one per line, at least one case.
   *  Desugared here into a require (OR of guards — completeness) + N ensures
   *  (`!old(guard_i) || postcondition_i` — per-case implication with entry-state guard). */
  lazy val ensureCasesBlock: Parser[List[ContractClauseAST]] =
    "ensure" ~> (ident ^? { case "cases" => () }) ~>
      Newline ~> Indent ~> rep1(ensureCase <~ rep1(stmtSep)) <~ opt(Newline) <~ Dedent ^^ {
      cases => desugarEnsureCases(cases)
    }

  lazy val ensureCase: Parser[(ExpressionAST, ExpressionAST, Option[String])] =
    expr ~ ("=>" ~> expr) ~ opt("," ~> stringLit) ^^ {
      case guard ~ post ~ msg => (guard, post, msg)
    }

  private def desugarEnsureCases(cases: List[(ExpressionAST, ExpressionAST, Option[String])]): List[ContractClauseAST] =
    val guards = cases.map(_._1)
    val completenessExpr = guards.reduce((a, b) => BinaryAST(a, "||", b))
    val completenessClause = ContractClauseAST(
      ContractRequire, completenessExpr,
      Some("ensure cases: no guard matched on entry"),
    )
    val perCase = cases.zipWithIndex.map { case ((g, p, msg), i) =>
      val oldG = CallAST("old", List(g))
      val implExpr = BinaryAST(UnaryAST("!", oldG), "||", p)
      ContractClauseAST(
        ContractEnsure, implExpr,
        Some(msg.getOrElse(s"ensure cases: case ${i + 1} violated")),
      )
    }
    completenessClause :: perCase

  /** A function block body: zero or more contract clauses at the top, followed by statements. */
  lazy val funBlockBody: Parser[BlockBodyAST] =
    Newline ~> Indent ~> rep(contractClause <~ rep1(stmtSep)) ~ stmts <~ opt(Newline) <~ Dedent ^^ {
      case contracts ~ stmts => BlockBodyAST(stmts, contracts.flatten)
    }

  lazy val bodyExprOrBlock: Parser[FunBodyAST] =
    Newline ~> Indent ~> rep(contractClause <~ rep1(stmtSep)) ~ stmts <~ opt(Newline) <~ Dedent ^^ {
      case contracts ~ stmts => BlockBodyAST(stmts, contracts.flatten)
    } |
      forStmt ^^ (s => BlockBodyAST(List(s))) |
      whileStmt ^^ (s => BlockBodyAST(List(s))) |
      doWhileStmt ^^ (s => BlockBodyAST(List(s))) |
      tupleExpr ^^ ExprBodyAST.apply

  // Ada-style parameter mode prefix (optional): `in`, `out`, or `inout` before the
  // param name. `in` is already reserved; `out` / `inout` are contextual keywords
  // (user identifiers named `out` or `inout` still work outside param position).
  private lazy val paramMode: Parser[ParamMode] =
    "in"                                            ^^ (_ => ParamMode.In)     |
    (ident ^? { case "inout" => ParamMode.Inout })                             |
    (ident ^? { case "out"   => ParamMode.Out   })

  // `=> T` — call-by-name marker on a param type. Only legal in param position
  // (the analyzer surfaces a clean error if it shows up in any other type
  // context). `=>` is otherwise the closure-arrow token, but in parser context
  // here it can only mean "by-name introducer" — a closure literal is an
  // expression, never a type.
  lazy val byNameTypeRef: Parser[TypeAST] =
    "=>" ~> typeRef ^^ ByNameTypeAST.apply

  lazy val paramTypeRef: Parser[TypeAST] = byNameTypeRef | typeRef

  // Two branches with explicit `|` alternation, not `opt(paramMode) ~ ident` — we
  // need backtracking when `paramMode` matches the *name* of a param (e.g. `out: T`
  // where the param is actually named `out`). `opt` commits on success, so the
  // modeful branch is tried first and failure falls through to the mode-less branch.
  lazy val param: Parser[ParamAST] =
    (paramMode ~ ident ~ (":" ~> paramTypeRef) ~ opt("=" ~> expr) ^^ {
      case mode ~ name ~ t ~ default => ParamAST(name, t, default, mode)
    }) |
    (ident ~ (":" ~> paramTypeRef) ~ opt("=" ~> expr) ^^ {
      case name ~ t ~ default => ParamAST(name, t, default)
    })

  // Function call argument: `name = expr` (named) or `expr` (positional).
  // The `ident ~ "="` lookahead must succeed only when both tokens are present.
  // Bare `_` is left as a placeholder so the enclosing call can absorb it
  // (partial application: `f(_, 0)` → `x -> f(x, 0)`); other exprs containing
  // `_` are wrapped here so the arg itself becomes the lambda body
  // (`f(_+1, 0)` → `f(x -> x+1, 0)`).
  lazy val callArg: Parser[ExpressionAST] =
    ident ~ ("=" ~> expr) ^^ { case name ~ value => NamedArgAST(name, wrapPlaceholders(value)) } |
      expr ^^ wrapPlaceholders

  // Optional type argument list for generic type references: [T], [T, U], or absent
  lazy val typeArgList: Parser[List[TypeAST]] =
    opt("[" ~> rep1sep(typeRef, ",") <~ "]") ^^ (_.getOrElse(Nil))

  lazy val typeName: Parser[TypeAST] =
    ("int" | "uint" | "long" | "ulong" | "short" | "ushort" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "float" | "f32" | "double" | "f64" | "bool" | "string") ^^ (n => NamedTypeAST(n)) |
      "unit" ^^^ NamedTypeAST("unit") |
      ident ~ typeArgList ^^ { case name ~ args => NamedTypeAST(name, args) }

  // Full type reference: *int, **int, &Node, [5]int, []int (slice), (int)->int, @escaping (int)->int, string, int, etc.
  lazy val typeRef: Parser[TypeAST] =
    "*" ~> typeRef ~ opt("not" ~> "null") ^^ {
      case t ~ Some(_) => PtrNonNullTypeAST(t)
      case t ~ None    => PtrTypeAST(t)
    } |
      "&" ~> typeRef ^^ RefTypeAST.apply |
      "[" ~> "]" ~> typeRef ^^ SliceTypeAST.apply |
      "[" ~> numericLit ~ ("]" ~> typeRef) ^^ { case n ~ t => ArrayTypeAST(n.toInt, t) } |
      "@" ~> "escaping" ~> funcTypeRef ^^ { case FuncTypeAST(p, r, _, eff) => FuncTypeAST(p, r, escaping = true, effects = eff); case t => t } |
      funcTypeRef |
      "(" ~> rep1sep(typeRef, ",") <~ ")" ^^ TupleTypeAST.apply |
      typeName

  /** Optional effect suffix on a function type: `#pure`, or any combination of
   *  `#reads(a, b)` / `#writes(c)` repeated. Distinguishes the three FuncEffects states
   *  used in subset-check (caller-vs-callee) at every indirect call site:
   *  - no suffix → Unknown (can only be called from unannotated callers)
   *  - `#pure` → Pure (callable from any annotated caller)
   *  - `#reads`/`#writes` → RW(reads, writes) (callable when subset of caller's effect set) */
  lazy val funcTypeEffects: Parser[FuncEffects] =
    rep("#" ~> ident ~ opt("(" ~> repsep(ident, ",") <~ ")")) ^^ { items =>
      var isPure = false
      var reads: Option[Set[String]] = None
      var writes: Option[Set[String]] = None
      for (name ~ args) <- items do
        name match
          case "pure" =>
            if args.exists(_.nonEmpty) then throw new RuntimeException("#pure on a function type takes no arguments")
            isPure = true
          case "reads" =>
            val r = args.getOrElse(Nil).toSet
            reads = Some(reads.getOrElse(Set.empty) ++ r)
          case "writes" =>
            val w = args.getOrElse(Nil).toSet
            writes = Some(writes.getOrElse(Set.empty) ++ w)
          case other => throw new RuntimeException(s"unknown effect annotation '#$other' on function type")
      if isPure && (reads.isDefined || writes.isDefined) then
        throw new RuntimeException("#pure on a function type cannot be combined with #reads/#writes")
      FuncEffects(isPure, reads, writes)
    }

  lazy val funcTypeRef: Parser[TypeAST] =
    // (int, int) -> int #pure    or   () -> unit #reads(g) #writes(h)    or just (...) -> ...
    "(" ~> repsep(typeRef, ",") ~ (")" ~> "->" ~> typeRef) ~ funcTypeEffects ^^ {
      case params ~ ret ~ eff => FuncTypeAST(params, ret, effects = eff)
    }

  // Array type for uninitialized declarations: [5]int
  lazy val typeExpr: Parser[TypeAST] =
    "[" ~> numericLit ~ ("]" ~> typeRef) ^^ { case n ~ t => ArrayTypeAST(n.toInt, t) }

  // --- Block ---

  lazy val block: Parser[List[StmtAST]] =
    Newline ~> Indent ~> stmts <~ opt(Newline) <~ Dedent

  // --- Statements ---

  lazy val stmtSep: Parser[Any] = rep1(Newline) | ";"
  lazy val stmts: Parser[List[StmtAST]] =
    rep1sep(stmt, rep1(stmtSep))

  lazy val asmStmt: Parser[AsmStmtAST] =
    "asm" ~> "(" ~> stringLit <~ ")" ^^ AsmStmtAST.apply

  lazy val invariantStmt: Parser[InvariantStmtAST] =
    "invariant" ~> expr ~ opt("," ~> stringLit) ^^ { case e ~ msg => InvariantStmtAST(e, msg) }

  lazy val variantStmt: Parser[VariantStmtAST] =
    "variant" ~> expr ^^ VariantStmtAST.apply

  lazy val assumeStmt: Parser[AssumeStmtAST] =
    "assume" ~> expr ~ opt("," ~> stringLit) ^^ { case e ~ msg => AssumeStmtAST(e, msg) }

  lazy val stmt: Parser[StmtAST] =
    ghostVarStmt | innerFunStmt | asmStmt | invariantStmt | variantStmt | assumeStmt | labeledLoop | forStmt | doWhileStmt | whileStmt | loopStmt | returnStmt | breakStmt | continueStmt | deferStmt | destructureStmt | derefAssignStmt | identStmt | expr ^^ (e => ExprStmtAST(wrapPlaceholders(e)))

  /** `def name(params) -> ret body` (or `def name -> ret body` zero-arg) at statement
   *  position — declares a recursively-callable named local closure. The analyzer lowers
   *  this to a `TClosure` with `selfName = Some(name)` and a `TVarStmt` binding. */
  lazy val innerFunStmt: Parser[StmtAST] =
    "def" ~> defDecl(false) ^^ InnerFunStmtAST.apply

  /** `#ghost var/val name = ...` at statement position — a ghost local declaration. Only
   *  accepts a plain var/val form (no `#address`, no `static_assert`, etc.). The resulting
   *  `VarStmtAST` has `isGhost = true`; the analyzer and strip pass handle the rest. */
  lazy val ghostVarStmt: Parser[StmtAST] =
    "#" ~> ident ~ identStmt ^? ({
      case "ghost" ~ (v: VarStmtAST) => v.copy(isGhost = true)
    }, {
      case "ghost" ~ other => s"#ghost at statement position must be followed by `var` or `val`, got $other"
      case name ~ _ => s"unknown statement-level attribute '#$name'"
    })

  lazy val destructureStmt: Parser[DestructureStmtAST] =
    mutability ~ ("(" ~> rep1sep(bindName, ",") <~ ")") ~ ("=" ~> tupleExpr) ^^ { case mut ~ names ~ init => DestructureStmtAST(names, init, mut.isMutable) } |
      ("(" ~> rep1sep(bindName, ",") <~ ")") ~ ("=" ~> tupleExpr) ^^ { case names ~ init => DestructureStmtAST(names, init) } |
      mutability ~ bindName ~ ("," ~> rep1sep(bindName, ",")) ~ ("=" ~> tupleExpr) ^^ { case mut ~ first ~ rest ~ init => DestructureStmtAST(first :: rest, init, mut.isMutable) } |
      "_" ~ ("," ~> rep1sep(bindName, ",")) ~ ("=" ~> tupleExpr) ^^ { case _ ~ rest ~ init => DestructureStmtAST("_" :: rest, init) } |
      ident ~ ("," ~> rep1sep(bindName, ",")) ~ ("=" ~> tupleExpr) ^^ { case first ~ rest ~ init => DestructureStmtAST(first :: rest, init) }

  lazy val breakStmt: Parser[BreakStmtAST] =
    "break" ~> opt(ident) ^^ BreakStmtAST.apply

  lazy val continueStmt: Parser[ContinueStmtAST] =
    "continue" ~> opt(ident) ^^ ContinueStmtAST.apply

  /** `label: for ...` / `label: while ...` / `label: do ...` / `label: loop ...` — labeled loop form. */
  lazy val labeledLoop: Parser[StmtAST] =
    (ident <~ ":") ~ (forStmt | doWhileStmt | whileStmt | loopStmt) ^^ {
      case name ~ loop => attachLoopLabel(name, loop)
    }

  private def attachLoopLabel(name: String, stmt: StmtAST): StmtAST = stmt match
    case f: ForStmtAST     => f.copy(label = Some(name))
    case w: WhileStmtAST   => w.copy(label = Some(name))
    case d: DoWhileStmtAST => d.copy(label = Some(name))
    case l: LoopStmtAST    => l.copy(label = Some(name))
    case other             => other

  lazy val deferStmt: Parser[DeferStmtAST] =
    "defer" ~> (derefAssignStmt | identStmt | expr ^^ ExprStmtAST.apply) ^^ DeferStmtAST.apply

  lazy val compoundOp: Parser[String] =
    "<<=" | ">>=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="

  // Parse a chain of .field and [index] postfixes after an identifier
  private type LvalueOp = Either[ExpressionAST, String] // Left = index, Right = field
  lazy val lvalueChain: Parser[List[LvalueOp]] =
    rep(("[" ~> expr <~ "]") ^^ (idx => Left(idx)) | ("." ~> ident) ^^ (f => Right(f)))

  // Build an assignment statement from a base identifier + chain of postfix ops + value
  private def buildAssign(name: String, chain: List[LvalueOp], value: ExpressionAST): StmtAST =
    chain match
      case Nil => AssignStmtAST(name, value)
      case _ =>
        val (initOps, lastOp) = (chain.init, chain.last)
        val base: ExpressionAST = initOps.foldLeft[ExpressionAST](VarRefAST(name)) {
          case (e, Left(idx)) => IndexAST(e, idx)
          case (e, Right(field)) => FieldAccessAST(e, field)
        }
        lastOp match
          case Right(field) => FieldAssignStmtAST(base, field, value)
          case Left(idx) => IndexAssignStmtAST(base, idx, value)

  // Build a compound assignment (+=, -=, etc.) from base + chain + op + value
  private def buildCompoundAssign(name: String, chain: List[LvalueOp], op: String, value: ExpressionAST): StmtAST =
    chain match
      case Nil => CompoundAssignStmtAST(name, op, value)
      case _ =>
        val (initOps, lastOp) = (chain.init, chain.last)
        val base: ExpressionAST = initOps.foldLeft[ExpressionAST](VarRefAST(name)) {
          case (e, Left(idx)) => IndexAST(e, idx)
          case (e, Right(field)) => FieldAccessAST(e, field)
        }
        lastOp match
          case Right(field) => FieldCompoundAssignStmtAST(base, field, op, value)
          case Left(idx) =>
            // No IndexCompoundAssignStmt, desugar: a[i] += v → a[i] = a[i] + v
            val fullLvalue = IndexAST(base, idx)
            IndexAssignStmtAST(base, idx, BinaryAST(fullLvalue, op, value))

  lazy val bindName: Parser[String] = ident | "_"

  /** Right-hand side of a `val`/`var` initializer or an assignment. Accepts
   *  either an inline expression (`val x = expr`) or an indented expression
   *  on the next line (`val x =` ⏎ Indent expr Dedent), mirroring the way
   *  function bodies take both `= expr` and `= ⏎ Indent stmts Dedent`. The
   *  multi-line form is common when the RHS is a long generic call or a
   *  deeply parenthesized constructor and the user wants to break after `=`.
   */
  lazy val valRhs: Parser[ExpressionAST] =
    (Newline ~> Indent ~> tupleExpr <~ opt(Newline) <~ Dedent) |
      tupleExpr

  lazy val identStmt: Parser[StmtAST] =
    opt("volatile") ~ mutability ~ bindName ~ (":" ~> typeExpr) ~ ("=" ~> valRhs) ^^ { case vol ~ mut ~ name ~ t ~ e => VarStmtAST(name, Some(t), e, mut.isMutable, vol.isDefined, mut.isConst) } |
      opt("volatile") ~ mutability ~ ident ~ (":" ~> typeExpr) ^^ { case vol ~ mut ~ name ~ t =>
        val size = t match { case ArrayTypeAST(s, _) => s; case _ => 0 }
        VarStmtAST(name, Some(t), ArrayDeclAST(size, t), mut.isMutable, vol.isDefined, mut.isConst)
      } |
      opt("volatile") ~ mutability ~ ident ~ (":" ~> typeRef) ~ not("=") ^^ { case vol ~ mut ~ name ~ t ~ _ =>
        VarStmtAST(name, Some(t), UninitDeclAST(t), mut.isMutable, vol.isDefined, mut.isConst)
      } |
      opt("volatile") ~ mutability ~ bindName ~ (":" ~> typeRef) ~ ("=" ~> valRhs) ^^ { case vol ~ mut ~ name ~ t ~ e => VarStmtAST(name, Some(t), e, mut.isMutable, vol.isDefined, mut.isConst) } |
      opt("volatile") ~ mutability ~ bindName ~ ("=" ~> valRhs) ^^ { case vol ~ mut ~ name ~ e => VarStmtAST(name, None, e, mut.isMutable, vol.isDefined, mut.isConst) } |
      ident ~ (":" ~> typeExpr) ~ ("=" ~> valRhs) ^^ { case name ~ t ~ e => VarStmtAST(name, Some(t), e) } |
      ident ~ (":" ~> typeExpr) ^^ { case name ~ t =>
        val size = t match { case ArrayTypeAST(s, _) => s; case _ => 0 }
        VarStmtAST(name, Some(t), ArrayDeclAST(size, t))
      } |
      ident ~ (":" ~> typeRef) ~ not("=") ^^ { case name ~ t ~ _ =>
        VarStmtAST(name, Some(t), UninitDeclAST(t))
      } |
      ident ~ (":" ~> typeRef) ~ ("=" ~> valRhs) ^^ { case name ~ t ~ e => VarStmtAST(name, Some(t), e) } |
      ident ~ lvalueChain ~ compoundOp ~ expr ^^ { case name ~ chain ~ op ~ value =>
        buildCompoundAssign(name, chain, op.init, value)
      } |
      ident ~ lvalueChain ~ ("=" ~> valRhs) ^^ { case name ~ chain ~ value =>
        buildAssign(name, chain, value)
      }

  // Expression-based lvalue: *expr, (*expr).field, (*expr)[idx], (expr)[idx], etc.
  // Parses a deref or parenthesized expression, then an optional chain of .field/[idx],
  // then = or compound-assign operator.
  lazy val derefAssignStmt: Parser[StmtAST] =
    ("*" ~> unary | "(" ~> expr <~ ")") ~ lvalueChain ~ compoundOp ~ expr ^^ {
      case base ~ chain ~ op ~ value =>
        buildExprCompoundAssign(base, chain, op.init, value)
    } |
    ("*" ~> unary | "(" ~> expr <~ ")") ~ lvalueChain ~ ("=" ~> expr) ^^ {
      case base ~ chain ~ value =>
        buildExprAssign(base, chain, value)
    }

  // Build assignment from expression base + lvalue chain
  private def buildExprAssign(base: ExpressionAST, chain: List[LvalueOp], value: ExpressionAST): StmtAST =
    chain match
      case Nil => DerefAssignStmtAST(base, value) // *expr = value (base is already the deref target)
      case _ =>
        val (initOps, lastOp) = (chain.init, chain.last)
        val fullBase = initOps.foldLeft(base) {
          case (e, Left(idx)) => IndexAST(e, idx)
          case (e, Right(field)) => FieldAccessAST(e, field)
        }
        lastOp match
          case Right(field) => FieldAssignStmtAST(fullBase, field, value)
          case Left(idx) => IndexAssignStmtAST(fullBase, idx, value)

  private def buildExprCompoundAssign(base: ExpressionAST, chain: List[LvalueOp], op: String, value: ExpressionAST): StmtAST =
    chain match
      case Nil => DerefAssignStmtAST(base, BinaryAST(DerefAST(base), op, value))
      case _ =>
        val (initOps, lastOp) = (chain.init, chain.last)
        val fullBase = initOps.foldLeft(base) {
          case (e, Left(idx)) => IndexAST(e, idx)
          case (e, Right(field)) => FieldAccessAST(e, field)
        }
        lastOp match
          case Right(field) => FieldCompoundAssignStmtAST(fullBase, field, op, value)
          case Left(idx) =>
            val fullLvalue = IndexAST(fullBase, idx)
            IndexAssignStmtAST(fullBase, idx, BinaryAST(fullLvalue, op, value))

  lazy val forBody: Parser[List[StmtAST]] =
    ("do" ~> (block | inlineStmt ^^ (s => List(s)))) | block

  lazy val rangeOp: Parser[String] = "..<" | ".." | "downTo"

  lazy val forStmt: Parser[ForStmtAST] =
    ("for" ~> identStmt ~ (";" ~> expr) ~ (";" ~> forUpdate) ~ ("do" ~> (block | inlineStmt ^^ (s => List(s)))) ^^ {
      case init ~ cond ~ update ~ body => ForStmtAST(init, cond, update, body)
    } |
      "for" ~> identStmt ~ (";" ~> expr) ~ (";" ~> forUpdate) ~ block ^^ {
        case init ~ cond ~ update ~ body => ForStmtAST(init, cond, update, body)
      } |
      "for" ~> ident ~ ("," ~> ident) ~ ("in" ~> logicalOr) ~ forBody ^^ {
        case idxName ~ valName ~ arr ~ body => buildForIndexValue(idxName, valName, arr, body)
      } |
      "for" ~> ident ~ ("in" ~> logicalOr) ~ rangeOp ~ logicalOr ~ opt("step" ~> logicalOr) ~ forBody ^^ {
        case name ~ lo ~ op ~ hi ~ step ~ body => buildForRange(name, lo, op, hi, step, body)
      } |
      "for" ~> ident ~ ("in" ~> reverseKw ~> logicalOr) ~ forBody ^^ {
        case valName ~ arr ~ body => buildForEach(valName, arr, body, reverse = true)
      } |
      "for" ~> ident ~ ("in" ~> logicalOr) ~ forBody ^^ {
        case valName ~ arr ~ body => buildForEach(valName, arr, body)
      }) <~ opt(endMarker("for"))

  // Contextual keyword: `reverse` is not a reserved word (so user identifiers named
  // `reverse` still work), but acts as a keyword directly after `in` in a for-loop.
  private lazy val reverseKw: Parser[Unit] = ident ^? { case "reverse" => () }

  private def buildForRange(name: String, lo: ExpressionAST, op: String, hi: ExpressionAST, step: Option[ExpressionAST], body: List[StmtAST]): ForStmtAST =
    val (condOp, updateOp) = op match
      case "..<"    => ("<",  "+")
      case ".."     => ("<=", "+")
      case "downTo" => (">=", "-")
    val update: StmtAST = step match
      case Some(s) => CompoundAssignStmtAST(name, updateOp, s)
      case None    => if updateOp == "+" then ExprStmtAST(PostIncAST(name)) else ExprStmtAST(PostDecAST(name))
    ForStmtAST(
      VarStmtAST(name, None, lo),
      BinaryAST(VarRefAST(name), condOp, hi),
      update,
      body,
    )

  private def buildForEach(valName: String, arr: ExpressionAST, body: List[StmtAST], reverse: Boolean = false): ForStmtAST =
    arr match
      // `for i in T::Range` desugars to `for i in T::First..T::Last` (or `T::Last downTo T::First`).
      case TypeAttrAST(typeName, "Range", None) =>
        val lo = TypeAttrAST(typeName, "First", None)
        val hi = TypeAttrAST(typeName, "Last", None)
        if reverse then buildForRange(valName, hi, "downTo", lo, None, body)
        else buildForRange(valName, lo, "..", hi, None, body)
      case _ if reverse =>
        // `for v in reverse arr` — iterate backward from len-1 to 0.
        val idxName = s"__foreach_idx_${valName}"
        ForStmtAST(
          VarStmtAST(idxName, None, BinaryAST(CallAST("len", List(arr)), "-", IntLitAST(1))),
          BinaryAST(VarRefAST(idxName), ">=", IntLitAST(0)),
          ExprStmtAST(PostDecAST(idxName)),
          VarStmtAST(valName, None, IndexAST(arr, VarRefAST(idxName))) :: body,
        )
      case _ =>
        val idxName = s"__foreach_idx_${valName}"
        ForStmtAST(
          VarStmtAST(idxName, None, IntLitAST(0)),
          BinaryAST(VarRefAST(idxName), "<", CallAST("len", List(arr))),
          ExprStmtAST(PostIncAST(idxName)),
          VarStmtAST(valName, None, IndexAST(arr, VarRefAST(idxName))) :: body,
        )

  private def buildForIndexValue(idxName: String, valName: String, arr: ExpressionAST, body: List[StmtAST]): ForStmtAST =
    ForStmtAST(
      VarStmtAST(idxName, None, IntLitAST(0)),
      BinaryAST(VarRefAST(idxName), "<", CallAST("len", List(arr))),
      ExprStmtAST(PostIncAST(idxName)),
      VarStmtAST(valName, None, IndexAST(arr, VarRefAST(idxName))) :: body,
    )

  lazy val forUpdate: Parser[StmtAST] =
    identStmt | expr ^^ ExprStmtAST.apply

  lazy val doWhileStmt: Parser[DoWhileStmtAST] =
    "do" ~> block ~ (Newline ~> "while" ~> expr) ^^ { case body ~ cond => DoWhileStmtAST(cond, body) } |
      "do" ~> inlineStmt ~ ("while" ~> expr) ^^ { case stmt ~ cond => DoWhileStmtAST(cond, List(stmt)) } |
      "do" ~> inlineStmt ~ (Newline ~> "while" ~> expr) ^^ { case stmt ~ cond => DoWhileStmtAST(cond, List(stmt)) }

  lazy val whileStmt: Parser[WhileStmtAST] =
    ("while" ~> expr ~ ("do" ~> (block | inlineStmt ^^ (s => List(s)))) ^^ { case cond ~ body => WhileStmtAST(cond, body) } |
      "while" ~> expr ~ block ^^ { case cond ~ body => WhileStmtAST(cond, body) }) <~ opt(endMarker("while"))

  /** Ada-style infinite loop: `loop` <indented body> [`end loop`]. */
  lazy val loopStmt: Parser[LoopStmtAST] =
    ("loop" ~> block <~ opt(endMarker("loop"))) ^^ (body => LoopStmtAST(body))

  /** Optional Scala-3-style `end <kw>` terminator. Always preceded by a newline. */
  private def endMarker(kw: String): Parser[Unit] =
    Newline ~> "end" ~> kw ^^^ (())

  lazy val returnStmt: Parser[ReturnStmtAST] =
    "return" ~> opt(tupleExpr) ^^ ReturnStmtAST.apply

  // Comma-separated expressions form a tuple at statement level (like Go/Python)
  // Inside f(args) and [elems], plain expr is used so commas stay as separators
  lazy val tupleExpr: Parser[ExpressionAST] =
    expr ~ rep1("," ~> expr) ^^ { case first ~ rest => TupleLitAST((first :: rest).map(wrapPlaceholders)) } |
      expr ^^ wrapPlaceholders

  // ----- Underscore placeholder expansion (Scala-style anonymous functions) -----
  //
  // `_` in expression position is parsed as `UnderscorePlaceholderAST` and then
  // desugared into a `ClosureAST` at the smallest enclosing "boundary" — see
  // `wrapPlaceholders` below. The boundary is the smallest enclosing expression
  // that does NOT propagate `_` further upward; specifically:
  //
  //   - parenthesized expression (`(_ + 1)` is `x -> x + 1`)
  //   - call / method-call / new-call argument that is not bare `_` (`f(_+1)`
  //     wraps the arg as `f(x -> x+1)`; `f(_)` instead absorbs the bare `_`
  //     into the call → `x -> f(x)` for partial application)
  //   - statement-level expression (`var f = _ + 1` is `var f = x -> x + 1`)
  //   - return value, array literal element, tuple literal element
  //
  // Within a single boundary, `_` "bubbles up" through binary/unary operators,
  // field/index access, and the receiver of a method/indirect call — these
  // refuse to absorb. Bare `_` at a call-arg position is handled separately
  // via `absorbBarePlaceholdersInCall` (the call itself becomes the lambda).

  /** Walk `e`, replacing each direct `UnderscorePlaceholderAST` with a fresh
   *  `VarRefAST`. Recurses through "non-absorbing" expression shapes; stops at
   *  closures, call-arg lists (already processed), and statement-shaped exprs.
   *  Returns the substituted expression and the list of fresh names in
   *  left-to-right order. */
  private def substitutePlaceholders(e: ExpressionAST): (ExpressionAST, List[String]) =
    val names = scala.collection.mutable.ListBuffer.empty[String]
    def fresh(): String =
      val n = s"_ph${names.size}"
      names += n
      n
    def go(x: ExpressionAST): ExpressionAST = x match
      case _: UnderscorePlaceholderAST => VarRefAST(fresh())
      case BinaryAST(l, op, r)         => BinaryAST(go(l), op, go(r))
      case UnaryAST(op, x2)            => UnaryAST(op, go(x2))
      case CallAST(name, args)         => CallAST(name, args)
      case MethodCallAST(obj, m, args) => MethodCallAST(go(obj), m, args)
      case IndirectCallAST(c, args)    => IndirectCallAST(go(c), args)
      case FieldAccessAST(obj, f)      => FieldAccessAST(go(obj), f)
      case IndexAST(o, i)              => IndexAST(go(o), go(i))
      case AddrOfFieldAST(obj, f)      => AddrOfFieldAST(go(obj), f)
      case AddrOfIndexAST(o, i)        => AddrOfIndexAST(go(o), go(i))
      case DerefAST(e2)                => DerefAST(go(e2))
      case CastAST(t, e2)              => CastAST(t, go(e2))
      case SliceExprAST(a, lo, hi)     => SliceExprAST(go(a), lo.map(go), hi.map(go))
      case TryAST(e2)                  => TryAST(go(e2))
      case other                       => other
    val transformed = go(e)
    (transformed, names.toList)

  /** Wrap `e` in a `ClosureAST` if it contains `_` placeholders to be bound
   *  here; otherwise return `e` unchanged. Bare `_` is left alone — that's
   *  reserved for outer call-argument absorption (partial application). */
  private def wrapPlaceholders(e: ExpressionAST): ExpressionAST = e match
    case _: UnderscorePlaceholderAST => e
    case _ =>
      val (sub, names) = substitutePlaceholders(e)
      if names.isEmpty then e
      else ClosureAST(names.map(n => ClosureParamAST(n, None)), ExprBodyAST(sub))

  /** If `args` contains any bare `_` placeholders, wrap the whole call in a
   *  ClosureAST (partial application). `rebuild` reconstructs the call AST
   *  from the substituted arg list. Otherwise the call is returned as-is. */
  private def absorbBarePlaceholdersInCall(args: List[ExpressionAST], rebuild: List[ExpressionAST] => ExpressionAST): ExpressionAST =
    val names = scala.collection.mutable.ListBuffer.empty[String]
    val newArgs = args.map {
      case _: UnderscorePlaceholderAST =>
        val n = s"_ph${names.size}"
        names += n
        VarRefAST(n)
      case other => other
    }
    if names.isEmpty then rebuild(args)
    else ClosureAST(names.toList.map(n => ClosureParamAST(n, None)), ExprBodyAST(rebuild(newArgs)))

  // --- Expressions ---

  lazy val expr: Parser[ExpressionAST] = closureExpr | matchExpr | ifIsExpr | ifExpr | logicalOr

  /** Contextual keywords `all` / `some` — only meaningful directly after `for` in an
   *  expression position. Not reserved at the lexer level so user identifiers named
   *  `all` or `some` still work everywhere else. */
  private lazy val quantKw: Parser[String] = ident ^? { case k @ ("all" | "some") => k }

  /** `for all x in lo..hi => P(x)` and `for some x in lo..hi => P(x)`. Lives in `primary`
   *  (reachable from any expression position including `if cond`/`while cond`/`require`).
   *  The body uses `expr` so it greedy-extends to the end of the surrounding expression
   *  — `for all x => P(x) && Q(x)` reads as `for all x => (P(x) && Q(x))`, matching Ada
   *  semantics. The for-loop parsers also start with `for` but require `<ident> = ...`
   *  or `<ident> in <iter> <body>`; neither matches `for all <ident> in ...`, so this
   *  alternative falls out cleanly on backtrack. */
  lazy val quantifierExpr: Parser[QuantifierAST] =
    "for" ~> quantKw ~ ident ~ ("in" ~> bitwiseOr) ~ (("..<" | "..") ~ bitwiseOr) ~ ("=>" ~> expr) ^^ {
      case kind ~ name ~ lo ~ (op ~ hi) ~ pred =>
        QuantifierAST(kind, name, lo, hi, op == "..", pred)
    }

  /** `if expr is Pattern then body [else elseBody]` — desugars to match. */
  lazy val ifIsExpr: Parser[MatchExprAST] =
    "if" ~> logicalOr ~ ("is" ~> matchPattern) ~ ("then" ~> thenBody) ^^ {
      case scrutinee ~ pattern ~ ((thenStmts, elseStmts)) =>
        val arm = MatchArmAST(List(pattern), None, thenStmts)
        MatchExprAST(scrutinee, List(arm), elseStmts)
    }

  lazy val closureExpr: Parser[ClosureAST] =
    // Zero params: () -> body
    "(" ~ ")" ~ "->" ~> closureBody ^^ { body => ClosureAST(Nil, body) } |
    // Multi params: (x, y) -> body or (x: int, y: int) -> body
    ("(" ~> rep1sep(closureParam, ",") <~ ")") ~ ("->" ~> closureBody) ^^ {
      case params ~ body => ClosureAST(params, body)
    } |
    // Single param: x -> body
    ident ~ ("->" ~> closureBody) ^^ { case name ~ body =>
      ClosureAST(List(ClosureParamAST(name, None)), body)
    }

  // `_` is accepted as a discard binder — the param has a fresh slot in the
  // closure but is unreferenceable in the body. The analyzer rewrites `_` to
  // a unique synthetic name so multiple `_` params don't collide and so any
  // `_` in the body falls through to the placeholder rule, not a var lookup.
  lazy val closureParam: Parser[ClosureParamAST] =
    (ident | "_") ~ opt(":" ~> typeRef) ^^ { case name ~ typ => ClosureParamAST(name, typ) }

  lazy val closureBody: Parser[FunBodyAST] =
    // `block` runs at top-level lambdas where the body is on its own indented
    // line(s) (Newline+Indent emitted). Inside parens — call args, casts,
    // tuples — the lexer suppresses Newline/Indent, so `block` won't match;
    // the body comes back as one large expression. Use full `expr` so that
    // expression includes if-then-else, match, and nested closures.
    block ^^ (stmts => BlockBodyAST(stmts)) |
    expr ^^ ExprBodyAST.apply

  lazy val matchExpr: Parser[MatchExprAST] =
    logicalOr ~ ("match" ~> (matchArmsIndented | matchArmsInline)) <~ opt(endMarker("match")) ^^ {
      case scrutinee ~ (arms ~ default) => MatchExprAST(scrutinee, arms, default)
    }

  // Indented form (top-level / outside parens). Newline/Indent/Dedent emitted by the lexer.
  private lazy val matchArmsIndented: Parser[List[MatchArmAST] ~ Option[List[StmtAST]]] =
    Newline ~> Indent ~> rep1(matchArm) ~ opt(matchElse) <~ Dedent

  // Inline form for paren / line-joining contexts (call arg, cast arg, tuple lit) where
  // the lexer suppresses Newline/Indent/Dedent. Arms are detected greedily by their
  // pattern; rep1 stops at the first token that doesn't begin a pattern (e.g. `,` or
  // `)` of the enclosing call). Bodies are single expressions, not blocks.
  private lazy val matchArmsInline: Parser[List[MatchArmAST] ~ Option[List[StmtAST]]] =
    rep1(matchArmInline) ~ opt(matchElseInline)

  lazy val matchArm: Parser[MatchArmAST] =
    rep1sep(matchPattern, ",") ~ opt("if" ~> logicalOr) ~ ("->" ~> (block | inlineStmt ^^ (s => List(s)))) <~ opt(Newline) ^^ {
      case patterns ~ guard ~ body => MatchArmAST(patterns, guard, body)
    }

  lazy val matchArmInline: Parser[MatchArmAST] =
    rep1sep(matchPattern, ",") ~ opt("if" ~> logicalOr) ~ ("->" ~> expr) ^^ {
      case patterns ~ guard ~ body => MatchArmAST(patterns, guard, List(ExprStmtAST(body)))
    }

  lazy val matchElseInline: Parser[List[StmtAST]] =
    "else" ~> "->" ~> expr ^^ (e => List(ExprStmtAST(e)))

  lazy val matchPattern: Parser[MatchPatternAST] =
    "_" ^^^ WildcardPatternAST |
      ident ~ ("(" ~> repsep(matchPattern, ",") <~ ")") ^^ { case name ~ fields => DestructurePatternAST(name, fields) } |
      logicalOr ~ (".." ~> logicalOr) ^^ { case lo ~ hi => RangePatternAST(lo, hi) } |
      logicalOr ^^ ValuePatternAST.apply

  lazy val matchElse: Parser[List[StmtAST]] =
    "else" ~> "->" ~> (block | inlineStmt ^^ (s => List(s))) <~ opt(Newline)

  lazy val ifExpr: Parser[IfExprAST] =
    ("if" ~> logicalOr ~ ("then" ~> thenBody) ^^ { case cond ~ ((tb, eb)) => IfExprAST(cond, tb, eb) } |
      "if" ~> logicalOr ~ block ~ opt(Newline ~> elseOrElif) ^^ {
        case cond ~ body ~ elseBody => IfExprAST(cond, body, elseBody)
      }) <~ opt(endMarker("if"))

  lazy val elifExpr: Parser[IfExprAST] =
    "elif" ~> logicalOr ~ ("then" ~> thenBody) ^^ { case cond ~ ((tb, eb)) => IfExprAST(cond, tb, eb) } |
      "elif" ~> logicalOr ~ block ~ opt(Newline ~> elseOrElif) ^^ {
        case cond ~ body ~ elseBody => IfExprAST(cond, body, elseBody)
      }

  lazy val thenBody: Parser[(List[StmtAST], Option[List[StmtAST]])] =
    block ~ opt(Newline ~> elseOrElif) ^^ { case body ~ eb => (body, eb) } |
      inlineStmt ~ opt(elseInline) ^^ { case s ~ eb => (List(s), eb) }

  lazy val elseInline: Parser[List[StmtAST]] =
    "else" ~> (ifExpr ^^ (e => List(ExprStmtAST(e))) | inlineStmt ^^ (s => List(s))) |
      Newline ~> elseOrElif

  lazy val elseOrElif: Parser[List[StmtAST]] =
    elifExpr ^^ (e => List(ExprStmtAST(e))) |
      "else" ~> (
        ifExpr ^^ (e => List(ExprStmtAST(e))) |
          block |
          inlineStmt ^^ (s => List(s))
      )

  lazy val inlineStmt: Parser[StmtAST] =
    breakStmt | continueStmt | returnStmt | derefAssignStmt |
      ident ~ lvalueChain ~ compoundOp ~ expr ^^ { case name ~ chain ~ op ~ value =>
        buildCompoundAssign(name, chain, op.init, wrapPlaceholders(value))
      } |
      ident ~ lvalueChain ~ ("=" ~> expr) ^^ { case name ~ chain ~ value =>
        buildAssign(name, chain, wrapPlaceholders(value))
      } |
      expr ^^ (e => ExprStmtAST(wrapPlaceholders(e)))

  // --- Precedence climbing ---
  //
  // Each level accepts its built-in operators *and* a user-op slot —
  // `userBinOp(firstChars)` matches any Keyword whose chars start with one
  // of the level's first chars and isn't a reserved/syntactic sigil. This
  // lets user-defined operators (e.g. `<>`, `>>>`, `|>`, `~~`) slot into
  // the precedence ladder via Scala-style first-char convention without
  // needing per-operator declarations. The analyzer dispatches the user
  // op via `lookupBinaryOperatorTrait` (Stage C).
  //
  // Compound assignments (`+=` etc.) and arrows (`->`, `=>`) are reserved
  // and never match an expression-level user op.

  lazy val logicalOr: Parser[ExpressionAST] =
    logicalAnd ~ rep("||" ~> logicalAnd) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "||", r))
    }

  lazy val logicalAnd: Parser[ExpressionAST] =
    comparison ~ rep("&&" ~> comparison) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "&&", r))
    }

  lazy val comparisonOp: Parser[String] =
    "==" | "!=" | "<=" | ">=" | "<" | ">" | userBinOp(comparisonFirstChars)

  lazy val comparison: Parser[ExpressionAST] =
    bitwiseOr ~ (opt("!") <~ "in") ~ bitwiseOr ~ (("..<" | "..") ~ bitwiseOr) ^^ {
      case x ~ neg ~ lo ~ (op ~ hi) =>
        val hiOp = if op == "..<" then "<" else "<="
        val inExpr = BinaryAST(BinaryAST(x, ">=", lo), "&&", BinaryAST(x, hiOp, hi))
        if neg.isDefined then UnaryAST("!", inExpr) else inExpr
    } |
      bitwiseOr ~ rep(comparisonOp ~ bitwiseOr) ^^ {
        case first ~ Nil => first
        case first ~ chain =>
          val operands = first :: chain.map { case _ ~ operand => operand }
          val ops = chain.map { case op ~ _ => op }
          val pairs = for i <- ops.indices yield
            BinaryAST(operands(i), ops(i), operands(i + 1))
          pairs.reduceLeft((l, r) => BinaryAST(l, "&&", r))
      }

  lazy val bitwiseOr: Parser[ExpressionAST] =
    bitwiseXor ~ rep(("|" | userBinOp(Set('|'))) ~ bitwiseXor) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val bitwiseXor: Parser[ExpressionAST] =
    bitwiseAnd ~ rep(("^" | "~" | userBinOp(Set('^', '~'))) ~ bitwiseAnd) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val bitwiseAnd: Parser[ExpressionAST] =
    shift ~ rep(("&" | userBinOp(Set('&'))) ~ shift) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val shift: Parser[ExpressionAST] =
    additive ~ rep(("<<" | ">>") ~ additive) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val additive: Parser[ExpressionAST] =
    multiplicative ~ rep(("+" | "-" | userBinOp(Set('+', '-'))) ~ multiplicative) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val multiplicative: Parser[ExpressionAST] =
    unary ~ rep(("*" | "/" | "%" | userBinOp(Set('*', '/', '%'))) ~ unary) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  // ----- User-defined binary operator slots -----

  // Operator strings that the language reserves and which therefore must
  // never be picked up as a user-defined binary op. Includes:
  //  - all built-in arithmetic / comparison / bitwise / shift / logical ops
  //  - `++` `--` (prefix/postfix inc/dec)
  //  - `=` and the compound assignments (statement-level)
  //  - `->` `=>` (function arrow, match-arm arrow)
  //  - `..` `..<` (range)
  //  - `!` (unary not — overloadable later if/when prefix support lands)
  private val reservedOps: Set[String] = Set(
    "+", "-", "*", "/", "%",
    "<<", ">>",
    "==", "!=", "<=", ">=", "<", ">",
    "&&", "||", "!", "&", "|", "^", "~",
    "++", "--",
    "=", "+=", "-=", "*=", "/=", "%=",
    "&=", "|=", "^=", "<<=", ">>=",
    "->", "=>",
    "..", "..<",
  )

  // Comparison level (level 4) — first chars of comparison user ops.
  // Built-in shifts (`<<` `>>`) and shift-assigns (`<<=` `>>=`) are excluded
  // via `reservedOps`; user ops like `<<<` / `>>>` slot here (Scala-style:
  // first char rules).
  private val comparisonFirstChars: Set[Char] = Set('<', '>', '=', '!')

  // Match any Keyword whose chars start with one of `firstChars`, and
  // which isn't reserved. Used by each precedence level above to admit
  // user-defined operators slotted by first-char convention.
  private def userBinOp(firstChars: Set[Char]): Parser[String] =
    acceptMatch(s"user binary operator", {
      case k: lexical.Keyword
          if k.chars.nonEmpty
            && firstChars.contains(k.chars.head)
            && !reservedOps.contains(k.chars) =>
        k.chars
    })

  // Match any Keyword that could be a user-defined prefix operator. Two
  // filters keep this from grabbing every Keyword in sight (parens, `then`,
  // `false`, …): (a) the chars must be made entirely of operator chars
  // (the same set the lexer's `operatorMuncher` consumes), and (b) the
  // string must not be reserved by the grammar (built-in prefix sigils
  // `-`, `!`, `~`, `*`, `&`, `++`, `--`, every entry in `reservedOps`).
  // Whatever survives is a candidate for the analyzer, which makes the
  // final call (registered via `#operator` vs. error).
  private val builtinPrefixOps: Set[String] =
    Set("-", "!", "~", "*", "&", "++", "--")

  private val opChars: Set[Char] =
    Set('+', '-', '*', '/', '%', '<', '>', '=', '!', '&', '|', '^', '~')

  private def userPrefixOp: Parser[String] =
    acceptMatch("user prefix operator", {
      case k: lexical.Keyword
          if k.chars.nonEmpty
            && k.chars.forall(opChars.contains)
            && !reservedOps.contains(k.chars)
            && !builtinPrefixOps.contains(k.chars) =>
        k.chars
    })

  lazy val unary: Parser[ExpressionAST] =
    "++" ~> ident ~ ("." ~> ident) ^^ { case obj ~ field => FieldPreIncAST(VarRefAST(obj), field) } |
      "--" ~> ident ~ ("." ~> ident) ^^ { case obj ~ field => FieldPreDecAST(VarRefAST(obj), field) } |
      "++" ~> ident ^^ PreIncAST.apply |
      "--" ~> ident ^^ PreDecAST.apply |
      "-" ~> unary ^^ (e => UnaryAST("-", e)) |
      "!" ~> unary ^^ (e => UnaryAST("!", e)) |
      "~" ~> unary ^^ (e => UnaryAST("~", e)) |
      "*" ~> (scalarCastType | ident) ~ ("(" ~> expr <~ ")") ^^ { case t ~ e => CastAST(PtrTypeAST(NamedTypeAST(t)), e) } |
      "*" ~> unary ^^ DerefAST.apply |
      "&" ~> ident ~ rep1("." ~> ident) ^^ { case name ~ fields =>
        val base: ExpressionAST = VarRefAST(name)
        val chain = fields.init.foldLeft(base)((e, f) => FieldAccessAST(e, f))
        AddrOfFieldAST(chain, fields.last)
      } |
      "&" ~> ident ~ rep1("[" ~> expr <~ "]") ^^ { case name ~ idxs =>
        val base: ExpressionAST = VarRefAST(name)
        val indexed = idxs.init.foldLeft(base)((e, idx) => IndexAST(e, idx))
        AddrOfIndexAST(indexed, idxs.last)
      } |
      "&" ~> ident ^^ AddrOfAST.apply |
      userPrefixOp ~ unary ^^ { case op ~ e => UnaryAST(op, e) } |
      postfix

  lazy val postfix: Parser[ExpressionAST] =
    ident ~ ("." ~> ident) <~ "++" ^^ { case obj ~ field => FieldPostIncAST(VarRefAST(obj), field) } |
      ident ~ ("." ~> ident) <~ "--" ^^ { case obj ~ field => FieldPostDecAST(VarRefAST(obj), field) } |
      ident <~ "++" ^^ PostIncAST.apply |
      ident <~ "--" ^^ PostDecAST.apply |
      primary ~ rep(
        ("[" ~> (
          ":" ~> opt(expr) ^^ (hi => (4, null, "", List(null, hi.orNull): List[ExpressionAST])) |
          // Function type as a generic type-arg, only inside `[ ]` (so it doesn't
          // collide with closures / match patterns / paren expressions elsewhere):
          //   Parser[(int, int) -> int]   chainl1[A](op: Parser[(A, A) -> A])
          // Tried BEFORE the expr alternative below — funcTypeRef requires `(... ) -> typeRef`
          // and backtracks cleanly when the bracket holds an indexing expression instead.
          funcTypeRef ^^ (t => (0, TypeRefExprAST(t), "", Nil: List[ExpressionAST])) |
          expr ~ opt(":" ~> opt(expr)) ^^ {
            case e ~ None => (0, e, "", Nil: List[ExpressionAST])
            case lo ~ Some(hi) => (4, null, "", List(lo, hi.orNull): List[ExpressionAST])
          }
        ) <~ "]") |
        ("." ~> ident) ~ ("(" ~> repsep(callArg, ",") <~ ")") ^^ { case m ~ args => (2, null, m, args) } |
        ("." ~> numericLit) ^^ (n => (1, null, s"_${n.toInt}", Nil)) |
        ("." ~> ident) ^^ (f => (1, null, f, Nil)) |
        ("(" ~> repsep(callArg, ",") <~ ")") ^^ (args => (3, null, "", args)) |
        "?" ^^^ ((5, null, "", Nil: List[ExpressionAST]))
      ) ^^ {
        case base ~ ops => ops.foldLeft(base) {
          case (e, (0, idx, _, _)) => IndexAST(e, idx)
          case (e, (1, _, field, _)) => FieldAccessAST(e, field)
          case (e, (2, _, method, args)) => absorbBarePlaceholdersInCall(args, a => MethodCallAST(e, method, a))
          case (e, (3, _, _, args)) =>
            // Indirect call: expr(args) — e is a function pointer
            e match
              case VarRefAST(name) => absorbBarePlaceholdersInCall(args, a => CallAST(name, a))
              case _               => absorbBarePlaceholdersInCall(args, a => IndirectCallAST(e, a))
          case (e, (4, _, _, args)) =>
            SliceExprAST(e, Option(args(0)), Option(args(1)))
          case (e, (5, _, _, _)) => TryAST(e)
          case (e, _) => e // shouldn't happen
        }
      }

  // charLit is no longer needed — char literals are handled in the lexer
  // as NumericLit with :char suffix, parsed in the numericLit branch of primary

  // sizeof argument: try pointer/array/func types first, then bare name
  // A bare name could be a type (struct) or a variable — analyzer decides
  lazy val sizeofArg: Parser[ExpressionAST] =
    "*" ~> typeRef ^^ (t => SizeofTypeAST(PtrTypeAST(t))) |
      "[" ~> numericLit ~ ("]" ~> typeRef) ^^ { case n ~ t => SizeofTypeAST(ArrayTypeAST(n.toInt, t)) } |
      funcTypeRef ^^ SizeofTypeAST.apply |
      "[" ~> "]" ~> typeRef ^^ (t => SizeofTypeAST(SliceTypeAST(t))) |
      ("int" | "uint" | "long" | "ulong" | "short" | "ushort" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "float" | "f32" | "double" | "f64" | "bool" | "string") ^^ (n => SizeofTypeAST(NamedTypeAST(n))) |
      "unit" ^^ (_ => SizeofTypeAST(NamedTypeAST("unit"))) |
      expr ^^ SizeofExprAST.apply

  lazy val scalarCastType: Parser[String] =
    "int" | "uint" | "long" | "ulong" | "short" | "ushort" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "float" | "f32" | "double" | "f64" | "bool"

  lazy val castType: Parser[String] =
    scalarCastType

  lazy val cast: Parser[CastAST] =
    castType ~ ("(" ~> expr <~ ")") ^^ { case t ~ e => CastAST(NamedTypeAST(t), e) }

  lazy val primary: Parser[ExpressionAST] =
    quantifierExpr |
      numericLit ^^ { n =>
      if n.contains(':') then
        val Array(value, suffix) = n.split(':')
        TypedIntLitAST(value.toLong, suffix)
      else if n.contains('.') || n.contains('e') || n.contains('E') then FloatLitAST(n.toDouble)
      else IntLitAST(n.toLong)
    } |
      stringLit ^^ StringLitExprAST.apply |
      "true" ^^^ BoolLitAST(true) |
      "false" ^^^ BoolLitAST(false) |
      // Slice / array types as expressions — used inside [] for generic type args:
      //   Parser[[]A](closure)  or  Parser[[5]int](closure)
      // These must be tried BEFORE the array-literal rule so that `[]A` isn't read as
      // the empty-array literal followed by a stray `A`.
      "[" ~> "]" ~> typeRef ^^ (t => TypeRefExprAST(SliceTypeAST(t))) |
      "[" ~> numericLit ~ ("]" ~> typeRef) ^^ { case n ~ t =>
        TypeRefExprAST(ArrayTypeAST(n.toInt, t))
      } |
      "[" ~> repsep(expr, ",") <~ "]" ^^ (es => ArrayLitAST(es.map(wrapPlaceholders))) |
      "asm" ~> "(" ~> stringLit <~ ")" ^^ AsmExprAST.apply |
      "sizeof" ~> "(" ~> sizeofArg <~ ")" |
      "new" ~> "[" ~> expr ~ ("]" ~> typeRef) ^^ { case size ~ elemType => NewArrayAST(wrapPlaceholders(size), elemType) } |
      "new" ~> ident ~ ("(" ~> repsep(callArg, ",") <~ ")") ^^ { case name ~ args =>
        absorbBarePlaceholdersInCall(args, a => NewExprAST(name, a))
      } |
      "string" ~> "(" ~> rep1sep(callArg, ",") <~ ")" ^^ { args =>
        absorbBarePlaceholdersInCall(args, a => CallAST("string", a))
      } |
      cast |
      ident ~ ("(" ~> repsep(callArg, ",") <~ ")") ^^ { case name ~ args =>
        absorbBarePlaceholdersInCall(args, a => CallAST(name, a))
      } |
      // Type attribute: Type::Attr or Type::Attr(arg). Must come before the bare
      // VarRefAST rule so the `::`-suffix is recognized.
      ident ~ ("::" ~> ident) ~ opt("(" ~> expr <~ ")") ^^ {
        case typeName ~ attr ~ argOpt => TypeAttrAST(typeName, attr, argOpt)
      } |
      // Scalar type keywords as expressions — used inside [] for generic type args: Box[int](42).
      // `string` belongs here for the same reason as the others — `Parser[string](...)` should
      // parse uniformly. The `string ~> "("` string-builder rule earlier in `primary` still
      // wins for the legitimate call form `string(arg, ...)` because it's tried first and
      // succeeds when the `(` is actually present; this fallback only fires when there's no
      // following `(` (i.e., the bare keyword inside a type-arg bracket).
      ("int" | "uint" | "long" | "ulong" | "short" | "ushort" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "float" | "f32" | "double" | "f64" | "bool" | "unit" | "string") ^^ VarRefAST.apply |
      "_" ^^^ UnderscorePlaceholderAST() |
      ident ^^ VarRefAST.apply |
      "(" ~ ")" ^^^ UnitLitAST() |  // `()` — unit value literal
      "(" ~> expr ~ rep("," ~> expr) <~ ")" ^^ {
        case first ~ Nil  => wrapPlaceholders(first)  // (expr) — parens form a placeholder boundary
        case first ~ rest => TupleLitAST((first :: rest).map(wrapPlaceholders))
      }
}
