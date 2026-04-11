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
    condDecl | importDecl | externDecl | structDecl | enumDecl | traitDecl | implDecl | interfaceDecl | typeAliasDecl | "private" ~> "def" ~> defDecl(true) | "private" ~> declBody(true) | "def" ~> defDecl(false) | declBody(false)

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
    "struct" ~> ident ~ typeParamList ~ (Newline ~> Indent ~> rep1sep(structField, rep1(Newline)) <~ opt(Newline) <~ Dedent) ^^ {
      case name ~ tps ~ fields => StructDeclAST(name, fields, tps)
    }

  lazy val structField: Parser[(String, TypeAST)] =
    ident ~ (":" ~> typeRef) ^^ { case name ~ typ => (name, typ) }

  lazy val enumDecl: Parser[DeclAST] =
    "enum" ~> ident ~ typeParamList ~ (Newline ~> Indent ~> rep1sep(enumVariantOrMember, rep1(Newline)) <~ opt(Newline) <~ Dedent) ^^ {
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
    ident ~ ("(" ~> repsep(structField, ",") <~ ")") ^^ { case name ~ fields => Right(EnumVariantAST(name, fields)) } |
      ident ~ ("=" ~> numericLit) ^^ { case name ~ value => Left((name, Some(value.toLong))) } |
      ident ^^ (name => Left((name, None)))

  lazy val typeAliasDecl: Parser[TypeAliasDeclAST] =
    "type" ~> ident ~ opt("[" ~> rep1sep(ident, ",") <~ "]") ~ ("=" ~> typeRef) ^^ {
      case name ~ tparams ~ target => TypeAliasDeclAST(name, target, tparams.getOrElse(Nil))
    }

  lazy val traitDecl: Parser[TraitDeclAST] =
    "trait" ~> ident ~ ("[" ~> ident <~ "]") ~
      (Newline ~> Indent ~> rep1sep(traitMethod, rep1(Newline)) <~ opt(Newline) <~ Dedent) ^^ {
        case name ~ tparam ~ methods => TraitDeclAST(name, tparam, methods)
      }

  lazy val traitMethod: Parser[TraitMethodAST] =
    rep(positioned(attribute) <~ rep1(Newline)) ~ ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ ("->" ~> typeRef) ~ opt(traitMethodBody) ^^ {
      case attrs ~ name ~ params ~ rt ~ body => TraitMethodAST(name, params, rt, body, attrs)
    }

  lazy val traitMethodBody: Parser[FunBodyAST] =
    "=" ~> bodyExprOrBlock |
      block ^^ (stmts => BlockBodyAST(stmts))

  lazy val implDecl: Parser[ImplDeclAST] =
    "impl" ~> ident ~ ("[" ~> typeRef <~ "]") ~
      (Newline ~> Indent ~> rep1sep(implMethod, rep1(Newline)) <~ opt(Newline) <~ Dedent) ^^ {
        case name ~ typ ~ methods => ImplDeclAST(name, typ, methods)
      }

  lazy val implMethod: Parser[FunDeclAST] =
    ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ funRest ^^ {
      case name ~ params ~ ((rt, body)) => FunDeclAST(name, params, rt, body)
    }

  lazy val interfaceDecl: Parser[InterfaceDeclAST] =
    "interface" ~> ident ~
      (Newline ~> Indent ~> rep1sep(interfaceMember, rep1(Newline)) <~ opt(Newline) <~ Dedent) ^^ {
        case name ~ members =>
          val methods = members.collect { case Right(m) => m }
          val embedded = members.collect { case Left(n) => n }
          InterfaceDeclAST(name, methods, embedded)
      }

  lazy val interfaceMember: Parser[Either[String, InterfaceMethodAST]] =
    ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ opt("->" ~> typeRef) ^^ {
      case name ~ params ~ rt => Right(InterfaceMethodAST(name, params, rt.getOrElse(NamedTypeAST("void"))))
    } |
    ident ^^ (name => Left(name))


  // Accept identifiers and type keywords (e.g., "string") in import paths
  private lazy val importIdent: Parser[String] =
    ident | "int" | "char" | "byte" | "bool" | "unit" | "string" |
      "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "double" | "f64"

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

  private def mutability: Parser[Boolean] =
    "var" ^^^ true | "val" ^^^ false

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
      opt(mutability) ~ ident ~ (":" ~> typeExpr) ~ ("=" ~> expr) ^^ {
        case mut ~ name ~ t ~ e => VarDeclAST(name, Some(t), e, priv, mut.getOrElse(true))
      } |
      opt(mutability) ~ ident ~ (":" ~> typeExpr) ^^ {
        case mut ~ name ~ t =>
          val size = t match { case ArrayTypeAST(s, _) => s; case _ => 0 }
          VarDeclAST(name, Some(t), ArrayDeclAST(size, t), priv, mut.getOrElse(true))
      } |
      opt(mutability) ~ ident ~ (":" ~> typeRef) ~ not("=") ^^ {
        case mut ~ name ~ t ~ _ =>
          VarDeclAST(name, Some(t), UninitDeclAST(t), priv, mut.getOrElse(true))
      } |
      opt(mutability) ~ ident ~ (":" ~> typeRef) ~ ("=" ~> expr) ^^ {
        case mut ~ name ~ t ~ e => VarDeclAST(name, Some(t), e, priv, mut.getOrElse(true))
      } |
      opt(mutability) ~ ident ~ ("=" ~> expr) ^^ {
        case mut ~ name ~ e => VarDeclAST(name, None, e, priv, mut.getOrElse(true))
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
    ident ~ ("->" ~> typeRef) ~ block ^^ {
      case name ~ rt ~ body => FunDeclAST(name, Nil, Some(rt), BlockBodyAST(body), priv, isDef = true)
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
      "->" ~> typeRef ~ block ^^ { case rt ~ body => (Some(rt), BlockBodyAST(body)) } |
      "=" ~> bodyExprOrBlock ^^ { body => (None, body) } |
      block ^^ { body => (None, BlockBodyAST(body)) }

  lazy val bodyExprOrBlock: Parser[FunBodyAST] =
    Newline ~> Indent ~> stmts <~ opt(Newline) <~ Dedent ^^ (s => BlockBodyAST(s)) |
      tupleExpr ^^ ExprBodyAST.apply

  lazy val param: Parser[ParamAST] =
    ident ~ (":" ~> typeRef) ^^ { case name ~ t => ParamAST(name, t) }

  // Optional type argument list for generic type references: [T], [T, U], or absent
  lazy val typeArgList: Parser[List[TypeAST]] =
    opt("[" ~> rep1sep(typeRef, ",") <~ "]") ^^ (_.getOrElse(Nil))

  lazy val typeName: Parser[TypeAST] =
    ("int" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "double" | "f64" | "bool" | "string") ^^ (n => NamedTypeAST(n)) |
      "unit" ^^^ NamedTypeAST("void") |
      ident ~ typeArgList ^^ { case name ~ args => NamedTypeAST(name, args) }

  // Full type reference: *int, **int, &Node, [5]int, []int (slice), func(int)->int, string, int, etc.
  lazy val typeRef: Parser[TypeAST] =
    "*" ~> typeRef ^^ PtrTypeAST.apply |
      "&" ~> typeRef ^^ RefTypeAST.apply |
      "[" ~> "]" ~> typeRef ^^ SliceTypeAST.apply |
      "[" ~> numericLit ~ ("]" ~> typeRef) ^^ { case n ~ t => ArrayTypeAST(n.toInt, t) } |
      funcTypeRef |
      "(" ~> rep1sep(typeRef, ",") <~ ")" ^^ TupleTypeAST.apply |
      typeName

  lazy val funcTypeRef: Parser[TypeAST] =
    // (int, int) -> int   or   () -> unit
    "(" ~> repsep(typeRef, ",") ~ (")" ~> "->" ~> typeRef) ^^ {
      case params ~ ret => FuncTypeAST(params, ret)
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

  lazy val stmt: Parser[StmtAST] =
    asmStmt | forStmt | doWhileStmt | whileStmt | returnStmt | breakStmt | continueStmt | deferStmt | destructureStmt | derefAssignStmt | identStmt | expr ^^ ExprStmtAST.apply

  lazy val destructureStmt: Parser[DestructureStmtAST] =
    mutability ~ ("(" ~> rep1sep(bindName, ",") <~ ")") ~ ("=" ~> tupleExpr) ^^ { case mut ~ names ~ init => DestructureStmtAST(names, init, mut) } |
      ("(" ~> rep1sep(bindName, ",") <~ ")") ~ ("=" ~> tupleExpr) ^^ { case names ~ init => DestructureStmtAST(names, init) } |
      mutability ~ bindName ~ ("," ~> rep1sep(bindName, ",")) ~ ("=" ~> tupleExpr) ^^ { case mut ~ first ~ rest ~ init => DestructureStmtAST(first :: rest, init, mut) } |
      "_" ~ ("," ~> rep1sep(bindName, ",")) ~ ("=" ~> tupleExpr) ^^ { case _ ~ rest ~ init => DestructureStmtAST("_" :: rest, init) } |
      ident ~ ("," ~> rep1sep(bindName, ",")) ~ ("=" ~> tupleExpr) ^^ { case first ~ rest ~ init => DestructureStmtAST(first :: rest, init) }

  lazy val breakStmt: Parser[BreakStmtAST] =
    "break" ^^^ BreakStmtAST()

  lazy val continueStmt: Parser[ContinueStmtAST] =
    "continue" ^^^ ContinueStmtAST()

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

  lazy val identStmt: Parser[StmtAST] =
    mutability ~ bindName ~ (":" ~> typeExpr) ~ ("=" ~> tupleExpr) ^^ { case mut ~ name ~ t ~ e => VarStmtAST(name, Some(t), e, mut) } |
      mutability ~ ident ~ (":" ~> typeExpr) ^^ { case mut ~ name ~ t =>
        val size = t match { case ArrayTypeAST(s, _) => s; case _ => 0 }
        VarStmtAST(name, Some(t), ArrayDeclAST(size, t), mut)
      } |
      mutability ~ ident ~ (":" ~> typeRef) ~ not("=") ^^ { case mut ~ name ~ t ~ _ =>
        VarStmtAST(name, Some(t), UninitDeclAST(t), mut)
      } |
      mutability ~ bindName ~ (":" ~> typeRef) ~ ("=" ~> tupleExpr) ^^ { case mut ~ name ~ t ~ e => VarStmtAST(name, Some(t), e, mut) } |
      mutability ~ bindName ~ ("=" ~> tupleExpr) ^^ { case mut ~ name ~ e => VarStmtAST(name, None, e, mut) } |
      ident ~ (":" ~> typeExpr) ~ ("=" ~> tupleExpr) ^^ { case name ~ t ~ e => VarStmtAST(name, Some(t), e) } |
      ident ~ (":" ~> typeExpr) ^^ { case name ~ t =>
        val size = t match { case ArrayTypeAST(s, _) => s; case _ => 0 }
        VarStmtAST(name, Some(t), ArrayDeclAST(size, t))
      } |
      ident ~ (":" ~> typeRef) ~ not("=") ^^ { case name ~ t ~ _ =>
        VarStmtAST(name, Some(t), UninitDeclAST(t))
      } |
      ident ~ (":" ~> typeRef) ~ ("=" ~> tupleExpr) ^^ { case name ~ t ~ e => VarStmtAST(name, Some(t), e) } |
      ident ~ lvalueChain ~ compoundOp ~ expr ^^ { case name ~ chain ~ op ~ value =>
        buildCompoundAssign(name, chain, op.init, value)
      } |
      ident ~ lvalueChain ~ ("=" ~> tupleExpr) ^^ { case name ~ chain ~ value =>
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
    "for" ~> identStmt ~ (";" ~> expr) ~ (";" ~> forUpdate) ~ ("do" ~> (block | inlineStmt ^^ (s => List(s)))) ^^ {
      case init ~ cond ~ update ~ body => ForStmtAST(init, cond, update, body)
    } |
      "for" ~> identStmt ~ (";" ~> expr) ~ (";" ~> forUpdate) ~ block ^^ {
        case init ~ cond ~ update ~ body => ForStmtAST(init, cond, update, body)
      } |
      "for" ~> ident ~ ("," ~> ident) ~ ("in" ~> logicalOr) ~ forBody ^^ {
        case idxName ~ valName ~ arr ~ body => buildForGo(idxName, valName, arr, body)
      } |
      "for" ~> ident ~ ("in" ~> logicalOr) ~ rangeOp ~ logicalOr ~ opt("step" ~> logicalOr) ~ forBody ^^ {
        case name ~ lo ~ op ~ hi ~ step ~ body => buildForRange(name, lo, op, hi, step, body)
      }

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

  private def buildForGo(idxName: String, valName: String, arr: ExpressionAST, body: List[StmtAST]): ForStmtAST =
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
    "while" ~> expr ~ ("do" ~> (block | inlineStmt ^^ (s => List(s)))) ^^ { case cond ~ body => WhileStmtAST(cond, body) } |
      "while" ~> expr ~ block ^^ { case cond ~ body => WhileStmtAST(cond, body) }

  lazy val returnStmt: Parser[ReturnStmtAST] =
    "return" ~> opt(tupleExpr) ^^ ReturnStmtAST.apply

  // Comma-separated expressions form a tuple at statement level (like Go/Python)
  // Inside f(args) and [elems], plain expr is used so commas stay as separators
  lazy val tupleExpr: Parser[ExpressionAST] =
    expr ~ rep1("," ~> expr) ^^ { case first ~ rest => TupleLitAST(first :: rest) } |
      expr

  // --- Expressions ---

  lazy val expr: Parser[ExpressionAST] = closureExpr | matchExpr | ifIsExpr | ifExpr | logicalOr

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

  lazy val closureParam: Parser[ClosureParamAST] =
    ident ~ opt(":" ~> typeRef) ^^ { case name ~ typ => ClosureParamAST(name, typ) }

  lazy val closureBody: Parser[FunBodyAST] =
    block ^^ BlockBodyAST.apply |
    logicalOr ^^ ExprBodyAST.apply

  lazy val matchExpr: Parser[MatchExprAST] =
    logicalOr ~ ("match" ~> Newline ~> Indent ~> rep1(matchArm) ~ opt(matchElse) <~ Dedent) ^^ {
      case scrutinee ~ (arms ~ default) => MatchExprAST(scrutinee, arms, default)
    }

  lazy val matchArm: Parser[MatchArmAST] =
    rep1sep(matchPattern, ",") ~ opt("if" ~> logicalOr) ~ ("->" ~> (block | inlineStmt ^^ (s => List(s)))) <~ opt(Newline) ^^ {
      case patterns ~ guard ~ body => MatchArmAST(patterns, guard, body)
    }

  lazy val matchPattern: Parser[MatchPatternAST] =
    "_" ^^^ WildcardPatternAST |
      ident ~ ("(" ~> repsep(matchPattern, ",") <~ ")") ^^ { case name ~ fields => DestructurePatternAST(name, fields) } |
      logicalOr ~ (".." ~> logicalOr) ^^ { case lo ~ hi => RangePatternAST(lo, hi) } |
      logicalOr ^^ ValuePatternAST.apply

  lazy val matchElse: Parser[List[StmtAST]] =
    "else" ~> "->" ~> (block | inlineStmt ^^ (s => List(s))) <~ opt(Newline)

  lazy val ifExpr: Parser[IfExprAST] =
    "if" ~> logicalOr ~ ("then" ~> thenBody) ^^ { case cond ~ ((tb, eb)) => IfExprAST(cond, tb, eb) } |
      "if" ~> logicalOr ~ block ~ opt(Newline ~> elseOrElif) ^^ {
        case cond ~ body ~ elseBody => IfExprAST(cond, body, elseBody)
      }

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
        buildCompoundAssign(name, chain, op.init, value)
      } |
      ident ~ lvalueChain ~ ("=" ~> expr) ^^ { case name ~ chain ~ value =>
        buildAssign(name, chain, value)
      } |
      expr ^^ ExprStmtAST.apply

  // --- Precedence climbing ---

  lazy val logicalOr: Parser[ExpressionAST] =
    logicalAnd ~ rep("||" ~> logicalAnd) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "||", r))
    }

  lazy val logicalAnd: Parser[ExpressionAST] =
    comparison ~ rep("&&" ~> comparison) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "&&", r))
    }

  lazy val comparisonOp: Parser[String] =
    "==" | "!=" | "<=" | ">=" | "<" | ">"

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
    bitwiseXor ~ rep("|" ~> bitwiseXor) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "|", r))
    }

  lazy val bitwiseXor: Parser[ExpressionAST] =
    bitwiseAnd ~ rep(("^" | "~") ~ bitwiseAnd) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val bitwiseAnd: Parser[ExpressionAST] =
    shift ~ rep("&" ~> shift) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "&", r))
    }

  lazy val shift: Parser[ExpressionAST] =
    additive ~ rep(("<<" | ">>") ~ additive) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val additive: Parser[ExpressionAST] =
    multiplicative ~ rep(("+" | "-") ~ multiplicative) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val multiplicative: Parser[ExpressionAST] =
    unary ~ rep(("*" | "/" | "%") ~ unary) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

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
      postfix

  lazy val postfix: Parser[ExpressionAST] =
    ident ~ ("." ~> ident) <~ "++" ^^ { case obj ~ field => FieldPostIncAST(VarRefAST(obj), field) } |
      ident ~ ("." ~> ident) <~ "--" ^^ { case obj ~ field => FieldPostDecAST(VarRefAST(obj), field) } |
      ident <~ "++" ^^ PostIncAST.apply |
      ident <~ "--" ^^ PostDecAST.apply |
      primary ~ rep(
        ("[" ~> (
          ":" ~> opt(expr) ^^ (hi => (4, null, "", List(null, hi.orNull): List[ExpressionAST])) |
          expr ~ opt(":" ~> opt(expr)) ^^ {
            case e ~ None => (0, e, "", Nil: List[ExpressionAST])
            case lo ~ Some(hi) => (4, null, "", List(lo, hi.orNull): List[ExpressionAST])
          }
        ) <~ "]") |
        ("." ~> ident) ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case m ~ args => (2, null, m, args) } |
        ("." ~> numericLit) ^^ (n => (1, null, s"_${n.toInt}", Nil)) |
        ("." ~> ident) ^^ (f => (1, null, f, Nil)) |
        ("(" ~> repsep(expr, ",") <~ ")") ^^ (args => (3, null, "", args)) |
        "?" ^^^ ((5, null, "", Nil: List[ExpressionAST]))
      ) ^^ {
        case base ~ ops => ops.foldLeft(base) {
          case (e, (0, idx, _, _)) => IndexAST(e, idx)
          case (e, (1, _, field, _)) => FieldAccessAST(e, field)
          case (e, (2, _, method, args)) => MethodCallAST(e, method, args)
          case (e, (3, _, _, args)) =>
            // Indirect call: expr(args) — e is a function pointer
            e match
              case VarRefAST(name) => CallAST(name, args)
              case _ => IndirectCallAST(e, args)
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
      ("int" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "double" | "f64" | "bool" | "string") ^^ (n => SizeofTypeAST(NamedTypeAST(n))) |
      "unit" ^^ (_ => SizeofTypeAST(NamedTypeAST("void"))) |
      expr ^^ SizeofExprAST.apply

  lazy val scalarCastType: Parser[String] =
    "int" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "double" | "f64" | "bool"

  lazy val castType: Parser[String] =
    scalarCastType

  lazy val cast: Parser[CastAST] =
    castType ~ ("(" ~> expr <~ ")") ^^ { case t ~ e => CastAST(NamedTypeAST(t), e) }

  lazy val primary: Parser[ExpressionAST] =
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
      "[" ~> repsep(expr, ",") <~ "]" ^^ ArrayLitAST.apply |
      "asm" ~> "(" ~> stringLit <~ ")" ^^ AsmExprAST.apply |
      "sizeof" ~> "(" ~> sizeofArg <~ ")" |
      "new" ~> "[" ~> expr ~ ("]" ~> typeRef) ^^ { case size ~ elemType => NewArrayAST(size, elemType) } |
      "new" ~> ident ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case name ~ args => NewExprAST(name, args) } |
      "string" ~> "(" ~> rep1sep(expr, ",") <~ ")" ^^ { args => CallAST("string", args) } |
      cast |
      ident ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case name ~ args => CallAST(name, args) } |
      // Scalar type keywords as expressions — used inside [] for generic type args: Box[int](42)
      ("int" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "double" | "f64" | "bool") ^^ VarRefAST.apply |
      ident ^^ VarRefAST.apply |
      "(" ~> expr ~ rep("," ~> expr) <~ ")" ^^ {
        case first ~ Nil => first  // (expr) — parenthesized expression
        case first ~ rest => TupleLitAST(first :: rest)  // (expr, expr, ...) — tuple
      }
}
