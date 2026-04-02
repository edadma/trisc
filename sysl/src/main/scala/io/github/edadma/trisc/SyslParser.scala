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

  // --- Program ---

  lazy val program: Parser[ProgramAST] =
    repsep(decl, rep1(Newline)) <~ opt(rep(Newline)) ^^ ProgramAST.apply

  // --- Declarations ---

  lazy val decl: Parser[DeclAST] =
    importDecl | externFuncDecl | structDecl | enumDecl | typeAliasDecl | "private" ~> declBody(true) | declBody(false)

  lazy val structDecl: Parser[StructDeclAST] =
    "struct" ~> ident ~ (Newline ~> Indent ~> rep1sep(structField, rep1(Newline)) <~ opt(Newline) <~ Dedent) ^^ {
      case name ~ fields => StructDeclAST(name, fields)
    }

  lazy val structField: Parser[(String, String)] =
    ident ~ (":" ~> typeRef) ^^ { case name ~ typ => (name, typ) }

  lazy val enumDecl: Parser[EnumDeclAST] =
    "enum" ~> ident ~ (Newline ~> Indent ~> rep1sep(enumMember, rep1(Newline)) <~ opt(Newline) <~ Dedent) ^^ {
      case name ~ members => EnumDeclAST(name, members)
    }

  lazy val enumMember: Parser[(String, Option[Long])] =
    ident ~ ("=" ~> numericLit) ^^ { case name ~ value => (name, Some(value.toLong)) } |
      ident ^^ (name => (name, None))

  lazy val typeAliasDecl: Parser[TypeAliasDeclAST] =
    "type" ~> ident ~ ("=" ~> typeRef) ^^ { case name ~ target => TypeAliasDeclAST(name, target) }

  lazy val importDecl: Parser[ImportDeclAST] =
    "import" ~> rep1sep(ident, ".") ~ opt("." ~> importTail) ^^ {
      case path ~ Some(selectors) => ImportDeclAST(path.mkString("/"), selectors)
      case path ~ None =>
        if path.length < 2 then sys.error(s"import requires selector: use 'import ${path.head}.*' or 'import ${path.head}.name'")
        ImportDeclAST(path.init.mkString("/"), List(NamedImport(path.last)))
    }

  lazy val importTail: Parser[List[ImportSelector]] =
    "*" ^^^ List(WildcardImport) |
      "{" ~> rep1sep(importItem, ",") <~ "}"

  lazy val importItem: Parser[NamedImport] =
    ident ~ ("=>" ~> ident) ^^ { case name ~ alias => NamedImport(name, Some(alias)) } |
      ident ^^ (name => NamedImport(name))

  lazy val externFuncDecl: Parser[ExternFuncDeclAST] =
    "extern" ~> ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ opt("->" ~> typeRef) ^^ {
      case name ~ params ~ rt => ExternFuncDeclAST(name, params, rt)
    }

  private def mutability: Parser[Boolean] =
    "var" ^^^ true | "val" ^^^ false

  def declBody(priv: Boolean): Parser[DeclAST] =
    ident ~ ("." ~> ident) ~ ("(" ~> repsep(param, ",") <~ ")") ~ funRest ^^ {
      case typeName ~ methodName ~ params ~ ((rt, body)) =>
        // Sem.wait(params) -> ret { body } desugars to Sem_wait(self: *Sem, params) -> ret { body }
        val selfParam = ParamAST("self", s"*$typeName")
        FunDeclAST(s"${typeName}_$methodName", selfParam :: params, rt, body, priv)
    } |
    ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ funRest ^^ {
      case name ~ params ~ ((rt, body)) => FunDeclAST(name, params, rt, body, priv)
    } |
      opt(mutability) ~ ident ~ (":" ~> typeExpr) ~ ("=" ~> expr) ^^ {
        case mut ~ name ~ t ~ e => VarDeclAST(name, Some(t), e, priv, mut.getOrElse(true))
      } |
      opt(mutability) ~ ident ~ (":" ~> typeExpr) ^^ {
        case mut ~ name ~ t => VarDeclAST(name, Some(t), ArrayDeclAST(t.drop(1).takeWhile(_.isDigit).toInt, t), priv, mut.getOrElse(true))
      } |
      opt(mutability) ~ ident ~ (":" ~> ident) ~ not("=") ^^ {
        case mut ~ name ~ t ~ _ => VarDeclAST(name, Some(t), UninitDeclAST(t), priv, mut.getOrElse(true))
      } |
      opt(mutability) ~ ident ~ (":" ~> typeRef) ~ ("=" ~> expr) ^^ {
        case mut ~ name ~ t ~ e => VarDeclAST(name, Some(t), e, priv, mut.getOrElse(true))
      } |
      opt(mutability) ~ ident ~ ("=" ~> expr) ^^ {
        case mut ~ name ~ e => VarDeclAST(name, None, e, priv, mut.getOrElse(true))
      }

  lazy val funRest: Parser[(Option[String], FunBodyAST)] =
    "->" ~> typeRef ~ ("=" ~> bodyExprOrBlock) ^^ { case rt ~ body => (Some(rt), body) } |
      "->" ~> typeRef ~ block ^^ { case rt ~ body => (Some(rt), BlockBodyAST(body)) } |
      "=" ~> bodyExprOrBlock ^^ { body => (None, body) } |
      block ^^ { body => (None, BlockBodyAST(body)) }

  lazy val bodyExprOrBlock: Parser[FunBodyAST] =
    Newline ~> Indent ~> stmts <~ opt(Newline) <~ Dedent ^^ (s => BlockBodyAST(s)) |
      expr ^^ ExprBodyAST.apply

  lazy val param: Parser[ParamAST] =
    ident ~ (":" ~> typeRef) ^^ { case name ~ t => ParamAST(name, t) }

  lazy val typeName: Parser[String] =
    "int" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "double" | "f64" | "bool" | "void" | "string" | ident

  // Full type reference: *int, **int, [5]int, []int (slice), func(int)->int, string, int, etc.
  lazy val typeRef: Parser[String] =
    "*" ~> typeRef ^^ (t => s"*$t") |
      "[" ~> "]" ~> typeRef ^^ (t => s"[]$t") |
      "[" ~> numericLit ~ ("]" ~> typeRef) ^^ { case n ~ t => s"[$n]$t" } |
      "(" ~> rep1sep(typeRef, ",") <~ ")" ^^ (ts => s"(${ts.mkString(",")})") |
      funcTypeRef |
      typeName

  lazy val funcTypeRef: Parser[String] =
    "func" ~> "(" ~> repsep(typeRef, ",") ~ (")" ~> "->" ~> typeRef) ^^ {
      case params ~ ret => s"func(${params.mkString(",")})->$ret"
    } |
      "func" ~> "(" ~> repsep(typeRef, ",") <~ ")" ^^ {
        params => s"func(${params.mkString(",")})->void"
      }

  // Array type for uninitialized declarations: [5]int
  lazy val typeExpr: Parser[String] =
    "[" ~> numericLit ~ ("]" ~> typeRef) ^^ { case n ~ t => s"[$n]$t" }

  // --- Block ---

  lazy val block: Parser[List[StmtAST]] =
    Newline ~> Indent ~> stmts <~ opt(Newline) <~ Dedent

  // --- Statements ---

  lazy val stmts: Parser[List[StmtAST]] =
    rep1sep(stmt, rep1(Newline))

  lazy val asmStmt: Parser[AsmStmtAST] =
    "asm" ~> "(" ~> stringLit <~ ")" ^^ AsmStmtAST.apply

  lazy val stmt: Parser[StmtAST] =
    asmStmt | forStmt | doWhileStmt | whileStmt | returnStmt | breakStmt | continueStmt | deferStmt | destructureStmt | derefAssignStmt | identStmt | expr ^^ ExprStmtAST.apply

  lazy val destructureStmt: Parser[DestructureStmtAST] =
    mutability ~ ("(" ~> rep1sep(ident, ",") <~ ")") ~ ("=" ~> expr) ^^ { case mut ~ names ~ init => DestructureStmtAST(names, init, mut) } |
      ("(" ~> rep1sep(ident, ",") <~ ")") ~ ("=" ~> expr) ^^ { case names ~ init => DestructureStmtAST(names, init) }

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

  lazy val identStmt: Parser[StmtAST] =
    mutability ~ ident ~ (":" ~> typeExpr) ~ ("=" ~> expr) ^^ { case mut ~ name ~ t ~ e => VarStmtAST(name, Some(t), e, mut) } |
      mutability ~ ident ~ (":" ~> typeExpr) ^^ { case mut ~ name ~ t => VarStmtAST(name, Some(t), ArrayDeclAST(t.drop(1).takeWhile(_.isDigit).toInt, t), mut) } |
      mutability ~ ident ~ (":" ~> typeRef) ~ ("=" ~> expr) ^^ { case mut ~ name ~ t ~ e => VarStmtAST(name, Some(t), e, mut) } |
      mutability ~ ident ~ ("=" ~> expr) ^^ { case mut ~ name ~ e => VarStmtAST(name, None, e, mut) } |
      ident ~ (":" ~> typeExpr) ~ ("=" ~> expr) ^^ { case name ~ t ~ e => VarStmtAST(name, Some(t), e) } |
      ident ~ (":" ~> typeExpr) ^^ { case name ~ t => VarStmtAST(name, Some(t), ArrayDeclAST(t.drop(1).takeWhile(_.isDigit).toInt, t)) } |
      ident ~ (":" ~> ident) ~ not("=") ^^ { case name ~ t ~ _ => VarStmtAST(name, Some(t), UninitDeclAST(t)) } |
      ident ~ (":" ~> typeRef) ~ ("=" ~> expr) ^^ { case name ~ t ~ e => VarStmtAST(name, Some(t), e) } |
      ident ~ lvalueChain ~ compoundOp ~ expr ^^ { case name ~ chain ~ op ~ value =>
        buildCompoundAssign(name, chain, op.init, value)
      } |
      ident ~ lvalueChain ~ ("=" ~> expr) ^^ { case name ~ chain ~ value =>
        buildAssign(name, chain, value)
      }

  lazy val derefAssignStmt: Parser[StmtAST] =
    "*" ~> unary ~ ("=" ~> expr) ^^ { case ptr ~ value => DerefAssignStmtAST(ptr, value) }

  lazy val forStmt: Parser[ForStmtAST] =
    "for" ~> identStmt ~ (";" ~> expr) ~ (";" ~> forUpdate) ~ ("do" ~> (block | inlineStmt ^^ (s => List(s)))) ^^ {
      case init ~ cond ~ update ~ body => ForStmtAST(init, cond, update, body)
    } |
      "for" ~> identStmt ~ (";" ~> expr) ~ (";" ~> forUpdate) ~ block ^^ {
        case init ~ cond ~ update ~ body => ForStmtAST(init, cond, update, body)
      }

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
    "return" ~> opt(expr) ^^ ReturnStmtAST.apply

  // --- Expressions ---

  lazy val expr: Parser[ExpressionAST] = ifExpr | logicalOr

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
    breakStmt | continueStmt | returnStmt |
      "*" ~> unary ~ ("=" ~> expr) ^^ { case ptr ~ value => DerefAssignStmtAST(ptr, value) } |
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
    bitwiseAnd ~ rep("^" ~> bitwiseAnd) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "^", r))
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
      "*" ~> scalarCastType ~ ("(" ~> expr <~ ")") ^^ { case t ~ e => CastAST(s"*$t", e) } |
      "*" ~> unary ^^ DerefAST.apply |
      "&" ~> ident ~ rep1("." ~> ident) ^^ { case name ~ fields =>
        val base: ExpressionAST = VarRefAST(name)
        val chain = fields.init.foldLeft(base)((e, f) => FieldAccessAST(e, f))
        AddrOfFieldAST(chain, fields.last)
      } |
      "&" ~> ident ~ ("[" ~> expr <~ "]") ^^ { case name ~ idx => AddrOfIndexAST(VarRefAST(name), idx) } |
      "&" ~> ident ^^ AddrOfAST.apply |
      postfix

  lazy val postfix: Parser[ExpressionAST] =
    ident ~ ("." ~> ident) <~ "++" ^^ { case obj ~ field => FieldPostIncAST(VarRefAST(obj), field) } |
      ident ~ ("." ~> ident) <~ "--" ^^ { case obj ~ field => FieldPostDecAST(VarRefAST(obj), field) } |
      ident <~ "++" ^^ PostIncAST.apply |
      ident <~ "--" ^^ PostDecAST.apply |
      primary ~ rep(
        ("[" ~> expr <~ "]") ^^ (idx => (0, idx, "", Nil: List[ExpressionAST])) |
        ("." ~> ident) ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case m ~ args => (2, null, m, args) } |
        ("." ~> numericLit) ^^ (n => (1, null, s"_${n.toInt}", Nil)) |
        ("." ~> ident) ^^ (f => (1, null, f, Nil)) |
        ("(" ~> repsep(expr, ",") <~ ")") ^^ (args => (3, null, "", args))
      ) ^^ {
        case base ~ ops => ops.foldLeft(base) {
          case (e, (0, idx, _, _)) => IndexAST(e, idx)
          case (e, (1, _, field, _)) => FieldAccessAST(e, field)
          case (e, (2, _, method, args)) => MethodCallAST(e, method, args)
          case (e, (3, _, _, args)) =>
            // Indirect call: expr(args) — e is a function pointer
            e match
              case VarRefAST(name) => CallAST(name, args)
              case _ => MethodCallAST(e, "", args) // TODO: indirect call on arbitrary expression
          case (e, _) => e // shouldn't happen
        }
      }

  // charLit is no longer needed — char literals are handled in the lexer
  // as NumericLit with :char suffix, parsed in the numericLit branch of primary

  // sizeof argument: try pointer/array/func types first, then bare name
  // A bare name could be a type (struct) or a variable — analyzer decides
  lazy val sizeofArg: Parser[ExpressionAST] =
    "*" ~> typeRef ^^ (t => SizeofTypeAST(s"*$t")) |
      "[" ~> numericLit ~ ("]" ~> typeRef) ^^ { case n ~ t => SizeofTypeAST(s"[$n]$t") } |
      funcTypeRef ^^ SizeofTypeAST.apply |
      "[" ~> "]" ~> typeRef ^^ (t => SizeofTypeAST(s"[]$t")) |
      ("int" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "double" | "f64" | "bool" | "void" | "string") ^^ SizeofTypeAST.apply |
      expr ^^ SizeofExprAST.apply

  lazy val scalarCastType: Parser[String] =
    "int" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "double" | "f64" | "bool"

  lazy val castType: Parser[String] =
    scalarCastType

  lazy val cast: Parser[CastAST] =
    castType ~ ("(" ~> expr <~ ")") ^^ { case t ~ e => CastAST(t, e) }

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
      "[" ~> rep1sep(expr, ",") <~ "]" ^^ ArrayLitAST.apply |
      "sizeof" ~> "(" ~> sizeofArg <~ ")" |
      cast |
      ident ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case name ~ args => CallAST(name, args) } |
      ident ^^ VarRefAST.apply |
      "(" ~> expr ~ rep("," ~> expr) <~ ")" ^^ {
        case first ~ Nil => first  // (expr) — parenthesized expression
        case first ~ rest => TupleLitAST(first :: rest)  // (expr, expr, ...) — tuple
      }
}
