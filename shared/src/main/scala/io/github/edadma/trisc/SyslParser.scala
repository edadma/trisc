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
    importDecl | structDecl | "private" ~> declBody(true) | declBody(false)

  lazy val structDecl: Parser[StructDeclAST] =
    "struct" ~> ident ~ (Newline ~> Indent ~> rep1sep(structField, rep1(Newline)) <~ opt(Newline) <~ Dedent) ^^ {
      case name ~ fields => StructDeclAST(name, fields)
    }

  lazy val structField: Parser[(String, String)] =
    ident ~ (":" ~> typeRef) ^^ { case name ~ typ => (name, typ) }

  lazy val importDecl: Parser[ImportDeclAST] =
    "import" ~> stringLit ^^ ImportDeclAST.apply

  private def mutability: Parser[Boolean] =
    "var" ^^^ true | "val" ^^^ false

  def declBody(priv: Boolean): Parser[DeclAST] =
    ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ funRest ^^ {
      case name ~ params ~ ((rt, body)) => FunDeclAST(name, params, rt, body, priv)
    } |
      opt(mutability) ~ ident ~ (":" ~> typeExpr) ^^ {
        case mut ~ name ~ t => VarDeclAST(name, Some(t), ArrayDeclAST(t.drop(1).takeWhile(_.isDigit).toInt, t), priv, mut.getOrElse(true))
      } |
      opt(mutability) ~ ident ~ (":" ~> ident) ~ not("=") ^^ {
        case mut ~ name ~ t ~ _ => VarDeclAST(name, Some(t), StructInitAST(t), priv, mut.getOrElse(true))
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
    "int" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "bool" | "void" | ident

  // Full type reference: *int, **int, [5]int, func(int)->int, int, etc.
  lazy val typeRef: Parser[String] =
    "*" ~> typeRef ^^ (t => s"*$t") |
      "[" ~> numericLit ~ ("]" ~> typeRef) ^^ { case n ~ t => s"[$n]$t" } |
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
    asmStmt | forStmt | doWhileStmt | whileStmt | returnStmt | breakStmt | continueStmt | derefAssignStmt | identStmt | expr ^^ ExprStmtAST.apply

  lazy val breakStmt: Parser[BreakStmtAST] =
    "break" ^^^ BreakStmtAST()

  lazy val continueStmt: Parser[ContinueStmtAST] =
    "continue" ^^^ ContinueStmtAST()

  lazy val compoundOp: Parser[String] =
    "<<=" | ">>=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="

  lazy val identStmt: Parser[StmtAST] =
    mutability ~ ident ~ (":" ~> typeExpr) ^^ { case mut ~ name ~ t => VarStmtAST(name, Some(t), ArrayDeclAST(t.drop(1).takeWhile(_.isDigit).toInt, t), mut) } |
      mutability ~ ident ~ (":" ~> typeRef) ~ ("=" ~> expr) ^^ { case mut ~ name ~ t ~ e => VarStmtAST(name, Some(t), e, mut) } |
      mutability ~ ident ~ ("=" ~> expr) ^^ { case mut ~ name ~ e => VarStmtAST(name, None, e, mut) } |
      ident ~ (":" ~> typeExpr) ^^ { case name ~ t => VarStmtAST(name, Some(t), ArrayDeclAST(t.drop(1).takeWhile(_.isDigit).toInt, t)) } |
      ident ~ (":" ~> ident) ~ not("=") ^^ { case name ~ t ~ _ => VarStmtAST(name, Some(t), StructInitAST(t)) } |
      ident ~ (":" ~> typeRef) ~ ("=" ~> expr) ^^ { case name ~ t ~ e => VarStmtAST(name, Some(t), e) } |
      ident ~ ("[" ~> expr <~ "]") ~ ("=" ~> expr) ^^ { case name ~ idx ~ value =>
        IndexAssignStmtAST(VarRefAST(name), idx, value)
      } |
      ident ~ ("." ~> ident) ~ compoundOp ~ expr ^^ { case obj ~ field ~ op ~ value =>
        FieldCompoundAssignStmtAST(VarRefAST(obj), field, op.init, value)
      } |
      ident ~ ("." ~> ident) ~ ("=" ~> expr) ^^ { case obj ~ field ~ value =>
        FieldAssignStmtAST(VarRefAST(obj), field, value)
      } |
      ident ~ compoundOp ~ expr ^^ { case name ~ op ~ e => CompoundAssignStmtAST(name, op.init, e) } |
      ident ~ ("=" ~> expr) ^^ { case name ~ e => AssignStmtAST(name, e) }

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
      ident ~ ("[" ~> expr <~ "]") ~ ("=" ~> expr) ^^ { case name ~ idx ~ value =>
        IndexAssignStmtAST(VarRefAST(name), idx, value)
      } |
      ident ~ compoundOp ~ expr ^^ { case name ~ op ~ e => CompoundAssignStmtAST(name, op.init, e) } |
      ident ~ ("=" ~> expr) ^^ { case name ~ e => AssignStmtAST(name, e) } |
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
      "*" ~> unary ^^ DerefAST.apply |
      "&" ~> ident ~ ("[" ~> expr <~ "]") ^^ { case name ~ idx => AddrOfIndexAST(VarRefAST(name), idx) } |
      "&" ~> ident ^^ AddrOfAST.apply |
      postfix

  lazy val postfix: Parser[ExpressionAST] =
    ident ~ ("." ~> ident) <~ "++" ^^ { case obj ~ field => FieldPostIncAST(VarRefAST(obj), field) } |
      ident ~ ("." ~> ident) <~ "--" ^^ { case obj ~ field => FieldPostDecAST(VarRefAST(obj), field) } |
      ident <~ "++" ^^ PostIncAST.apply |
      ident <~ "--" ^^ PostDecAST.apply |
      primary ~ rep(("[" ~> expr <~ "]") ^^ (idx => Left(idx)) | ("." ~> ident) ^^ (f => Right(f))) ^^ {
        case base ~ ops => ops.foldLeft(base) {
          case (e, Left(idx)) => IndexAST(e, idx)
          case (e, Right(field)) => FieldAccessAST(e, field)
        }
      }

  lazy val charLit: Parser[ExpressionAST] =
    stringLit ^? ({
      case s if s.length == 1 => IntLitAST(s.charAt(0).toLong)
      case s if s.length == 2 && s.charAt(0) == '\\' => IntLitAST(s.charAt(1) match
        case 'n' => '\n'.toLong
        case 't' => '\t'.toLong
        case 'r' => '\r'.toLong
        case '0' => 0L
        case '\\' => '\\'.toLong
        case '\'' => '\''.toLong
        case '"' => '"'.toLong
        case c => c.toLong
      )
    }, s => s"invalid char literal: '$s'")

  // sizeof argument: try pointer/array/func types first, then bare name
  // A bare name could be a type (struct) or a variable — analyzer decides
  lazy val sizeofArg: Parser[ExpressionAST] =
    "*" ~> typeRef ^^ (t => SizeofTypeAST(s"*$t")) |
      "[" ~> numericLit ~ ("]" ~> typeRef) ^^ { case n ~ t => SizeofTypeAST(s"[$n]$t") } |
      funcTypeRef ^^ SizeofTypeAST.apply |
      ("int" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "bool" | "void") ^^ SizeofTypeAST.apply |
      expr ^^ SizeofExprAST.apply

  lazy val castType: Parser[String] =
    "int" | "char" | "byte" | "i8" | "i16" | "i32" | "i64" | "bool"

  lazy val cast: Parser[CastAST] =
    castType ~ ("(" ~> expr <~ ")") ^^ { case t ~ e => CastAST(t, e) }

  lazy val primary: Parser[ExpressionAST] =
    numericLit ^^ (n => IntLitAST(n.toLong)) |
      charLit |
      stringLit ^^ StringLitExprAST.apply |
      "true" ^^^ BoolLitAST(true) |
      "false" ^^^ BoolLitAST(false) |
      "sizeof" ~> "(" ~> sizeofArg <~ ")" |
      cast |
      ident ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case name ~ args => CallAST(name, args) } |
      ident ^^ VarRefAST.apply |
      "(" ~> expr <~ ")"
}
