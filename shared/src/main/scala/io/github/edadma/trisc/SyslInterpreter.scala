package io.github.edadma.trisc

import scala.collection.mutable

class Cell(var value: Long)

class SyslInterpreter(output: String => Unit = s => print(s)):
  case class ReturnException(value: Long) extends RuntimeException
  case class RuntimeError(msg: String) extends RuntimeException(msg)

  private type Env = mutable.LinkedHashMap[String, Cell]

  // Pointer values encode the Cell reference as an identity hash
  private val pointerMap = new mutable.HashMap[Long, Cell]
  private var nextPtr = 1L

  private def allocPtr(cell: Cell): Long =
    val id = nextPtr
    nextPtr += 1
    pointerMap(id) = cell
    id

  private def derefPtr(id: Long): Cell =
    pointerMap.getOrElse(id, throw RuntimeError(s"invalid pointer: $id"))

  private val globals: Env = new mutable.LinkedHashMap
  private val functions = new mutable.LinkedHashMap[String, FunDeclAST]

  private val builtins: Map[String, List[Long] => Long] = Map(
    "putchar" -> (args => { output(args.head.toChar.toString); args.head }),
    "print" -> (args => { args.foreach(a => output(a.toString)); 0 }),
    "println" -> (args => { args.foreach(a => output(a.toString)); output("\n"); 0 }),
  )

  def run(program: ProgramAST): Long =
    for decl <- program.decls do
      decl match
        case f: FunDeclAST => functions(f.name) = f
        case VarDeclAST(name, _, init) =>
          val cell = new Cell(eval(init, new mutable.LinkedHashMap))
          globals(name) = cell

    functions.get("main") match
      case Some(main) => call(main, Nil)
      case None => throw RuntimeError("no main function")

  private def call(fun: FunDeclAST, args: List[Long]): Long =
    val env: Env = new mutable.LinkedHashMap

    for (param, arg) <- fun.params.zip(args) do
      env(param.name) = new Cell(arg)

    fun.body match
      case ExprBodyAST(expr) => eval(expr, env)
      case BlockBodyAST(stmts) =>
        try
          if stmts.nonEmpty then
            execBlock(stmts.init, env)
            stmts.last match
              case ExprStmtAST(expr) => eval(expr, env)
              case ReturnStmtAST(value) => value.map(eval(_, env)).getOrElse(0)
              case other => exec(other, env); 0
          else 0
        catch
          case ReturnException(v) => v

  private def execBlock(stmts: List[StmtAST], env: Env): Unit =
    for stmt <- stmts do exec(stmt, env)

  private def lookupCell(name: String, env: Env): Cell =
    env.getOrElse(name, globals.getOrElse(name, throw RuntimeError(s"undefined variable: $name")))

  private def exec(stmt: StmtAST, env: Env): Unit =
    stmt match
      case VarStmtAST(name, _, init) =>
        env(name) = new Cell(eval(init, env))

      case AssignStmtAST(target, value) =>
        val v = eval(value, env)
        if env.contains(target) then env(target).value = v
        else if globals.contains(target) then globals(target).value = v
        else env(target) = new Cell(v) // first assignment = declaration

      case DerefAssignStmtAST(pointer, value) =>
        val ptr = eval(pointer, env)
        val v = eval(value, env)
        derefPtr(ptr).value = v

      case ReturnStmtAST(value) =>
        throw ReturnException(value.map(e => eval(e, env)).getOrElse(0))

      case WhileStmtAST(cond, body) =>
        while eval(cond, env) != 0 do execBlock(body, env)

      case ExprStmtAST(expr) =>
        eval(expr, env)

  private def eval(expr: ExpressionAST, env: Env): Long =
    expr match
      case IntLitAST(n) => n
      case CharLitAST(c) => c.toLong
      case BoolLitAST(b) => if b then 1 else 0
      case StringLitAST(_) => throw RuntimeError("string values not yet supported in expressions")

      case VarRefAST(name) => lookupCell(name, env).value

      case AddrOfAST(name) =>
        val cell = lookupCell(name, env)
        allocPtr(cell)

      case DerefAST(expr) =>
        val ptr = eval(expr, env)
        derefPtr(ptr).value

      case IfExprAST(cond, thenBody, elseBody) =>
        if eval(cond, env) != 0 then
          if thenBody.nonEmpty then
            execBlock(thenBody.init, env)
            thenBody.last match
              case ExprStmtAST(e) => eval(e, env)
              case ReturnStmtAST(v) => throw ReturnException(v.map(eval(_, env)).getOrElse(0))
              case other => exec(other, env); 0
          else 0
        else
          elseBody match
            case Some(stmts) if stmts.nonEmpty =>
              execBlock(stmts.init, env)
              stmts.last match
                case ExprStmtAST(e) => eval(e, env)
                case ReturnStmtAST(v) => throw ReturnException(v.map(eval(_, env)).getOrElse(0))
                case other => exec(other, env); 0
            case _ => 0

      case BinaryAST(left, op, right) =>
        val l = eval(left, env)
        op match
          case "&&" => if l == 0 then 0 else if eval(right, env) != 0 then 1 else 0
          case "||" => if l != 0 then 1 else if eval(right, env) != 0 then 1 else 0
          case _ =>
            val r = eval(right, env)
            op match
              case "+"  => l + r
              case "-"  => l - r
              case "*"  => l * r
              case "/"  => if r == 0 then throw RuntimeError("division by zero") else l / r
              case "%"  => if r == 0 then throw RuntimeError("modulo by zero") else l % r
              case "==" => if l == r then 1 else 0
              case "!=" => if l != r then 1 else 0
              case "<"  => if l < r then 1 else 0
              case ">"  => if l > r then 1 else 0
              case "<=" => if l <= r then 1 else 0
              case ">=" => if l >= r then 1 else 0
              case _    => throw RuntimeError(s"unknown operator: $op")

      case UnaryAST(op, operand) =>
        val v = eval(operand, env)
        op match
          case "-" => -v
          case "!" => if v == 0 then 1 else 0
          case _   => throw RuntimeError(s"unknown unary operator: $op")

      case CallAST(name, args) =>
        val argValues = args.map(eval(_, env))
        builtins.get(name) match
          case Some(f) => f(argValues)
          case None =>
            functions.get(name) match
              case Some(fun) => call(fun, argValues)
              case None => throw RuntimeError(s"undefined function: $name")
