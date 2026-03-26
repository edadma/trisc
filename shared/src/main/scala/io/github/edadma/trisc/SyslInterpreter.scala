package io.github.edadma.trisc

import scala.collection.mutable

class Cell(var value: Any)

class SyslInterpreter(output: String => Unit = s => print(s)):
  case class ReturnException(value: Any) extends RuntimeException
  case class RuntimeError(msg: String) extends RuntimeException(msg)

  private type Env = mutable.LinkedHashMap[String, Cell]

  private def toLong(v: Any): Long = v match
    case n: Long => n
    case _       => throw RuntimeError(s"expected integer, got ${v.getClass.getSimpleName}")

  private val globals: Env = new mutable.LinkedHashMap
  private val functions = new mutable.LinkedHashMap[String, FunDeclAST]

  private val builtins: Map[String, List[Any] => Any] = Map(
    "putchar" -> (args => { output(toLong(args.head).toChar.toString); args.head }),
    "print" -> (args => { args.foreach(a => output(toLong(a).toString)); 0L }),
    "println" -> (args => { args.foreach(a => output(toLong(a).toString)); output("\n"); 0L }),
  )

  def run(program: ProgramAST): Long =
    for decl <- program.decls do
      decl match
        case f: FunDeclAST => functions(f.name) = f
        case VarDeclAST(name, _, init) =>
          val cell = new Cell(evalAny(init, new mutable.LinkedHashMap))
          globals(name) = cell

    functions.get("main") match
      case Some(main) => toLong(call(main, Nil))
      case None => throw RuntimeError("no main function")

  private def call(fun: FunDeclAST, args: List[Any]): Any =
    val env: Env = new mutable.LinkedHashMap

    for (param, arg) <- fun.params.zip(args) do
      env(param.name) = new Cell(arg)

    fun.body match
      case ExprBodyAST(expr) => evalAny(expr, env)
      case BlockBodyAST(stmts) =>
        try
          evalBlock(stmts, env)
        catch
          case ReturnException(v) => v

  private def evalBlock(stmts: List[StmtAST], env: Env): Any =
    if stmts.nonEmpty then
      execBlock(stmts.init, env)
      stmts.last match
        case ExprStmtAST(expr) => evalAny(expr, env)
        case other => exec(other, env); 0L
    else 0L

  private def execBlock(stmts: List[StmtAST], env: Env): Unit =
    for stmt <- stmts do exec(stmt, env)

  private def lookupCell(name: String, env: Env): Cell =
    env.getOrElse(name, globals.getOrElse(name, throw RuntimeError(s"undefined variable: $name")))

  private def derefCell(v: Any): Cell = v match
    case c: Cell => c
    case _       => throw RuntimeError("cannot dereference non-pointer")

  private def exec(stmt: StmtAST, env: Env): Unit =
    stmt match
      case VarStmtAST(name, _, init) =>
        env(name) = new Cell(evalAny(init, env))

      case AssignStmtAST(target, value) =>
        val v = evalAny(value, env)
        if env.contains(target) then env(target).value = v
        else if globals.contains(target) then globals(target).value = v
        else env(target) = new Cell(v)

      case DerefAssignStmtAST(pointer, value) =>
        val cell = derefCell(evalAny(pointer, env))
        cell.value = evalAny(value, env)

      case ReturnStmtAST(value) =>
        throw ReturnException(value.map(evalAny(_, env)).getOrElse(0L))

      case WhileStmtAST(cond, body) =>
        while toLong(evalAny(cond, env)) != 0 do execBlock(body, env)

      case ExprStmtAST(expr) =>
        evalAny(expr, env)

  // evalAny returns the raw value — could be Long or Cell (pointer)
  private def evalAny(expr: ExpressionAST, env: Env): Any =
    expr match
      case IntLitAST(n) => n
      case CharLitAST(c) => c.toLong
      case BoolLitAST(b) => if b then 1L else 0L
      case StringLitAST(_) => throw RuntimeError("string values not yet supported")

      case VarRefAST(name) => lookupCell(name, env).value

      case AddrOfAST(name) => lookupCell(name, env) // return the Cell itself

      case DerefAST(expr) =>
        val cell = derefCell(evalAny(expr, env))
        cell.value

      case IfExprAST(cond, thenBody, elseBody) =>
        if toLong(evalAny(cond, env)) != 0 then
          evalBlock(thenBody, env)
        else
          elseBody match
            case Some(stmts) => evalBlock(stmts, env)
            case None => 0L

      case BinaryAST(left, op, right) =>
        val l = toLong(evalAny(left, env))
        op match
          case "&&" => if l == 0 then 0L else if toLong(evalAny(right, env)) != 0 then 1L else 0L
          case "||" => if l != 0 then 1L else if toLong(evalAny(right, env)) != 0 then 1L else 0L
          case _ =>
            val r = toLong(evalAny(right, env))
            (op match
              case "+"  => l + r
              case "-"  => l - r
              case "*"  => l * r
              case "/"  => if r == 0 then throw RuntimeError("division by zero") else l / r
              case "%"  => if r == 0 then throw RuntimeError("modulo by zero") else l % r
              case "==" => if l == r then 1L else 0L
              case "!=" => if l != r then 1L else 0L
              case "<"  => if l < r then 1L else 0L
              case ">"  => if l > r then 1L else 0L
              case "<=" => if l <= r then 1L else 0L
              case ">=" => if l >= r then 1L else 0L
              case _    => throw RuntimeError(s"unknown operator: $op")
            )

      case UnaryAST(op, operand) =>
        val v = toLong(evalAny(operand, env))
        op match
          case "-" => -v
          case "!" => if v == 0 then 1L else 0L
          case _   => throw RuntimeError(s"unknown unary operator: $op")

      case CallAST(name, args) =>
        val argValues = args.map(evalAny(_, env))
        builtins.get(name) match
          case Some(f) => f(argValues)
          case None =>
            functions.get(name) match
              case Some(fun) => call(fun, argValues)
              case None => throw RuntimeError(s"undefined function: $name")
