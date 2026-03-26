package io.github.edadma.trisc

import scala.collection.mutable

class SyslInterpreter(output: String => Unit = s => print(s)):
  case class ReturnException(value: Long) extends RuntimeException
  case class RuntimeError(msg: String) extends RuntimeException(msg)

  private val globals = new mutable.LinkedHashMap[String, Long]
  private val functions = new mutable.LinkedHashMap[String, FunDeclAST]

  private val builtins: Map[String, List[Long] => Long] = Map(
    "putchar" -> (args => { output(args.head.toChar.toString); args.head }),
    "print" -> (args => { args.foreach(a => output(a.toString)); 0 }),
    "println" -> (args => { args.foreach(a => output(a.toString)); output("\n"); 0 }),
  )

  def run(program: ProgramAST): Long =
    // Register all declarations
    for decl <- program.decls do
      decl match
        case f: FunDeclAST => functions(f.name) = f
        case VarDeclAST(_, name, init) => globals(name) = init.map(e => eval(e, mutable.LinkedHashMap.empty)).getOrElse(0)

    // Call main
    functions.get("main") match
      case Some(main) => call(main, Nil)
      case None => throw RuntimeError("no main function")

  private def call(fun: FunDeclAST, args: List[Long]): Long =
    val env = mutable.LinkedHashMap[String, Long]()

    // Bind parameters
    for (param, arg) <- fun.params.zip(args) do
      env(param.name) = arg

    try
      execBlock(fun.body, env)
      0 // void return
    catch
      case ReturnException(v) => v

  private def execBlock(stmts: List[StmtAST], env: mutable.LinkedHashMap[String, Long]): Unit =
    for stmt <- stmts do exec(stmt, env)

  private def exec(stmt: StmtAST, env: mutable.LinkedHashMap[String, Long]): Unit =
    stmt match
      case VarStmtAST(_, name, init) =>
        env(name) = init.map(e => eval(e, env)).getOrElse(0)

      case AssignStmtAST(target, value) =>
        val v = eval(value, env)
        if env.contains(target) then env(target) = v
        else if globals.contains(target) then globals(target) = v
        else throw RuntimeError(s"undefined variable: $target")

      case ReturnStmtAST(value) =>
        throw ReturnException(value.map(e => eval(e, env)).getOrElse(0))

      case IfStmtAST(cond, thenBody, elseBody) =>
        if eval(cond, env) != 0 then execBlock(thenBody, env)
        else elseBody.foreach(execBlock(_, env))

      case WhileStmtAST(cond, body) =>
        while eval(cond, env) != 0 do execBlock(body, env)

      case ExprStmtAST(expr) =>
        eval(expr, env)

  private def eval(expr: ExpressionAST, env: mutable.LinkedHashMap[String, Long]): Long =
    expr match
      case IntLitAST(n) => n

      case CharLitAST(c) => c.toLong

      case StringLitAST(_) => throw RuntimeError("string values not yet supported in expressions")

      case VarRefAST(name) =>
        if env.contains(name) then env(name)
        else if globals.contains(name) then globals(name)
        else throw RuntimeError(s"undefined variable: $name")

      case BinaryAST(left, op, right) =>
        val l = eval(left, env)
        // Short-circuit for logical operators
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
