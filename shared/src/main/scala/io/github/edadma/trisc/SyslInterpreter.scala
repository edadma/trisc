package io.github.edadma.trisc

import scala.collection.mutable

enum Value:
  case IntVal(n: Long)
  case PtrVal(cell: Cell)
  case ArrVal(cells: Array[Cell], offset: Int)

class Cell(var value: Value)

class SyslInterpreter(output: String => Unit = s => print(s)):
  import Value.*

  case class ReturnException(value: Value) extends RuntimeException
  case class RuntimeError(msg: String) extends RuntimeException(msg)

  private type Env = mutable.LinkedHashMap[String, Cell]

  private def toLong(v: Value): Long = v match
    case IntVal(n)       => n
    case PtrVal(_)       => throw RuntimeError("expected integer, got pointer")
    case ArrVal(_, _)    => throw RuntimeError("expected integer, got array")

  private val globals: Env = new mutable.LinkedHashMap
  private val functions = new mutable.LinkedHashMap[String, FunDeclAST]

  private val builtins: Map[String, List[Value] => Value] = Map(
    "putchar" -> (args => { output(toLong(args.head).toChar.toString); args.head }),
    "print" -> (args => { args.foreach(a => output(toLong(a).toString)); IntVal(0) }),
    "println" -> (args => { args.foreach(a => output(toLong(a).toString)); output("\n"); IntVal(0) }),
  )

  def run(program: ProgramAST): Long =
    for decl <- program.decls do
      decl match
        case f: FunDeclAST => functions(f.name) = f
        case VarDeclAST(name, _, init) =>
          globals(name) = new Cell(evalAny(init, new mutable.LinkedHashMap))

    functions.get("main") match
      case Some(main) => toLong(call(main, Nil))
      case None => throw RuntimeError("no main function")

  private def call(fun: FunDeclAST, args: List[Value]): Value =
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

  private def evalBlock(stmts: List[StmtAST], env: Env): Value =
    if stmts.nonEmpty then
      execBlock(stmts.init, env)
      stmts.last match
        case ExprStmtAST(expr) => evalAny(expr, env)
        case other => exec(other, env); IntVal(0)
    else IntVal(0)

  private def execBlock(stmts: List[StmtAST], env: Env): Unit =
    for stmt <- stmts do exec(stmt, env)

  private def lookupCell(name: String, env: Env): Cell =
    env.getOrElse(name, globals.getOrElse(name, throw RuntimeError(s"undefined variable: $name")))

  private def derefCell(v: Value): Cell = v match
    case PtrVal(c)          => c
    case ArrVal(cells, off) => cells(off)
    case _                  => throw RuntimeError("cannot dereference non-pointer")

  private def indexCell(v: Value, idx: Int): Cell = v match
    case ArrVal(cells, off) =>
      val i = off + idx
      if i < 0 || i >= cells.length then throw RuntimeError(s"array index out of bounds: $i")
      cells(i)
    case PtrVal(c) =>
      if idx == 0 then c
      else throw RuntimeError("cannot index a non-array pointer with offset != 0")
    case _ => throw RuntimeError("cannot index non-array")

  private def exec(stmt: StmtAST, env: Env): Unit =
    stmt match
      case VarStmtAST(name, _, init) =>
        env(name) = new Cell(evalAny(init, env))

      case AssignStmtAST(target, value) =>
        val v = evalAny(value, env)
        if env.contains(target) then env(target).value = v
        else if globals.contains(target) then globals(target).value = v
        else env(target) = new Cell(v)

      case CompoundAssignStmtAST(target, op, value) =>
        val cell = lookupCell(target, env)
        val l = toLong(cell.value)
        val r = toLong(evalAny(value, env))
        cell.value = IntVal(op match
          case "+" => l + r
          case "-" => l - r
          case "*" => l * r
          case "/" => if r == 0 then throw RuntimeError("division by zero") else l / r
          case "%" => if r == 0 then throw RuntimeError("modulo by zero") else l % r
        )

      case DerefAssignStmtAST(pointer, value) =>
        val cell = derefCell(evalAny(pointer, env))
        cell.value = evalAny(value, env)

      case IndexAssignStmtAST(array, index, value) =>
        val arr = evalAny(array, env)
        val idx = toLong(evalAny(index, env)).toInt
        val cell = indexCell(arr, idx)
        cell.value = evalAny(value, env)

      case ReturnStmtAST(value) =>
        throw ReturnException(value.map(evalAny(_, env)).getOrElse(IntVal(0)))

      case WhileStmtAST(cond, body) =>
        while toLong(evalAny(cond, env)) != 0 do execBlock(body, env)

      case ExprStmtAST(expr) =>
        evalAny(expr, env)

  private def evalAny(expr: ExpressionAST, env: Env): Value =
    expr match
      case IntLitAST(n) => IntVal(n)
      case CharLitAST(c) => IntVal(c.toLong)
      case BoolLitAST(b) => IntVal(if b then 1L else 0L)
      case StringLitAST(_) => throw RuntimeError("string values not yet supported")

      case VarRefAST(name) => lookupCell(name, env).value

      case AddrOfAST(name) => PtrVal(lookupCell(name, env))

      case AddrOfIndexAST(array, index) =>
        val arrVal = evalAny(array, env)
        val idx = toLong(evalAny(index, env)).toInt
        arrVal match
          case ArrVal(cells, off) => ArrVal(cells, off + idx)
          case _ => PtrVal(indexCell(arrVal, idx))

      case PreIncAST(name) =>
        val cell = lookupCell(name, env)
        val v = toLong(cell.value) + 1
        cell.value = IntVal(v)
        IntVal(v)

      case PreDecAST(name) =>
        val cell = lookupCell(name, env)
        val v = toLong(cell.value) - 1
        cell.value = IntVal(v)
        IntVal(v)

      case PostIncAST(name) =>
        val cell = lookupCell(name, env)
        val old = toLong(cell.value)
        cell.value = IntVal(old + 1)
        IntVal(old)

      case PostDecAST(name) =>
        val cell = lookupCell(name, env)
        val old = toLong(cell.value)
        cell.value = IntVal(old - 1)
        IntVal(old)

      case DerefAST(expr) => derefCell(evalAny(expr, env)).value

      case IndexAST(arr, index) =>
        val arrVal = evalAny(arr, env)
        val idx = toLong(evalAny(index, env)).toInt
        indexCell(arrVal, idx).value

      case ArrayDeclAST(size, _) =>
        val cells = Array.fill(size)(new Cell(IntVal(0)))
        ArrVal(cells, 0)

      case IfExprAST(cond, thenBody, elseBody) =>
        if toLong(evalAny(cond, env)) != 0 then
          evalBlock(thenBody, env)
        else
          elseBody match
            case Some(stmts) => evalBlock(stmts, env)
            case None => IntVal(0)

      case BinaryAST(left, op, right) =>
        val lv = evalAny(left, env)
        // pointer arithmetic: arr + int or ptr + int
        (lv, op) match
          case (ArrVal(cells, off), "+") =>
            val r = toLong(evalAny(right, env)).toInt
            return ArrVal(cells, off + r)
          case (ArrVal(cells, off), "-") =>
            val r = toLong(evalAny(right, env)).toInt
            return ArrVal(cells, off - r)
          case _ =>

        val l = toLong(lv)
        op match
          case "&&" => IntVal(if l == 0 then 0L else if toLong(evalAny(right, env)) != 0 then 1L else 0L)
          case "||" => IntVal(if l != 0 then 1L else if toLong(evalAny(right, env)) != 0 then 1L else 0L)
          case _ =>
            val r = toLong(evalAny(right, env))
            IntVal(op match
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
        IntVal(op match
          case "-" => -v
          case "!" => if v == 0 then 1L else 0L
          case _   => throw RuntimeError(s"unknown unary operator: $op")
        )

      case CallAST(name, args) =>
        val argValues = args.map(evalAny(_, env))
        builtins.get(name) match
          case Some(f) => f(argValues)
          case None =>
            functions.get(name) match
              case Some(fun) => call(fun, argValues)
              case None => throw RuntimeError(s"undefined function: $name")
