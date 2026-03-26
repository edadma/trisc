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
    case IntVal(n)    => n
    case PtrVal(_)    => throw RuntimeError("expected integer, got pointer")
    case ArrVal(_, _) => throw RuntimeError("expected integer, got array")

  private val globals: Env = new mutable.LinkedHashMap
  private val functions = new mutable.LinkedHashMap[String, TFunDecl]

  private val builtins: Map[String, List[Value] => Value] = Map(
    "putchar" -> (args => { output(toLong(args.head).toChar.toString); args.head }),
    "print" -> (args => { args.foreach(a => output(toLong(a).toString)); IntVal(0) }),
    "println" -> (args => { args.foreach(a => output(toLong(a).toString)); output("\n"); IntVal(0) }),
  )

  def run(program: TProgram): Long =
    for decl <- program.decls do
      decl match
        case f: TFunDecl => functions(f.name) = f
        case TVarDecl(name, _, init) =>
          globals(name) = new Cell(evalAny(init, new mutable.LinkedHashMap))

    functions.get("main") match
      case Some(main) => toLong(call(main, Nil))
      case None => throw RuntimeError("no main function")

  private def call(fun: TFunDecl, args: List[Value]): Value =
    val env: Env = new mutable.LinkedHashMap

    for (param, arg) <- fun.params.zip(args) do
      env(param.name) = new Cell(arg)

    fun.body match
      case TExprBody(expr) => evalAny(expr, env)
      case TBlockBody(stmts) =>
        try
          evalBlock(stmts, env)
        catch
          case ReturnException(v) => v

  private def evalBlock(stmts: List[TStmt], env: Env): Value =
    if stmts.nonEmpty then
      execBlock(stmts.init, env)
      stmts.last match
        case TExprStmt(expr) => evalAny(expr, env)
        case other => exec(other, env); IntVal(0)
    else IntVal(0)

  private def execBlock(stmts: List[TStmt], env: Env): Unit =
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

  private def exec(stmt: TStmt, env: Env): Unit =
    stmt match
      case TVarStmt(name, _, init) =>
        env(name) = new Cell(evalAny(init, env))

      case TAssignStmt(target, value) =>
        val v = evalAny(value, env)
        if env.contains(target) then env(target).value = v
        else if globals.contains(target) then globals(target).value = v
        else env(target) = new Cell(v)

      case TCompoundAssignStmt(target, op, value) =>
        val cell = lookupCell(target, env)
        val l = toLong(cell.value)
        val r = toLong(evalAny(value, env))
        cell.value = IntVal(op match
          case "+"  => l + r
          case "-"  => l - r
          case "*"  => l * r
          case "/"  => if r == 0 then throw RuntimeError("division by zero") else l / r
          case "%"  => if r == 0 then throw RuntimeError("modulo by zero") else l % r
          case "&"  => l & r
          case "|"  => l | r
          case "^"  => l ^ r
          case "<<" => l << r.toInt
          case ">>" => l >> r.toInt
        )

      case TDerefAssignStmt(pointer, value) =>
        val cell = derefCell(evalAny(pointer, env))
        cell.value = evalAny(value, env)

      case TIndexAssignStmt(array, index, value) =>
        val arr = evalAny(array, env)
        val idx = toLong(evalAny(index, env)).toInt
        val cell = indexCell(arr, idx)
        cell.value = evalAny(value, env)

      case TReturnStmt(value) =>
        throw ReturnException(value.map(evalAny(_, env)).getOrElse(IntVal(0)))

      case TWhileStmt(cond, body) =>
        while toLong(evalAny(cond, env)) != 0 do execBlock(body, env)

      case TExprStmt(expr) =>
        evalAny(expr, env)

  private def evalAny(expr: TExpr, env: Env): Value =
    expr match
      case TIntLit(n, _) => IntVal(n)
      case TBoolLit(b, _) => IntVal(if b then 1L else 0L)

      case TStringLit(s, _) =>
        val bytes = s.getBytes("UTF-8")
        val cells = Array.fill(bytes.length + 1)(new Cell(IntVal(0)))
        for i <- bytes.indices do cells(i).value = IntVal(bytes(i) & 0xff)
        ArrVal(cells, 0)

      case TArrayDecl(size, _, _) =>
        val cells = Array.fill(size)(new Cell(IntVal(0)))
        ArrVal(cells, 0)

      case TVarRef(name, _) => lookupCell(name, env).value

      case TAddrOf(name, _) => PtrVal(lookupCell(name, env))

      case TAddrOfIndex(array, index, _) =>
        val arrVal = evalAny(array, env)
        val idx = toLong(evalAny(index, env)).toInt
        arrVal match
          case ArrVal(cells, off) => ArrVal(cells, off + idx)
          case _ => PtrVal(indexCell(arrVal, idx))

      case TPreInc(name, _) =>
        val cell = lookupCell(name, env)
        val v = toLong(cell.value) + 1
        cell.value = IntVal(v)
        IntVal(v)

      case TPreDec(name, _) =>
        val cell = lookupCell(name, env)
        val v = toLong(cell.value) - 1
        cell.value = IntVal(v)
        IntVal(v)

      case TPostInc(name, _) =>
        val cell = lookupCell(name, env)
        val old = toLong(cell.value)
        cell.value = IntVal(old + 1)
        IntVal(old)

      case TPostDec(name, _) =>
        val cell = lookupCell(name, env)
        val old = toLong(cell.value)
        cell.value = IntVal(old - 1)
        IntVal(old)

      case TDeref(inner, _) => derefCell(evalAny(inner, env)).value

      case TIndex(arr, index, _) =>
        val arrVal = evalAny(arr, env)
        val idx = toLong(evalAny(index, env)).toInt
        indexCell(arrVal, idx).value

      case TIfExpr(cond, thenBody, elseBody, _) =>
        if toLong(evalAny(cond, env)) != 0 then
          evalBlock(thenBody, env)
        else
          elseBody match
            case Some(stmts) => evalBlock(stmts, env)
            case None => IntVal(0)

      case TBinary(left, op, right, _) =>
        val lv = evalAny(left, env)
        (lv, op) match
          case (ArrVal(cells, off), "+") =>
            return ArrVal(cells, off + toLong(evalAny(right, env)).toInt)
          case (ArrVal(cells, off), "-") =>
            return ArrVal(cells, off - toLong(evalAny(right, env)).toInt)
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
              case "&"  => l & r
              case "|"  => l | r
              case "^"  => l ^ r
              case "<<" => l << r.toInt
              case ">>" => l >> r.toInt
              case _    => throw RuntimeError(s"unknown operator: $op")
            )

      case TUnary(op, operand, _) =>
        val v = toLong(evalAny(operand, env))
        IntVal(op match
          case "-" => -v
          case "!" => if v == 0 then 1L else 0L
          case "~" => ~v
          case _   => throw RuntimeError(s"unknown unary operator: $op")
        )

      case TCall(name, args, _) =>
        val argValues = args.map(evalAny(_, env))
        builtins.get(name) match
          case Some(f) => f(argValues)
          case None =>
            functions.get(name) match
              case Some(fun) => call(fun, argValues)
              case None => throw RuntimeError(s"undefined function: $name")
