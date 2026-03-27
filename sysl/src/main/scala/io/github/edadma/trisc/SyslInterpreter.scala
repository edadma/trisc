package io.github.edadma.trisc

import scala.collection.mutable

enum Value:
  case IntVal(n: Long)
  case FloatVal(d: Double)
  case PtrVal(cell: Cell)
  case ArrVal(cells: Array[Cell], offset: Int)
  case FuncVal(name: String)

class Cell(var value: Value)

class SyslInterpreter(output: String => Unit = s => print(s)):
  import Value.*

  case class ReturnException(value: Value) extends RuntimeException
  case object BreakException extends RuntimeException
  case object ContinueException extends RuntimeException
  case class RuntimeError(msg: String) extends RuntimeException(msg)

  private type Env = mutable.LinkedHashMap[String, Cell]

  private def toLong(v: Value): Long = v match
    case IntVal(n)    => n
    case FloatVal(d)  => d.toLong
    case PtrVal(_)    => throw RuntimeError("expected integer, got pointer")
    case ArrVal(_, _) => throw RuntimeError("expected integer, got array")
    case FuncVal(_)   => throw RuntimeError("expected integer, got function")

  private def toDouble(v: Value): Double = v match
    case FloatVal(d)  => d
    case IntVal(n)    => n.toDouble
    case _            => throw RuntimeError("expected numeric value")

  private val globals: Env = new mutable.LinkedHashMap
  private val functions = new mutable.LinkedHashMap[String, TFunDecl]

  // Format a double consistently across platforms: no trailing .0 for whole numbers
  private def formatDouble(d: Double): String =
    if d.isWhole && !d.isInfinite && !d.isNaN then
      val l = d.toLong
      l.toString
    else d.toString

  private val builtins: Map[String, List[Value] => Value] = Map(
    "putchar" -> (args => { output(toLong(args.head).toChar.toString); args.head }),
    "print" -> (args => { args.foreach { case FloatVal(d) => output(formatDouble(d)); case a => output(toLong(a).toString) }; IntVal(0) }),
    "println" -> (args => { args.foreach { case FloatVal(d) => output(formatDouble(d)); case a => output(toLong(a).toString) }; output("\n"); IntVal(0) }),
  )

  def run(program: TProgram): Long =
    for decl <- program.decls do
      decl match
        case _: TImportDecl => // not handled in interpreter
        case _: TStructDecl => // type only, no runtime effect
        case f: TFunDecl => functions(f.name) = f
        case TVarDecl(name, _, init, _) =>
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
        val rv = evalAny(value, env)
        (cell.value, rv) match
          case (FloatVal(_), _) | (_, FloatVal(_)) =>
            val l = toDouble(cell.value)
            val r = toDouble(rv)
            cell.value = FloatVal(op match
              case "+"  => l + r
              case "-"  => l - r
              case "*"  => l * r
              case "/"  => l / r
              case "%"  => l % r
              case _    => throw RuntimeError(s"unsupported float compound operator: $op")
            )
          case _ =>
            val l = toLong(cell.value)
            val r = toLong(rv)
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

      case TFieldAssignStmt(obj, fieldIndex, value) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        cells(off + fieldIndex).value = evalAny(value, env)

      case TFieldCompoundAssignStmt(obj, fieldIndex, op, value) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        val cell = cells(off + fieldIndex)
        val l = toLong(cell.value)
        val r = toLong(evalAny(value, env))
        cell.value = IntVal(op match
          case "+"  => l + r
          case "-"  => l - r
          case "*"  => l * r
          case "/"  => l / r
          case "%"  => l % r
          case "&"  => l & r
          case "|"  => l | r
          case "^"  => l ^ r
          case "<<" => l << r.toInt
          case ">>" => l >> r.toInt
        )

      case TReturnStmt(value) =>
        throw ReturnException(value.map(evalAny(_, env)).getOrElse(IntVal(0)))

      case TForStmt(init, cond, update, body) =>
        exec(init, env)
        var running = true
        while running && toLong(evalAny(cond, env)) != 0 do
          try
            execBlock(body, env)
            exec(update, env)
          catch
            case BreakException => running = false
            case ContinueException => exec(update, env) // continue still runs update

      case TWhileStmt(cond, body) =>
        var running = true
        while running && toLong(evalAny(cond, env)) != 0 do
          try
            execBlock(body, env)
          catch
            case BreakException => running = false
            case ContinueException => // skip rest of body, re-check condition

      case TDoWhileStmt(cond, body) =>
        var running = true
        while running do
          try
            execBlock(body, env)
          catch
            case BreakException    => running = false
            case ContinueException => // skip rest of body, re-check condition
          if running then running = toLong(evalAny(cond, env)) != 0

      case TBreakStmt => throw BreakException
      case TContinueStmt => throw ContinueException

      case TAsmStmt(_) => // no-op in interpreter

      case TExprStmt(expr) =>
        evalAny(expr, env)

  private def evalAny(expr: TExpr, env: Env): Value =
    expr match
      case TIntLit(n, _) => IntVal(n)
      case TFloatLit(d, _) => FloatVal(d)
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

      case TPreInc(name, typ) =>
        val cell = lookupCell(name, env)
        cell.value match
          case ArrVal(cells, off) =>
            val nv = ArrVal(cells, off + 1)
            cell.value = nv
            nv
          case _ =>
            val v = toLong(cell.value) + 1
            cell.value = IntVal(v)
            IntVal(v)

      case TPreDec(name, typ) =>
        val cell = lookupCell(name, env)
        cell.value match
          case ArrVal(cells, off) =>
            val nv = ArrVal(cells, off - 1)
            cell.value = nv
            nv
          case _ =>
            val v = toLong(cell.value) - 1
            cell.value = IntVal(v)
            IntVal(v)

      case TPostInc(name, typ) =>
        val cell = lookupCell(name, env)
        cell.value match
          case old @ ArrVal(cells, off) =>
            cell.value = ArrVal(cells, off + 1)
            old
          case _ =>
            val old = toLong(cell.value)
            cell.value = IntVal(old + 1)
            IntVal(old)

      case TPostDec(name, typ) =>
        val cell = lookupCell(name, env)
        cell.value match
          case old @ ArrVal(cells, off) =>
            cell.value = ArrVal(cells, off - 1)
            old
          case _ =>
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

        // Short-circuit logical operators
        op match
          case "&&" =>
            val l = toLong(lv)
            return IntVal(if l == 0 then 0L else if toLong(evalAny(right, env)) != 0 then 1L else 0L)
          case "||" =>
            val l = toLong(lv)
            return IntVal(if l != 0 then 1L else if toLong(evalAny(right, env)) != 0 then 1L else 0L)
          case _ =>

        val rv = evalAny(right, env)

        // Float path: if either operand is float, use float arithmetic
        (lv, rv) match
          case (FloatVal(_), _) | (_, FloatVal(_)) =>
            val l = toDouble(lv)
            val r = toDouble(rv)
            return (op match
              case "+"  => FloatVal(l + r)
              case "-"  => FloatVal(l - r)
              case "*"  => FloatVal(l * r)
              case "/"  => FloatVal(l / r)
              case "%"  => FloatVal(l % r)
              case "==" => IntVal(if l == r then 1L else 0L)
              case "!=" => IntVal(if l != r then 1L else 0L)
              case "<"  => IntVal(if l < r then 1L else 0L)
              case ">"  => IntVal(if l > r then 1L else 0L)
              case "<=" => IntVal(if l <= r then 1L else 0L)
              case ">=" => IntVal(if l >= r then 1L else 0L)
              case _    => throw RuntimeError(s"unsupported float operator: $op")
            )
          case _ =>

        // Integer path
        val l = toLong(lv)
        val r = toLong(rv)
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
        val v = evalAny(operand, env)
        v match
          case FloatVal(d) =>
            op match
              case "-" => FloatVal(-d)
              case _   => throw RuntimeError(s"unsupported float unary operator: $op")
          case _ =>
            val n = toLong(v)
            IntVal(op match
              case "-" => -n
              case "!" => if n == 0 then 1L else 0L
              case "~" => ~n
              case _   => throw RuntimeError(s"unknown unary operator: $op")
            )

      case TCast(inner, target) =>
        val v = evalAny(inner, env)
        import SyslType.*
        target match
          case DoubleType  => FloatVal(toDouble(v))
          case BoolType => IntVal(if toLong(v) != 0 then 1L else 0L)
          case IntType(64) => IntVal(toLong(v))
          case IntType(32) => IntVal(toLong(v) & 0xFFFFFFFFL)
          case IntType(16) => IntVal(toLong(v) & 0xFFFFL)
          case IntType(8)  => IntVal(toLong(v) & 0xFFL)
          case _: IntType  => IntVal(toLong(v))
          case _ => v

      case TSizeof(size, _) => IntVal(size)

      case TFieldPreInc(obj, fieldIndex, _) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        val cell = cells(off + fieldIndex)
        val v = toLong(cell.value) + 1
        cell.value = IntVal(v)
        IntVal(v)

      case TFieldPreDec(obj, fieldIndex, _) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        val cell = cells(off + fieldIndex)
        val v = toLong(cell.value) - 1
        cell.value = IntVal(v)
        IntVal(v)

      case TFieldPostInc(obj, fieldIndex, _) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        val cell = cells(off + fieldIndex)
        val old = toLong(cell.value)
        cell.value = IntVal(old + 1)
        IntVal(old)

      case TFieldPostDec(obj, fieldIndex, _) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        val cell = cells(off + fieldIndex)
        val old = toLong(cell.value)
        cell.value = IntVal(old - 1)
        IntVal(old)

      case TStructLit(SyslType.StructType(_, fields)) =>
        val cells = Array.fill(fields.size)(new Cell(IntVal(0)))
        ArrVal(cells, 0)

      case TFieldAccess(obj, fieldIndex, _) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        cells(off + fieldIndex).value

      case TFuncRef(name, _) => FuncVal(name)

      case TCall(name, args, _) =>
        val argValues = args.map(evalAny(_, env))
        builtins.get(name) match
          case Some(f) => f(argValues)
          case None =>
            functions.get(name) match
              case Some(fun) => call(fun, argValues)
              case None => throw RuntimeError(s"undefined function: $name")

      case TIndirectCall(callee, args, _) =>
        val FuncVal(name) = evalAny(callee, env): @unchecked
        val argValues = args.map(evalAny(_, env))
        builtins.get(name) match
          case Some(f) => f(argValues)
          case None =>
            functions.get(name) match
              case Some(fun) => call(fun, argValues)
              case None => throw RuntimeError(s"undefined function: $name")
