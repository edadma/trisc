package io.github.edadma.trisc

import scala.collection.mutable

/** Statement evaluation for `SyslInterpreter` — the `exec` switch.
  *
  * Mixed into `SyslInterpreter` via `extends SyslInterpreterStatements`;
  * self-typed so all the helpers (`evalAny`, `refIncr`/`refDecr`,
  * `lookupCell`, `derefCell`, `indexCell`, `deepCopyValue`,
  * `toLong`/`toDouble`/`truncateNarrow`, the path-dependent exception
  * types `RuntimeError` / `ReturnException` / etc., and the mutable
  * state `globals` / `deferStack` / `mmioMemory`) are reachable
  * without qualification. Pure refactor — no semantic changes from the
  * pre-split monolith.
  */
trait SyslInterpreterStatements {
  self: SyslInterpreter =>

  import Value.*

  protected def exec(stmt: TStmt, env: Env): Unit =
    chargeStep()
    stmt match
      case TVarStmt(name, _, init, _, _) =>
        val raw = evalAny(init, env)
        // Value-struct binding: deep-copy so `var b = a` doesn't alias.
        val v = deepCopyValue(init.typ, raw)
        // Increment refcount for copies only — TNew/TNewArray already set refcount=1
        init match
          case _: TNew | _: TNewArray => // owned, no incr
          case _ => refIncr(v)
        env(name) = new Cell(v)

      case TDestructureStmt(names, _, init) =>
        val (cells, off) = evalAny(init, env) match
          case ArrVal(c, o) => (c, o)
          case RefVal(c, _, _) => (c, 0)
          case other => throw RuntimeError(s"cannot destructure $other")
        for (name, i) <- names.zipWithIndex if name != "_" do
          env(name) = new Cell(cells(off + i).value)

      case TDestructureAssignStmt(names, _, init) =>
        // Parallel assignment: evaluate RHS fully, then assign all values
        val (cells, off) = evalAny(init, env) match
          case ArrVal(c, o) => (c, o)
          case RefVal(c, _, _) => (c, 0)
          case other => throw RuntimeError(s"cannot destructure $other")
        val values = names.indices.map(i => cells(off + i).value)
        for (name, v) <- names.zip(values) if name != "_" do
          lookupCell(name, env).value = v

      case TAssignStmt(target, value) =>
        val raw = evalAny(value, env)
        // Value-struct binding: deep-copy so reassignment from another struct
        // var doesn't alias.
        val v = deepCopyValue(value.typ, raw)
        // Increment refcount for copies only
        value match
          case _: TNew | _: TNewArray => // owned, no incr
          case _ => refIncr(v)
        if env.contains(target) then
          refDecr(env(target).value)
          env(target).value = v
        else if globals.contains(target) then
          refDecr(globals(target).value)
          globals(target).value = v
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
          case (PtrVal(ptr), _) =>
            val n = toLong(rv).toInt
            cell.value = op match
              case "+" => PtrVal(ptr.add(n))
              case "-" => PtrVal(ptr.sub(n))
              case _ => throw RuntimeError(s"unsupported pointer compound operator: $op")
          case (ArrVal(cells, off), _) =>
            val n = toLong(rv).toInt
            cell.value = op match
              case "+" => ArrVal(cells, off + n)
              case "-" => ArrVal(cells, off - n)
              case _ => throw RuntimeError(s"unsupported pointer compound operator: $op")
          case _ =>
            val l = toLong(cell.value)
            val r = toLong(rv)
            val raw = op match
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
            cell.value = IntVal(truncateNarrow(raw, value.typ))

      case TDerefAssignStmt(TCast(TIntLit(addr, _), SyslType.PtrType(_)), value) =>
        // #address MMIO write: store into the virtual mmio map keyed by the literal address.
        mmioMemory(addr) = toLong(evalAny(value, env))

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
        val raw = op match
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
        cell.value = IntVal(truncateNarrow(raw, value.typ))

      case TReturnStmt(value) =>
        throw ReturnException(value.map(evalAny(_, env)).getOrElse(IntVal(0)))

      case TDeferStmt(body) =>
        deferStack += ((body, env))

      case TForStmt(init, cond, update, body, myLabel) =>
        exec(init, env)
        var running = true
        while running && toLong(evalAny(cond, env)) != 0 do
          val savedKeys = env.keySet.toSet
          try
            execBlock(body, env)
            exec(update, env)
          catch
            case e: BreakException if claimsLoop(e.label, myLabel) => running = false
            case e: ContinueException if claimsLoop(e.label, myLabel) => exec(update, env)
          // Release refs for variables created in this iteration
          for key <- env.keySet.toSet -- savedKeys do
            refDecr(env(key).value)
            env.remove(key)

      case TWhileStmt(cond, body, myLabel) =>
        var running = true
        while running && toLong(evalAny(cond, env)) != 0 do
          val savedKeys = env.keySet.toSet
          try
            execBlock(body, env)
          catch
            case e: BreakException if claimsLoop(e.label, myLabel) => running = false
            case e: ContinueException if claimsLoop(e.label, myLabel) =>
          // Release refs for variables created in this iteration
          for key <- env.keySet.toSet -- savedKeys do
            refDecr(env(key).value)
            env.remove(key)

      case TDoWhileStmt(cond, body, myLabel) =>
        var running = true
        while running do
          val savedKeys = env.keySet.toSet
          try
            execBlock(body, env)
          catch
            case e: BreakException if claimsLoop(e.label, myLabel) => running = false
            case e: ContinueException if claimsLoop(e.label, myLabel) =>
          // Release refs for variables created in this iteration
          for key <- env.keySet.toSet -- savedKeys do
            refDecr(env(key).value)
            env.remove(key)
          if running then running = toLong(evalAny(cond, env)) != 0

      case TLoopStmt(body, myLabel) =>
        var running = true
        while running do
          val savedKeys = env.keySet.toSet
          try
            execBlock(body, env)
          catch
            case e: BreakException if claimsLoop(e.label, myLabel) => running = false
            case e: ContinueException if claimsLoop(e.label, myLabel) =>
          for key <- env.keySet.toSet -- savedKeys do
            refDecr(env(key).value)
            env.remove(key)

      case TBreakStmt(label) => throw BreakException(label)
      case TContinueStmt(label) => throw ContinueException(label)

      case TAsmStmt(_) => // no-op in interpreter

      case TMultiStmt(children) =>
        for s <- children do exec(s, env)

      case TContractCheck(kind, expr, message) =>
        val v = toLong(evalAny(expr, env))
        if v == 0 then
          val suffix = if message == kind then "" else s": $message"
          throw RuntimeError(s"$kind check failed$suffix")

      case TExprStmt(expr) =>
        evalAny(expr, env)
}
