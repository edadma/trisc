package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

trait SyslSVMCodegenStatements:
  self: SyslSVMCodegen =>

  // ========================================================================
  // emitDefers — replays each defer-site body counter-times at every fn-exit
  // ========================================================================
  def emitDefers(): Unit =
    for body <- deferBodies.reverseIterator do
      val slot = deferSiteSlot(body)
      val loopLbl = newLabel("defer_loop")
      val endLbl = newLabel("defer_end")
      emit(s"$loopLbl:")
      emit(s"  local_get $slot")
      emit("  eqz")
      emit(s"  jumpnz $endLbl")
      emit(s"  local_get $slot")
      emit("  dec")
      emit(s"  local_set $slot")
      genStmt(body)
      emit(s"  jump $loopLbl")
      emit(s"$endLbl:")

  // ========================================================================
  // genStmts / genStmt
  // ========================================================================
  def genStmts(stmts: List[TStmt]): Unit = stmts.foreach(genStmt)

  /** Generate statements where the last one leaves its value on the stack (for if-expr, match-expr, function bodies).
    * Always pushes exactly 1 value on the stack. If the last expression is void-typed (which can occur when an
    * if-expression's branches have mismatched types — the analyzer types the enclosing expression based on the
    * first branch only), synthesize a push_0 so the stack stays balanced.
    */
  def genStmtsAsExpr(stmts: List[TStmt]): Unit =
    if stmts.isEmpty then emitPushInt(0)
    else
      genStmts(stmts.init)
      stmts.last match
        case TExprStmt(expr) =>
          genExpr(expr)
          if expr.typ == SyslType.UnitType then emitPushInt(0)
        case TReturnStmt(Some(expr)) => genExpr(expr); maybeCoerceReturnToEnum(expr.typ); emitDefers(); emitFunctionExitRefDecrs(); emit("  ret")
        case other => genStmt(other); emitPushInt(0)

  def genStmt(stmt: TStmt): Unit = stmt match
    case TVarStmt(name, typ, init, _, _) if addressedLocals.contains(name) && !needsMemAlloc(typ) && typ != SyslType.StringType && !typ.isInstanceOf[SyslType.SliceType] =>
      // Scalar local whose address is taken. Allocate an 8-byte cell on the
      // memory stack; the local slot holds the cell's address. Loads and
      // stores go through the pointer so &x and the local refer to the
      // same storage.
      val idx = allocLocal(name, typ)
      emitMemAlloc(8)
      emit("  dup")
      emit(s"  local_set $idx")
      genExpr(init)
      emit("  swap")
      emitStore(typ)

    case TVarStmt(name, typ, init, _, _) =>
      val idx = allocLocal(name, typ)
      if needsMemAlloc(typ) then
        // Allocate memory on the memory stack, store address in local
        val size = typ.sizeOf
        emitMemAlloc(size)
        emit(s"  dup")
        emit(s"  local_set $idx") // local holds the address
        // Zero-initialize the memory
        val aligned = ((size + 7) / 8 * 8).toInt
        for i <- 0 until aligned by 8 do
          emit("  dup")
          if i > 0 then { emitPushInt(i); emit("  add") }
          emit("  push_0")
          emit("  swap")
          emit("  store64")
        emit("  drop")
        // If init is an array literal or struct construct, populate values
        init match
          case TArrayLit(elements, _) =>
            val elemType = typ match { case SyslType.ArrayType(e, _) => e; case _ => SyslType.I64 }
            for (elem, i) <- elements.zipWithIndex do
              emit(s"  local_get $idx")
              emitPushInt(i * elemType.sizeOf)
              emit("  add")
              genExpr(elem)
              emit("  swap")
              emitStore(elemType)
          case TStructConstruct(structType, args) =>
            for (arg, i) <- args.zipWithIndex do
              val off = fieldOffset(structType, i)
              val fieldType = structType.fields(i)._2
              emit(s"  local_get $idx")
              if off != 0 then { emitPushInt(off); emit("  add") }
              genExpr(arg)
              emit("  swap")
              emitStore(fieldType)
          case TEnumConstruct(et, variantIndex, args) =>
            // Tag at offset 0 (i32)
            emit(s"  local_get $idx")
            emitPushInt(variantIndex)
            emit("  swap")
            emit("  store32")
            // Variant fields at dataOffset
            val dataOff = et.dataOffset.toInt
            val variantFields = et.variants(variantIndex)._2
            var fieldOff = 0
            for (arg, i) <- args.zipWithIndex do
              val (_, fieldType) = variantFields(i)
              val align = fieldType.alignOf.toInt.max(1)
              fieldOff = ((fieldOff + align - 1) / align) * align
              emit(s"  local_get $idx")
              val totalOff = dataOff + fieldOff
              if totalOff != 0 then { emitPushInt(totalOff); emit("  add") }
              genExpr(arg)
              emit("  swap")
              emitStore(fieldType)
              fieldOff += fieldType.sizeOf.toInt
          case _: TArrayDecl | _: TStructLit => // already zeroed
          case _ =>
            // General case: init returns an address, bulk copy into our allocation
            genExpr(init) // ( src_addr )
            val copySize = ((typ.sizeOf + 7) / 8 * 8).toInt
            for i <- 0 until copySize by 8 do
              emit("  dup")
              if i > 0 then { emitPushInt(i); emit("  add") }
              emit("  load64")
              emit(s"  local_get $idx")
              if i > 0 then { emitPushInt(i); emit("  add") }
              emit("  store64")
            emit("  drop") // drop src_addr
      else
        genExpr(init)
        // For &T refs to a struct: incr the buffer's refcount if the source is
        // borrowed (TVarRef etc.); track the local for scope-exit decr.
        isStructRef(typ) match
          case Some(st) =>
            if !isOwnedRefExpr(init) then
              emitRefIncr()  // ( ptr ) → ( ptr ) — incr at ptr-8
            emit(s"  local_set $idx")
            refLocals += ((idx, st))
          case None =>
            emit(s"  local_set $idx")

    case TAssignStmt(target, value) =>
      // For an existing `&T` local being reassigned, apply the release/acquire
      // refcount protocol: INCR NEW first (so self-assign `r = r` keeps the
      // buffer alive across the decr) → DECR OLD → STORE NEW.
      locals.get(target) match
        case Some(LocalInfo(idx, typ)) if isStructRef(typ).isDefined =>
          val st = isStructRef(typ).get
          genExpr(value)                  // ( new_ptr )
          if !isOwnedRefExpr(value) then
            emitRefIncr()                 // incr new_ptr, leave ( new_ptr )
          // Save new_ptr to local first so we can read OLD via local_get
          // ... but local_set overwrites OLD before decr fires. So:
          // ( new_ptr ) — DUP, then decr OLD via local_get, then store NEW.
          emit("  dup")                   // ( new_ptr, new_ptr )
          emit(s"  local_get $idx")       // ( new_ptr, new_ptr, old_ptr )
          emitRefDecr(st)                 // consumes old_ptr → ( new_ptr, new_ptr )
          emit(s"  local_set $idx")       // ( new_ptr )
          emit("  drop")                  // ( )
        case _ =>
          genExpr(value)
          locals.get(target) match
            case Some(LocalInfo(idx, typ)) if addressedLocals.contains(target) && !needsMemAlloc(typ) && typ != SyslType.StringType && !typ.isInstanceOf[SyslType.SliceType] =>
              // Addressed scalar: write through the cell's pointer.
              emit(s"  local_get $idx")
              emitStore(typ)
            case Some(LocalInfo(idx, _)) => emit(s"  local_set $idx")
            case None if globals.contains(target) =>
              // Scalars are 8-byte cells (matches load64 in TVarRef); aggregates
              // (strings, structs, slices, ...) are address-represented, and
              // assignment is a sizeof-bytes copy via emitStore-aggregate.
              emit(s"  push_i64 $target")
              globals(target).underlying match
                case _: SyslType.StructType | _: SyslType.EnumType
                   | SyslType.StringType | _: SyslType.SliceType
                   | _: SyslType.ArrayType | _: SyslType.FuncType =>
                  emitStore(globals(target))
                case _ => emit("  store64")
            case None =>
              // Implicit local declaration (e.g. `v = expr?` sugar lowered by
              // the analyzer into `TAssignStmt` with a fresh target).
              val idx = allocLocal(target, value.typ)
              emit(s"  local_set $idx")

    case TCompoundAssignStmt(target, op, value) =>
      locals.get(target) match
        case Some(LocalInfo(idx, typ)) if addressedLocals.contains(target) && !needsMemAlloc(typ) && typ != SyslType.StringType && !typ.isInstanceOf[SyslType.SliceType] =>
          // Addressed scalar: read, compute, write through pointer.
          emit(s"  local_get $idx")
          emitLoad(typ)
          genExpr(value)
          emitBinaryOp(op, typ)
          emit(s"  local_get $idx")
          emitStore(typ)
        case Some(LocalInfo(idx, typ)) =>
          emit(s"  local_get $idx")
          genExpr(value)
          emitBinaryOp(op, typ)
          emit(s"  local_set $idx")
        case None =>
          // Global: load, compute, store
          emit(s"  push_i64 $target")
          emit("  dup")
          emit("  load64")
          genExpr(value)
          emitBinaryOp(op, globals.getOrElse(target, SyslType.I64))
          emit("  swap")
          emit("  store64")

    case TDerefAssignStmt(pointer, value) =>
      genExpr(value)
      genExpr(pointer)
      pointer.typ match
        case SyslType.PtrType(pointee) => emitStore(pointee)
        case _ => emit("  store64")

    case TIndexAssignStmt(array, index, value) =>
      val elemType = array.typ match
        case SyslType.ArrayType(e, _) => e
        case SyslType.PtrType(e) => e
        case SyslType.SliceType(e) => e
        case SyslType.RefType(SyslType.SliceType(e)) => e
        case _ => SyslType.I64
      genExpr(value)
      genExpr(array)
      // Bounds check, mirroring TIndex. Stack invariant after this block is
      // [value, base_or_data_ptr, idx] for non-pointer types and the original
      // [value, base, idx] for raw PtrType. See TIndex for the layout notes
      // (also: stack-only, no locals — countLocals doesn't see temps).
      array.typ match
        case SyslType.ArrayType(_, size) =>
          genExpr(index)                                  // [value, base, idx]
          emit("  dup")                                   // [value, base, idx, idx]
          emitPushInt(size)
          emit("  ltu")                                   // [value, base, idx, ok]
          val pass = newLabel("oob_pass")
          emit(s"  jumpnz $pass")
          emit("  halt")
          emit(s"$pass:")
        case SyslType.SliceType(_) | SyslType.RefType(SyslType.SliceType(_)) =>
          emit("  dup")                                   // [value, base, base]
          emit("  load64")                                // [value, base, data_ptr]
          emit("  swap")                                  // [value, data_ptr, base]
          emitPushInt(8)
          emit("  add")
          emit("  load32")                                // [value, data_ptr, len]
          genExpr(index)                                  // [value, data_ptr, len, idx]
          emit("  dup")                                   // [value, data_ptr, len, idx, idx]
          emit("  rot")                                   // [value, data_ptr, idx, idx, len]
          emit("  ltu")                                   // [value, data_ptr, idx, ok]
          val pass = newLabel("oob_pass")
          emit(s"  jumpnz $pass")
          emit("  halt")
          emit(s"$pass:")                                 // [value, data_ptr, idx]
        case _ =>
          // PtrType (raw, unsafe — no check) and any other fallthrough.
          genExpr(index)
      emitPushInt(elemType.sizeOf)
      emit("  mul")
      emit("  add")
      emitStore(elemType)

    case TFieldAssignStmt(obj, fieldIndex, value) =>
      val st = structOf(obj.typ)
      val off = fieldOffset(st, fieldIndex)
      val fieldType = st.fields(fieldIndex)._2
      genExpr(value)
      genStructAddr(obj)
      if off != 0 then
        emitPushInt(off)
        emit("  add")
      emitStore(fieldType)

    case TReturnStmt(Some(expr)) =>
      genExpr(expr)
      maybeCoerceReturnToEnum(expr.typ)
      emitDefers()
      emitFunctionExitRefDecrs()
      emit("  ret")

    case TReturnStmt(None) =>
      emitDefers()
      emitFunctionExitRefDecrs()
      emit("  ret")

    case TWhileStmt(cond, body, userLabel) =>
      val loopLabel = newLabel("while")
      val endLabel = newLabel("while_end")
      breakLabels.push(endLabel)
      continueLabels.push(loopLabel)
      loopNameStack.push(userLabel)
      emit(s"$loopLabel:")
      genExpr(cond)
      emit(s"  jumpz $endLabel")
      genStmts(body)
      emit(s"  jump $loopLabel")
      emit(s"$endLabel:")
      loopNameStack.pop()
      breakLabels.pop()
      continueLabels.pop()

    case TForStmt(init, cond, update, body, userLabel) =>
      val loopLabel = newLabel("for")
      val updateLabel = newLabel("for_upd")
      val endLabel = newLabel("for_end")
      genStmt(init)
      breakLabels.push(endLabel)
      continueLabels.push(updateLabel)
      loopNameStack.push(userLabel)
      emit(s"$loopLabel:")
      genExpr(cond)
      emit(s"  jumpz $endLabel")
      genStmts(body)
      emit(s"$updateLabel:")
      genStmt(update)
      emit(s"  jump $loopLabel")
      emit(s"$endLabel:")
      loopNameStack.pop()
      breakLabels.pop()
      continueLabels.pop()

    case TDoWhileStmt(cond, body, userLabel) =>
      val loopLabel = newLabel("do")
      val endLabel = newLabel("do_end")
      breakLabels.push(endLabel)
      continueLabels.push(loopLabel)
      loopNameStack.push(userLabel)
      emit(s"$loopLabel:")
      genStmts(body)
      genExpr(cond)
      emit(s"  jumpnz $loopLabel")
      emit(s"$endLabel:")
      loopNameStack.pop()
      breakLabels.pop()
      continueLabels.pop()

    case TLoopStmt(body, userLabel) =>
      val loopLabel = newLabel("loop")
      val endLabel = newLabel("loop_end")
      breakLabels.push(endLabel)
      continueLabels.push(loopLabel)
      loopNameStack.push(userLabel)
      emit(s"$loopLabel:")
      genStmts(body)
      emit(s"  jump $loopLabel")
      emit(s"$endLabel:")
      loopNameStack.pop()
      breakLabels.pop()
      continueLabels.pop()

    case TBreakStmt(lbl) =>
      val idx = resolveLoopIdx(lbl)
      emit(s"  jump ${breakLabels(idx)}")

    case TContinueStmt(lbl) =>
      val idx = resolveLoopIdx(lbl)
      emit(s"  jump ${continueLabels(idx)}")

    case TExprStmt(TMatchExpr(scrutinee, arms, default, matchTyp)) =>
      genMatch(scrutinee, arms, default, matchTyp, asExpr = matchTyp != SyslType.UnitType)
      if matchTyp != SyslType.UnitType then emit("  drop")

    case TExprStmt(TIfExpr(cond, thenBody, elseBody, ifTyp)) if ifTyp == SyslType.UnitType =>
      // Void-typed if-stmt: generate bodies as plain statements (no synthetic
      // 0 push, which would leak onto the data stack because the outer
      // TExprStmt won't drop void-typed values).
      val elseLabel = newLabel("else")
      val endLabel = newLabel("endif")
      genExpr(cond)
      emit(s"  jumpz $elseLabel")
      genStmts(thenBody)
      emit(s"  jump $endLabel")
      emit(s"$elseLabel:")
      elseBody match
        case Some(stmts) => genStmts(stmts)
        case None =>
      emit(s"$endLabel:")

    case TExprStmt(expr) =>
      genExpr(expr)
      if expr.typ != SyslType.UnitType then emit("  drop")

    case TAsmStmt(code) =>
      emit(s"  $code")

    case TDeferStmt(body) =>
      // Allocate a counter slot the first time we see this defer-site (keyed
      // by body identity), then bump the counter at this point in the
      // control flow. emitDefers replays the body `counter` times in a
      // while-loop at every fn-exit path. The slot is zero-initialised by
      // the `frame N` opcode at fn entry — DO NOT emit a push_0/local_set
      // here, because if this defer is inside a loop body the explicit reset
      // would zero the counter on every iteration and the defer would only
      // ever fire once.
      val slot = deferSiteSlot.getOrElseUpdate(body, {
        deferBodies += body
        val s = nextLocalIndex
        nextLocalIndex += 1
        s
      })
      emit(s"  local_get $slot")
      emit("  inc")
      emit(s"  local_set $slot")

    case TMultiStmt(children) =>
      children.foreach(genStmt)

    case TContractCheck(kind, expr, message) =>
      // Emit a kind-tagged trap on failure rather than a bare `halt`. The
      // SVM `trap u8` opcode (0x6A) calls `handleTrap(num)`; the default impl
      // halts on any non-zero number, but a debugging harness can override
      // it to recover the kind. The `; <kind>: <message>` comment is emitted
      // immediately above the trap so the message survives in the asm output
      // — historically this was discarded entirely, audit item #17.
      genExpr(expr)
      val pass = newLabel("contract_pass")
      emit(s"  jumpnz $pass")
      val tag = if message == kind then kind else s"$kind: $message"
      emit(s"  ; $tag")
      emit("  trap 1")
      emit(s"$pass:")

    case TFieldCompoundAssignStmt(obj, fieldIndex, op, value) =>
      val st = structOf(obj.typ)
      val off = fieldOffset(st, fieldIndex)
      val fieldType = st.fields(fieldIndex)._2
      // Load current value
      genStructAddr(obj)
      if off != 0 then { emitPushInt(off); emit("  add") }
      emit("  dup") // keep address
      emitLoad(fieldType)
      genExpr(value)
      emitBinaryOp(op, fieldType)
      emit("  swap") // ( new_val addr )
      emitStore(fieldType)

    case TDestructureStmt(names, types, init) =>
      genExpr(init) // address of struct on stack
      for (name, i) <- names.zipWithIndex do
        if name != "_" then
          val idx = allocLocal(name, types(i))
          val st = structOf(init.typ)
          val off = fieldOffset(st, i)
          emit("  dup") // keep struct addr
          if off != 0 then { emitPushInt(off); emit("  add") }
          emitLoad(types(i))
          emit(s"  local_set $idx")
      emit("  drop") // discard struct address

    case TDestructureAssignStmt(names, types, init) =>
      // Like TDestructureStmt, but the names already refer to existing locals.
      genExpr(init)
      for (name, i) <- names.zipWithIndex do
        if name != "_" then
          val st = structOf(init.typ)
          val off = fieldOffset(st, i)
          val target = locals.getOrElse(name, {
            val idx = allocLocal(name, types(i))
            LocalInfo(idx, types(i))
          })
          emit("  dup")
          if off != 0 then { emitPushInt(off); emit("  add") }
          emitLoad(types(i))
          emit(s"  local_set ${target.index}")
      emit("  drop")

    case _ => sys.error(s"unhandled TStmt in SVM codegen: ${stmt.getClass.getSimpleName}")

