package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

trait SyslSVMCodegenMatch:
  self: SyslSVMCodegen =>

  // Recursively emit a discriminator check for a (possibly nested) match
  // pattern. The outer scrutinee value's address is in local `scrIdx`.
  // `absOff` is the offset from the scrutinee's address where this nested
  // sub-value lives. For variant patterns, loads the tag at the field
  // address (i32 at offset 0 of the nested enum) and `jumpz`-es to
  // `failLabel` on mismatch; recurses for any deeper nested patterns.
  // For struct destructure patterns, recurses without a discriminator
  // check. Other pattern shapes (TWildcard / primitives) act as
  // wildcards in nested position.
  def emitNestedPatternCheckSVM(
      pat: TMatchPattern,
      fieldType: SyslType,
      scrIdx: Int,
      absOff: Int,
      failLabel: String,
  ): Unit = pat match
    case TWildcard => ()
    case TVariantPattern(et, variantIndex, _, _, deeperNested) =>
      // Push field address (= scrutinee addr + absOff)
      emit(s"  local_get $scrIdx")
      if absOff != 0 then { emitPushInt(absOff); emit("  add") }
      // Load tag (i32 at offset 0 of the nested enum)
      emit("  load32")
      emitPushInt(variantIndex)
      emit("  eq")
      emit(s"  jumpz $failLabel")
      // Recurse into deeper nested
      val variantFields = et.variants(variantIndex)._2
      val dataOff = et.dataOffset.toInt
      var fieldOff = 0
      for ((deeperOpt, i) <- deeperNested.zipWithIndex) do
        val (_, deeperFieldType) = variantFields(i)
        val align = deeperFieldType.alignOf.toInt.max(1)
        fieldOff = ((fieldOff + align - 1) / align) * align
        deeperOpt.foreach { deeper =>
          emitNestedPatternCheckSVM(deeper, deeperFieldType, scrIdx, absOff + dataOff + fieldOff, failLabel)
        }
        fieldOff += deeperFieldType.sizeOf.toInt
    case TDestructurePattern(st, _, _, deeperNested) =>
      for ((deeperOpt, i) <- deeperNested.zipWithIndex) do
        deeperOpt.foreach { deeper =>
          val deeperFieldType = st.fields(i)._2
          val off = fieldOffset(st, i).toInt
          emitNestedPatternCheckSVM(deeper, deeperFieldType, scrIdx, absOff + off, failLabel)
        }
    case _ => () // primitive nested patterns — treat as wildcard

  // Recursively emit name bindings for a (possibly nested) match pattern.
  // The outer scrutinee value's address is in local `scrIdx`. `absOff`
  // is the offset from the scrutinee where this nested sub-value lives.
  // Each named binding inside the nested pattern allocates a new local
  // and copies the field value (loaded relative to scrutinee + absOff +
  // local field offset).
  def emitNestedPatternBindingsSVM(
      pat: TMatchPattern,
      fieldType: SyslType,
      scrIdx: Int,
      absOff: Int,
  ): Unit = pat match
    case TVariantPattern(et, variantIndex, bindings, fieldTypes, deeperNested) =>
      val variantFields = et.variants(variantIndex)._2
      val dataOff = et.dataOffset.toInt
      var fieldOff = 0
      for (((binding, ft), i) <- bindings.zip(fieldTypes).zipWithIndex) do
        val align = ft.alignOf.toInt.max(1)
        fieldOff = ((fieldOff + align - 1) / align) * align
        binding.foreach { name =>
          val localIdx = nextLocalIndex
          nextLocalIndex += 1
          locals(name) = LocalInfo(localIdx, ft)
          emit(s"  local_get $scrIdx")
          val totalOff = absOff + dataOff + fieldOff
          if totalOff != 0 then { emitPushInt(totalOff); emit("  add") }
          emitLoad(ft)
          emit(s"  local_set $localIdx")
        }
        if i < deeperNested.length then deeperNested(i).foreach { deeper =>
          emitNestedPatternBindingsSVM(deeper, ft, scrIdx, absOff + dataOff + fieldOff)
        }
        fieldOff += ft.sizeOf.toInt
    case TDestructurePattern(st, bindings, fieldTypes, deeperNested) =>
      for (((binding, ft), i) <- bindings.zip(fieldTypes).zipWithIndex) do
        val off = fieldOffset(st, i).toInt
        binding.foreach { name =>
          val localIdx = nextLocalIndex
          nextLocalIndex += 1
          locals(name) = LocalInfo(localIdx, ft)
          emit(s"  local_get $scrIdx")
          val totalOff = absOff + off
          if totalOff != 0 then { emitPushInt(totalOff); emit("  add") }
          emitLoad(ft)
          emit(s"  local_set $localIdx")
        }
        if i < deeperNested.length then deeperNested(i).foreach { deeper =>
          emitNestedPatternBindingsSVM(deeper, ft, scrIdx, absOff + off)
        }
    case _ => ()

  /** Generate a match expression. If asExpr, each body leaves a value on the stack. */
  def genMatch(scrutinee: TExpr, arms: List[TMatchArm], default: Option[List[TStmt]], matchTyp: SyslType, asExpr: Boolean): Unit =
    val scrIdx = nextLocalIndex
    nextLocalIndex += 1
    genExpr(scrutinee)
    emit(s"  local_set $scrIdx")
    val endLabel = newLabel("match_end")
    for arm <- arms do
      val hitLabel = newLabel("match_hit")
      val nextArm = newLabel("match_next")
      for pat <- arm.patterns do pat match
        case TWildcard =>
          emit(s"  jump $hitLabel")
        case TBindPattern(_, _) =>
          // Binding pattern matches anything; the actual name->slot
          // wiring happens after the hit label below.
          emit(s"  jump $hitLabel")
        case TValuePattern(v) =>
          genExpr(v)
          emit(s"  local_get $scrIdx")
          if scrutinee.typ == SyslType.StringType then
            // Strings are 16-byte fat pointers; the generic `eq` opcode
            // compares only the descriptor addresses (each TStringLit
            // allocates a fresh descriptor, so two equal-content strings
            // never compare equal under raw eq). Route through the
            // dedicated byte-wise __svm_str_eq helper.
            emit("  call __svm_str_eq")
            needsStrEq = true
          else
            scrutinee.typ.underlying match
              case et: SyslType.EnumType if et.variants.forall(_._2.isEmpty) =>
                // Simple-enum scrutinee is stored as a pointer to an enum
                // buffer (tag at offset 0). Pattern compares against the
                // variant's i32 value, so deref the tag first.
                emit("  load32")
              case _ =>
            emit("  eq")
          emit(s"  jumpnz $hitLabel")
        case TRangePattern(lo, hi) =>
          val rangeNext = newLabel("match_rng")
          emit(s"  local_get $scrIdx")
          genExpr(lo)
          emit(if scrutinee.typ.isUnsigned then "  geu" else "  ge")
          emit(s"  jumpz $rangeNext")
          emit(s"  local_get $scrIdx")
          genExpr(hi)
          emit(if scrutinee.typ.isUnsigned then "  leu" else "  le")
          emit(s"  jumpnz $hitLabel")
          emit(s"$rangeNext:")
        case TDestructurePattern(st, _, _, nested) =>
          if nested.forall(_.isEmpty) then
            emit(s"  jump $hitLabel")
          else
            val patFail = newLabel("pat_fail")
            for ((subOpt, i) <- nested.zipWithIndex) do subOpt.foreach { sub =>
              val off = fieldOffset(st, i)
              emitNestedPatternCheckSVM(sub, st.fields(i)._2, scrIdx, off.toInt, patFail)
            }
            emit(s"  jump $hitLabel")
            emit(s"$patFail:")
        case TVariantPattern(et, variantIndex, _, _, nested) =>
          // Load tag (i32 at offset 0 of enum), compare with variant index
          emit(s"  local_get $scrIdx")
          emit("  load32")
          emitPushInt(variantIndex)
          emit("  eq")
          if nested.forall(_.isEmpty) then
            emit(s"  jumpnz $hitLabel")
          else
            val patFail = newLabel("pat_fail")
            emit(s"  jumpz $patFail")
            val variantFields = et.variants(variantIndex)._2
            val dataOff = et.dataOffset.toInt
            var fieldOff = 0
            for ((subOpt, i) <- nested.zipWithIndex) do
              val (_, fieldType) = variantFields(i)
              val align = fieldType.alignOf.toInt.max(1)
              fieldOff = ((fieldOff + align - 1) / align) * align
              subOpt.foreach { sub =>
                emitNestedPatternCheckSVM(sub, fieldType, scrIdx, dataOff + fieldOff, patFail)
              }
              fieldOff += fieldType.sizeOf.toInt
            emit(s"  jump $hitLabel")
            emit(s"$patFail:")
      emit(s"  jump $nextArm")
      emit(s"$hitLabel:")
      // Bind destructure/variant pattern fields to locals before guard
      for pat <- arm.patterns do pat match
        case TBindPattern(name, typ) =>
          // Top-level binding: alias the user's name to the scrutinee slot.
          // No copy needed — arm body won't mutate the synthetic slot.
          locals(name) = LocalInfo(scrIdx, typ)
        case TVariantPattern(et, variantIndex, bindings, _, nested) =>
          val dataOff = et.dataOffset.toInt
          val variantFields = et.variants(variantIndex)._2
          var fieldOff = 0
          for (binding, i) <- bindings.zipWithIndex do
            val (_, fieldType) = variantFields(i)
            val align = fieldType.alignOf.toInt.max(1)
            fieldOff = ((fieldOff + align - 1) / align) * align
            binding.foreach { name =>
              val localIdx = nextLocalIndex
              nextLocalIndex += 1
              locals(name) = LocalInfo(localIdx, fieldType)
              emit(s"  local_get $scrIdx")
              val totalOff = dataOff + fieldOff
              if totalOff != 0 then { emitPushInt(totalOff); emit("  add") }
              emitLoad(fieldType)
              emit(s"  local_set $localIdx")
            }
            if i < nested.length then nested(i).foreach { sub =>
              emitNestedPatternBindingsSVM(sub, fieldType, scrIdx, dataOff + fieldOff)
            }
            fieldOff += fieldType.sizeOf.toInt
        case TDestructurePattern(st, bindings, _, nested) =>
          for (binding, i) <- bindings.zipWithIndex do
            val fieldType = st.fields(i)._2
            binding.foreach { name =>
              val localIdx = nextLocalIndex
              nextLocalIndex += 1
              locals(name) = LocalInfo(localIdx, fieldType)
              val off = fieldOffset(st, i)
              emit(s"  local_get $scrIdx")
              if off != 0 then { emitPushInt(off); emit("  add") }
              emitLoad(fieldType)
              emit(s"  local_set $localIdx")
            }
            if i < nested.length then nested(i).foreach { sub =>
              emitNestedPatternBindingsSVM(sub, fieldType, scrIdx, fieldOffset(st, i).toInt)
            }
        case _ =>
      arm.guard.foreach { g =>
        genExpr(g)
        emit(s"  jumpz $nextArm")
      }
      if asExpr then genStmtsAsExpr(arm.body)
      else genStmts(arm.body)
      emit(s"  jump $endLabel")
      emit(s"$nextArm:")
    default match
      case Some(stmts) =>
        if asExpr then genStmtsAsExpr(stmts)
        else genStmts(stmts)
      case None =>
        if asExpr then emitPushInt(0)
    emit(s"$endLabel:")

