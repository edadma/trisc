package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

trait SyslSVMCodegenExpressions:
  self: SyslSVMCodegen =>

  // ========================================================================
  // genExpr — leaves exactly one value on the data stack
  // ========================================================================
  def genExpr(expr: TExpr): Unit = expr match
    case TIntLit(n, _) => emitPushInt(n)

    case TFloatLit(d, _) =>
      val bits = java.lang.Double.doubleToLongBits(d)
      if bits == 0L then emit("  push_f0")
      else if d == 1.0 then emit("  push_f1")
      else emit(s"  push_i64 $bits")

    case TBoolLit(true, _) => emit("  push_1")
    case TBoolLit(false, _) => emit("  push_0")

    case TUnitLit(_) => emit("  push_0")  // unit is 0-byte; represent at runtime as 0

    case TSizeof(size, _) => emitPushInt(size)

    case TVarRef(name, typ) =>
      // Captures (when compiling a hoisted closure body): read from env_ptr
      // (local 0) at the capture's offset.
      closureCaptures.get(name) match
        case Some((off, capTyp)) =>
          emit("  local_get 0")               // env_ptr
          if off > 0 then { emitPushInt(off); emit("  add") }
          // For aggregates, the address into env IS the value. For scalars, load.
          if !needsMemAlloc(capTyp) && capTyp != SyslType.StringType && !capTyp.isInstanceOf[SyslType.SliceType] then
            emitLoad(capTyp)
          return
        case None =>
      locals.get(name) match
        case Some(LocalInfo(idx, localTyp)) if addressedLocals.contains(name) && !needsMemAlloc(localTyp) && localTyp != SyslType.StringType && !localTyp.isInstanceOf[SyslType.SliceType] =>
          // Addressed scalar: load through the cell's pointer.
          emit(s"  local_get $idx")
          emitLoad(localTyp)
        case Some(LocalInfo(idx, _)) => emit(s"  local_get $idx")
        case None =>
          // Global. Scalars load the cell; aggregates (string / slice /
          // struct / enum / array) are address-represented so the symbol's
          // address IS the value.
          emit(s"  push_i64 $name")
          typ.underlying match
            case _: SyslType.StructType | _: SyslType.EnumType
               | SyslType.StringType | _: SyslType.SliceType
               | _: SyslType.ArrayType => ()
            case _ => emit("  load64")

    case TAddrOf(name, _) =>
      locals.get(name) match
        case Some(LocalInfo(idx, typ)) if needsMemAlloc(typ) =>
          // Aggregate local: the local already holds the memory address
          emit(s"  local_get $idx")
        case Some(LocalInfo(idx, typ)) if addressedLocals.contains(name) =>
          // Addressed scalar: the local already holds the cell's pointer.
          emit(s"  local_get $idx")
        case Some(LocalInfo(idx, typ)) =>
          // Scalar local, not pre-flagged as addressed. Spill to a new slot
          // — caveat: subsequent modifications through this pointer will
          // NOT sync back to the local (fallback path for unscanned uses).
          emitMemAlloc(8)
          emit("  dup")
          emit(s"  local_get $idx")
          emit("  swap")
          emit("  store64")
        case None =>
          emit(s"  push_i64 $name")

    case TAddrOfIndex(array, index, typ) =>
      genExpr(array)
      val elemType = array.typ.underlying match
        case SyslType.ArrayType(e, _) => e
        case SyslType.PtrType(e) => e
        case SyslType.SliceType(e) => e
        case SyslType.RefType(SyslType.SliceType(e)) => e
        case _ => SyslType.I64
      array.typ.underlying match
        case SyslType.SliceType(_) | SyslType.RefType(SyslType.SliceType(_)) =>
          emit("  load64") // slice struct → data ptr
        case _ =>
      genExpr(index)
      emitPushInt(elemType.sizeOf)
      emit("  mul")
      emit("  add")

    case TAddrOfField(obj, fieldIndex, typ) =>
      genStructAddr(obj)
      val st = obj.typ match
        case s: SyslType.StructType => s
        case SyslType.RefType(s: SyslType.StructType) => s
        case SyslType.PtrType(s: SyslType.StructType) => s
        case _ => sys.error(s"field addr on non-struct: ${obj.typ}")
      val off = fieldOffset(st, fieldIndex)
      if off != 0 then
        emitPushInt(off)
        emit("  add")

    case TDeref(ptr, typ) =>
      genExpr(ptr)
      // Aggregates are address-represented; dereferencing a pointer to one
      // is a no-op — the pointer value IS the aggregate "value".
      if !needsMemAlloc(typ) && typ != SyslType.StringType && !typ.isInstanceOf[SyslType.SliceType] then
        emitLoad(typ)

    case TIndex(array, index, typ) =>
      genExpr(array)
      val elemType = array.typ match
        case SyslType.ArrayType(e, _) => e
        case SyslType.PtrType(e) => e
        case SyslType.SliceType(e) => e
        case SyslType.RefType(SyslType.SliceType(e)) => e
        case SyslType.StringType => SyslType.UIntType(8)
        case _ => typ
      // Bounds check before deref. PtrType is the unsafe escape hatch and
      // skips the check by design; arrays use the compile-time size; slice /
      // ref-slice / string read the runtime length from the descriptor at
      // offset 8 (field is i32 — load32 zero-extends).
      // The check halts on failure (unsigned compare catches negatives too).
      // After the check the stack still holds [base, idx] before the load.
      // Bounds check before the load. PtrType is the unsafe escape hatch and
      // skips the check by design. The check halts on failure (unsigned compare
      // catches negatives too). Stack manipulation only — locals can't be used
      // here because countLocals' pre-pass doesn't see expression-scoped temps,
      // and an unaccounted local_set would land past the declared frame size.
      array.typ match
        case SyslType.ArrayType(_, size) =>
          genExpr(index)                                  // [base, idx]
          emit("  dup")                                   // [base, idx, idx]
          emitPushInt(size)
          emit("  ltu")                                   // [base, idx, ok]
          val pass = newLabel("oob_pass")
          emit(s"  jumpnz $pass")
          emit("  halt")
          emit(s"$pass:")                                 // [base, idx]
        case SyslType.PtrType(_) =>
          genExpr(index)
        case SyslType.SliceType(_) | SyslType.RefType(SyslType.SliceType(_)) | SyslType.StringType =>
          // Slice descriptor: ptr@0 (i64), len@8 (i32 for slices, i64 for
          // strings — the two SVM layouts diverge here; see emitStringFromSlice).
          val isString = array.typ == SyslType.StringType
          emit("  dup")                                   // [base, base]
          emit("  load64")                                // [base, data_ptr]
          emit("  swap")                                  // [data_ptr, base]
          emitPushInt(8)
          emit("  add")
          emit(if isString then "  load64" else "  load32") // [data_ptr, len]
          genExpr(index)                                  // [data_ptr, len, idx]
          emit("  dup")                                   // [data_ptr, len, idx, idx]
          emit("  rot")                                   // [data_ptr, idx, idx, len]
          emit("  ltu")                                   // [data_ptr, idx, ok]
          val pass = newLabel("oob_pass")
          emit(s"  jumpnz $pass")
          emit("  halt")
          emit(s"$pass:")                                 // [data_ptr, idx]
        case _ =>
          genExpr(index)
      emitPushInt(elemType.sizeOf)
      emit("  mul")
      emit("  add")
      emitLoad(typ)

    case TFieldAccess(obj, fieldIndex, typ) =>
      genStructAddr(obj)
      val st = canonicalStruct(obj.typ.underlying match
        case s: SyslType.StructType => s
        case SyslType.RefType(s) => s.underlying match
          case ss: SyslType.StructType => ss
          case _ => sys.error(s"field access on non-struct: ${obj.typ}")
        case SyslType.PtrType(s) => s.underlying match
          case ss: SyslType.StructType => ss
          case _ => sys.error(s"field access on non-struct: ${obj.typ}")
        case _ => sys.error(s"field access on non-struct: ${obj.typ}"))
      val off = fieldOffset(st, fieldIndex)
      if off != 0 then
        emitPushInt(off)
        emit("  add")
      emitLoad(typ)

    case TBinary(left, "&&", right, _) =>
      val falseLabel = newLabel("and_f")
      val endLabel = newLabel("and_end")
      genExpr(left)
      emit(s"  jumpz $falseLabel")
      genExpr(right)
      emit(s"  jumpz $falseLabel")
      emit("  push_1")
      emit(s"  jump $endLabel")
      emit(s"$falseLabel:")
      emit("  push_0")
      emit(s"$endLabel:")

    case TBinary(left, "||", right, _) =>
      val trueLabel = newLabel("or_t")
      val endLabel = newLabel("or_end")
      genExpr(left)
      emit(s"  jumpnz $trueLabel")
      genExpr(right)
      emit(s"  jumpnz $trueLabel")
      emit("  push_0")
      emit(s"  jump $endLabel")
      emit(s"$trueLabel:")
      emit("  push_1")
      emit(s"$endLabel:")

    case TBinary(left, op @ ("+" | "-"), right, typ) if left.typ.isInstanceOf[SyslType.PtrType] =>
      // Pointer arithmetic: scale the integer operand by pointee size
      val pointee = left.typ.asInstanceOf[SyslType.PtrType].pointee
      genExpr(left)
      genExpr(right)
      val elemSize = pointee.sizeOf
      if elemSize != 1 then
        emitPushInt(elemSize)
        emit("  mul")
      emitBinaryOp(op, SyslType.I64)

    case TBinary(left, "+", right, SyslType.StringType) =>
      genExpr(left)
      genExpr(right)
      emit("  call __svm_str_concat")
      needsStrConcat = true

    case TBinary(left, op @ ("==" | "!="), right, _) if left.typ == SyslType.StringType =>
      genExpr(left)
      genExpr(right)
      emit("  call __svm_str_eq")
      if op == "!=" then emit("  eqz")
      needsStrEq = true

    case TBinary(left, op @ ("<" | "<=" | ">" | ">="), right, _) if left.typ == SyslType.StringType =>
      // Lexicographic byte-wise compare via __svm_str_cmp (returns signed
      // 3-way: negative / zero / positive). Reduce to bool with the matching
      // zero-relative predicate.
      genExpr(left)
      genExpr(right)
      emit("  call __svm_str_cmp")
      op match
        case "<"  => emit("  ltz")
        case "<=" => emit("  lez")
        case ">"  => emit("  gtz")
        case ">=" => emit("  gez")
      needsStrCmp = true

    case TBinary(left, op, right, typ) =>
      genExpr(left)
      genExpr(right)
      emitBinaryOp(op, left.typ)

    case TUnary("-", operand, _) =>
      genExpr(operand)
      if isFloat(operand.typ) then emit("  fneg")
      else emit("  neg")
      truncateForNarrow(operand.typ)

    case TUnary("!", operand, _) =>
      genExpr(operand)
      emit("  eqz")

    case TUnary("~", operand, _) =>
      genExpr(operand)
      emit("  not")
      truncateForNarrow(operand.typ)

    case TRangeCheck(inner, range, _, _) =>
      genExpr(inner) // stack: [val]
      val failLbl = newLabel("range_fail")
      val passLbl = newLabel("range_pass")
      val u = inner.typ.underlying.isUnsigned
      val f = inner.typ.underlying.isFloat
      def pushNum(n: Any): Unit = n match
        case v: Long => emitPushInt(v)
        case v: Double =>
          val bits = java.lang.Double.doubleToRawLongBits(v)
          emit(s"  push_i64 $bits")
      def geOp(): String = if f then "fge" else if u then "geu" else "ge"
      def ltOp(): String = if f then "flt" else if u then "ltu" else "lt"
      def leOp(): String = if f then "fle" else if u then "leu" else "le"
      range match
        case IntRange(lo, hi, excl) =>
          emit("  dup")
          pushNum(lo)
          emit(s"  ${geOp()}")
          emit(s"  jumpz $failLbl")
          emit("  dup")
          pushNum(hi)
          emit(s"  ${if excl then ltOp() else leOp()}")
          emit(s"  jumpz $failLbl")
        case FloatRange(lo, hi, excl) =>
          emit("  dup")
          pushNum(lo)
          emit(s"  ${geOp()}")
          emit(s"  jumpz $failLbl")
          emit("  dup")
          pushNum(hi)
          emit(s"  ${if excl then ltOp() else leOp()}")
          emit(s"  jumpz $failLbl")
      emit(s"  jump $passLbl")
      emit(s"$failLbl:")
      emit("  halt")
      emit(s"$passLbl:")

    case TStringFromSlice(slice, _) =>
      // []byte -> string: copy the slice's bytes into a fresh memory-stack
      // buffer and return a new 16-byte {ptr, len} descriptor pointing at
      // the copy. The reference (§3231) makes this an explicit copy so
      // later mutation of the source array doesn't alias into the string.
      genExpr(slice)
      val srcIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $srcIdx")
      // Slice layout is {ptr i64 @0, len i32 @8, cap i32 @12, backref ...}.
      // Extract ptr + len-as-i64 (load32 zero-extends).
      val srcPtrIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_get $srcIdx"); emit("  load64"); emit(s"  local_set $srcPtrIdx")
      val lenIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_get $srcIdx"); emitPushInt(8); emit("  add"); emit("  load32")
      emit(s"  local_set $lenIdx")
      val bufIdx = emitDynByteAllocAndCopy(srcPtrIdx, lenIdx)
      emitMemAlloc(16)
      val dstIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $dstIdx")
      emit(s"  local_get $bufIdx"); emit(s"  local_get $dstIdx"); emit("  store64")
      emit(s"  local_get $lenIdx"); emit(s"  local_get $dstIdx"); emitPushInt(8); emit("  add"); emit("  store64")
      emit(s"  local_get $dstIdx")

    case TStringFromPtr(ptr, len, _) =>
      // string(ptr, len) -> string: copy `len` bytes from `ptr` into a fresh
      // buffer and stash that buffer in the new descriptor. Reference
      // (§3231) prescribes the copy so subsequent writes through `ptr`
      // don't bleed into the string.
      genExpr(ptr)
      val ptrIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $ptrIdx")
      genExpr(len)
      val lenIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $lenIdx")
      val bufIdx = emitDynByteAllocAndCopy(ptrIdx, lenIdx)
      emitMemAlloc(16)
      val dstIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $dstIdx")
      emit(s"  local_get $bufIdx"); emit(s"  local_get $dstIdx"); emit("  store64")
      emit(s"  local_get $lenIdx"); emit(s"  local_get $dstIdx"); emitPushInt(8); emit("  add"); emit("  store64")
      emit(s"  local_get $dstIdx")

    case TCast(inner, target) =>
      genExpr(inner)
      emitCast(inner.typ, target)

    case TCall(name, args, _) =>
      // Push args left-to-right, materializing a slice struct when the param
      // expects a slice and the caller is handing over a fixed array.
      val paramTypes = funcParamTypes.getOrElse(name, Nil)
      for (arg, idx) <- args.zipWithIndex do
        val paramType = paramTypes.lift(idx)
        (arg.typ.underlying, paramType.map(_.underlying)) match
          case (SyslType.ArrayType(_, size), Some(_: SyslType.SliceType)) =>
            emitArrayToSlice(arg, size)
          case _ => genExpr(arg)
      emit(s"  call $name")

    case TStr(inner) =>
      inner.typ.underlying match
        case SyslType.StringType => genExpr(inner) // identity
        case SyslType.BoolType =>
          genExpr(inner)
          emit("  call __svm_str_from_bool")
          needsStrFromBool = true
        case t if t.isIntegral =>
          genExpr(inner)
          // Widen narrow ints to i64 for the runtime helper. Sign-extend signed
          // types; zero-extend unsigned.
          if t.bitWidth < 64 then
            if t.isSigned then
              emitPushInt(64 - t.bitWidth); emit("  shl")
              emitPushInt(64 - t.bitWidth); emit("  sar")
            else
              t.bitWidth match
                case 8 => emitPushInt(0xff); emit("  and")
                case 16 => emitPushInt(0xffff); emit("  and")
                case 32 => emit("  push_i64 4294967295"); emit("  and")
                case _ => ()
          emit("  call __svm_str_from_i64")
          needsStrFromI64 = true
        case _: SyslType.EnumType =>
          // Simple enum (no data variants): use the runtime int helper on the tag.
          // Data-enum variants would need the analyzer's tag-dispatch helpers,
          // which std/ doesn't currently exercise on SVM. Fall through to ???
          // for non-simple enums.
          val isSimple = inner.typ.underlying match
            case SyslType.EnumType(_, vs) => vs.forall(_._2.isEmpty)
            case _ => false
          if isSimple then
            genExpr(inner)
            // Tag is loaded as i32; load it from the address and convert.
            emit("  load32s")
            emit("  call __svm_str_from_i64")
            needsStrFromI64 = true
          else
            emitStrPlaceholder(inner)
        case SyslType.FloatType(64) =>
          genExpr(inner)
          emit("  call __svm_str_from_f64")
          needsStrFromF64 = true
        case SyslType.FloatType(32) =>
          // Widen f32 → f64, then format. SVM expression results for f32 live
          // in the same 8-byte slot as f64; reinterpreting as double is a
          // bit-pattern issue. Use a `dup; fneg; fneg` no-op? Simpler: route
          // through the runtime helper as-is — f32 values in SVM are already
          // stored as f64 bit patterns because the stack is 8 bytes wide.
          // (If f32 ever genuinely materialises here, the helper still treats
          // the bits as f64.)
          genExpr(inner)
          emit("  call __svm_str_from_f64")
          needsStrFromF64 = true
        case _ =>
          emitStrPlaceholder(inner)

    case TTempAddr(inner, _) =>
      inner.typ.underlying match
        case _: SyslType.StructType | _: SyslType.EnumType
           | SyslType.StringType | _: SyslType.SliceType
           | _: SyslType.ArrayType =>
          // Address-represented aggregate: genExpr already returns an address.
          genExpr(inner)
        case _ =>
          // Scalar: spill to 8-byte slot on the memory stack, return slot addr.
          genExpr(inner)
          emitMemAlloc(8)
          emit("  dup")           // (val, slot, slot)
          emit("  rot")           // (slot, slot, val)
          emit("  swap")          // (slot, val, slot)
          emit("  store64")       // stack: (slot)

    case TIfExpr(cond, thenBody, Some(elseBody), typ) =>
      val elseLabel = newLabel("else")
      val endLabel = newLabel("endif")
      genExpr(cond)
      emit(s"  jumpz $elseLabel")
      if typ == SyslType.UnitType then genStmts(thenBody) else genStmtsAsExpr(thenBody)
      emit(s"  jump $endLabel")
      emit(s"$elseLabel:")
      if typ == SyslType.UnitType then genStmts(elseBody) else genStmtsAsExpr(elseBody)
      emit(s"$endLabel:")

    case TIfExpr(cond, thenBody, None, typ) =>
      if typ == SyslType.UnitType then
        val endLabel = newLabel("endif")
        genExpr(cond)
        emit(s"  jumpz $endLabel")
        genStmts(thenBody)
        emit(s"$endLabel:")
      else
        // Non-void if-without-else: skip path needs a synthetic value so the
        // stack is balanced regardless of branch taken. (Analyzer types such
        // expressions non-void based on the then-body's last expression.)
        val elseLabel = newLabel("else")
        val endLabel = newLabel("endif")
        genExpr(cond)
        emit(s"  jumpz $elseLabel")
        genStmtsAsExpr(thenBody)
        emit(s"  jump $endLabel")
        emit(s"$elseLabel:")
        emitPushInt(0)
        emit(s"$endLabel:")

    case TMatchExpr(scrutinee, arms, default, matchTyp) =>
      genMatch(scrutinee, arms, default, matchTyp, asExpr = true)

    case TPreInc(name, _) =>
      val LocalInfo(idx, _) = locals(name): @unchecked
      emit(s"  local_get $idx")
      emit("  inc")
      emit("  dup")
      emit(s"  local_set $idx")

    case TPreDec(name, _) =>
      val LocalInfo(idx, _) = locals(name): @unchecked
      emit(s"  local_get $idx")
      emit("  dec")
      emit("  dup")
      emit(s"  local_set $idx")

    case TPostInc(name, _) =>
      val LocalInfo(idx, _) = locals(name): @unchecked
      emit(s"  local_get $idx")
      emit("  dup")
      emit("  inc")
      emit(s"  local_set $idx")

    case TPostDec(name, _) =>
      val LocalInfo(idx, _) = locals(name): @unchecked
      emit(s"  local_get $idx")
      emit("  dup")
      emit("  dec")
      emit(s"  local_set $idx")

    case TStringLit(value, _) =>
      labelCounter += 1
      val label = if modulePrefix.nonEmpty then s"__str_${modulePrefix}_$labelCounter" else s"__str_$labelCounter"
      stringLiterals += ((label, value))
      val bytes = value.getBytes("ISO-8859-1")
      // String is a 16-byte fat pointer {ptr, len} allocated on memory stack.
      // The label points past the refcount header to the byte data.
      emitMemAlloc(16)
      emit("  dup")
      emit(s"  push_i64 $label") // ptr to byte data
      emit("  swap")
      emit("  store64")          // store ptr at offset 0
      emit("  dup")
      emitPushInt(8)
      emit("  add")
      emitPushInt(bytes.length)
      emit("  swap")
      emit("  store64")          // store len at offset 8

    case TAsmExpr(code, _) =>
      emit(s"  $code")

    case TFmtStr(inner, spec) =>
      // Lower formatted-string interpolations to a runtime helper. For
      // integer verbs we route through __svm_str_fmt_i64 with the appropriate
      // base/width/flag bits. For %s we just pass the string through (with
      // optional padding via __svm_str_fmt_i64 — not supported yet, fall back
      // to the unpadded string).
      val verb = spec.verb
      verb match
        case 'd' | 'x' | 'X' | 'o' | 'b' if inner.typ.isIntegral =>
          genExpr(inner)
          // Widen narrow ints to i64 for the runtime helper.
          val t = inner.typ.underlying
          if t.bitWidth < 64 then
            if t.isSigned then
              emitPushInt(64 - t.bitWidth); emit("  shl")
              emitPushInt(64 - t.bitWidth); emit("  sar")
            else
              t.bitWidth match
                case 8 => emitPushInt(0xff); emit("  and")
                case 16 => emitPushInt(0xffff); emit("  and")
                case 32 => emit("  push_i64 4294967295"); emit("  and")
                case _ => ()
          val base = verb match
            case 'd' => 10; case 'x' | 'X' => 16; case 'o' => 8; case 'b' => 2
            case _ => 10
          emitPushInt(base)
          emitPushInt(spec.width)
          var flags = 0
          if spec.zeroPad then flags |= 0x1
          if spec.leftAlign then flags |= 0x2
          if spec.showSign then flags |= 0x4
          if spec.upperCase || verb == 'X' then flags |= 0x8
          emitPushInt(flags)
          emit("  call __svm_str_fmt_i64")
          needsStrFmtI64 = true
        case 's' if inner.typ.underlying == SyslType.StringType =>
          genExpr(inner)
          if spec.width > 0 then
            emitPushInt(spec.width)
            emitPushInt(if spec.leftAlign then 1 else 0)
            emit("  call __svm_str_fmt_str")
            needsStrFmtStr = true
        case 'c' if inner.typ.isIntegral =>
          // %c emits a 1-byte string with the value's low 8 bits. Stack-only
          // sequence (countLocals doesn't see TFmtStr-scoped temps, so any
          // anonymous local here would land past the declared frame size at
          // runtime — same trap that surfaced during the bounds-check
          // campaign). Width and alignment flags are intentionally not
          // honored on this path; padded `%c` would be unusual and the
          // existing `%-Ns` path already covers padded strings.
          genExpr(inner)
          emitPushInt(0xff); emit("  and")    // ( byte )
          emitMemAlloc(8);                     // ( byte buf )
          emit("  dup"); emit("  rot")         // ( buf buf byte )
          emit("  swap"); emit("  store8")     // ( buf )   buf[0] = byte
          emitMemAlloc(16)                     // ( buf desc )
          emit("  swap"); emit("  over")       // ( desc buf desc )
          emit("  store64")                    // ( desc )  desc[0..8] = buf
          emit("  dup")                        // ( desc desc )
          emitPushInt(8); emit("  add")        // ( desc desc+8 )
          emitPushInt(1); emit("  swap")       // ( desc 1 desc+8 )
          emit("  store64")                    // ( desc )  desc[8..16] = 1
        case _ =>
          // Any other shape: fall back to plain TStr semantics.
          genExpr(TStr(inner))

    case TQuantifier(kind, name, nameType, lo, hi, inclusive, pred, _) =>
      // Lower to a short-circuiting loop. `result` is the accumulator —
      // starts at 1 for "all" (vacuous truth on empty range) and 0 for
      // "some". On a counterexample (all) or witness (some), set the result
      // and break out of the loop.
      val resultIdx = nextLocalIndex; nextLocalIndex += 1
      val iterIdx = nextLocalIndex; nextLocalIndex += 1
      val endIdx = nextLocalIndex; nextLocalIndex += 1
      val initBit = if kind == "all" then 1 else 0
      emitPushInt(initBit)
      emit(s"  local_set $resultIdx")
      genExpr(lo)
      emit(s"  local_set $iterIdx")
      genExpr(hi)
      if !inclusive then emit("  dec")
      emit(s"  local_set $endIdx")
      // Bind the loop variable so genExpr(pred) finds it as a regular local.
      val savedBinding = locals.get(name)
      locals(name) = LocalInfo(iterIdx, nameType)
      val condLbl = newLabel("quant_cond")
      val incLbl = newLabel("quant_inc")
      val endLbl = newLabel("quant_end")
      emit(s"$condLbl:")
      emit(s"  local_get $iterIdx")
      emit(s"  local_get $endIdx")
      emit(if nameType.isUnsigned then "  leu" else "  le")
      emit(s"  jumpz $endLbl")
      genExpr(pred)
      if kind == "all" then
        // pred true → continue; pred false → set 0 and break
        emit(s"  jumpnz $incLbl")
        emit("  push_0")
        emit(s"  local_set $resultIdx")
        emit(s"  jump $endLbl")
      else
        // pred true → set 1 and break; pred false → continue
        emit(s"  jumpz $incLbl")
        emit("  push_1")
        emit(s"  local_set $resultIdx")
        emit(s"  jump $endLbl")
      emit(s"$incLbl:")
      emit(s"  local_get $iterIdx")
      emit("  inc")
      emit(s"  local_set $iterIdx")
      emit(s"  jump $condLbl")
      emit(s"$endLbl:")
      // Restore prior binding (or remove the synthetic one).
      savedBinding match
        case Some(b) => locals(name) = b
        case None => locals.remove(name)
      emit(s"  local_get $resultIdx")

    case TIntrinsicCall(intrName, args, retTyp) =>
      // Compiler intrinsics — wrapping/saturating arithmetic. SVM int ops
      // wrap naturally for i64; for narrow types `emitBinaryOp` already
      // truncates. Saturating variants need explicit overflow detection.
      intrName match
        case "wrapping_add" | "wrapping_sub" | "wrapping_mul" =>
          genExpr(args(0))
          genExpr(args(1))
          val op = intrName.stripPrefix("wrapping_") match
            case "add" => "+"; case "sub" => "-"; case "mul" => "*"
          emitBinaryOp(op, retTyp)
        case "saturating_add" | "saturating_sub" | "saturating_mul" =>
          val signed = retTyp.isSigned
          val width = retTyp.bitWidth
          // Bounds for the target type
          val (minV, maxV) = if signed then
            (-(1L << (width - 1)), (1L << (width - 1)) - 1)
          else
            (0L, if width == 64 then -1L else (1L << width) - 1)
          // Stash a, b in temp locals
          val aIdx = nextLocalIndex; nextLocalIndex += 1
          val bIdx = nextLocalIndex; nextLocalIndex += 1
          val rIdx = nextLocalIndex; nextLocalIndex += 1
          genExpr(args(0))
          emit(s"  local_set $aIdx")
          genExpr(args(1))
          emit(s"  local_set $bIdx")
          // r = wrapping op (full i64 then truncate at the end)
          emit(s"  local_get $aIdx")
          emit(s"  local_get $bIdx")
          val op = intrName match
            case "saturating_add" => "+"
            case "saturating_sub" => "-"
            case "saturating_mul" => "*"
          emit(op match { case "+" => "  add"; case "-" => "  sub"; case "*" => "  mul" })
          emit(s"  local_set $rIdx")
          val satLbl = newLabel("sat_done")
          if !signed then
            // unsigned overflow detection:
            // add: width<64 → r > MAX; width=64 → r < a (wrap)
            // sub: a < b → underflow → set 0
            // mul: if a != 0 && r/a != b → overflow → set MAX
            intrName match
              case "saturating_add" =>
                if width < 64 then
                  emit(s"  local_get $rIdx"); emitPushInt(maxV); emit("  gtu")
                else
                  emit(s"  local_get $rIdx"); emit(s"  local_get $aIdx"); emit("  ltu")
                val notOv = newLabel("sat_no_ov")
                emit(s"  jumpz $notOv")
                emitPushInt(maxV)
                emit(s"  local_set $rIdx")
                emit(s"$notOv:")
              case "saturating_sub" =>
                emit(s"  local_get $aIdx"); emit(s"  local_get $bIdx")
                emit("  ltu")
                val notUf = newLabel("sat_no_uf")
                emit(s"  jumpz $notUf")
                emit("  push_0")
                emit(s"  local_set $rIdx")
                emit(s"$notUf:")
              case "saturating_mul" =>
                // For narrow widths the wrapping result already truncated; check
                // against MAX.  For i64, use divu by a to detect overflow.
                if width < 64 then
                  emit(s"  local_get $rIdx"); emitPushInt(maxV); emit("  gtu")
                  val notOv = newLabel("sat_no_ov")
                  emit(s"  jumpz $notOv")
                  emitPushInt(maxV); emit(s"  local_set $rIdx")
                  emit(s"$notOv:")
                else
                  // 64-bit unsigned saturating_mul: if a != 0 && r/a != b → overflow.
                  emit(s"  local_get $aIdx"); emit("  push_0"); emit("  neq")
                  val skip = newLabel("sat_skip")
                  emit(s"  jumpz $skip")     // a == 0 → r already 0, no overflow
                  emit(s"  local_get $rIdx"); emit(s"  local_get $aIdx"); emit("  divu")
                  emit(s"  local_get $bIdx"); emit("  neq")
                  val notOv = newLabel("sat_no_ov")
                  emit(s"  jumpz $notOv")
                  emit("  push_m1")          // unsigned MAX = -1
                  emit(s"  local_set $rIdx")
                  emit(s"$notOv:")
                  emit(s"$skip:")
              case _ => ()
          else
            // signed overflow detection
            intrName match
              case "saturating_add" =>
                if width < 64 then
                  // Narrow signed add: SVM does the sum in full i64, so
                  // r doesn't wrap yet — overflow is whether r escapes
                  // [minV, maxV]. r > maxV → MAX; r < minV → MIN.
                  val noOv = newLabel("sat_no_ov")
                  val tryUf = newLabel("sat_try_uf")
                  emit(s"  local_get $rIdx"); emitPushInt(maxV); emit("  gt")
                  emit(s"  jumpz $tryUf")
                  emitPushInt(maxV); emit(s"  local_set $rIdx")
                  emit(s"  jump $noOv")
                  emit(s"$tryUf:")
                  emit(s"  local_get $rIdx"); emitPushInt(minV); emit("  lt")
                  emit(s"  jumpz $noOv")
                  emitPushInt(minV); emit(s"  local_set $rIdx")
                  emit(s"$noOv:")
                else
                  // 64-bit signed add: wrapping has already happened in r,
                  // so detect by sign pattern of inputs vs result.
                  // overflow if (a >= 0 && b >= 0 && r < 0) → MAX
                  // underflow if (a < 0 && b < 0 && r >= 0) → MIN
                  val noOv = newLabel("sat_no_ov")
                  val checkUf = newLabel("sat_check_uf")
                  emit(s"  local_get $aIdx"); emit("  push_0"); emit("  lt")
                  emit(s"  jumpnz $checkUf") // a < 0 → check underflow
                  emit(s"  local_get $bIdx"); emit("  push_0"); emit("  lt")
                  emit(s"  jumpnz $noOv")    // b < 0 → no overflow
                  emit(s"  local_get $rIdx"); emit("  push_0"); emit("  lt")
                  emit(s"  jumpz $noOv")     // r >= 0 → no overflow
                  emitPushInt(maxV); emit(s"  local_set $rIdx")
                  emit(s"  jump $noOv")
                  emit(s"$checkUf:")
                  emit(s"  local_get $bIdx"); emit("  push_0"); emit("  lt")
                  emit(s"  jumpz $noOv")     // b >= 0 → no underflow
                  emit(s"  local_get $rIdx"); emit("  push_0"); emit("  lt")
                  emit(s"  jumpnz $noOv")    // r < 0 → no underflow
                  emitPushInt(minV); emit(s"  local_set $rIdx")
                  emit(s"$noOv:")
              case "saturating_sub" =>
                if width < 64 then
                  // Narrow signed sub: same range-check shape as narrow add.
                  val noOv = newLabel("sat_no_ov")
                  val tryUf = newLabel("sat_try_uf")
                  emit(s"  local_get $rIdx"); emitPushInt(maxV); emit("  gt")
                  emit(s"  jumpz $tryUf")
                  emitPushInt(maxV); emit(s"  local_set $rIdx")
                  emit(s"  jump $noOv")
                  emit(s"$tryUf:")
                  emit(s"  local_get $rIdx"); emitPushInt(minV); emit("  lt")
                  emit(s"  jumpz $noOv")
                  emitPushInt(minV); emit(s"  local_set $rIdx")
                  emit(s"$noOv:")
                else
                  // 64-bit signed sub: wrapping happened in r, detect by signs.
                  // overflow if (a >= 0 && b < 0 && r < 0) → MAX
                  // underflow if (a < 0 && b >= 0 && r >= 0) → MIN
                  val noOv = newLabel("sat_no_ov")
                  val checkUf = newLabel("sat_check_uf")
                  emit(s"  local_get $aIdx"); emit("  push_0"); emit("  lt")
                  emit(s"  jumpnz $checkUf")
                  emit(s"  local_get $bIdx"); emit("  push_0"); emit("  lt")
                  emit(s"  jumpz $noOv")
                  emit(s"  local_get $rIdx"); emit("  push_0"); emit("  lt")
                  emit(s"  jumpz $noOv")
                  emitPushInt(maxV); emit(s"  local_set $rIdx")
                  emit(s"  jump $noOv")
                  emit(s"$checkUf:")
                  emit(s"  local_get $bIdx"); emit("  push_0"); emit("  lt")
                  emit(s"  jumpnz $noOv")
                  emit(s"  local_get $rIdx"); emit("  push_0"); emit("  lt")
                  emit(s"  jumpnz $noOv")
                  emitPushInt(minV); emit(s"  local_set $rIdx")
                  emit(s"$noOv:")
              case "saturating_mul" =>
                // For narrow widths: check against [minV, maxV].
                if width < 64 then
                  val skip = newLabel("sat_skip")
                  val ov = newLabel("sat_ov")
                  emit(s"  local_get $rIdx"); emitPushInt(maxV); emit("  gt")
                  emit(s"  jumpnz $ov")
                  emit(s"  local_get $rIdx"); emitPushInt(minV); emit("  lt")
                  emit(s"  jumpz $skip")
                  emit(s"$ov:")
                  // sign of (a XOR b) determines clamp direction
                  emit(s"  local_get $aIdx"); emit(s"  local_get $bIdx"); emit("  xor")
                  emit("  push_0"); emit("  lt")
                  val negSign = newLabel("sat_neg")
                  emit(s"  jumpnz $negSign")
                  emitPushInt(maxV); emit(s"  local_set $rIdx")
                  emit(s"  jump $skip")
                  emit(s"$negSign:")
                  emitPushInt(minV); emit(s"  local_set $rIdx")
                  emit(s"$skip:")
                else
                  // 64-bit signed saturating_mul: omit (rare; std/ doesn't use)
                  ()
              case _ => ()
          emit(s"$satLbl:")
          emit(s"  local_get $rIdx")
        case _ =>
          sys.error(s"unsupported intrinsic '$intrName' on SVM backend")

    case TAddrLit(fpOffset) =>
      // Address relative to the frame pointer — used only by hidden return
      // slot args, which SVM doesn't use. Push the local slot's address as
      // the byte offset; rely on the fact that locals are 8-byte cells.
      // Since SVM doesn't have a frame-relative address mode, surface this as
      // an error if it ever gets exercised — std/ doesn't reach here.
      sys.error(s"TAddrLit(fp+$fpOffset) unsupported on SVM (no frame-relative addressing)")

    case TFuncRef(name, typ) =>
      // FuncType is a 16-byte aggregate {func_ptr, env_ptr}. Construct a
      // descriptor on the memory stack pointing at a per-function shim that
      // ignores env and forwards to `name`. Without the shim, an indirect
      // call would push env_ptr as a hidden first arg that `name` does not
      // accept.
      val (paramTypes, retType) = typ match
        case SyslType.FuncType(p, r, _, _) => (p, r)
        case _ => (Nil, SyslType.UnitType)
      val shim = shimNameFor(name)
      if !emittedShims.contains(shim) then
        emittedShims += shim
        pendingShims += ((shim, name, paramTypes, retType))
      emitMemAlloc(16)
      emit("  dup")
      emit(s"  push_i64 $shim")
      emit("  swap")
      emit("  store64")            // descr[0] = shim_ptr
      emit("  dup")
      emitPushInt(8)
      emit("  add")
      emit("  push_0")
      emit("  swap")
      emit("  store64")            // descr[8] = 0 (no env)

    case c: TClosure =>
      genClosureExpr(c)

    case TIndirectCall(callee, args, _) =>
      // Closure-style indirect call: callee evaluates to a 16-byte descriptor
      // address. We push env_ptr as a hidden first arg, then explicit args,
      // then load the func_ptr and `callr`. Plain function pointers go through
      // their per-function shim (constructed by TFuncRef) which ignores env.
      callee.typ match
        case _: SyslType.FuncType =>
          genExpr(callee)               // descr_addr
          val descrIdx = nextLocalIndex
          nextLocalIndex += 1
          emit(s"  local_set $descrIdx")
          // Push env_ptr (hidden first arg)
          emit(s"  local_get $descrIdx")
          emitPushInt(8)
          emit("  add")
          emit("  load64")
          // Push explicit args
          for a <- args do genExpr(a)
          // Push func_ptr and callr
          emit(s"  local_get $descrIdx")
          emit("  load64")
          emit("  callr")
        case _ =>
          // Legacy/non-FuncType callee: treat as raw 8-byte function pointer.
          for a <- args do genExpr(a)
          genExpr(callee)
          emit("  callr")

    case TLen(inner, _) =>
      inner.typ match
        case SyslType.StringType =>
          genExpr(inner)
          emitPushInt(8)
          emit("  add")
          emit("  load64")
        case SyslType.SliceType(_) | SyslType.RefType(SyslType.SliceType(_)) =>
          genExpr(inner)
          emitPushInt(8)
          emit("  add")
          emit("  load32")
        case SyslType.ArrayType(_, size) =>
          emitPushInt(size)
        case _ =>
          genExpr(inner)

    case TCap(inner, _) =>
      inner.typ match
        case SyslType.SliceType(_) | SyslType.RefType(SyslType.SliceType(_)) =>
          genExpr(inner)
          emitPushInt(12)
          emit("  add")
          emit("  load32")
        case SyslType.ArrayType(_, size) =>
          emitPushInt(size)
        case _ =>
          genExpr(inner)

    case TStructLit(typ) =>
      // Zero-initialized struct on memory stack
      val size = typ.sizeOf
      emitMemAlloc(size)
      // emitMemAlloc already returns fresh (zeroed by convention? no — we must zero)
      val aligned = ((size + 7) / 8 * 8).toInt
      for i <- 0 until aligned by 8 do
        emit("  dup")
        if i > 0 then { emitPushInt(i); emit("  add") }
        emit("  push_0")
        emit("  swap")
        emit("  store64")

    case TNewArray(elemType, size) =>
      // __svm_new_slice(byteSize, elemCount) — returns pointer to 24-byte slice struct
      genExpr(size)                 // elemCount
      emit("  dup")                 // dup for byteSize computation
      emitPushInt(elemType.sizeOf)
      emit("  mul")                 // byteSize on TOS
      emit("  swap")                // (byteSize, elemCount)
      emit("  call __svm_new_slice")
      needsNewSlice = true

    case TAppend(slice, elem, SyslType.SliceType(elemType)) =>
      val elemSize = elemType.sizeOf
      // Eval slice addr, save to local
      genExpr(slice)
      val sliceIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $sliceIdx")
      // oldPtr = slice.ptr
      emit(s"  local_get $sliceIdx")
      emit("  load64")
      val oldPtrIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $oldPtrIdx")
      // oldLen = slice.len
      emit(s"  local_get $sliceIdx")
      emitPushInt(8)
      emit("  add")
      emit("  load32")
      val oldLenIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $oldLenIdx")
      // Allocate new slice of (oldLen + 1) elements
      emit(s"  local_get $oldLenIdx")
      emit("  inc")
      emit("  dup")
      emitPushInt(elemSize)
      emit("  mul")
      emit("  swap")
      emit("  call __svm_new_slice")
      val newIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $newIdx")
      needsNewSlice = true
      // dst = new.ptr
      emit(s"  local_get $newIdx")
      emit("  load64")
      val dstIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $dstIdx")
      // remaining = oldLen * elemSize
      emit(s"  local_get $oldLenIdx")
      emitPushInt(elemSize)
      emit("  mul")
      val remIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $remIdx")
      // byte copy loop
      val loop = newLabel("app_copy")
      val done = newLabel("app_copy_done")
      emit(s"$loop:")
      emit(s"  local_get $remIdx"); emit("  eqz"); emit(s"  jumpnz $done")
      emit(s"  local_get $oldPtrIdx"); emit("  load8")
      emit(s"  local_get $dstIdx"); emit("  store8")
      emit(s"  local_get $oldPtrIdx"); emit("  inc"); emit(s"  local_set $oldPtrIdx")
      emit(s"  local_get $dstIdx"); emit("  inc"); emit(s"  local_set $dstIdx")
      emit(s"  local_get $remIdx"); emit("  dec"); emit(s"  local_set $remIdx")
      emit(s"  jump $loop")
      emit(s"$done:")
      // Store new element at new.ptr + oldLen * elemSize
      genExpr(elem)
      emit(s"  local_get $newIdx")
      emit("  load64")
      emit(s"  local_get $oldLenIdx")
      emitPushInt(elemSize)
      emit("  mul")
      emit("  add")
      emitStore(elemType)
      // Leave new slice addr on TOS
      emit(s"  local_get $newIdx")

    case TInterfaceBox(inner, iface, owns) =>
      // Box a concrete value into a 16-byte {itable_ptr, data_ptr} struct
      // on the memory stack. For struct values the data_ptr is the struct's
      // backing address; for pointer/ref types the pointer IS the data_ptr.
      // owns=true (set by the analyzer for boxes that escape their source
      // frame, e.g. return position): heap-copy the source struct so the
      // data buffer outlives the source's local slot.
      val structName = inner.typ.underlying match
        case SyslType.StructType(n, _, _) => n
        case SyslType.PtrType(s) => s.underlying match
          case SyslType.StructType(n, _, _) => n
          case other => sys.error(s"TInterfaceBox: unsupported $other")
        case SyslType.RefType(s) => s.underlying match
          case SyslType.StructType(n, _, _) => n
          case other => sys.error(s"TInterfaceBox: unsupported $other")
        case other => sys.error(s"TInterfaceBox: unsupported $other")
      val itableName = s"__itable_${structName}_${iface.name}"
      if !itables.contains(itableName) then
        itables(itableName) = (iface, structName)
      // Evaluate inner — for struct types genExpr leaves the struct address
      // on TOS; for ptr/ref types it leaves the pointer value.
      genExpr(inner)
      val dataIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $dataIdx")
      // Allocate 16-byte iface struct
      emitMemAlloc(16)
      val ifaceIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $ifaceIdx")
      // struct.itable = &itableName
      emit(s"  push_i64 $itableName")
      emit(s"  local_get $ifaceIdx")
      emit("  store64")
      // struct.data = dataPtr
      emit(s"  local_get $dataIdx")
      emit(s"  local_get $ifaceIdx")
      emitPushInt(8)
      emit("  add")
      emit("  store64")
      emit(s"  local_get $ifaceIdx")

    case TInterfaceDispatch(ifaceVal, methodIndex, args, _) =>
      // Load data_ptr (becomes first arg, as implicit self), push user args,
      // then call through itable[methodIndex].
      genExpr(ifaceVal)                      // iface struct addr
      val ifaceIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $ifaceIdx")
      emit(s"  local_get $ifaceIdx")
      emitPushInt(8)
      emit("  add")
      emit("  load64")                        // data_ptr → pushed as first arg
      for a <- args do genExpr(a)
      emit(s"  local_get $ifaceIdx")
      emit("  load64")                        // itable_ptr
      if methodIndex != 0 then
        emitPushInt(methodIndex * 8)
        emit("  add")
      emit("  load64")                        // method fn ptr
      emit("  callr")

    case TNewEnum(et, variantIndex, args) =>
      // Heap-allocated enum variant. SVM has no real heap; allocate on the
      // memory stack and leak per-test (same convention as TNew). The result
      // type is RefType(EnumType) but at the bytecode level the value IS the
      // data pointer — there is no separate rc header on SVM.
      val size = et.sizeOf
      emitMemAlloc(size)
      val aligned = ((size + 7) / 8 * 8).toInt
      for i <- 0 until aligned by 8 do
        emit("  dup")
        if i > 0 then { emitPushInt(i); emit("  add") }
        emit("  push_0")
        emit("  swap")
        emit("  store64")
      // Tag at offset 0 (i32)
      emit("  dup")
      emitPushInt(variantIndex)
      emit("  swap")
      emit("  store32")
      val dataOff = et.dataOffset.toInt
      val variantFields = et.variants(variantIndex)._2
      var fieldOff = 0
      for (arg, i) <- args.zipWithIndex do
        val (_, fieldType) = variantFields(i)
        val align = fieldType.alignOf.toInt.max(1)
        fieldOff = ((fieldOff + align - 1) / align) * align
        emit("  dup")
        val totalOff = dataOff + fieldOff
        if totalOff != 0 then { emitPushInt(totalOff); emit("  add") }
        genExpr(arg)
        emit("  swap")
        emitStore(fieldType)
        fieldOff += fieldType.sizeOf.toInt

    case TNew(structType, args) =>
      // Allocate on memory stack (no real heap in SVM); behaves like
      // TStructConstruct but with an 8-byte refcount header before the data,
      // and the type is RefType(StructType). Returned TOS is the DATA address
      // (= header + 8); user code holds this. emitRefIncr/Decr reach the
      // header via ptr-8.
      val size = structType.sizeOf
      emitNewRefAlloc(size)
      // Zero-init the data area
      val aligned = ((size + 7) / 8 * 8).toInt
      for i <- 0 until aligned by 8 do
        emit("  dup")
        if i > 0 then { emitPushInt(i); emit("  add") }
        emit("  push_0")
        emit("  swap")
        emit("  store64")
      for (arg, i) <- args.zipWithIndex do
        val off = fieldOffset(structType, i)
        val fieldType = structType.fields(i)._2
        emit("  dup")
        if off != 0 then { emitPushInt(off); emit("  add") }
        genExpr(arg)
        emit("  swap")
        emitStore(fieldType)

    case TSliceExpr(array, lowOpt, highOpt, resultTyp)
        if array.typ == SyslType.StringType && resultTyp == SyslType.StringType =>
      // String sub-slice — result is a fresh 16-byte string descriptor
      // {ptr, len(i64)}, NOT the 24-byte slice struct used for []T. Mixing
      // the two layouts corrupts every downstream string consumer (concat,
      // interp, equality) because they all read len at offset 8 as i64.
      val baseIdx = nextLocalIndex; nextLocalIndex += 1
      val srcLenIdx = nextLocalIndex; nextLocalIndex += 1
      genExpr(array)
      emit("  dup")
      emit("  load64")          // ptr
      emit(s"  local_set $baseIdx")
      emitPushInt(8)
      emit("  add")
      emit("  load64")          // len (i64, string layout)
      emit(s"  local_set $srcLenIdx")
      val loIdx = nextLocalIndex; nextLocalIndex += 1
      lowOpt match
        case Some(e) => genExpr(e); emit(s"  local_set $loIdx")
        case None    => emitPushInt(0); emit(s"  local_set $loIdx")
      val hiIdx = nextLocalIndex; nextLocalIndex += 1
      highOpt match
        case Some(e) => genExpr(e); emit(s"  local_set $hiIdx")
        case None    => emit(s"  local_get $srcLenIdx"); emit(s"  local_set $hiIdx")
      emitMemAlloc(16)
      val descIdx = nextLocalIndex; nextLocalIndex += 1
      emit("  dup")
      emit(s"  local_set $descIdx")
      // desc.ptr = base + lo   (byte advance — element size is 1)
      emit(s"  local_get $baseIdx")
      emit(s"  local_get $loIdx")
      emit("  add")
      emit("  swap")
      emit("  store64")
      // desc.len = hi - lo   (i64)
      emit(s"  local_get $hiIdx")
      emit(s"  local_get $loIdx")
      emit("  sub")
      emit(s"  local_get $descIdx")
      emitPushInt(8)
      emit("  add")
      emit("  store64")
      emit(s"  local_get $descIdx")

    case TSliceExpr(array, lowOpt, highOpt, resultTyp) =>
      // Allocate a 24-byte slice struct on memory stack, fill with
      //   ptr = base + lo * elemSize
      //   len = hi - lo
      //   cap = hi - lo
      //   backref = 0
      val elemType = resultTyp match
        case SyslType.SliceType(e) => e
        case SyslType.RefType(SyslType.SliceType(e)) => e
        case _ => SyslType.I64
      // Compute base pointer + source length based on array.typ
      val baseIdx = nextLocalIndex; nextLocalIndex += 1
      val lenIdx = nextLocalIndex; nextLocalIndex += 1
      array.typ match
        case SyslType.ArrayType(_, n) =>
          genExpr(array)
          emit(s"  local_set $baseIdx")
          emitPushInt(n)
          emit(s"  local_set $lenIdx")
        case SyslType.SliceType(_) =>
          // array is address of 24-byte slice struct
          genExpr(array)
          emit("  dup")             // keep addr
          emit("  load64")          // ptr
          emit(s"  local_set $baseIdx")
          emitPushInt(8)
          emit("  add")
          emit("  load32")          // len (i32)
          emit(s"  local_set $lenIdx")
        case SyslType.RefType(SyslType.SliceType(_)) =>
          // refs are pointers to slice structs in our impl; treat as slice
          genExpr(array)
          emit("  dup")
          emit("  load64")
          emit(s"  local_set $baseIdx")
          emitPushInt(8)
          emit("  add")
          emit("  load32")
          emit(s"  local_set $lenIdx")
        case _ =>
          genExpr(array)
          emit(s"  local_set $baseIdx")
          emitPushInt(0)
          emit(s"  local_set $lenIdx")
      // Evaluate lo (default 0)
      val loIdx = nextLocalIndex; nextLocalIndex += 1
      lowOpt match
        case Some(e) => genExpr(e); emit(s"  local_set $loIdx")
        case None    => emitPushInt(0); emit(s"  local_set $loIdx")
      // Evaluate hi (default len)
      val hiIdx = nextLocalIndex; nextLocalIndex += 1
      highOpt match
        case Some(e) => genExpr(e); emit(s"  local_set $hiIdx")
        case None    => emit(s"  local_get $lenIdx"); emit(s"  local_set $hiIdx")
      // Allocate 24-byte slice struct
      emitMemAlloc(24)
      val structIdx = nextLocalIndex; nextLocalIndex += 1
      emit("  dup")
      emit(s"  local_set $structIdx")
      // struct.ptr = base + lo * elemSize
      emit(s"  local_get $baseIdx")
      emit(s"  local_get $loIdx")
      emitPushInt(elemType.sizeOf)
      emit("  mul")
      emit("  add")
      emit("  swap")                // (ptr, struct_addr)
      emit("  store64")
      // struct.len = hi - lo
      emit(s"  local_get $hiIdx")
      emit(s"  local_get $loIdx")
      emit("  sub")
      emit(s"  local_get $structIdx")
      emitPushInt(8)
      emit("  add")
      emit("  store32")
      // struct.cap = hi - lo
      emit(s"  local_get $hiIdx")
      emit(s"  local_get $loIdx")
      emit("  sub")
      emit(s"  local_get $structIdx")
      emitPushInt(12)
      emit("  add")
      emit("  store32")
      // struct.backref = 0
      emit("  push_0")
      emit(s"  local_get $structIdx")
      emitPushInt(16)
      emit("  add")
      emit("  store64")
      // leave struct addr on TOS
      emit(s"  local_get $structIdx")

    case TEnumConstruct(et, variantIndex, args) =>
      // Allocate enum on memory stack, zero-init, populate tag + variant fields
      val size = et.sizeOf
      emitMemAlloc(size)
      val aligned = ((size + 7) / 8 * 8).toInt
      for i <- 0 until aligned by 8 do
        emit("  dup")
        if i > 0 then { emitPushInt(i); emit("  add") }
        emit("  push_0")
        emit("  swap")
        emit("  store64")
      // Tag at offset 0 (i32)
      emit("  dup")
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
        emit("  dup") // keep enum addr
        val totalOff = dataOff + fieldOff
        if totalOff != 0 then { emitPushInt(totalOff); emit("  add") }
        genExpr(arg)
        emit("  swap")
        emitStore(fieldType)
        fieldOff += fieldType.sizeOf.toInt

    case TStructConstruct(structType, args) =>
      // Allocate struct on memory stack, populate fields
      val size = structType.sizeOf
      emitMemAlloc(size)
      // Zero-init first
      val aligned = ((size + 7) / 8 * 8).toInt
      for i <- 0 until aligned by 8 do
        emit("  dup")
        if i > 0 then { emitPushInt(i); emit("  add") }
        emit("  push_0")
        emit("  swap")
        emit("  store64")
      // Store each field
      for (arg, i) <- args.zipWithIndex do
        val off = fieldOffset(structType, i)
        val fieldType = structType.fields(i)._2
        emit("  dup") // keep struct addr
        if off != 0 then { emitPushInt(off); emit("  add") }
        genExpr(arg)
        emit("  swap")
        emitStore(fieldType)

    case TFieldPreInc(obj, fieldIndex, typ) =>
      val st = structOf(obj.typ)
      val off = fieldOffset(st, fieldIndex)
      val fieldType = st.fields(fieldIndex)._2
      genStructAddr(obj)
      if off != 0 then { emitPushInt(off); emit("  add") }
      emit("  dup") // keep address
      emitLoad(fieldType)
      emit("  inc")
      emit("  dup")  // ( addr new_val new_val )
      emit("  rot")  // ( new_val new_val addr )
      emitStore(fieldType)

    case TFieldPreDec(obj, fieldIndex, typ) =>
      val st = structOf(obj.typ)
      val off = fieldOffset(st, fieldIndex)
      val fieldType = st.fields(fieldIndex)._2
      genStructAddr(obj)
      if off != 0 then { emitPushInt(off); emit("  add") }
      emit("  dup")
      emitLoad(fieldType)
      emit("  dec")
      emit("  dup")
      emit("  rot")
      emitStore(fieldType)

    case TFieldPostInc(obj, fieldIndex, typ) =>
      val st = structOf(obj.typ)
      val off = fieldOffset(st, fieldIndex)
      val fieldType = st.fields(fieldIndex)._2
      genStructAddr(obj)
      if off != 0 then { emitPushInt(off); emit("  add") }
      emit("  dup")
      emitLoad(fieldType)
      emit("  dup")  // ( addr old_val old_val )
      emit("  inc")  // ( addr old_val new_val )
      emit("  rot")  // ( old_val new_val addr )
      emitStore(fieldType)

    case TFieldPostDec(obj, fieldIndex, typ) =>
      val st = structOf(obj.typ)
      val off = fieldOffset(st, fieldIndex)
      val fieldType = st.fields(fieldIndex)._2
      genStructAddr(obj)
      if off != 0 then { emitPushInt(off); emit("  add") }
      emit("  dup")
      emitLoad(fieldType)
      emit("  dup")
      emit("  dec")
      emit("  rot")
      emitStore(fieldType)

    case TArrayLit(elements, arrType) =>
      // Allocate the array on the memory stack, populate elements, leave
      // base address on TOS. Element layout matches `[N]T`: each slot at
      // offset `i * elemType.sizeOf`.
      val (elemType, declaredLen) = arrType match
        case SyslType.ArrayType(e, n) => (e, n.toInt)
        case _                        => (SyslType.I64, elements.length)
      val len = declaredLen.max(elements.length)
      val size = elemType.sizeOf * len
      emitMemAlloc(size)
      // Zero-init the whole region first (so any tail past `elements.length`
      // is well-defined; matches the TVarDecl/TStructConstruct paths).
      val aligned = ((size + 7) / 8 * 8).toInt
      for i <- 0 until aligned by 8 do
        emit("  dup")
        if i > 0 then { emitPushInt(i); emit("  add") }
        emit("  push_0")
        emit("  swap")
        emit("  store64")
      // Now write each element. Stack invariant during the loop: ( base ).
      for (elem, i) <- elements.zipWithIndex do
        emit("  dup")                          // ( base base )
        if i != 0 then
          emitPushInt(i * elemType.sizeOf)
          emit("  add")                        // ( base base+offset )
        genExpr(elem)                          // ( base base+offset value )
        emit("  swap")                         // ( base value base+offset )
        emitStore(elemType)                    // ( base )

    case _ =>
      sys.error(s"unhandled TExpr in SVM codegen: ${expr.getClass.getSimpleName}")

