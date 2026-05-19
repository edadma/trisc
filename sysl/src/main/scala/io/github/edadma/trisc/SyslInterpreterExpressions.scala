package io.github.edadma.trisc

import scala.collection.mutable

/** Expression evaluation for `SyslInterpreter` — the `evalAny` switch.
  *
  * Mixed into `SyslInterpreter` via `extends SyslInterpreterExpressions`;
  * self-typed so all helpers (`exec`, `toLong`/`toDouble`, `refIncr`/`refDecr`,
  * pattern-match helpers `matchPattern`/`bindPattern`, conversion helpers, the
  * mutable state `globals`/`functions`/`builtins`/`heapCells`/`heapBreak`/
  * `mmioMemory`/`ptrToAddr`/`addrToPtr`/`nextAddr`/`deferStack`/`deinitMap`,
  * the path-dependent exception types) are reachable without qualification.
  * Pure refactor — no semantic changes from the pre-split monolith.
  */
trait SyslInterpreterExpressions {
  self: SyslInterpreter =>

  import Value.*

  protected def evalAny(expr: TExpr, env: Env): Value =
    expr match
      case TIntLit(n, _) => IntVal(n)
      case TFloatLit(d, _) => FloatVal(d)
      case TBoolLit(b, _) => IntVal(if b then 1L else 0L)
      case TUnitLit(_)    => IntVal(0L)  // 0-byte type — represented as 0 at runtime

      case TStringLit(s, _) => StringVal(s.getBytes("ISO-8859-1"))

      case TArrayDecl(size, typ) =>
        def initElem(t: SyslType): Value = t match
          case SyslType.ArrayType(elem, sz) =>
            val cells = Array.fill(sz)(new Cell(initElem(elem)))
            ArrVal(cells, 0)
          case st: SyslType.StructType => evalAny(TStructLit(st), env)
          case _ => IntVal(0)
        val elemType = typ match
          case SyslType.ArrayType(e, _) => e
          case _ => SyslType.I64
        val cells = Array.fill(size)(new Cell(initElem(elemType)))
        ArrVal(cells, 0)

      case TArrayLit(elements, _) =>
        val cells = elements.map(e => new Cell(evalAny(e, env))).toArray
        ArrVal(cells, 0)

      case TVarRef(name, _) => lookupCell(name, env).value

      case TAddrOf(name, _) => PtrVal(CellPtr(lookupCell(name, env)))

      case TTempAddr(expr, _) =>
        // Evaluate expression, store in a temporary cell, return pointer to it
        val value = evalAny(expr, env)
        val cell = new Cell(value)
        PtrVal(CellPtr(cell))

      case TAddrOfField(obj, fieldIndex, _) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        PtrVal(ArrayPtr(cells, off + fieldIndex))

      case TAddrOfIndex(array, index, _) =>
        val arrVal = evalAny(array, env)
        val idx = toLong(evalAny(index, env)).toInt
        arrVal match
          case ArrVal(cells, off) => PtrVal(ArrayPtr(cells, off + idx))
          case PtrVal(ptr) => PtrVal(ptr.add(idx))
          case _ => PtrVal(CellPtr(indexCell(arrVal, idx)))

      case TPreInc(name, typ) =>
        val cell = lookupCell(name, env)
        cell.value match
          case PtrVal(ptr) =>
            val nv = PtrVal(ptr.add(1))
            cell.value = nv
            nv
          case ArrVal(cells, off) =>
            val nv = ArrVal(cells, off + 1)
            cell.value = nv
            nv
          case _ =>
            val v = truncateNarrow(toLong(cell.value) + 1, typ)
            cell.value = IntVal(v)
            IntVal(v)

      case TPreDec(name, typ) =>
        val cell = lookupCell(name, env)
        cell.value match
          case PtrVal(ptr) =>
            val nv = PtrVal(ptr.sub(1))
            cell.value = nv
            nv
          case ArrVal(cells, off) =>
            val nv = ArrVal(cells, off - 1)
            cell.value = nv
            nv
          case _ =>
            val v = truncateNarrow(toLong(cell.value) - 1, typ)
            cell.value = IntVal(v)
            IntVal(v)

      case TPostInc(name, typ) =>
        val cell = lookupCell(name, env)
        cell.value match
          case old @ PtrVal(ptr) =>
            cell.value = PtrVal(ptr.add(1))
            old
          case old @ ArrVal(cells, off) =>
            cell.value = ArrVal(cells, off + 1)
            old
          case _ =>
            val old = toLong(cell.value)
            cell.value = IntVal(truncateNarrow(old + 1, typ))
            IntVal(old)

      case TPostDec(name, typ) =>
        val cell = lookupCell(name, env)
        cell.value match
          case old @ PtrVal(ptr) =>
            cell.value = PtrVal(ptr.sub(1))
            old
          case old @ ArrVal(cells, off) =>
            cell.value = ArrVal(cells, off - 1)
            old
          case _ =>
            val old = toLong(cell.value)
            cell.value = IntVal(truncateNarrow(old - 1, typ))
            IntVal(old)

      case TDeref(TCast(TIntLit(addr, _), SyslType.PtrType(_)), typ) =>
        // #address MMIO read: pull from the virtual mmio map (0 if never written).
        val raw = mmioMemory.getOrElse(addr, 0L)
        IntVal(truncateNarrow(raw, typ))

      case TDeref(inner, _) =>
        evalAny(inner, env) match
          case RefVal(cells, _, _) => ArrVal(cells, 0)  // deref &Struct → expose struct fields
          case RefEnumVal(tag, fields, _) => EnumVal(tag, fields) // deref &Enum → value enum
          case other => derefCell(other).value

      case TIndex(arr, index, _) =>
        val arrVal = evalAny(arr, env)
        val idx = toLong(evalAny(index, env)).toInt
        arrVal match
          case StringVal(bytes) =>
            if idx < 0 || idx >= bytes.length then throw RuntimeError(s"string index out of bounds: $idx (length ${bytes.length})")
            IntVal(bytes(idx) & 0xff)
          case SliceVal(cells, off, len, _) =>
            if idx < 0 || idx >= len then throw RuntimeError(s"slice index out of bounds: $idx (length $len)")
            cells(off + idx).value
          case _ =>
            indexCell(arrVal, idx).value

      case TSliceExpr(arr, low, high, _) =>
        val arrVal = evalAny(arr, env)
        arrVal match
          case StringVal(bytes) =>
            val lo = low.map(l => toLong(evalAny(l, env)).toInt).getOrElse(0)
            val hi = high.map(h => toLong(evalAny(h, env)).toInt).getOrElse(bytes.length)
            if lo < 0 || hi < lo || hi > bytes.length then
              throw RuntimeError(s"string slice bounds out of range [$lo:$hi] with length ${bytes.length}")
            StringVal(bytes.slice(lo, hi))
          case SliceVal(cells, off, len, cap) =>
            val lo = low.map(l => toLong(evalAny(l, env)).toInt).getOrElse(0)
            val hi = high.map(h => toLong(evalAny(h, env)).toInt).getOrElse(len)
            if lo < 0 || hi < lo || hi > len then
              throw RuntimeError(s"slice bounds out of range [$lo:$hi] with length $len")
            SliceVal(cells, off + lo, hi - lo, cap - lo)
          case RefSliceVal(cells, length, _) =>
            val lo = low.map(l => toLong(evalAny(l, env)).toInt).getOrElse(0)
            val hi = high.map(h => toLong(evalAny(h, env)).toInt).getOrElse(length)
            if lo < 0 || hi < lo || hi > length then
              throw RuntimeError(s"slice bounds out of range [$lo:$hi] with length $length")
            SliceVal(cells, lo, hi - lo, length - lo)
          case ArrVal(cells, off) =>
            val totalLen = cells.length - off
            val lo = low.map(l => toLong(evalAny(l, env)).toInt).getOrElse(0)
            val hi = high.map(h => toLong(evalAny(h, env)).toInt).getOrElse(totalLen)
            if lo < 0 || hi < lo || hi > totalLen then
              throw RuntimeError(s"slice bounds out of range [$lo:$hi] with length $totalLen")
            SliceVal(cells, off + lo, hi - lo, totalLen - lo)
          case _ => throw RuntimeError("cannot sub-slice non-slice value")

      case TAppend(sliceExpr, elemExpr, _) =>
        val sliceVal: SliceVal = evalAny(sliceExpr, env) match
          case s: SliceVal => s
          case RefSliceVal(cells, length, _) => SliceVal(cells, 0, length, length)
          case _ => throw RuntimeError("append requires a slice")
        val newElem = evalAny(elemExpr, env)
        if sliceVal.length < sliceVal.capacity then
          sliceVal.cells(sliceVal.offset + sliceVal.length).value = newElem
          SliceVal(sliceVal.cells, sliceVal.offset, sliceVal.length + 1, sliceVal.capacity)
        else
          val newCap = if sliceVal.capacity == 0 then 1 else sliceVal.capacity * 2
          val newCells = Array.fill(newCap)(new Cell(IntVal(0)))
          for i <- 0 until sliceVal.length do
            newCells(i).value = sliceVal.cells(sliceVal.offset + i).value
          newCells(sliceVal.length).value = newElem
          SliceVal(newCells, 0, sliceVal.length + 1, newCap)

      case TStr(inner) =>
        val v = evalAny(inner, env)
        val s = inner.typ.underlying match
          case SyslType.BoolType =>
            // bool → "true" / "false" — canonical across all seven backends.
            // The numeric "1"/"0" form predates a deliberate choice; aligning
            // here also fixes the prior interpreter divergence flagged in
            // feedback_sysl_str_bool_divergence.md.
            v match
              case IntVal(0) => "false"
              case IntVal(_) => "true"
              case _ => throw RuntimeError(s"str(bool): unexpected non-int value $v")
          case _ =>
            v match
              case IntVal(n) => n.toString
              case FloatVal(d) => formatDouble(d)
              case _ => throw RuntimeError(s"str(): unsupported value $v")
        StringVal(s.getBytes("UTF-8"))

      case TFmtStr(inner, spec) =>
        val v = evalAny(inner, env)
        // %c is special: emit a 1-byte string containing the value's low 8
        // bits, NOT a decimal representation. Width-flag interactions
        // (`%-5c`, etc.) fall through to the post-format padding step
        // identically to other verbs.
        if spec.verb == 'c' then
          val byte = (toLong(v) & 0xFF).toByte
          val singleByte = Array(byte)
          val padded: Array[Byte] = if spec.width > 1 then
            val pad = Array.fill[Byte](spec.width - 1)(' '.toByte)
            if spec.leftAlign then singleByte ++ pad else pad ++ singleByte
          else singleByte
          return StringVal(padded)
        val raw = v match
          case IntVal(n) =>
            val base = spec.verb match
              case 'x' => 16
              case 'o' => 8
              case 'b' => 2
              case _   => 10
            val s = if base == 10 then
              val r = n.toString
              if spec.showSign && n >= 0 then "+" + r else r
            else
              val unsigned = if n < 0 then
                "-" + java.lang.Long.toUnsignedString(-n, base)
              else
                java.lang.Long.toUnsignedString(n, base)
              if spec.upperCase then unsigned.toUpperCase else unsigned
            s
          case FloatVal(d) => d.toString
          case StringVal(b) => new String(b, "UTF-8")
          case _ => throw RuntimeError(s"fmt: unsupported value $v")
        // Apply width padding
        val padded = if spec.width > 0 && raw.length < spec.width then
          val pad = spec.width - raw.length
          if spec.leftAlign then raw + " " * pad
          else if spec.zeroPad && (spec.verb != 's') then
            if raw.startsWith("-") then "-" + "0" * pad + raw.substring(1)
            else if raw.startsWith("+") then "+" + "0" * pad + raw.substring(1)
            else "0" * pad + raw
          else " " * pad + raw
        else raw
        StringVal(padded.getBytes("UTF-8"))

      case TStringFromPtr(ptrExpr, lenExpr, _) =>
        val ptr = evalAny(ptrExpr, env)
        val len = toLong(evalAny(lenExpr, env)).toInt
        val bytes = new Array[Byte](len)
        ptr match
          case PtrVal(p) =>
            for i <- 0 until len do
              bytes(i) = toLong(p.add(i).deref.value).toByte
          case ArrVal(cells, off) =>
            for i <- 0 until len do
              bytes(i) = toLong(cells(off + i).value).toByte
          case _ => throw RuntimeError(s"string(): expected pointer, got $ptr")
        StringVal(bytes)

      case TStringFromSlice(sliceExpr, _) =>
        val (cells, off, slen) = evalAny(sliceExpr, env) match
          case SliceVal(c, o, l, _) => (c, o, l)
          case RefSliceVal(c, l, _) => (c, 0, l)
          case other => throw RuntimeError(s"string() requires a slice, got $other")
        val bytes = new Array[Byte](slen)
        for i <- 0 until slen do
          bytes(i) = toLong(cells(off + i).value).toByte
        StringVal(bytes)

      case TIfExpr(cond, thenBody, elseBody, _) =>
        if toLong(evalAny(cond, env)) != 0 then
          evalBlock(thenBody, env)
        else
          elseBody match
            case Some(stmts) => evalBlock(stmts, env)
            case None => IntVal(0)

      case TQuantifier(kind, name, _, lo, hi, inclusive, pred, _) =>
        val loVal = toLong(evalAny(lo, env))
        val hiVal = toLong(evalAny(hi, env))
        val end = if inclusive then hiVal else hiVal - 1
        val saved = env.get(name)
        val cell = new Cell(IntVal(loVal))
        env(name) = cell
        var result: Long = if kind == "all" then 1L else 0L
        var i = loVal
        var done = false
        while i <= end && !done do
          cell.value = IntVal(i)
          val pv = toLong(evalAny(pred, env))
          if kind == "all" then
            if pv == 0L then { result = 0L; done = true }
          else
            if pv != 0L then { result = 1L; done = true }
          i += 1
        saved match
          case Some(c) => env(name) = c
          case None => env.remove(name)
        IntVal(result)

      case TMatchExpr(scrutinee, arms, default, _) =>
        val sv = evalAny(scrutinee, env)
        val matched = arms.find { arm =>
          val patternMatches = arm.patterns.exists(p => matchPattern(p, sv, env))
          if patternMatches then
            // Bind destructure patterns before checking guard
            arm.patterns.find(p => matchPattern(p, sv, env)).foreach(p => bindPattern(p, sv, env))
            arm.guard.forall(g => toLong(evalAny(g, env)) != 0)
          else false
        }
        matched match
          case Some(arm) => evalBlock(arm.body, env)
          case None =>
            default match
              case Some(stmts) => evalBlock(stmts, env)
              case None => IntVal(0)

      case TBinary(left, op, right, resultType) =>
        val lv = evalAny(left, env)
        (lv, op) match
          case (PtrVal(ptr), "+") =>
            return PtrVal(ptr.add(toLong(evalAny(right, env)).toInt))
          case (PtrVal(ptr), "-") =>
            return PtrVal(ptr.sub(toLong(evalAny(right, env)).toInt))
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

        // String path: concatenation and comparison (lexicographic, byte-wise unsigned).
        (lv, rv) match
          case (StringVal(lb), StringVal(rb)) =>
            return (op match
              case "+" =>
                val newBytes = new Array[Byte](lb.length + rb.length)
                System.arraycopy(lb, 0, newBytes, 0, lb.length)
                System.arraycopy(rb, 0, newBytes, lb.length, rb.length)
                StringVal(newBytes)
              case "==" => IntVal(if java.util.Arrays.equals(lb, rb) then 1L else 0L)
              case "!=" => IntVal(if !java.util.Arrays.equals(lb, rb) then 1L else 0L)
              case "<" | "<=" | ">" | ">=" =>
                val n = math.min(lb.length, rb.length)
                var i = 0
                var diff = 0
                while i < n && diff == 0 do
                  diff = (lb(i) & 0xFF) - (rb(i) & 0xFF)
                  i += 1
                if diff == 0 then diff = lb.length - rb.length
                val ok = op match
                  case "<"  => diff < 0
                  case "<=" => diff <= 0
                  case ">"  => diff > 0
                  case ">=" => diff >= 0
                IntVal(if ok then 1L else 0L)
              case _ => throw RuntimeError(s"unsupported string operator: $op")
            )
          case _ =>

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
        val unsigned = left.typ.isUnsigned
        val raw = op match
          case "+"  => l + r
          case "-"  => l - r
          case "*"  => l * r
          case "/"  =>
            if r == 0 then throw RuntimeError("division by zero")
            else if unsigned then java.lang.Long.divideUnsigned(l, r)
            else l / r
          case "%"  =>
            if r == 0 then throw RuntimeError("modulo by zero")
            else if unsigned then java.lang.Long.remainderUnsigned(l, r)
            else l % r
          case "==" => if l == r then 1L else 0L
          case "!=" => if l != r then 1L else 0L
          case "<"  => if (if unsigned then java.lang.Long.compareUnsigned(l, r) < 0 else l < r) then 1L else 0L
          case ">"  => if (if unsigned then java.lang.Long.compareUnsigned(l, r) > 0 else l > r) then 1L else 0L
          case "<=" => if (if unsigned then java.lang.Long.compareUnsigned(l, r) <= 0 else l <= r) then 1L else 0L
          case ">=" => if (if unsigned then java.lang.Long.compareUnsigned(l, r) >= 0 else l >= r) then 1L else 0L
          case "&"  => l & r
          case "|"  => l | r
          case "^"  => l ^ r
          case "<<" => l << r.toInt
          case ">>" => if unsigned then l >>> r.toInt else l >> r.toInt
          case _    => throw RuntimeError(s"unknown operator: $op")
        IntVal(truncateNarrow(raw, resultType))

      case TUnary(op, operand, resultType) =>
        val v = evalAny(operand, env)
        v match
          case FloatVal(d) =>
            op match
              case "-" => FloatVal(-d)
              case _   => throw RuntimeError(s"unsupported float unary operator: $op")
          case _ =>
            val n = toLong(v)
            val raw = op match
              case "-" => -n
              case "!" => if n == 0 then 1L else 0L
              case "~" => ~n
              case _   => throw RuntimeError(s"unknown unary operator: $op")
            IntVal(truncateNarrow(raw, resultType))

      case TRangeCheck(inner, range, aliasName, _) =>
        val v = evalAny(inner, env)
        import SyslType.*
        range match
          case IntRange(lo, hi, excl) =>
            val n = toLong(v)
            val ok = if excl then n >= lo && n < hi else n >= lo && n <= hi
            if !ok then throw RuntimeError(s"range check failed: $aliasName (value $n out of range ${lo}..${if excl then "<" else ""}${hi})")
          case FloatRange(lo, hi, excl) =>
            val d = toDouble(v)
            val ok = if excl then d >= lo && d < hi else d >= lo && d <= hi
            if !ok then throw RuntimeError(s"range check failed: $aliasName (value $d out of range ${lo}..${if excl then "<" else ""}${hi})")
        v

      case TCast(inner, target) =>
        val v = evalAny(inner, env)
        import SyslType.*
        target.underlying match
          case FloatType(32) => FloatVal(toDouble(v).toFloat.toDouble)  // narrow to f32 precision
          case FloatType(64) => FloatVal(toDouble(v))
          case _: FloatType  => FloatVal(toDouble(v))
          case BoolType => v match
            case FuncVal(_) => IntVal(1L) // function references are always non-null
            case RefVal(_, _, _) | RefEnumVal(_, _, _) | RefSliceVal(_, _, _) | StringVal(_) => IntVal(1L)
            case _ => IntVal(if toLong(v) != 0 then 1L else 0L)
          case IntType(64)  => IntVal(toLong(v))
          case IntType(32)  => IntVal((toLong(v) << 32) >> 32)  // sign-extend from 32 bits
          case IntType(16)  => IntVal((toLong(v) << 48) >> 48)  // sign-extend from 16 bits
          case IntType(8)   => IntVal((toLong(v) << 56) >> 56)  // sign-extend from 8 bits
          case _: IntType   => IntVal(toLong(v))
          case UIntType(64) => IntVal(toLong(v))
          case UIntType(32) => IntVal(toLong(v) & 0xFFFFFFFFL)
          case UIntType(16) => IntVal(toLong(v) & 0xFFFFL)
          case UIntType(8)  => IntVal(toLong(v) & 0xFFL)
          case _: UIntType  => IntVal(toLong(v))
          case _: PtrType =>
            v match
              case PtrVal(_) | ArrVal(_, _) => v  // already a pointer
              // ref-struct → *Struct: wrap the ref's flat field cells in an
              // ArrVal so the resulting pointer's field-access/deref paths see
              // the same struct shape a TAddrOf(localStruct) produces. The
              // wrapper cell shares the `cells` array, so writes through `*p`
              // propagate back through the original ref.
              case RefVal(cells, _, _) => PtrVal(CellPtr(new Cell(ArrVal(cells, 0))))
              // ref-enum → *Enum: same idea — wrap as a value-enum so the
              // pointer's deref/match path matches the EnumVal shape.
              case RefEnumVal(tag, fields, _) =>
                PtrVal(CellPtr(new Cell(EnumVal(tag, fields))))
              case FuncVal(name) => IntVal(0) // func to pointer (address not meaningful in interpreter)
              case IntVal(0) => PtrVal(ArrayPtr(Array.empty[Cell], 0))  // null pointer
              case IntVal(n) => PtrVal(longToPointer(n))  // integer to pointer
              case _ => v
          case _ => v

      case TAsmExpr(_, _) => IntVal(0) // no-op in interpreter

      case TSizeof(size, _) => IntVal(size)

      case TLen(inner, _) =>
        evalAny(inner, env) match
          case StringVal(bytes) => IntVal(bytes.length.toLong)
          case SliceVal(_, _, len, _) => IntVal(len.toLong)
          case ArrVal(cells, _) => IntVal(cells.length.toLong)
          case RefSliceVal(_, length, _) => IntVal(length.toLong)
          case _ => throw RuntimeError("len: unsupported type")

      case TCap(inner, _) =>
        evalAny(inner, env) match
          case SliceVal(_, _, _, cap) => IntVal(cap.toLong)
          case ArrVal(cells, _) => IntVal(cells.length.toLong)
          case RefSliceVal(_, length, _) => IntVal(length.toLong)
          case _ => throw RuntimeError("cap: unsupported type")

      case TIntrinsicCall(name, args, typ) =>
        val a = toLong(evalAny(args(0), env))
        val b = toLong(evalAny(args(1), env))
        val width = typ.bitWidth
        val signed = typ.isSigned
        val (minV, maxV) =
          if signed then (-(1L << (width - 1)), (1L << (width - 1)) - 1)
          else (0L, if width == 64 then -1L else (1L << width) - 1)  // unsigned: -1L = max u64
        def mask(v: Long): Long = truncateNarrow(v, typ)

        // Saturating ops detect overflow by inspecting operand values directly,
        // avoiding BigInt allocation. Mirrors the LLVM / SVM / TRISC inline
        // sequences (carry-flag tricks, signed-overflow predicate, divide-back
        // for mul). Narrow signed widths fit in Long without wrap; narrow
        // unsigned multiplications can exceed signed Long range when both
        // operands approach u32 max, so unsigned ops always use unsigned
        // compares / divideUnsigned.

        def satAdd: Long =
          if signed then
            // Signed: narrow widths can't wrap in Long (operands fit; sum fits).
            // Just clamp against [minV, maxV]. For i64 use the signs predicate.
            if width < 64 then
              val v = a + b
              if v > maxV then maxV else if v < minV then minV else v
            else
              val v = a + b
              // Overflow iff operand signs match and result sign differs.
              if ((a ^ b) >= 0L) && ((a ^ v) < 0L) then
                if a >= 0L then Long.MaxValue else Long.MinValue
              else v
          else
            // Unsigned: a + b in Long fits for u8/u16/u32 without wrap (operands
            // < 2^32, sum < 2^33). For u64 the sum can wrap; carry trick handles
            // both uniformly via unsigned-compare.
            val v = a + b
            if width == 64 then
              if java.lang.Long.compareUnsigned(v, a) < 0 then -1L else v
            else
              if java.lang.Long.compareUnsigned(v, maxV) > 0 then maxV else v

        def satSub: Long =
          if signed then
            if width < 64 then
              val v = a - b
              if v > maxV then maxV else if v < minV then minV else v
            else
              val v = a - b
              // Overflow iff operand signs differ and result sign differs from a's.
              if ((a ^ b) < 0L) && ((a ^ v) < 0L) then
                if a >= 0L then Long.MaxValue else Long.MinValue
              else v
          else
            // Unsigned: underflow iff a < b (unsigned). Clamps to 0.
            if java.lang.Long.compareUnsigned(a, b) < 0 then 0L else a - b

        def satMul: Long =
          if signed then
            if width < 64 then
              // Narrow signed: operands ≤ |2^31|, product fits in Long signed.
              val v = a * b
              if v > maxV then maxV else if v < minV then minV else v
            else
              // i64: special-case the two |i64_min| pairs (|i64_min| > i64_max
              // so they always overflow); then verify via divide-back.
              if a == 0L || b == 0L then 0L
              else if a == Long.MinValue && b == -1L then Long.MaxValue
              else if b == Long.MinValue && a == -1L then Long.MaxValue
              else
                val v = a * b
                if v / b != a then
                  if (a >= 0L) == (b >= 0L) then Long.MaxValue else Long.MinValue
                else v
          else
            // Unsigned: u32 × u32 can exceed signed Long range, so the
            // wrapped Long `v` interprets the LOW 64 bits. For narrow widths
            // the true u64 product still fits in u64 (no high-64 carry), so
            // an unsigned compare of v vs maxV detects width overflow.
            // For u64, true product can carry past u64; divideUnsigned
            // detects that case.
            val v = a * b
            if width == 64 then
              if a != 0L && java.lang.Long.divideUnsigned(v, a) != b then -1L
              else v
            else
              if java.lang.Long.compareUnsigned(v, maxV) > 0 then maxV else v

        name match
          case "wrapping_add" => IntVal(mask(a + b))
          case "wrapping_sub" => IntVal(mask(a - b))
          case "wrapping_mul" => IntVal(mask(a * b))
          case "saturating_add" => IntVal(satAdd)
          case "saturating_sub" => IntVal(satSub)
          case "saturating_mul" => IntVal(satMul)
          case other => throw RuntimeError(s"unknown intrinsic: $other")

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

      case TStructLit(SyslType.StructType(_, fields, _)) =>
        val cells = fields.map((_, typ) => new Cell(zeroValueForType(typ, env))).toArray
        ArrVal(cells, 0)

      case TNew(SyslType.StructType(name, fields, _), args) =>
        val cells = fields.zip(args).map { case ((_, _), arg) =>
          new Cell(evalAny(arg, env))
        }.toArray
        RefVal(cells, new java.util.concurrent.atomic.AtomicInteger(1), name)

      case TNewEnum(_, variantIndex, args) =>
        val cells = args.map(arg => new Cell(evalAny(arg, env))).toArray
        RefEnumVal(variantIndex, cells, new java.util.concurrent.atomic.AtomicInteger(1))

      case TNewArray(elemType, sizeExpr) =>
        val n = toLong(evalAny(sizeExpr, env)).toInt
        val cells = Array.fill(n)(new Cell(zeroValueForType(elemType, env)))
        RefSliceVal(cells, n, new java.util.concurrent.atomic.AtomicInteger(1))

      case TStructConstruct(SyslType.StructType(_, fields, _), args) =>
        val cells = fields.zip(args).map { case ((_, typ), arg) =>
          val value = evalAny(arg, env)
          new Cell(value)
        }.toArray
        ArrVal(cells, 0)

      case TEnumConstruct(_, variantIndex, args) =>
        val cells = args.map(arg => new Cell(evalAny(arg, env))).toArray
        EnumVal(variantIndex, cells)

      case TFieldAccess(obj, fieldIndex, _) =>
        val struct = evalAny(obj, env) match
          case arr: ArrVal => arr
          case PtrVal(ptr) => ptr.deref.value.asInstanceOf[ArrVal]  // auto-deref pointer to struct
          case other => throw RuntimeError(s"cannot access field on $other")
        struct.cells(struct.offset + fieldIndex).value

      case TFuncRef(name, _) => FuncVal(name)

      case TClosure(params, _, body, captures, _, _, selfName) =>
        // Capture current values by value (copy). The self-name (if any) is bound to a
        // fresh cell after the ClosureVal is built so the body can recurse via name.
        val capturedEnv = new mutable.LinkedHashMap[String, Cell]
        for (varName, _) <- captures if !selfName.contains(varName) do
          val cell = lookupCell(varName, env)
          capturedEnv(varName) = new Cell(cell.value) // copy value, not share cell
        val closureVal = ClosureVal(body, params, capturedEnv)
        selfName.foreach { n => capturedEnv(n) = new Cell(closureVal) }
        closureVal

      case TInterfaceBox(expr, iface, _) =>
        val dataVal = evalAny(expr, env)
        // Build method map: interface method name → actual registered function name
        val structName = expr.typ match
          case SyslType.StructType(name, _, _) => name
          case SyslType.PtrType(SyslType.StructType(name, _, _)) => name
          case SyslType.RefType(SyslType.StructType(name, _, _)) => name
          case other => throw RuntimeError(s"cannot box $other into interface")
        val methodMap = iface.methods.map { (mname, _, _, _) =>
          val shortKey = s"${structName}_$mname"
          // Try short name first, then search for mangled variant
          val funcName = functions.get(shortKey) match
            case Some(f) => f.name
            case None =>
              functions.values.find(f => f.name.endsWith(s"__$shortKey"))
                .map(_.name)
                .getOrElse(shortKey) // fallback to short name
          (mname, funcName)
        }.toMap
        InterfaceVal(methodMap, dataVal, expr.typ)

      case TInterfaceDispatch(ifaceVal, methodIndex, args, _) =>
        val InterfaceVal(methodMap, dataVal, concreteType) = evalAny(ifaceVal, env): @unchecked
        val iface = ifaceVal.typ.asInstanceOf[SyslType.InterfaceType]
        val (methodName, _, _, _) = iface.methods(methodIndex)
        val funcName = methodMap(methodName)
        val argValues = args.map(evalAny(_, env))
        // Build self arg — for value types, wrap in a cell so the method can modify via pointer
        val selfArg = concreteType match
          case _: SyslType.StructType =>
            // Wrap data in a single-element array to create a pointer-like cell
            val cells = Array(new Cell(dataVal))
            PtrVal(ArrayPtr(cells, 0))
          case _ => dataVal // already a pointer or ref
        functions.get(funcName) match
          case Some(fun) => call(fun, selfArg :: argValues)
          case None => throw RuntimeError(s"interface dispatch: undefined method '$funcName'")

      case TCall(name, args, _) =>
        val argValues = args.map(evalAny(_, env))
        functions.get(name) match
          case Some(fun) => call(fun, argValues)
          case None =>
            builtins.get(name) match
              case Some(f) => f(argValues)
              case None => throw RuntimeError(s"undefined function: $name")

      case TIndirectCall(callee, args, _) =>
        val calleeVal = evalAny(callee, env)
        val argValues = args.map(evalAny(_, env))
        calleeVal match
          case FuncVal(name) =>
            functions.get(name) match
              case Some(fun) => call(fun, argValues)
              case None =>
                builtins.get(name) match
                  case Some(f) => f(argValues)
                  case None => throw RuntimeError(s"undefined function: $name")
          case c: ClosureVal => invokeClosure(c, argValues)
          case other => throw RuntimeError(s"cannot call ${other}")
}
