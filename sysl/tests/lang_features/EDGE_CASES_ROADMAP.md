Sysl Language Features — Edge-Case Hardening Roadmap
======================================================

This file tracks **edge cases inside well-tested feature areas** —
the corners of established categories that are likely to hide
backend-divergence bugs because the common case is green but a
specific boundary value, empty input, NaN, INT_MIN, or similar
has never been exercised against the runtime.

Distinct from [`ROADMAP.md`](ROADMAP.md): that file tracks whether
each *feature* has any coverage at all; this one tracks whether
each feature's *corners* are nailed down. A category can be 🟢
in ROADMAP.md and still have 🔴 rows here.

The campaign style is the same as the closed-out feature work:
one `.lsysl` test file per edge-case cluster, runs against all
seven backends, file lands 🟢 only when every backend agrees.
Backend divergences uncovered along the way get filed as compiler
bugs (CLAUDE.md rule #8 — highest priority) and fixed before
shipping the test.

Format (each row): | test file | what it pins |

Status legend:
- 🔴 **Not pinned** — no test for this corner.
- 🟡 **Partial** — file exists but one or more backends diverge,
  bug catalogued.
- 🟢 **Pinned** — all seven backends agree.

---

## 1. Numeric extremes — 🟢

Integer and float boundary values. The campaign already shipped
fixes here recently (TRISC narrow-int sext at `sysl@2c4a30ac0`,
SVM saturating-add at `SyslSVMCodegen.scala:2096`, str-from-f64
at `sysl@8c8016e69`); adjacent corners are now pinned.

| File | Corners pinned |
|---|---|
| `integers/integer_extremes.lsysl` 🟢 | signed wrap at i8/i16/i32/i64 MAX+1 and MIN-1; unsigned wrap at u8/u16/u32 MAX+1 and 0-1; `INT_MIN * -1` and `INT_MIN / -1` wrap-to-`INT_MIN` (C-UB cases pinned to two's-complement); shift by 0; shift by width-1; signed arithmetic right-shift preserves sign; unsigned right-shift zero-fills; modulo with negative operands (truncating semantics — sign of dividend). 20 tests, all 7 backends green on first sweep. |
| `floats/float_extremes.lsysl` 🟢 | signed zeros (`-0.0 == +0.0` but `1.0 / -0.0 == -Inf`); NaN identity (`!= NaN`); all four NaN orderings (`< > <= >=`) are false, including `nan <= nan`; NaN propagates through `+ - *` (incl. `NaN * 0 == NaN`); `Inf - Inf == NaN`, `0 * Inf == NaN`, `Inf + 1 == Inf`; f64↔f32 roundtrip exact for small, lossy for 1e10+1; `0.1 + 0.2 != 0.3`. 15 new tests added on top of 4 pre-existing. **Surfaced an SVM bug**: f64→f32 cast was a silent no-op because `SyslSVMCodegen.emitCast` had no float-narrowing case. Fixed by adding a new `f64tof32` opcode (0xAA) to SVM that rounds through f32 precision; emitter now emits it on f64→f32. |

---

## 2. Empty collections — 🟢

Backends frequently have a fast path that skips length-0 and
mis-handles the allocator. Empty inputs are also where slice
descriptors, string headers, and refcount headers most often
get a stale pointer. No new bugs surfaced — all 7 backends green
on first sweep.

| File | Corners pinned |
|---|---|
| `slices/empty_slice_ops.lsysl` 🟢 | four sub-slice forms produce empty (`s[0:0]`, `s[len:]`, `s[:0]`, `s[i:i]` interior); `for i in 0..<len(empty)` and `for v in empty` both zero-iter; append to borrowed-empty / append-chain / append-past-cap force regrow with data preserved; chained empty sub-slice; `len==0` short-circuit recognises empty; filter-all-out leaves length-0 result. (12 tests) |
| `strings/empty_string_ops.lsysl` 🟢 | concat with empty as right operand / both-empty / triple-empty; `str("")` identity; interpolation `s"[$e]"` and `s"${a}X${b}"` with empties; `f"$e%s"` with empty; slice forms `""[0:0]`, `""[:0]`, `""[:]`; comparison orderings `"" < "x"`, `"x" > ""`, `"" <= ""`, `"" != "x"`; equality across construction paths (literal vs returned vs concat). (17 tests, on top of `string_len_empty.lsysl`'s 9.) |

---

## 3. Format string corners — 🟢

Four format-string bugs landed during the lang_features sweep
(see [[feedback_sysl_fstring_divergences]] — all four fixed at
`sysl@725f635a1` + `b2b0d168c`). This file pinned the untested
neighbours AND surfaced two new cross-backend bugs along the way:

1. **`%c` was silently producing decimal on every backend.** The
   analyzer accepts the verb but each codegen's TFmtStr handler
   fell through to `%d`. Fixed in all 4 codegens: interpreter,
   SVM (stack-only sequence — no locals because countLocals
   doesn't see TFmtStr-scoped temps), LLVM (1-byte buffer alloc +
   struct.string wrap), TRISC (malloc(9) for refcount header + 1
   data byte, then 16-byte descriptor on stack).
2. **SVM `%-Nd` (left-align integer) crashed at runtime** with
   "address not found" — the runtime helper `__svm_str_fmt_i64`
   wrote `pad_count` spaces past the end of the 64-byte digit
   buffer. Fixed by rewriting the leftAlign branch: build the
   result at the START of the buffer (sign, forward-copied
   digits, then spaces) instead of appending past the tail.

| File | Corners pinned |
|---|---|
| `strings/fmt_corners.lsysl` 🟢 | `%08d` with negative (width includes the sign — `-5` → `"-0000005"`); `%Nd` without `0` flag defaults to space padding; `%-Nd` left-aligns integer with spaces on the right; `%+08d` combined sign+zero-pad+width (positive, negative, zero); literal `%%` followed by `%d` and other specs; `%c` for ASCII / control / NUL bytes (each yields a 1-byte string, NOT a decimal representation). 17 tests, all 7 backends green after the two bugs above were fixed. Float verbs (`%f`, `%e`, `.Nf`) are NOT in sysl's spec — verb set is `d/x/X/o/b/s/c`. |

---

## 4. Defer interactions — 🟢

Two defer bugs caught recently: field-self-concat across LLVM
backends (`sysl@c8981cf49`) and defer-on-ARC-return
(`sysl@103a54dba`). After-the-fact corner check finds **zero
new bugs** — the per-defer-site counter scheme handles every
remaining corner uniformly. Three new files + two existing
ones cover the surface:

| File | Corners pinned |
|---|---|
| `defer/defer_lifo.lsysl` 🟢 (existing) | Multiple `defer` in the same block run LIFO (`defer a; defer b; defer c;` → `c, b, a`) |
| `defer/defer_in_loop.lsysl` 🟢 (existing) | Per-iter queuing in `for` and `while`; zero-iter case queues nothing; two sites in one body each get their own counter |
| `defer/defer_with_try.lsysl` 🟢 (new, 9 tests) | `?` operator on `Result` + `Option`: defer queued *before* `?` fires on both Ok-success and Err-early-return paths; defer queued *after* a `?` that short-circuited does NOT fire; three-step chain with each `?` site triggering. |
| `defer/defer_returns.lsysl` 🟢 (new, 7 tests) | Defer body calls a helper that returns — value silently discarded; stacked helpers fire LIFO and discard each return; helper returning `string` (16-byte descriptor) also discarded; `defer panic(msg)` unwinds; `defer assert(false, msg)` unwinds; second-queued defer panic fires first (LIFO before earlier defer); return value captured *at* `return`, defer mutation of same local doesn't reach caller; defer's mutation of caller-shared state via `*Trace` IS observable. The parser does NOT accept `return` as a defer body — that's a compile-time syntactic constraint not exercised here. |
| `defer/defer_in_match.lsysl` 🟢 (new, 11 tests) | Defer inside an indented match arm queues dynamically (only if the arm is taken); defers in both arms compose with one firing per arm; arm-level + fn-level defers compose LIFO at fn exit; multiple stacked defers within one arm fire LIFO; arm body without explicit `return` still queues defer correctly; per-defer-site counter scheme handles match-arm defers inside a `for` loop (per-iter queuing). |

---

## 5. Refcount lifetime corners — 🟢

Recursive-enum auto-box (1 row 🟢). Closure-captured refs and
branch-local refs both surfaced real cross-backend bugs.
Slice-of-refs and asymmetric struct-cycles were already
correct.

| File | Corners pinned / bugs surfaced |
|---|---|
| `arc/ref_self_assign.lsysl` — covered by existing `new_ref_assignment.lsysl` 🟢 | `r = r` is incr-then-decr (existing test) |
| `arc/ref_in_slice.lsysl` 🟢 (new, 7 tests) | `new [n]&T` slice of refs: write/read; alloc/drop loop; cell reassignment drops old occupant; cell read shares storage; sub-slice shares cells with parent; binding copy aliases; single-cell repeated write (200 iters) with no allocator drift. **Zero new bugs**. |
| `arc/ref_in_escaping_closure.lsysl` 🟢 (new, 5 tests) | Captured `&T` ref's refcount must be extended for the closure env. **LLVM-derived backends (llvm-host, riscv64, riscv32, wasm32) were missing both the incref-on-capture and decref-on-deinit for RefType captures** — `emitValueRC` only handled string/struct-with-strings, never RefType. Fix at `SyslLLVMCodegen.scala` TClosure HeapEnv path + `emitClosureEnvDeinit`. SVM/TRISC/interpreter were already correct. |
| `arc/ref_conditional_drop.lsysl` 🟢 (new, 13 tests) | Ref declared in a branch dropped at branch-scope exit, not fn-scope; either-arm both arms; early return inside a branch; reassign in branch; nested branches. **SVM `emitRefDecr` was missing a null-check** — function-exit decref of a branch-local ref slot that was never written (slot=0) tried to load at -8 and trapped. Mirror of LLVM's existing null-check. |
| `arc/ref_mutual_struct.lsysl` 🟢 (new, 4 tests) | Safe asymmetric parent/child: parent owns child via `&`, child carries `*Parent` raw back-pointer. Construction, reassignment, 100-iter alloc/drop loop, bidirectional mutation. The all-`&`-both-ways cycle pattern is intentionally NOT tested — it requires `unowned` (planned in CLAUDE.md TODO) to be safe, and an ARC-without-cycles cycle just leaks. **Zero new bugs**. |

---

## 6. Match exhaustiveness corners — 🔴 P2

Now testable as compile-fail tests via the new
`SyslCompileErrorTests` helper.

| Surface | Pinned in |
|---|---|
| Non-exhaustive on single-variant enum 🔴 | `SyslCompileErrorTests` |
| Guard that's statically true/false 🔴 | `SyslCompileErrorTests` |
| Mixed range + value patterns covering same int 🔴 | `SyslCompileErrorTests` |
| Two patterns binding same name in different arms 🔴 | `SyslCompileErrorTests` |
| Match on a function-call result (rvalue scrutinee) 🔴 | `pattern_matching/match_rvalue_scrutinee.lsysl` |
| Patterns that overlap via int range vs literal 🔴 | `pattern_matching/match_pattern_overlap.lsysl` |

---

## 7. Pointer pathologies — 🟢

The four sub-rows split:

  - **Round-trips** — new file pins both `*&v` and `&*p`
    identities, scalar + struct field + ref field; mutation
    through `&*p` reaches the original; round-trip across
    a function param preserves identity.
  - **Comparison** — already pinned by the existing
    `ptr_compare.lsysl` (same-target equal, different-target
    unequal, retarget changes equality). No new file needed.
  - **Null deref** — intentionally NOT pinned cross-backend.
    Behaviour is non-uniform by design: host-mapped backends
    (interpreter, llvm-host, riscv64/32, wasm32) trap on
    read/write of address 0; raw-memory emulators (SVM,
    TRISC) treat address 0 as a normal byte of the flat
    address space and silently succeed. Pinning would
    require an MMU in SVM/TRISC, not a sysl-language fix.
    `ptr_null.lsysl` already pins null construction +
    comparison; that's the testable surface.
  - **Pointer to zero-length fixed array** — too esoteric to
    pay rent. `[0]T` is degenerate; the meaningful
    pointer-to-element coverage lives in `ptr_arithmetic.
    lsysl` (`&a[0]` for non-empty arrays).

| File | Corners pinned |
|---|---|
| `pointers/ptr_compare.lsysl` 🟢 (existing) | Same/different target equality; retarget |
| `pointers/ptr_roundtrip.lsysl` 🟢 (new, 7 tests) | `*&v` on scalar / struct field; `&*p == p`; mutation through `&*p`; double round-trip `*&*&v`; `*&` through a `&T` ref's field; round-trip across a fn param. Zero new bugs. |
| `pointers/ptr_null.lsysl` 🟢 (existing) | Null construction (`*int(0)`); null/null equality; null/non-null inequality; default-init `*T` is null. (Null *deref* is intentionally backend-divergent and not pinned — see above.) |
| `pointers/ptr_arithmetic.lsysl` 🟢 (existing) | `&a[0]`, `p+n`, walking an array via pointer. (Subsumes the original "pointer to zero-length array" row, which is degenerate.) |

---

## Validation cadence

Same as the main ROADMAP: every test file must pass on all seven
backends before the row flips 🔴 → 🟢. CLAUDE.md rule #11
applies — when a fix lands on a shared codepath, run
`syslJVM/test` + `--backend <B> std/` on every backend except
trisc (full sweep >6h).

When a row uncovers a backend divergence, file the bug in memory
(`feedback_*.md`) AND fix it before shipping the row. Don't ship
a partial 🟡 with an open backend bug — that's exactly the
"backends silently diverge" failure mode CLAUDE.md rule #11
exists to prevent.
