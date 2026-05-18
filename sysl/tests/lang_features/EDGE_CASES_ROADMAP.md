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

## 4. Defer interactions — 🔴 P1

Two defer bugs caught recently: field-self-concat across LLVM
backends (`sysl@c8981cf49`) and defer-on-ARC-return
(`sysl@103a54dba`). Untested:

| File | Corners pinned |
|---|---|
| `defer/defer_multiple_lifo.lsysl` 🔴 | Multiple `defer` in the same block run LIFO (`defer a; defer b; defer c;` → `c, b, a`); a captured side-effect at defer-decl time is read at defer-fire time |
| `defer/defer_in_loop.lsysl` 🔴 | `defer` inside a `for` body — does it fire per-iter or once at fn exit? Document and pin the answer |
| `defer/defer_with_try.lsysl` 🔴 | `defer` + early return via `?` operator (and via plain `return`); `defer` fires on BOTH success and error exit paths |
| `defer/defer_returns.lsysl` 🔴 | `defer` whose body itself contains a `return` (or panics); does it preempt the surrounding return value? |
| `defer/defer_in_match.lsysl` 🔴 | `defer` in a match arm — scope-bound or fn-bound? |

---

## 5. Refcount lifetime corners — 🔴 P1

Recursive-enum auto-box was one slice (now 🟢). Other refcount
shapes that haven't been pinned across all backends:

| File | Corners pinned |
|---|---|
| `arc/ref_self_assign.lsysl` 🔴 | `r = r` (same ref) — refcount stable, NOT bumped-then-dropped-to-zero |
| `arc/ref_in_slice.lsysl` 🔴 | `var xs: []&Box = ...` — refs stored in slice; behavior on slice grow / drop / sub-slice (do refs incref on copy?) |
| `arc/ref_in_escaping_closure.lsysl` 🔴 | Ref captured by a closure that escapes (returned from fn) — does the closure incref? Does the captured ref outlive the original local? |
| `arc/ref_conditional_drop.lsysl` 🔴 | `if cond then val r = f() else 0` — one branch keeps a ref, other doesn't; verify no double-drop on the "doesn't" branch |
| `arc/ref_mutual_struct.lsysl` 🔴 | Mutually-referencing **structs** (not enums — that's recursive-enum land); cycle handling (ARC doesn't collect; `unowned` should be required) |

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

## 7. Pointer pathologies — 🔴 P2

| File | Corners pinned |
|---|---|
| `pointers/null_deref.lsysl` 🔴 | Null pointer deref must trap; cast `0` to `*T` then `*p` |
| `pointers/ptr_roundtrip.lsysl` 🔴 | `*&v` and `&*p` round-trips preserve value and address |
| `pointers/ptr_comparison.lsysl` 🔴 | Pointer compared with `0` (null); two pointers to same/different objects |
| `pointers/ptr_to_zero_array.lsysl` 🔴 | Pointer to first element of `[0]int` fixed array |

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
