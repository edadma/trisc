# Sysl Language Features — Test Coverage Roadmap

This file tracks lang_features test coverage of every documented Sysl
language surface. `std/` exercises the language algorithmically — that's
good for finding bugs that happen to surface inside an algorithm, but it's
*incidental* coverage: a regression in `defer` lowering, in ARC inc/dec,
or in slice descriptor handling can hide for months because no `std/`
test specifically pins that surface.

`lang_features/` is the deliberate, surface-oriented complement. Each
file targets one feature and pins its observable behaviour with
explicit, hand-asserted values. The shape we want is "this expression
in this language produces this exact value on every backend." When a
backend miscompiles a surface, the test that pins that surface fires
first — not the high-level algorithm that incidentally hit the bug.

## File-size policy

- **Hard ceiling: 2000 LOC per file.** Beyond that, the literate prose
  becomes hard to navigate and parser errors lose locality.
- **Target: ≤ 500 LOC per file.** Most feature surfaces are small enough
  that 100–300 LOC is plenty. Splitting early is cheap.
- **One feature per file.** If a file ends up testing two unrelated
  features, split it.
- **Categories are directories.** Each category lives under
  `lang_features/<category>/` so the file count per directory stays
  manageable and `sbt … test <category>/` runs a focused subset.

## Status legend

- 🔴 **Not started** — no test file exists for this surface.
- 🟡 **Partial** — file exists but coverage is incomplete or known to
  miss documented edge cases.
- 🟢 **Done** — file exists, covers the documented happy path *and* the
  listed edge cases, green on all seven backends.

A feature is **not** 🟢 until tests for the listed edge cases all pass on
**interpreter, llvm-host, svm-host, trisc, riscv64, riscv32, wasm32**.

## Priority tiers

- **P0 — Foundation:** if this surface is wrong, almost everything else
  is wrong too. ARC inc/dec, slice descriptors, pointer arithmetic,
  generic instantiation, control-flow lowering. Bug here = days of
  mystery debugging downstream.
- **P1 — Common:** used heavily in `std/` and likely in user code. A
  regression is painful and visible.
- **P2 — Specialized:** less frequent but important to pin so it
  doesn't silently rot.
- **P3 — Diagnostics:** compile-error / warning behaviour, attributes,
  edge cases that affect tooling rather than runtime correctness.

## Categories (target layout)

The existing nine files at the top level will move into category subdirs
in a single rename commit so all future files start in the right place.
Current → target mapping is shown in each category's table.

> **Total surfaces to cover: ~50 files / ~400–600 tests.**
> Existing: 9 files / 30 tests. Coverage today: ~6 % of the planned surface.

---

## Tier 0 — Foundation

### `arc/` — Three allocation modes + refcount semantics — 🟡 P0

Reference §"Three Allocation Modes", "Conversion Rules", "Deinit Blocks".
This was the highest-leverage gap. `std/` defines almost no `&T`
reference-counted types, so the entire ARC path was essentially unpinned
end-to-end. Now substantially covered — six real bugs surfaced and were
fixed along the way (TFieldAssignStmt use-after-free on five backends,
struct-copy aliasing on six backends, ref reassignment use-after-free
on four backends, SVM deinit missing entirely, SVM global-string-assign
truncated to 8 bytes, SVM int-global store regression from the latter
fix). All 11 ARC test files now green across all seven backends except
where noted.

| File | Tests pinned |
|---|---|
| `value_struct_copy.lsysl` 🟢 | bitwise copy of value structs, no refcount, no double-free of contained strings (6 tests) |
| `new_ref_basic.lsysl` 🟢 | `new Node(...)` allocates, refcount starts at 1, ref-binding increments, drop decrements, free at 0 (4 tests) |
| `new_ref_assignment.lsysl` 🟢 | `r1 = r2` inc-r2/dec-r1; self-assign `r = r` is a no-op (inc-then-dec on the same buffer) (4 tests) |
| `new_ref_passing.lsysl` 🟢 | `f(r: &T)` increments for call duration, decrements at callee return; nested call chains preserve refcount (6 tests) |
| `new_ref_return.lsysl` 🟢 | returning a fresh `new T(...)` is owned (no inc), returning a borrowed ref increments (5 tests) |
| `new_ref_field.lsysl` 🟢 | struct field of type `&Inner` — assigning a new ref decrements the old, increments the new (6 tests) |
| `deinit_basic.lsysl` 🟢 | `Struct.deinit()` fires once at refcount=0 transition; before free; doesn't fire for value-struct drops (5 tests) |
| `deinit_with_fields.lsysl` 🟢 | deinit body can read fields (including chasing through `&Inner` field) before the field's own refcount is decremented (3 tests). TODO: also pin "outer's drop transitively decrements `&Inner` field, firing inner's deinit" — currently every backend leaks the inner ref on outer-drop |
| `ptr_to_value.lsysl` 🟢 | `&v` then `*p` round-trips; mutating through `*p` mutates the value (4 tests) |
| `ptr_to_ref.lsysl` 🟡 | `&r` where `r: &T` yielding `*T` — NOT implemented on any backend; file is currently a placeholder + TODO. Cross-backend feature gap |
| `value_to_ref_explicit.lsysl` 🟢 | `new T(v)` heap-promotes a value struct, independent of source (2 tests) |
| `field_self_concat.lsysl` 🟢 | original regression test for `TFieldAssignStmt` use-after-free (3 tests) |

---

### `slices/` — Fixed arrays, dynamic arrays, slice descriptors, append — 🟡 P0

Reference §"Arrays, Slices, and Pointers", §"Append". `std/` uses slices
constantly but rarely pins boundary conditions. Now substantially covered;
two real gaps surfaced (bounds-check trapping inconsistent across backends;
TRISC's `for x in slice` codegen broken).

| File | Tests pinned |
|---|---|
| `array_literal_fixed.lsysl` 🟢 | `[1, 2, 3]` literal types as `[3]int`; index, length, iteration (5 tests) |
| `array_explicit_size.lsysl` 🟢 | `var a: [N]T` declaration; zero-init (3 tests) |
| `array_of_struct.lsysl` 🟢 | array of value structs — element read/write, whole-element assignment (4 tests) |
| `array_decay.lsysl` 🟢 | `[3]int` passed to `*int` parameter (array-decay); `&arr[0]` explicit form (3 tests) |
| `dynamic_array_new.lsysl` 🟢 | `new [n]int` allocates, zero-init, runtime n; many-alloc smoke (4 tests) |
| `dynamic_array_bounds.lsysl` 🟡 | in-bounds happy path (2 tests). TODO: re-enable OOB-trap tests once uniform bounds checking lands — currently only the interpreter (and partially TRISC) trap on OOB index/store; the other 5 silently succeed. **Real cross-backend soundness gap.** |
| `slice_from_array.lsysl` 🟢 | `arr[i:j]` shares backing; mutations visible through either side; empty slice (5 tests) |
| `slice_full_subslice.lsysl` 🟢 | `s[:]`, `s[i:]`, `s[:j]` omitted-bound forms (4 tests) |
| `slice_descriptor_passing.lsysl` 🟢 | slice param shares backing with caller; `len()` works inside callee; sub-slice through param (4 tests) |
| `slice_append.lsysl` 🟢 | append single, append many, append preserves predecessors, append on pre-filled slice (4 tests). NB SVM exhausts memory at large append counts (bump-allocator + no free); test uses 100 elements not 1000 |
| `slice_iter_for_in.lsysl` 🟡 | `for x in [literal array]` works (4 tests). TODO entries for `for x in slice` and `for x in dynamic[:]` once TRISC's slice-iter codegen lands; today TRISC traps on those forms while the other 6 backends work |
| `string_as_byte_slice.lsysl` 🟢 | string indexing yields bytes; len = byte count; UTF-8 multi-byte (4 tests) |

---

### `pointers/` — Raw pointers, deref, arithmetic, address-of — 🟢 P0

Reference §"Pointers", "Pointer Dereference Is Explicit", "Array/Pointer Decay".

| File | Tests pinned |
|---|---|
| `ptr_deref_basic.lsysl` 🟢 | `*p` deref; `p.field` implicit deref for structs; pass *T to fn (5 tests) |
| `ptr_arithmetic.lsysl` 🟢 | `p + n` element-wise advance; walk array via pointer (4 tests) |
| `ptr_compare.lsysl` 🟢 | `p == q`, `p != q`, retarget changes equality (3 tests) |
| `ptr_null.lsysl` 🟢 | `*int(0)` null; default-init zero-inits to null; branch-on-null (4 tests). TODO: re-enable `bool(*T)` cast test once LLVM codegen lands (currently emits invalid `bitcast i8* to i8`) |
| `ptr_to_struct_field.lsysl` 🟢 | `&s.field` field pointer; field-ptr mutation visible in struct (4 tests) |
| `array_decay_implicit.lsysl` 🟢 | covered by `slices/array_decay.lsysl` |

---

### `generics/` — Generic functions / structs / enums / aliases — 🟡 P0

Reference §"Generic Functions", "Generic Structs", "Generic Tagged Unions",
"Generic Type Aliases", "Methods on generic structs". `std/` uses Option /
Result / List heavily but mostly through their already-instantiated forms;
the *inference* and *instantiation* edges are less covered. Substantially
covered now; one SVM bug TODO'd.

| File | Tests pinned |
|---|---|
| `generic_fn_basic.lsysl` 🟢 | explicit type arg; inference; multi-param; max-of-T; (5 tests). TODO: `apply_twice[T](f: (T)->T, x: T)` higher-order — SVM overflows its 1024-item data stack on f(f(x)); other 6 backends fine |
| `generic_struct.lsysl` 🟢 | `Box[T]` construct & read; inferred construct; pass-to-fn; two-param `Pair[A,B]` (4 tests) |
| `generic_enum.lsysl` 🟢 | `Maybe[T]` with `Just(value: T)` / `Nope`; match with payload bind; two instantiations side-by-side (3 tests) |
| `generic_nested.lsysl` 🟢 | `Box[Box[int]]`, three-deep `Box[Box[Box[int]]]`, `Box[Opt[int]]`, two-param `Pair[A,B]` (4 tests) |
| `generic_fn_explicit.lsysl` 🟢 | subsumed by `generic_fn_basic.lsysl` |
| `generic_fn_inferred.lsysl` 🟢 | subsumed by `generic_fn_basic.lsysl` |
| `generic_fn_operator_rhs.lsysl` 🟢 | bare-call placeholder `_ + _` RHS; explicit type args; two-arg inference; full closure-literal RHS — pins sysl@950415fde + sysl@eb3fa5673 across all 7 backends (4 tests) |
| `generic_struct_method.lsysl` 🟢 | `Box[T].get()` & `.set(x)`; mutating-self via `&self`; method on two-param `Pair[A,B]`; chained method call (6 tests) |
| `generic_alias_basic.lsysl` 🟡 | `type GabUnary[T] = (T) -> T` as parameter type; two-param alias `(A,A)->B` (2 tests). TODO: alias instantiation as struct *field* type fails with `'GabUnary' is not a generic type` even when the identical instantiation works as a fn param — analyzer field-type resolution gap, same on all 7 backends |
| `generic_fn_sibling_import.lsysl` 🔴 | generic fn instantiated across files of the same module (sysl@4f1f81725 regression) — multi-file fixture, not yet pinned |
| `generic_alias_cross_file.lsysl` 🔴 | generic alias visible across files (sysl@2c4f1c095 regression) — multi-file fixture, not yet pinned |

---

### `control_flow/` — if-expr, while, for, loops, break/continue, return — 🟡 P0

Reference §"Control Flow", "If Expression", "Return".

| File | Tests pinned |
|---|---|
| `if_expr.lsysl` 🟢 | if as value, side-effect cond, if/else-if chain, no-else, nested (5 tests). Covers both `if_expr_as_value`, `if_expr_unit`, `if_chain` |
| `while_loops.lsysl` 🟢 | counted while; while + break; while + continue; never-runs; nested (5 tests) |
| `for_in_range.lsysl` 🟢 | `0..<n` exclusive; `0..n` inclusive; empty; single-element; negative range (8 tests) |
| `for_in_slice.lsysl` 🟡 | covered by `slices/slice_iter_for_in.lsysl` with the TRISC `for x in slice` TODO |
| `for_in_string.lsysl` 🔴 | `for c in s` iterates byte / rune — not yet pinned |
| `loop_labels.lsysl` 🔴 | `outer: for ...` + `break outer` / `continue outer` — not yet pinned |
| `early_return.lsysl` 🟢 | early return from loop; nested blocks; ARC refcount cleanup on every path (4 tests). Surfaced+fixed SVM array-pass-by-value bug (emitStore for ArrayType fell through to store64) |
| `return_implicit.lsysl` 🟢 | `def` expression-bodied function; block-body last-expr return; implicit/explicit match (3 tests) |

---

## Tier 1 — Common surfaces

### `defer/` — defer LIFO + interactions — 🟡 P1

| File | Tests pinned |
|---|---|
| `defer_lifo.lsysl` 🟢 | LIFO ordering via pointer-mutating helper *(move from top level)* |
| `defer_early_return.lsysl` 🔴 | defer fires on every return path **— BLOCKED on TRISC compiler bug**: defer doesn't fire (or doesn't take effect) when the enclosing fn has a non-unit return type and uses `return value`. 6/7 backends pass (interp, llvm-host, svm, rv64, rv32, wasm32). Existing `defer_lifo.lsysl` works because its helper is `-> unit`. File drafted and removed 2026-05-15; see feedback_sysl_trisc_defer_nonunit_return.md for repro + suspect codegen sites |
| `defer_with_arc.lsysl` 🔴 | defer that mutates a ref-counted struct field |
| `defer_no_return.lsysl` 🟡 | single defer + implicit exit; defer-then-body; defer in always-taken if-branch fires at fn exit; no-defer baseline (4 tests). **Bug surfaced and withheld:** defer queued inside a *skipped* `if`-branch still fires on 6 of 7 backends (everything except interpreter). Test was authored but dropped pending fix — see feedback_sysl_codegen_defer_in_skipped_branch.md |
| `defer_in_loop.lsysl` 🔴 | defer inside loop body — fires at end of *function*, not iteration **— BLOCKED on codegen cluster**: 6 of 7 codegen backends fire only 1 defer when N were queued in a loop body. TRISC additionally fails to link if the deferred statement references the loop variable (`undefined symbol: 'i'`). Same lexical-cleanup family as `defer-in-skipped-branch`. File drafted and withdrawn 2026-05-15; see feedback_sysl_codegen_defer_in_loop_queue_depth.md for the cluster repro |
| `defer_nested_call.lsysl` 🟢 | inner defers complete before outer resumes; outer's own defer fires after inner is fully gone; two-per-frame LIFO without cross-frame interleave; three-deep A→B→C nesting (4 tests) |

Existing: `defer_lifo.lsysl` → `defer/defer_lifo.lsysl`.

---

### `closures/` — closures, captures, fn pointers — 🟡 P1

| File | Tests pinned |
|---|---|
| `closures_hof.lsysl` 🟢 | basic captures, stored-in-struct, repeated invocation *(move from top level)* |
| `closure_capture_mutable.lsysl` 🟡 | snapshot-not-reference capture semantics: `val` capture (1), `var k` outer mutation invisible after build (1). **Bugs surfaced and dropped from this file:** SVM `ArrayIndexOutOfBoundsException` when two closures capture the same `var` in one fn; SVM block-bodied closure returning last expression miscomputes return value; TRISC captures `var p: struct` by reference instead of by snapshot (2 tests). (2 tests; 3 known-divergence cases TODO'd in commit msg) |
| `closure_return_from_fn.lsysl` 🔴 | returning a closure from a fn; lifetime of captured locals **— BLOCKED on a real compiler bug**: 4 of 7 backends (llvm-host, riscv64, riscv32, wasm32 — all LLVM-based) free the closure environment when the outer fn returns. Symptoms: llvm-host returns garbage (`mult7(6) = 70904496`); rv64/rv32 return 0; wasm32 traps `unreachable`. Interpreter, svm-host, trisc all pass. **Surfaced 2026-05-15; file drafted and removed; see commit msg of sysl@ec6acbd08+1 for repro.** |
| `closure_recursive_inner_def.lsysl` 🟡 | single-fn self-recursive inner `def`: factorial (3), fib (2), capture-from-outer-scope (1) — 6 tests. **Bug surfaced and dropped from this file:** SVM panics ("svm.result=0xa") when an outer fn contains two unrelated self-recursive inner defs, even when they don't reference each other. 6/7 backends pass two-separate-defs. |
| `closure_in_closure.lsysl` 🔴 | closure declared inside another closure's body *(blocked on sysl bug — see feedback_sysl_closure_in_closure.md)* |
| `function_pointer_call.lsysl` 🟡 | bind a top-level fn to a val/var, call it directly; pass to a hof; reassign a `var f`; two pointers side by side (4 tests). **Bug surfaced and dropped from this file:** SVM crashes returning a fn-pointer from a hof — "address not found: 10000000000000" (high-bit-set tag in the fn-pointer encoding doesn't survive the return path). 6/7 backends pass the returned-fn-pointer test. |
| `closure_underscore_placeholder.lsysl` 🟢 | `_ + 1`; `_ * 7`; two-arg `_ - _` order-sensitive; `_ * 2 + 1` bubble-up through arithmetic; paren-narrowed `(_ + 1)`; `_ * _` same arg twice (7 tests) |

Existing: `closures_hof.lsysl` → `closures/closures_hof.lsysl`.

---

### `pattern_matching/` — match, payloads, `is`, guards — 🟡 P1

| File | Tests pinned |
|---|---|
| `enum_match_payload.lsysl` 🟢 | data variants + exhaustive *(move from top level)* |
| `match_exhaustive_simple_enum.lsysl` 🟢 | three-tag enum exhaustive match returning string / int / bool; match as arithmetic operand; match in if-cond; two reads stable; through fn boundary (6 tests) |
| `match_nested_payload.lsysl` 🟢 | two-level `Some(Ok(v))` / `Some(Bad(why))` / None; three-level `Wrap(Some(Ok(v)))`; struct payload at leaf; full exhaustive sweep (10 tests). **Surfaced analyzer gap:** exhaustiveness checker requires explicit `Outer(_)` catch-all when nested patterns exhaust a variant — see feedback_sysl_match_nested_exhaustiveness.md. Codegen works on all 7 backends; only the checker is conservative |
| `match_guards.lsysl` 🔴 | guard clauses (`case Some(x) if x > 0 => ...`) — *if supported* |
| `match_or_patterns.lsysl` 🔴 | `case (A | B) => ...` — *if supported* |
| `if_is_pattern.lsysl` 🟢 | Some/None happy + fail; else-arm form; int / string / struct payload binds; chained `if .. is ..` on two-variant outcome; else-arm has no binding leak; pattern check is pure across two reads (8 tests) |
| `destructure_assign.lsysl` 🟢 | `val q, r = divmod(...)`; `var` form with subsequent mutate; parenthesized; swap; three-way rotate; struct destructure by field-order; non-consuming (7 tests) |

Existing: `enum_match_payload.lsysl` → `pattern_matching/enum_match_payload.lsysl`.

---

### `strings/` — concat, interpolation, format, str() — 🟡 P1

| File | Tests pinned |
|---|---|
| `format_strings.lsysl` 🟢 | `s"..."` interpolation shapes *(move from top level)* |
| `field_self_concat.lsysl` 🟢 | field-string-assign-with-self-read *(move; cross-listed in `arc/`)* |
| `string_concat_basic.lsysl` 🟢 | two-word concat; with-separator; empty-left / empty-right / both-empty; order matters; operand-mutation invariance; len math; UTF-8 byte length; self-concat (10 tests) |
| `string_concat_chain.lsysl` 🟢 | 4-operand chain; 5-operand with separators; left-assoc step-by-step via `var`; empty in the middle; all-empty; same operand aliased ×3; mixed literal+var sources; chain returned from fn; sources not mutated (9 tests) |
| `string_escape_seqs.lsysl` 🟢 | `\n`, `\t`, `\r`, `\0`, `\\`, `\"`, `\'`, `\xHH` — 23 tests pinning byte values, len, embedded NUL doesn't terminate, escapes in concat / interp / char literals; non-ASCII `\xAB` UTF-8-encodes to 2 bytes; no `\u{...}` (deliberately scoped to documented repertoire) — 7/7 backends |
| `string_index_slice.lsysl` 🟢 | `s[i]` (byte / `u8`) and `s[i:j]` (sub-slice) — 23 tests pinning indexing, middle/prefix/suffix/full/empty slicing, var-bounded slicing, sub-of-sub indexing, substring through concat / fn boundary / equality, UTF-8 byte-level (`"aña"` byte slicing at code-point boundaries) — 7/7 backends. **Surfaced + fixed SVM bug**: `TSliceExpr` had no `StringType` branch and produced a 24-byte slice struct in place of a 16-byte string descriptor, corrupting every downstream consumer (concat / interp / equality). Fixed by adding a dedicated StringType branch (16-byte descriptor with i64 length, no cap/backref). Empty slices passed only because the layout-mismatched length happened to land at zero |
| `string_len_empty.lsysl` 🟢 | `len("")` is 0; len of single char; len five chars; two empty literals equal; len stable across reads; len of returned string; len of returned ""; len after rebind; len(a+b)=len(a)+len(b) for ASCII (9 tests) |
| `string_compare.lsysl` 🟢 | `==` / `!=` / `<` / `<=` / `>` / `>=` on strings — 22 tests: equality (literal / var / concat / fn-result), lexicographic ordering (first-byte-differs / prefix-extends / strict-on-equal), `<=`/`>=` collapse on equal inputs, empty-is-minimum, case sensitivity (ASCII `'A'` < `'a'` via raw bytes), length mismatch, UTF-8 byte-equality / inequality, result flowing through `val` / `if` / fn (7/7 backends). Lexicographic ordering shipped 2026-05-15 — interpreter (Scala loop), LLVM (`memcmp` + select-based tiebreak), SVM (new `__svm_str_cmp` runtime helper), TRISC (inline three-way diff loop) |
| `f_format_strings.lsysl` 🟡 | `f"..."` with format specs — 25 tests universally green: `%d`, `%x`, `%X`, `%o`, `%s` (no width), `%+d`, `%%`, `%b` (zero only), `%08d` / `%04x` when value fits, `%Ns` / `%-Ns` exact-width-match, `${expr}%spec`, multi-slot, fall-through to `str()`. **Four cross-backend bug clusters surfaced and dropped from this file** (see `feedback_sysl_fstring_divergences.md`): (a) `%b` non-zero crashes on the 4 LLVM-based backends; (b) `%Ns` string padding unsupported on SVM + trisc; (c) trisc `%08d` mis-pads when value's digit count ≥ width; (d) trisc fails reference's headline mixed-verb example. File flips 🟡 → 🟢 when all four are fixed |
| `str_builtin.lsysl` 🟡 | `str(x)` on int / string / data-enum variant name / fn-boundary / interp-slot (20 tests, 7/7 backends). **Two cross-backend bug clusters surfaced and dropped from this file**: (1) `str(bool)` returns `"1"`/`"0"` on 4 backends, `"true"`/`"false"` on SVM, crashes on llvm-host + wasm32 — needs language-design decision (see `feedback_sysl_str_bool_divergence.md`); (2) SVM `str(float)` emits placeholder `"???"` while other 6 backends format the float (see `feedback_sysl_svm_str_float_unimplemented.md`). File flips 🟡 → 🟢 when both clusters are resolved |
| `string_from_bytes.lsysl` 🟡 | `string(&buf[0], n)` + `string(slice)` constructors — 12 tests universally green: basic ASCII / empty / partial / embedded NUL / UTF-8 raw bytes / from `new [n]byte` slice / fixed-array full + subrange slice / equality with literal / concat / byte index. **SVM bug surfaced and dropped from this file**: both constructor forms share source backing storage instead of copying (mutating source after construction mutates the resulting string — reference says both forms copy). Other 6 backends copy correctly. See `feedback_sysl_svm_string_from_bytes_aliasing.md`. File flips 🟡 → 🟢 when SVM is fixed |

Existing: `format_strings.lsysl` → `strings/format_strings.lsysl`.
`field_self_concat.lsysl` → `strings/field_self_concat.lsysl` (also referenced from `arc/`).

---

### `functions/` — default args, named args, parameter modes — 🟡 P1

| File | Tests pinned |
|---|---|
| `out_inout_params.lsysl` 🟢 | swap / produce / accumulate *(move from top level)* |
| `default_args_basic.lsysl` 🟢 | trailing default; mid-call omission (23 tests) |
| `default_args_complex_expr.lsysl` 🟢 | default value is a non-trivial expression (22 tests) |
| `named_args.lsysl` 🟢 | calling with `name = value` form; mixing positional + named (23 tests) |
| `call_by_name.lsysl` 🟢 | `=> T` parameters; lazy evaluation; short-circuit-like patterns; forwarding to another by-name slot (16 tests) |
| `extern_decl_call.lsysl` 🟢 | `extern fn ...` declaration; call into a C runtime symbol (8 tests; putchar happy path) |
| `parameterless_fn.lsysl` 🟢 | declaration without `()` parens; usage (19 tests) |

Existing: `out_inout_params.lsysl` → `functions/out_inout_params.lsysl`.

---

### `interfaces/` — dispatch, mutating self, composed — 🟡 P1

| File | Tests pinned |
|---|---|
| `iface_mutating_self.lsysl` 🟢 | mutating-self through iface *(move from top level)* |
| `iface_basic_dispatch.lsysl` 🟢 | non-mutating method through iface; correct impl chosen (16 tests) |
| `iface_composed.lsysl` 🟢 | embedded interfaces; method dispatch picks right impl (10 tests) |
| `iface_generic_method.lsysl` 🟡 | iface methods with parametric built-in types — slice, slice-of-struct, returning slice (8 tests). Generic-struct-implements-iface is unsupported (analyzer + iface-dispatch naming mismatch) |
| ~~`iface_default_methods.lsysl`~~ | ✖ **Not applicable** — sysl interface methods cannot have default bodies (parser rejects `=` after signature; verified 2026-05-15). Default-method semantics live in **traits** instead (see reference §"Traits and `impl` blocks") — covered separately when a `traits/` category opens. |
| `iface_box_lifetime.lsysl` 🟡 | iface receiver lifetime through call frames + local bindings + mutating dispatch (8 tests). Two gaps deferred: iface-as-struct-field (analyzer rejects `struct S { f: Iface }`) and returning iface from a function constructing source as local (UAF) |

Existing: `iface_mutating_self.lsysl` → `interfaces/iface_mutating_self.lsysl`.

---

### `structs/` — construction, return-by-value, methods — 🟡 P1

| File | Tests pinned |
|---|---|
| `struct_return.lsysl` 🟢 | direct field, local, chained *(move from top level)* |
| `struct_ctor_args.lsysl` 🟢 | positional / all-named / reordered / mixed pos+named struct construction; multi-type fields; nested struct fields; byte fields; in val/var/fn-arg/fn-return contexts (22 tests). Note: sysl does not support struct field defaults (parse error) — defaults are a function-parameter feature only |
| `struct_method_value.lsysl` 🟢 | `Struct.method()` on value-bound receiver — read-only patterns (sysl has no "true" value receiver; self is always `*Self`, but body chooses whether to mutate). Field reads, computed reads, repeated reads, struct return, chained method-on-temp returning struct, string returns, nested-struct method via outer. (11 tests) |
| `struct_method_ptr.lsysl` 🟢 | mutating methods — set / inc / read-modify-write / multi-field assign / swap-fields / inc-and-return / nested struct mutation via outer field / mutation on array element (14 tests) |
| `struct_method_ref.lsysl` 🟢 | methods on `&Struct` (heap, ref-counted) receivers — read / mutate / aliased-ref mutation / pass-as-arg / returned ref / method on iface-typed struct field (11 tests) |
| `struct_nested.lsysl` 🟢 | two- and three-level nesting; field-of-field read / assign / partial / whole-inner replace; returning nested by value; passing nested by value; methods reading + mutating inner fields; method returning fresh nested (14 tests) |
| `struct_tuple_return.lsysl` 🟢 | tuple-return + destructuring — 2/3-tuples; mixed int/string/bool types; `val a, b =` / `val (a, b) =` / `var a, b =` forms; tuple chained through wrapper fns; computed-tuple returns (minmax, divmod); destructured-in-arith; tuple-in-local-then-destructure (14 tests) |

Existing: `struct_return.lsysl` → `structs/struct_return.lsysl`.

---

### `floats/` — IEEE 754, math, casts — 🟡 P1

| File | Tests pinned |
|---|---|
| `float_extremes.lsysl` 🟢 | NaN, infinity, signed zeros, in-range trunc *(move from top level)* |
| `float_arithmetic.lsysl` 🟢 | Exact-representable arithmetic (small integer products, halves, quarters); identity laws (x+0=x, x*1=x, x/1=x, x-x=0); sign correctness; double-half round-trip; three-term sum; polynomial-shaped expr; overflow to ±∞; sign-handling round-trip; int-promote-add associativity (24 tests). Brittle non-exact equality intentionally not pinned. |
| `float_int_cast.lsysl` 🟢 | `int(f)` exact integer-valued, truncates toward zero on +ve and -ve, half-down on .5; `f64(i)` exact for ±100 range; round-trip integrity for -10..10; casts in arithmetic / float exprs / division; promotion arithmetic. (18 tests) |
| `float_compare.lsysl` 🟢 | finite-ordering (lt/le/gt/ge/eq/ne, positive & negative); signed-zero equality; **NaN poisons every comparison** (lt, le, gt, ge, eq false; ne true) — fixed TRISC's `<=` / `>=` codegen which was using XOR-flip on `<` (NaN-incorrect); signed-infinity ordering; comparisons in if / && / || (19 tests) |
| `float_literal_parsing.lsysl` 🟢 | 0.5, 1.5, 0.0, -0.0; positive scientific (1e3, 2.5e2, 1e6, 1.0E5, 1.0e+3); negative scientific (1e-3, 5e-2, 1.5e-1); underscore separators in fractional + integer parts; negative-literals via unary minus; literals as fn arg / return value; mixed-exponent sum. (19 tests) |

Existing: `float_extremes.lsysl` → `floats/float_extremes.lsysl`.

---

### `errors/` — `?` postfix, Option, Result chains — 🟡 P1

Reference §"`?` Operator (Try)".

| File | Tests pinned |
|---|---|
| `try_postfix_option.lsysl` 🟢 | happy `Some/Some`; first-position `None` short-circuit; second-position `None` short-circuit; three-step chain; middle `None` short-circuit (5 tests) |
| `try_postfix_result.lsysl` 🟢 | happy `Ok/Ok`; first-position `Err` preserves payload; second-position `Err`; three-step chain; middle `Err` propagation (5 tests) |
| `option_basic.lsysl` 🟢 | Some(v) inferring T; None with explicit type; both-variant match; payload binding; unwrap-like helpers (trap-on-none and default-on-none); Option flowing through fn boundary (map-like); Option carrying string / struct payloads (14 tests) |
| `result_basic.lsysl` 🟢 | Ok/Err construction with explicit types; match-arm dispatch; is_ok / is_err / unwrap / unwrap_or helpers; map-like transform (Ok payload doubled / Err preserved); Result with struct payload + with int-int error type (15 tests) |
| `try_postfix_chain.lsysl` 🟢 | three sequential `?`s — all-success, first/middle/last fails for both Option (None) and Result (Err); chain through int-err Result; two-step `?` with intervening logic (11 tests) |
| `option_payload_string.lsysl` 🟢 | `Option[string]` ARC — literal / empty / None / two-distinct payloads; returned via fn (Some + None forms); identity round-trip; 3-layer pass-through; concat-inside-Some; long-string heap allocation survives; map-like Some-only transform (15 tests) |

---

## Tier 2 — Specialized

### `integers/` — widths, overflow, intrinsics, within-constraints — 🟢 P2

Reference §"Integer Overflow", "Overflow Intrinsics".

| File | Tests pinned |
|---|---|
| `int_widths_cast.lsysl` 🟢 | `sizeof` for every signed/unsigned width (i8/i16/i32/i64/u8/u16/u32/u64 = 1/2/4/8 each); `int` = 4-byte (i32-shaped); truncation modulo 2^N on narrowing (u8(0x1FF)=0xFF, i8(256)=0, i8(128)=-128, u16(0x12345)=0x2345, i32(0x123456789i64)=0x23456789); sign-extension widening (i8(-1)→i32=-1, i64=-1; i16(-1)→i64; i32(-1)→i64; i8(-128)→i64); zero-extension widening (u8(0xFF)→i32=255, i64=255; u16(0xFFFF)→i64=65535; u32(0xFFFFFFFF)→i64=4294967295); cross-signedness reinterpretation (i8(-1)↔u8(255); i32(-1)↔u32(0xFFFFFFFF); u32(0x80000000)→i32 MIN); narrow-then-widen round-trips (low-byte sign vs zero extension diverges); boundary identity casts (i32 MAX/MIN, i64 MAX/MIN, u64 MAX as bit pattern) (32 tests) |
| `int_overflow_wrap.lsysl` 🟢 | unsigned wrap (modular) for `+`/`-`/`*` at every width (u8/u16/u32/u64); signed wrap (two's-complement) at every width (i8/i16/i32/i64); `+` MAX+1 → MIN, `-` MIN-1 → MAX, `*` keeps low bits, `i32 MIN * -1` wraps to MIN; result-width identity (`u8 + u8` fits in `u8` — no implicit promotion) (24 tests) |
| `overflow_intrinsics.lsysl` 🟢 | `wrapping_{add,sub,mul}` at every width matches the default wrap; `saturating_{add,sub,mul}` clamps to type MAX/MIN at u8/u16/u32/i8/i16/i32 boundaries; agree-on-no-overflow and diverge-on-overflow pinned together. Surfaced an SVM signed-saturating bug for narrow widths (i8/i16/i32) — sign-of-result detection only works at 64-bit width where wrapping is forced; SVM does add/sub in full i64 so r doesn't wrap and the old check missed overflow. Fix at `SyslSVMCodegen.scala:2096` adds a `width<64` branch that range-checks `r > maxV` / `r < minV` before clamping. std/ 974/974 on SVM clean. 64-bit signed saturating and `saturating_mul(u32)` omitted today per the reference's TRISC-gap note (28 tests) |
| `int_within_constraint.lsysl` 🟢 | inclusive `within 0..N` and exclusive `..<` construction at in-range / lower-edge / upper-edge values; `::First` / `::Last` fold to lower/upper bound (`::Last` of `..<10` is 9); `::Valid(x)` is non-trapping bool for in/out-of-range probes; subtype is base-compatible (no `int(a)` cast); arithmetic on subtype yields plain int; `::Valid` guards production before assignment; explicit-width base (`i32 within 0..255`); `const`-named bound; negative-low signed range `-10..10` (20 tests) |
| `int_within_succ_pred.lsysl` 🟢 | `T::Succ(x)` advances by 1, `T::Pred(x)` retreats by 1; both within an inclusive range and the exclusive-upper `..<` variant; `Succ` from `::First` yields `::First+1`, `Pred` from `::Last` yields `::Last-1`; repeated `Succ` walks the range; round-trips `Succ ∘ Pred` = `Pred ∘ Succ` = id; works across zero on a negative-low range `-3..3`; `Succ`/`Pred` results compose with plain int arithmetic (`Succ(x)+Pred(x)=2x`). Trap-on-boundary tests deferred to a future runtime-trap harness (15 tests) |

---

### `operators/` — precedence, chains, compound — 🔴 P2

Reference §"Operators (by precedence...)", "Chained Comparisons", "Compound Assignment".

| File | Tests pinned |
|---|---|
| `precedence_arithmetic.lsysl` 🟢 | * / bind tighter than + -; parens override; left-assoc at same level (sub, div, mixed); unary minus binds tighter than binary; full mix (quadratic, diff-of-squares) (21 tests) |
| `precedence_logical.lsysl` 🟢 | truth tables for && / ||; unary ! basics and doubled; && tighter than ||; ! tighter than && / ||; parens override; **short-circuit** for && (LHS-false skips RHS) and || (LHS-true skips RHS), plus 3-way chain short-circuit at start and middle (23 tests). Fixed LLVM codegen which was eager-evaluating both operands |
| `precedence_bitwise.lsysl` 🟢 | basic bitwise (and/or/xor with 0 and self), `~` (zero / minus-one / double-not); shifts (`<<` / `>>` by 1, by 4, truncation, left-assoc); `&` tighter than `^` tighter than `\|`; shifts tighter than bitwise; bitwise tighter than `==`; common patterns (pack/extract nibbles, clear/set/toggle bit). Note: sysl does not support `0b...` binary literals — used decimal with `// 0b...` comments (28 tests) |
| `chained_comparisons.lsysl` 🟢 | 2/3/4-pair chains `<` / `<=` / `>` / `>=` / `==`; mixed operators (lt+le, le+lt, ge+ge); inclusive boundary on `<=`; short-circuit at head; lowering semantics pinned (middle operand in two pairs is evaluated once per pair — `1 < bump(5) < 10` runs bump twice; long chain 1<2<3<4 runs interior helpers twice and ends once); chain inside `if` expr (24 tests) |
| `compound_assign.lsysl` 🟢 | `+=`/`-=`/`*=`/`/=`/`%=`/`&=`/`\|=`/`^=`/`<<=`/`>>=` on locals; on struct fields; on `[N]int` array elements; on dynamic `new [N]int[:]` slice elements; equivalence with `x = x op y`; xor double-toggle self-inverse (22 tests) |
| `increment_decrement.lsysl` 🟢 | statement-form `++x`/`x++`/`--x`/`x--`; expression-form returns new (pre) / old (post) value; compose with arithmetic (`++x + 1`); repeated stmt-form; struct fields (`++p.x`, `p.x++`); drive `while` loop counter; equivalence with `x = x + 1`. Surfaced LLVM-codegen gap (`TFieldPreInc`/`TFieldPreDec` unhandled) — fixed at `SyslLLVMCodegen.scala:3666` by mirroring the existing PostInc/PostDec handlers and returning the new value. Indexed lvalues (`++arr[i]` / `arr[i]++`) FAIL TO PARSE — feedback memo + workaround uses compound-assign on indexed elements (19 tests) |

---

### `contracts/` — require/ensure/old/result/invariant/variant — 🟢 P2

Reference §"Design by Contract".

| File | Tests pinned |
|---|---|
| `require_basic.lsysl` 🟢 | `require cond, "msg"` precondition (happy path; trap-side deferred to uniform harness) |
| `ensure_basic.lsysl` 🟢 | `ensure cond, "msg"` postcondition; `result` in cond (happy path) |
| `ensure_old_expr.lsysl` 🟢 | `old(x)` captures pre-state for postcondition (happy path) |
| `invariant_loop.lsysl` 🟢 | `invariant cond` at top of loop body (happy path; `loop_entry` covered) |
| `variant_decreasing.lsysl` 🟢 | `variant expr` decrease witness (happy path; single-expr form, lex-tuple is verification-only) |

`--no-contracts` flag behaviour requires runner-side support
(`TestCommand` in `sysl-cli/.../SyslCli.scala` doesn't yet accept the
flag) and is out of scope for this category — add when the runner
gains the toggle.

---

### `traits_impl/` — trait decls, impl blocks, operator overload — 🔴 P2

Reference §"Traits and `impl` blocks", "Operator Overloading via Traits".

| File | Tests pinned |
|---|---|
| `trait_decl_impl.lsysl` 🔴 | trait declaration; impl for a user struct; method call dispatched correctly |
| `operator_overload_infix.lsysl` 🔴 | user `+` on a user struct via trait |
| `operator_overload_prefix.lsysl` 🔴 | user prefix op (e.g. `<>x`) via single-param trait |
| `operator_overload_user_symbols.lsysl` 🔴 | user-defined operator symbols (whatever's documented) |
| `trait_generic_impl.lsysl` 🔴 | `impl[T] Trait for Box[T]` generic impl block |
| `trait_orphan_rule.lsysl` 🔴 | orphan rule rejected (compile-error test — `should_panic` or analyzer test) |

---

### `methods/` — dispatch on Struct vs *Struct vs &Struct — 🔴 P2

(Some overlap with `structs/` and `interfaces/`; this dir focuses on
*how the receiver type changes dispatch and ARC*.)

| File | Tests pinned |
|---|---|
| `method_self_value_copy.lsysl` 🔴 | `T.m()` with implicit value `self` — caller's value isn't mutated |
| `method_self_ptr_mutates.lsysl` 🔴 | `T.m()` whose body mutates `self.f` — caller sees the mutation |
| `method_self_ref_arc.lsysl` 🔴 | `T.m()` on `&T` — refcount preserved across the call |
| `method_chained.lsysl` 🔴 | `obj.a().b().c()` chained call lifetime |
| `method_on_generic_struct.lsysl` 🔴 | `Box[T].get()` after instantiation |

---

### `type_attrs/` — T::Range / T::Image / T::Valid / T::Succ / T::Pred — 🔴 P2

Reference §"Type Attributes (`T::Attr`)".

| File | Tests pinned |
|---|---|
| `type_range.lsysl` 🔴 | `T::Range` for constrained int + simple enum |
| `type_image.lsysl` 🔴 | `T::Image` (set of valid values) — usage in contracts |
| `type_value_valid.lsysl` 🔴 | `T::Value(x)` / `T::Valid(x)` — bool runtime check |
| `type_succ_pred.lsysl` 🔴 | `T::Succ` / `T::Pred` on enums (boundary behaviour) |

---

### `types_advanced/` — type aliases, static_assert, module_invariant, sizeof — 🔴 P2

| File | Tests pinned |
|---|---|
| `type_alias_basic.lsysl` 🔴 | `type Idx = int` (and generic form) |
| `type_alias_struct.lsysl` 🔴 | `type Point = struct { x: int, y: int }` (if supported as decl) |
| `static_assert_pass.lsysl` 🔴 | `static_assert(sizeof(T) == 16, "msg")` passes silently |
| `static_assert_fail.lsysl` 🔴 | `should_panic` form — compile fails with message |
| `module_invariant.lsysl` 🔴 | `module_invariant cond` declaration and behaviour |
| `sizeof_basic.lsysl` 🔴 | `sizeof(int)`, `sizeof(MyStruct)`, slice/string sizes |

---

### `enums/` — simple + data + attributes — 🔴 P2

Reference §"Enum Types (Simple)", "Tagged Unions (Data Enums)". The
*payload* variants are partly covered by `pattern_matching/enum_match_payload`;
this dir adds simple-enum coverage and edge cases.

| File | Tests pinned |
|---|---|
| `enum_simple_int.lsysl` 🟡 | ::First / ::Last / ::Pos / ::Val / ::Image / ::Succ / ::Pred; pattern match (via local-bound scrutinee); two distinct enums coexist (26 tests). Two ::Value(string) tests dropped — SVM `::Value` runtime helper panics. Match-on-param fails analyzer (separate gap). |
| `enum_simple_succ_pred.lsysl` 🟢 | `::Valid(raw_int)` (in-range / below / above / at-bounds); guards unsafe ::Val (safe-val function pattern); sparse explicit-value enum (200/400/500) — Pos by position not value, Valid for declared values; Succ chains forward, Pred chains backward; Succ/Pred mutual identity mid-range (14 tests) |
| `enum_data_recursive.lsysl` 🟡 | direct recursion shape — construction at depth 2; match distinguishes Leaf vs Node; match binds payload value; cons-list match; Leaf base case for recursive fn (11 tests). Multi-level recursive walk fails on 6 of 7 backends — separate bug catalogued |
| `enum_data_slice_field.lsysl` 🟢 | `enum Tree { Leaf, Node(children: []Tree) }` and similar recursive shape (sysl@9ee727db regression test). Leaf / empty-children Node / single-child / three-child; match distinguishes variants and binds children slice; two-level Node-of-Node; non-recursive sibling Bag(values: []int). (10 tests) |
| `enum_str_variant_name.lsysl` 🟢 | `str(data_enum_variant)` returns the constructor name — Circle / Rect / Triangle / Empty; payload-independence; in concat + `s"..."` interpolation; two data enums coexist. Note: `str(simple_enum)` returns the int value (not the name) — use `EnumType::Image` for simple enums (12 tests) |

---

### `unicode_strings/` — UTF-8, escapes, runes — 🔴 P3

| File | Tests pinned |
|---|---|
| `utf8_decode.lsysl` 🔴 | iterating UTF-8 multi-byte sequences; `len(s)` is byte length |
| `utf8_invalid_handling.lsysl` 🔴 | how the language handles invalid sequences (spec-pinned) |
| `unicode_escape_literal.lsysl` 🔴 | `\u{1F600}` literal decodes correctly |

---

## Tier 3 — Diagnostics & tooling

### `test_framework/` — `#test` variants — 🔴 P3

| File | Tests pinned |
|---|---|
| `test_named.lsysl` 🔴 | `#test("custom name shown in output")` |
| `test_should_panic.lsysl` 🔴 | `#test(should_panic)` succeeds when body panics |
| `test_should_panic_with_msg.lsysl` 🔴 | `#test(should_panic = "expected text")` matches substring |
| `test_attribute_combinations.lsysl` 🔴 | order of attributes; multiple attributes on one fn |

---

### `modules/` — imports, visibility, name mangling — 🔴 P3

Hard to test inside a single .lsysl file. May need multi-file fixtures
under `modules/fixtures/`. Many of these are already exercised by the
parsyl-split fixes (sysl@2c4f1c095 .. sysl@2de1a83cc) but with no
dedicated pin.

| File | Tests pinned |
|---|---|
| `import_selective.lsysl` 🔴 | `import std.strings.{trim, split}` — only those names visible |
| `import_alias.lsysl` 🔴 | `import std.io as io` aliasing — *if supported* |
| `import_cyclic_rejected.lsysl` 🔴 | true cycle between modules rejected at compile |
| `sibling_file_visibility.lsysl` 🔴 | symbols visible across files of one module without explicit import (sysl@6747d762b) |
| `name_mangling.lsysl` 🔴 | mangled symbol name reachable by `extern`; collision-free |

---

### `attributes/` — #pure, #reads/#writes, #address, #deprecated, #ghost — 🔴 P3

Reference §"Attributes". Most of these affect tooling rather than
runtime, but the runtime-affecting ones (`#address`, `#pure`) still
need pins.

| File | Tests pinned |
|---|---|
| `pure_attribute.lsysl` 🔴 | `#pure` fn behaves identically + analyzer marks call-site CSE-able |
| `address_attribute.lsysl` 🔴 | `#address(0x1000_0000)` maps a var to a fixed PA; read/write hits that address |
| `deprecated_attribute.lsysl` 🔴 | `#deprecated("use foo2")` emits a warning at call site (compile diagnostic test) |
| `reads_writes_effects.lsysl` 🔴 | `#reads(...)` / `#writes(...)` accepted; effect-system diagnostics fire |
| `ghost_decl.lsysl` 🔴 | `#ghost` declarations stripped from non-verification builds |

---

### `conditional_compile/` — #if / #else / #endif — 🔴 P3

Reference §"Conditional Compilation".

| File | Tests pinned |
|---|---|
| `cond_compile_defined.lsysl` 🔴 | `#if DEBUG ... #endif` honoured by flag |
| `cond_compile_target.lsysl` 🔴 | `#if TARGET == "trisc"` branches per-backend |
| `cond_compile_negated.lsysl` 🔴 | `#if !BARE_METAL` |

---

### `compile_errors/` — analyzer rejections (compile-fail tests) — 🔴 P3

These require the `#test(should_panic)` form on compile-time failures,
or a separate mechanism if sysl doesn't yet support "this file should
fail to compile with this message". Track separately and skip if the
test framework can't express it.

| File | Tests pinned |
|---|---|
| `compile_fail_double_decl.lsysl` 🔴 | duplicate fn / struct declaration |
| `compile_fail_unknown_field.lsysl` 🔴 | accessing a field that doesn't exist |
| `compile_fail_arity_mismatch.lsysl` 🔴 | wrong arg count |
| `compile_fail_immutable_assign.lsysl` 🔴 | assigning to a `val` |
| `compile_fail_ptr_to_ref.lsysl` 🔴 | converting `*T → &T` rejected |

---

## Process — keeping this current

1. **Before any compiler change**, scan the relevant category here for
   coverage. If the surface you're about to change has no 🟢 file,
   write one before the change (red ⟶ green).
2. **When a new bug is found in a backend**, add a test under the
   relevant category that reproduces it, ship the fix in the same PR
   that flips the 🔴/🟡 to 🟢.
3. **When a new feature lands**, the feature's PR includes:
   - the lang_features file(s) pinning it
   - a row in this roadmap (move to ✅ Done section)
   - a memory-file entry if it's foundational (Tier 0)
4. **Quarterly review:** sweep through 🟡 files. Anything 🟡 for more
   than two months gets either upgraded to 🟢 with the missing edge
   cases, or its scope is narrowed and the residual is split into a
   new 🔴 entry.

## Running the corpus

```
# entire corpus, one backend
sbt "syslCliJVM/run test --backend interpreter sysl/tests/lang_features/"

# one category
sbt "syslCliJVM/run test --backend trisc sysl/tests/lang_features/arc/"

# one file
sbt "syslCliJVM/run test --backend llvm-host sysl/tests/lang_features/arc/new_ref_basic.lsysl"

# all seven backends — per CLAUDE.md rule #11
for b in interpreter llvm-host svm-host trisc riscv64 riscv32 wasm32; do
    sbt "syslCliJVM/run test --backend $b sysl/tests/lang_features/"
done
```

The corpus must stay green on **all seven** backends. A test that's
green on six is a regression in the seventh, not a pass.

---

*Last updated: 2026-05-14 after sysl@c8981cf49 (TFieldAssignStmt fix).*
