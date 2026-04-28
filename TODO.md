# Sysl Language — Missing & Incomplete Features

## Planned Features

### `unowned` references
For back-pointers and non-owning references (e.g. parent pointers in trees). ARC can't collect cycles — `unowned` is the escape hatch. Design not started.

### Explicit type arguments at call sites
`swap[int](&x, &y)` / `make_empty[int]()` syntax. Phases 1, 2, 4, 5 of the original generics plan are done; only this Phase-3 ergonomic remains. Useful when inference can't resolve (zero-arg generics, return-type-only generics). Implementation: `CallAST` needs a `typeArgs: List[TypeAST]` field and the analyzer needs to prefer explicit over inferred (with consistency check when both are given).


---

## LLVM Backend

`SyslLLVMCodegen.scala` was originally landed as a stub but is now substantial (~75% feature coverage, 21 dedicated test files). Confirmed correctness gaps from the 2026-04-28 audit (see `project_sysl_audit_bugs.md` in auto-memory):

- ~~**Unsigned widening uses `sext` instead of `zext`**~~ — verified already fixed (audit tests pass; `emitSextIfNeeded` honours signedness).
- ~~**Unsigned comparisons emit signed predicates**~~ — verified already fixed (predicates branch on `isUnsigned`).
- `TPreInc` / `TPreDec` not lowered.
- `TSizeof` missing for some cases.
- `?` operator (Try) end-to-end coverage unverified — parser/analyzer desugar, but no LLVM gap-test.
- `#address(N)` MMIO attribute, conditional compilation (`#if/#else`), and the `#pure`/`#reads`/`#writes`/`#ghost`/`#deprecated` attributes are not handled at codegen.
- Data-enum pattern-match payload destructuring is incomplete.

## TRISC Backend

- ~~`saturating_mul` on `u32`~~ — fixed at sysl@f918d56b (`mulu` + unsigned compare against u32 max).
- ~~`saturating_*` on 64-bit types throw "not supported"~~ — fixed at sysl@271e7696 (six implementations using overflow detection on the wrapped result).
- ~~No explicit divide-by-zero check inserted before `div`/`divu`~~ — fixed at sysl@5cfe61c6.
- `f"..."` format strings not implemented (interpreter and LLVM have them).
- ~~TRISC backend is not wired into the `sysl test --backend` runner~~ — fixed at sysl@c2f59c2c. `runOneTRISC` mirrors `runOneSVM`. Limitations: should_panic message-substring not honoured (asserts/panics drop the message); tests needing malloc/free fail at link. Cross-backend smoke at `sysl/tests/panic_test/`.

## SVM Backend

- ~~`genExpr` / `genStmt` default cases were silent placeholders~~ — fixed at sysl@3ada8ece (2026-04-28); both now `sys.error(...)` on unhandled nodes.
- Type-attribute helpers (`T::Valid`, `T::Image`, `T::Pos`, etc.) not synthesized.
- Param modes (`in`/`out`/`inout`) ignored — all params treated as `in`.
- `require`/`ensure` discard the message string (bare `halt` instead of message-bearing trap).
