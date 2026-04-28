# Sysl Language — Missing & Incomplete Features

## Planned Features

### `unowned` references
For back-pointers and non-owning references (e.g. parent pointers in trees). ARC can't collect cycles — `unowned` is the escape hatch. Design not started.

### Explicit type arguments at call sites
`swap[int](&x, &y)` / `make_empty[int]()` syntax. Phases 1, 2, 4, 5 of the original generics plan are done; only this Phase-3 ergonomic remains. Useful when inference can't resolve (zero-arg generics, return-type-only generics). Implementation: `CallAST` needs a `typeArgs: List[TypeAST]` field and the analyzer needs to prefer explicit over inferred (with consistency check when both are given).


---

## LLVM Backend

`SyslLLVMCodegen.scala` was originally landed as a stub but is now substantial (~75% feature coverage, 21 dedicated test files). Confirmed correctness gaps from the 2026-04-28 audit (see `project_sysl_audit_bugs.md` in auto-memory):

- **Unsigned widening uses `sext` instead of `zext`** (`SyslLLVMCodegen.scala:1550-1551`). `u32 → i64` widening turns high-bit-set values negative. Critical for systems code.
- **Unsigned comparisons emit signed predicates** (`:1594, 1599, 1604, 1609`). Same impact.
- `TPreInc` / `TPreDec` not lowered.
- `TSizeof` missing for some cases.
- `?` operator (Try) end-to-end coverage unverified — parser/analyzer desugar, but no LLVM gap-test.
- `#address(N)` MMIO attribute, conditional compilation (`#if/#else`), and the `#pure`/`#reads`/`#writes`/`#ghost`/`#deprecated` attributes are not handled at codegen.
- Data-enum pattern-match payload destructuring is incomplete.

## TRISC Backend

- `saturating_mul` on `u32` and `saturating_*` on 64-bit types throw "not supported" at codegen time (`SyslTriscCodegen.scala:2878, 2880`). Needs 128-bit detection or runtime helpers.
- No explicit divide-by-zero check inserted before `div`/`divu`.
- `f"..."` format strings not implemented (interpreter and LLVM have them).
- TRISC backend is not wired into the `sysl test --backend` runner (`SyslCli.scala:572-574` errors with "not yet implemented"). Infrastructure gap for per-feature TRISC testing.

## SVM Backend

- `genExpr` default case emits placeholder `push_0` for unhandled `TExpr` types (`SyslSVMCodegen.scala:2406`). Silent miscompile — should be a hard error.
- `genStmt` default case is a TODO (`:1226`).
- Type-attribute helpers (`T::Valid`, `T::Image`, `T::Pos`, etc.) not synthesized.
- Param modes (`in`/`out`/`inout`) ignored — all params treated as `in`.
- `require`/`ensure` discard the message string (bare `halt` instead of message-bearing trap).
