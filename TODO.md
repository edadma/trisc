# Sysl Language — Missing & Incomplete Features

## Runtime Safety (codegen)

These features work in the interpreter but are missing or incomplete in TRISC codegen.

### `trap` instruction
The TRISC CPU has no trap instruction. One must be added — it should halt execution with an error code. This is the foundation for all runtime error handling in generated code.

### Bounds checking on slice indexing
`&[]T[i]` indexing loads the slice length but never emits a trap on out-of-bounds access. The interpreter catches this; generated code silently corrupts memory.

**Location:** `SyslTriscCodegen.scala` line ~1599

### Malloc null check
`new Struct(...)` and `new [n]T` call malloc but don't check for null return. If allocation fails, the generated code writes to address 0.

**Location:** `SyslTriscCodegen.scala`, `TNew` (~line 1817) and `TNewArray` (~line 1756)

### `abort()` builtin
POSIX-required function. Should emit `trap`. On bare metal: halt CPU. On OS: kill process. Not yet implemented.

---

---

## Codegen Gaps

### `len()` / `cap()` on all types
`len()` works for strings, fixed arrays, and `RefType(SliceType)`. Other type variants (e.g. bare `SliceType`) fall through to a TODO comment. Same for `cap()`.

**Location:** `SyslTriscCodegen.scala` lines ~1725, ~1736

### Catch-all fallthroughs
Unmatched statement and expression types in `genStmt`/`genExpr` silently emit `# TODO` assembly comments instead of failing at compile time.

**Location:** `SyslTriscCodegen.scala` lines ~944, ~1934

---

## Planned Features

### `unowned` references
For back-pointers and non-owning references (e.g. parent pointers in trees). ARC can't collect cycles — `unowned` is the escape hatch. Design not started.

### Array decay cleanup
Arrays should implicitly pass as `*T` without requiring `&arr[0]`. Partial support exists but the rules aren't finalized.

---

## LLVM Backend

`SyslLLVMCodegen.scala` exists as a stub. Only a handful of statement/expression types are implemented. Severely incomplete — not usable.
