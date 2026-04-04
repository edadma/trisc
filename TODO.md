# Sysl Language — Missing & Incomplete Features

## Runtime Safety (codegen)

### Malloc null check
`new Struct(...)` and `new [n]T` call malloc but don't check for null return. If allocation fails, the generated code writes to address 0. Should emit `trap 1` with error code 2 (null pointer).

**Location:** `SyslTriscCodegen.scala`, `TNew` and `TNewArray`

### `abort()` builtin
POSIX-required function. Should emit `trap 1` with error code 3. On bare metal: halts CPU. On OS: kills thread via trap1_fault handler. Not yet implemented.

---

## Planned Features

### `unowned` references
For back-pointers and non-owning references (e.g. parent pointers in trees). ARC can't collect cycles — `unowned` is the escape hatch. Design not started.

---

## LLVM Backend

`SyslLLVMCodegen.scala` exists as a stub. Only a handful of statement/expression types are implemented. Severely incomplete — not usable.
