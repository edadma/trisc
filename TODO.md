# Sysl Language — Missing & Incomplete Features

## Planned Features

### `unowned` references
For back-pointers and non-owning references (e.g. parent pointers in trees). ARC can't collect cycles — `unowned` is the escape hatch. Design not started.

---

## LLVM Backend

`SyslLLVMCodegen.scala` exists as a stub. Only a handful of statement/expression types are implemented. Severely incomplete — not usable.
