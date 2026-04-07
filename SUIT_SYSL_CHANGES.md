# Sysl Changes for Suit UI Toolkit Port

Language and standard library changes that would make the Suit immediate-mode UI toolkit translate cleanly to Sysl, producing pleasant, readable code.

Suit is ~850 lines of Scala. The platform-specific parts (Swing listeners, JPanel, Graphics2D) need TRISC-specific rewrites regardless. The core UI logic (UI, Layout, DrawCommand, Renderer, Style, Rect) is platform-independent and is what this document targets.

---

## Language Changes

### 1. Default Parameter Values

**Impact: Critical** — Suit's API ergonomics depend entirely on this.

Every widget function has 3–8 parameters where most callers only care about 1–2. Without defaults, every call site becomes a wall of boilerplate.

```sysl
// With defaults — readable, matches Suit's Scala API
button(label: string, disabled: bool = false, tooltip: string = "") -> bool

// Call sites
if ui.button("Save") then ...
if ui.button("Delete", disabled = true) then ...
if ui.button("Help", tooltip = "Click for help") then ...
```

Without defaults, every caller must pass everything:
```sysl
if ui.button("Save", false, "") then ...
if ui.button("Delete", true, "") then ...
if ui.button("Help", false, "Click for help") then ...
```

Functions affected: `button`, `label`, `checkbox`, `radioButton`, `textField`, `textArea`, `select`, `progressBar`, `card`, `table`, `drawText`, `drawTextLeft`, `measureText`.

**Implementation sketch:** Extend `ParamAST` with an optional `default: ExprAST`. At call sites, the compiler fills in missing trailing arguments from defaults. No named-argument resolution needed for the minimal version — just allow omitting trailing args that have defaults.

### 2. Named Parameters at Call Sites

**Impact: High** — Makes multi-parameter widget calls self-documenting.

```sysl
// With named params — clear what each arg means
theme = ui.select(id = "f-theme", options = themes, selected = theme, tooltip = "UI theme")

// Without — have to count positions
theme = ui.select("f-theme", themes, theme, 200, false, "UI theme")
```

This matters most for `textField` (7 params), `textArea` (6 params), `select` (6 params), `progressBar` (6 params), `card` (3 params + body), and `table` (4 params).

Named params interact well with default params — together they let callers specify only the non-default values by name, in any order.

### 3. Closures / By-Name Parameters

**Impact: Critical** — The layout API is built around block-scoped nesting.

Suit uses by-name parameters for compositional layout:
```scala
ui.row:
  ui.button("A")
  ui.button("B")

ui.card("Status"):
  ui.label("CPU: 42%")
  if ui.button("Refresh") then ...
```

Without closures, this must degrade to begin/end pairs:
```sysl
ui.beginRow()
ui.button("A")
ui.button("B")
ui.endRow()
```

Begin/end pairs work but are error-prone (forgotten `endRow`, mismatched nesting) and less readable. Closures are already on the roadmap — this is a strong motivating use case.

**Minimum needed:** Zero-argument closures passed as function pointers, or by-name parameters that evaluate a block. No captures needed for the layout case since the UI context is passed explicitly.

Actually — if these are truly zero-capture, they could be implemented as plain `func() -> void` function pointers today, with the caller passing a named function. But that's ugly. The real win is inline block syntax.

### 4. Variadic Functions

**Impact: Low-Medium** — Nice-to-have for table construction.

Suit's `table` takes `Seq[String]` for headers and `Seq[Seq[String]]` for rows. In Sysl this becomes slices, which works fine. Variadic syntax would just make literals cleaner:

```sysl
// With variadics
ui.table(["Name", "Role", "Status"], [
  ["Alice", "Admin", "Online"],
  ["Bob", "Editor", "Away"],
])
```

This already works with slice literals, so variadic functions are not blocking. Listing here only because Suit's test code uses `Seq(...)` everywhere.

---

## Standard Library Additions

### 5. `std.collections.IntMap` — Integer-Keyed Map

**Impact: Critical** — Suit uses `Map[Int, Int]` for per-widget state.

Widget state (cursor positions, scroll offsets, content heights) is keyed by integer widget IDs. Four separate maps in `UI`:
- `cursorPositions: Map[Int, Int]`
- `scrollOffsets: Map[Int, Int]`
- `scrollContentHeights: Map[Int, Int]`
- `textAreaScrolls: Map[Int, Int]`

Needed operations: `get(key) -> (int, bool)`, `set(key, value)`, `get_or_default(key, default) -> int`.

**Implementation:** Open-addressing hash table with integer keys. Doesn't need to be generic initially — `IntMap` (int → int) covers the Suit use case. A simple linear-probe table with power-of-two sizing would be ~100 lines of Sysl.

### 6. `std.collections.IntSet` — Integer Set

**Impact: Medium** — Used for keyboard state tracking.

`InputState` tracks `keysDown: Set[Int]` and `keysPressed: Set[Int]` with operations: add, remove, contains, clear, isEmpty, iteration.

**Alternative:** Since key codes fit in 0–255, a `[256]bool` bitfield works and is simpler. But a proper `IntSet` is more general-purpose.

### 7. `std.collections.Stack[T]` or Fixed-Size Stack

**Impact: Medium** — Layout uses a stack of groups; Graphics2DRenderer uses a clip stack.

```sysl
struct IntStack
    items: [16]int   // or generic [N]T
    top: int
```

Operations: `push`, `pop`, `peek`, `isEmpty`, `clear`. Maximum depth is small and bounded (layout nesting rarely exceeds 8–10 levels).

**Alternative:** Just use a fixed array + index variable inline. But a reusable Stack type is cleaner.

### 8. `std.strings` Additions: `take`, `drop`, `char_at`

**Impact: High** — Text editing (textField, textArea) does heavy string slicing.

Suit's text editing core:
```scala
newValue = value.take(cursor) + ch + value.drop(cursor)           // insert
newValue = value.take(cursor - 1) + value.drop(cursor)            // backspace
newValue = value.take(cursor) + value.drop(cursor + 1)            // delete
```

Needed in `std.strings`:
- `take(s: string, n: int) -> string` — first n bytes (or runes)
- `drop(s: string, n: int) -> string` — everything after first n
- `char_at(s: string, n: int) -> u32` — nth character (already have `s[n]` for bytes)
- `insert(s: string, pos: int, ch: string) -> string` — insert at position
- `remove(s: string, pos: int) -> string` — remove char at position
- `remove_range(s: string, start: int, end_exclusive: int) -> string` — remove range

`split` already exists. `take`/`drop` can be built on slicing (`s[0:n]`, `s[n:]`) if string slicing works that way — need to verify.

### 9. `std.slices` Additions: `take`, `drop`

**Impact: Low** — Convenience for DrawCommand list manipulation.

```sysl
take[T](s: []T, n: int) -> []T    // s[:n] — already works via slice syntax
drop[T](s: []T, n: int) -> []T    // s[n:] — already works via slice syntax
```

These are just wrappers around existing slice syntax, so they may not be worth adding. Listing for completeness.

### 10. Hash Function for Strings

**Impact: Medium** — Suit uses `.hashCode` on strings to generate widget IDs.

```sysl
hash(s: string) -> int
```

Suit identifies widgets by hashing their label or ID string. A simple FNV-1a or djb2 hash would suffice. Could live in `std.strings` or a new `std.hash` module.

**Alternative:** Require the caller to assign explicit integer IDs to every widget. This is what most C IMGUI libraries do (e.g., `dear imgui` uses `ImGui::PushID(int)`). It's more explicit but more tedious.

---

## Summary by Priority

| # | Change | Type | Impact | Blocks Translation? |
|---|--------|------|--------|---------------------|
| 1 | Default parameters | Language | Critical | Yes — every widget call |
| 2 | Named parameters | Language | High | No, but code is ugly without |
| 3 | Closures | Language | Critical | Yes — layout nesting (begin/end workaround exists) |
| 5 | IntMap | Stdlib | Critical | Yes — widget state storage |
| 8 | String take/drop/insert | Stdlib | High | Yes — text editing |
| 10 | String hash | Stdlib | Medium | Yes — widget IDs (explicit IDs as workaround) |
| 6 | IntSet | Stdlib | Medium | No — `[256]bool` workaround |
| 7 | Stack | Stdlib | Medium | No — array + index workaround |
| 4 | Variadic functions | Language | Low | No — slice literals work |
| 9 | Slice take/drop | Stdlib | Low | No — slice syntax works |

### Minimum viable set for a clean port

1. **Default parameters** (language)
2. **IntMap** (stdlib)
3. **String take/drop/insert** (stdlib)
4. **String hash** (stdlib)

With these four, the Suit core translates mechanically. Closures have a begin/end workaround. Named parameters are nice but not blocking. IntSet and Stack have trivial inline alternatives.

### For truly pleasant code, also add

5. **Closures** — block-scoped layout nesting
6. **Named parameters** — self-documenting widget calls
