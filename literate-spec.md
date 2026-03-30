# Literate Sysl — Redesign Spec

## Overview

A literate `.lsysl` file is a Markdown document with embedded Sysl code. The parser distinguishes prose from code using two rules:

1. **Column 0 = prose** (Markdown)
2. **Indented = code** (Sysl)

The one exception: indented Markdown constructs (nested lists, continuation paragraphs) remain prose. The parser recognizes these by their Markdown syntax.

Optional `@tag` annotations allow prose to be categorized for selective output (e.g., developer notes vs. user-facing docs) without changing the prose/code distinction.

## Parsing Rules

### Prose

Any line starting at column 0 is prose. Prose is Markdown — headings, paragraphs, lists, block quotes, inline LaTeX (`$...$`, `$$...$$`), links, emphasis, etc.

Blank lines are structural separators (as in Markdown).

### Code

Any indented line is code by default. One level of indentation is stripped to produce the tangled output (matching current behavior: tab, 4 spaces, or 2 spaces).

A code block is a contiguous run of indented lines, possibly containing blank lines. A code block ends when a non-blank column-0 line appears.

### Indented Markdown (the exception)

Certain indented lines are recognized as prose, not code, when they appear in an active Markdown list context:

1. **Nested list items** — indented lines starting with `- `, `* `, `+ `, or `N. ` (ordered list marker)
2. **Continuation paragraphs** — indented text following a blank line within a list context

A **list context** is active when the most recent column-0 prose element is a list item (starts with `- `, `* `, `+ `, or `N. `). The list context ends when:
- A column-0 line that is not a list item appears (heading, paragraph, etc.)
- Two consecutive blank lines appear (hard break)

Within a list context, an indented line is prose if:
- It starts with a list marker (`- `, `* `, `+ `, `N. `), OR
- It follows a blank line and does NOT look like Sysl (i.e., the first non-whitespace token is not a Sysl keyword or identifier followed by declaration syntax)

Outside a list context, all indented lines are code.

### Fenced Code Blocks

Markdown fenced code blocks (`` ``` ``) in prose are passed through as prose. They are for non-Sysl code examples in documentation (e.g., showing assembly output, shell commands). They are never tangled.

Fenced blocks start with `` ``` `` at column 0 and end with `` ``` `` at column 0. Everything between is prose.

## Example

```
# Circle Drawing

This module draws circles using Bresenham's algorithm.

Key properties:

- Uses only integer arithmetic
- Efficient for embedded systems:
  - No floating-point unit required
  - Constant memory usage
- Produces pixel-perfect circles

The algorithm maintains an error term that determines whether to
step in x, y, or both.

    import "graphics"

    struct Point
        x: i32
        y: i32

    draw_circle(center: Point, radius: i32)
        var d = 3 - 2 * radius
        var x = 0
        var y = radius

        while x <= y
            plot_octants(center, x, y)
            if d < 0
                d = d + 4 * x + 6
            else
                d = d + 4 * (x - y) + 10
                y = y - 1
            x = x + 1

The helper function plots all eight symmetric points:

    plot_octants(c: Point, x: i32, y: i32)
        plot(c.x + x, c.y + y)
        plot(c.x - x, c.y + y)
        plot(c.x + x, c.y - y)
        plot(c.x - x, c.y - y)
        plot(c.x + y, c.y + x)
        plot(c.x - y, c.y + x)
        plot(c.x + y, c.y - x)
        plot(c.x - y, c.y - x)
```

### Tangle output

Strips all prose, concatenates code blocks, strips one indent level:

```sysl
import "graphics"

struct Point
    x: i32
    y: i32

draw_circle(center: Point, radius: i32)
    var d = 3 - 2 * radius
    var x = 0
    var y = radius

    while x <= y
        plot_octants(center, x, y)
        if d < 0
            d = d + 4 * x + 6
        else
            d = d + 4 * (x - y) + 10
            y = y - 1
        x = x + 1

plot_octants(c: Point, x: i32, y: i32)
    plot(c.x + x, c.y + y)
    plot(c.x - x, c.y + y)
    plot(c.x + x, c.y - y)
    plot(c.x - x, c.y - y)
    plot(c.x + y, c.y + x)
    plot(c.x - y, c.y + x)
    plot(c.x + y, c.y - x)
    plot(c.x - y, c.y - x)
```

### Weave output

Produces Markdown with code in fenced blocks:

````markdown
# Circle Drawing

This module draws circles using Bresenham's algorithm.

Key properties:

- Uses only integer arithmetic
- Efficient for embedded systems:
  - No floating-point unit required
  - Constant memory usage
- Produces pixel-perfect circles

The algorithm maintains an error term that determines whether to
step in x, y, or both.

```sysl
import "graphics"

struct Point
    x: i32
    y: i32

draw_circle(center: Point, radius: i32)
    var d = 3 - 2 * radius
    var x = 0
    var y = radius

    while x <= y
        plot_octants(center, x, y)
        if d < 0
            d = d + 4 * x + 6
        else
            d = d + 4 * (x - y) + 10
            y = y - 1
        x = x + 1
```

The helper function plots all eight symmetric points:

```sysl
plot_octants(c: Point, x: i32, y: i32)
    plot(c.x + x, c.y + y)
    plot(c.x - x, c.y + y)
    plot(c.x + x, c.y - y)
    plot(c.x - x, c.y - y)
    plot(c.x + y, c.y + x)
    plot(c.x - y, c.y + x)
    plot(c.x + y, c.y - x)
    plot(c.x - y, c.y - x)
```
````

## Tags

Tags are optional annotations for categorizing prose. They control which prose appears in different weave outputs (e.g., developer docs vs. user-facing website). Tags never affect code blocks or tangling.

### `@tag` (auto-closing)

A bare `@tag` on its own line tags all prose that follows it, up to the next code block:

```
@internal
This explanation is for developers only. It covers the
implementation details of the scheduler.

    schedule() -> int
        ...
```

The `@internal` tag applies to the paragraph. When the code block starts, the tag scope ends — the next prose block is untagged again.

### `@tag:` (explicit close)

A `@tag:` with a colon requires a closing `@` on its own line. Use this when tagged prose spans across code blocks:

```
@tutorial:
First we define the data structure:

    struct Point
        x: i32
        y: i32

Then we write the constructor:

    make_point(x: i32, y: i32) -> Point
        val p: Point
        p.x = x
        p.y = y
        p
@
```

Everything between `@tutorial:` and `@` (both prose and code) is tagged. The code is still tangled normally — the tag only affects weave output.

### `@api` (declaration doc)

`@api` is a special tag for documenting declarations. Two forms:

**Inline:** `@api` followed by text on the same line — the text is the description, associated with the next code declaration:

```
@api Draws a circle using Bresenham's algorithm.
    draw_circle(center: Point, radius: i32)
        ...
```

**Block:** `@api` on its own line — the parser looks at the next code block and extracts the function, variable, or type declaration signature automatically:

```
@api
    draw_circle(center: Point, radius: i32)
        ...
```

`@api` never requires a closing tag. Its scope is always the immediately following declaration.

### Tag Filtering in Weave

Weave accepts a `--tags` option to include/exclude tagged prose:

```
docs weave input.lsysl -o output.md                     # all prose
docs weave input.lsysl -o output.md --tags tutorial      # only @tutorial prose + untagged
docs weave input.lsysl -o output.md --exclude internal   # everything except @internal
docs weave input.lsysl -o output.md --tags api           # API reference
```

Untagged prose is included by default in all modes.

## Source Line Map

The parser maintains a mapping from tangled code line numbers back to `.lsysl` line numbers. This allows compiler errors to reference the original literate source.

## What's Removed

- Indentation as the sole prose/code distinguisher — replaced by Markdown-aware parsing
- Predefined channels (code, dev, website, api, reference) — replaced by free-form tags with `--tags`/`--exclude` filtering
- `ChannelConfig` — removed
- Format suffixes (`@tag-latex`) — LaTeX is inline via Markdown `$...$`
- `@` block syntax for prose — replaced by `@tag` (auto-close at code) and `@tag:` ... `@` (explicit close)

## Operations

### Tangle

Extract code: collect all code blocks, strip one indent level, join with blank line separators. Produce source line map.

### Weave

Generate documentation: pass prose through as Markdown, wrap code blocks in `` ```sysl `` fenced blocks. The output is a standard Markdown file renderable by any Markdown processor.

## File Extension

`.lsysl` (unchanged).

## CLI

```
docs tangle input.lsysl -o output.sysl
docs weave input.lsysl -o output.md
```

Batch mode for directories remains supported.
