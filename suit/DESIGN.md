# SUIT — Sysl UI Toolkit

A lightweight UI toolkit for SLIX, built on the display server and DrawEngine.

## Design Philosophy

Declarative retained mode — immediate mode ergonomics with retained state.
You describe the UI with concise nested function calls each frame. The toolkit
retains a shadow tree for layout, event dispatch, animations, and widget state.

Inspired by SwiftUI/Flutter/Compose, adapted for Sysl's constraints.

## API Style

```
ui_window("Settings", 300, 200)
    ui_vbox()
        ui_label("Name:")
        val name = ui_input(20)
        ui_hbox()
            if ui_button("OK")
                save(name)
            if ui_button("Cancel")
                close()
```

### Key principles

- **Call stack = tree structure.** Container calls (vbox, hbox, panel) push a
  layout context. Children are subsequent calls at the next indent level.
  No explicit add/remove/parent wiring.

- **Buttons return clicks.** `if ui_button("OK")` — no callbacks, no event
  listeners, no signal/slot. As concise as immediate mode.

- **State is retained.** Text fields keep cursor, selection, scroll position
  internally. The toolkit tracks widgets by position in the call tree
  (stable implicit IDs). App state lives in app variables, not the toolkit.

- **Layout is automatic.** vbox/hbox do flex layout. Hints via `ui_expand()`,
  `ui_fixed(width)`, `ui_spacing(n)`. Two-pass layout (measure then place)
  handles content-dependent sizing.

- **Animations are built in.** The toolkit owns a frame timer.
  `ui_animate(from, to, duration)` returns the current interpolated value.
  Blinking cursors, transitions, progress bars — no manual timers.

- **Redraw on change.** The shadow tree diffs against the previous frame.
  Only dirty regions are redrawn. No redraw when nothing changed.

## Architecture

```
Application code
    |
    v
SUIT (widget description, layout, event dispatch, state)
    |
    v
Display Server (window management, compositing, IPC)
    |
    v
DrawEngine (2D rendering, surfaces, decorations)
    |
    v
FramebufferImage (pixel output)
```

SUIT talks to the display server via IPC for window management and uses
direct DrawEngine access for rendering (client-side rendering pattern).

## Widget Set (planned)

### Basic
- `ui_label(text)` — static text
- `ui_button(text) -> bool` — returns true on click
- `ui_input(width) -> *i8` — single-line text input, returns buffer
- `ui_checkbox(label, *bool)` — toggle, reads/writes bool pointer
- `ui_slider(min, max, *int)` — horizontal slider

### Layout
- `ui_vbox()` / `ui_hbox()` — vertical/horizontal flex container
- `ui_expand()` — next widget fills remaining space
- `ui_fixed(size)` — next widget has fixed size
- `ui_spacing(n)` — gap between children
- `ui_padding(n)` — inset content
- `ui_scroll()` — scrollable container

### Composite
- `ui_panel(title)` — bordered group with title
- `ui_tabs()` / `ui_tab(label)` — tabbed container
- `ui_menu()` / `ui_menu_item(label) -> bool` — dropdown menu
- `ui_list(items, count, render_fn)` — virtual scrolling list
- `ui_tree(node, render_fn)` — collapsible tree

### Feedback
- `ui_progress(value, max)` — progress bar
- `ui_spinner()` — indeterminate loading
- `ui_toast(message)` — temporary notification
- `ui_tooltip(text)` — hover text

## Theming

Flat style with a small color palette. Colors defined as module-level
constants that applications can override:

- `SUIT_BG`, `SUIT_FG` — base background/foreground
- `SUIT_ACCENT` — buttons, focus rings, selection
- `SUIT_INPUT_BG` — text field background
- `SUIT_BORDER` — widget borders
- `SUIT_HOVER`, `SUIT_ACTIVE` — interaction states

Font size and family configurable via `ui_set_font(name, size)`.

## Event Model

Events flow top-down through the widget tree (hit testing) and bubble up
for handling. The display server delivers raw mouse/keyboard events via IPC.
SUIT translates them into widget-level events (click, key press, focus change).

Focus is tracked internally — Tab moves focus, widgets receive key events
only when focused. The focused widget gets a visible focus ring.

## Shadow Tree and Diffing

Each frame, the application's UI calls build a "description" of the tree.
The toolkit compares this against the retained shadow tree:

- New widgets: create, measure, place, render
- Removed widgets: dispose state, mark region dirty
- Changed widgets: update properties, re-render if visual change
- Unchanged widgets: skip entirely

Widget identity is determined by position in the call tree plus type.
An explicit `ui_key(id)` override is available for dynamic lists.

## Dependencies

- Display server (IPC for window create/destroy/composite)
- DrawEngine (direct register access for rendering)
- Keyboard/mouse events via display server
- OS kernel (threads, IPC, timers for animations)
- Sysl string support (for text handling)

## Status

Design phase. Implementation blocked on Sysl string handling improvements.
