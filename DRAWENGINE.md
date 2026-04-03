# DrawEngine — Feature Roadmap

## Current Capabilities

- **Surfaces**: per-window BufferedImage backing stores, surface 0 = screen
- **Window table**: position, z-order, title, visibility, decorated flag
- **Compositor**: blits all visible windows onto screen with decorations
- **Decorations**: macOS-style title bar, traffic light buttons, shadow, rounded corners
- **Drawing primitives**: fill/stroke rect, rounded rect, line, circle, path, bezier curve, text, image
- **Font rendering**: size and style (plain/bold/italic), hardcoded Sans Serif
- **Clip regions**: per-surface clipping

## Missing — Critical for Desktop OS

- [x] **Hit testing** — HIT_TEST returns window ID at (x,y) in RESULT, flags title bar hit
- [x] **Window resize** — RESIZE_WINDOW recreates surface, preserves content
- [x] **Mouse cursor** — SET_CURSOR_POS, SET_CURSOR_VISIBLE, SET_CURSOR_SURFACE (custom or default arrow)
- [x] **Focus indicator** — topmost window gets brighter title bar, accent border, vivid buttons

## Missing — Important

- [ ] **Dirty tracking** — COMPOSITE redraws everything; should track changed regions
- [x] **Window minimize/maximize** — MINIMIZE, MAXIMIZE, RESTORE with saved normal geometry
- [ ] **Alpha compositing** — windows are opaque blits; support per-window opacity
- [x] **Font family selection** — SET_FONT by name, LOAD_FONT from RAM binary data
- [ ] **Scrolling** — viewport offset per surface for apps with content larger than their window
- [ ] **Window move by drag** — host-side: detect title bar drag, update window position
- [ ] **Z-order query** — command to read current window order

## Missing — Nice to Have

- [ ] **Wallpaper** — command to set a background image that persists across composites
- [ ] **Window animations** — fade in/out, minimize effect
- [ ] **Drop shadows** — Gaussian blur shadow instead of solid offset
- [ ] **Subpixel text rendering** — LCD-optimized text
- [ ] **Gradient fills** — linear and radial gradients
- [ ] **Icon support** — per-window icon for title bar and taskbar
- [ ] **Multi-monitor** — multiple framebuffer outputs
