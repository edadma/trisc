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

- [ ] **Hit testing** — command to query "which window is at (x,y)?" for input routing
- [ ] **Window resize** — resize a window (recreate surface, preserve content or clear)
- [ ] **Mouse cursor** — render a cursor sprite at a given position during composite
- [ ] **Focus indicator** — visually distinguish the focused window (brighter title bar, accent border)

## Missing — Important

- [ ] **Dirty tracking** — COMPOSITE redraws everything; should track changed regions
- [ ] **Window minimize/maximize** — flags exist but no behavior
- [ ] **Alpha compositing** — windows are opaque blits; support per-window opacity
- [ ] **Font family selection** — register to choose Sans Serif, Monospaced, Serif
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
