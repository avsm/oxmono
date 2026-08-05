# Search Bar Redesign: Cyberpunk Split Layout

## Overview

Redesign the arod search modal to visually separate internal bushel items from
external links using a vertically split layout with a cyberpunk/neon aesthetic.

## Layout

The search input sits at the vertical center. Bushel results (papers, notes,
projects, ideas, videos) expand upward above it. External links expand downward
below it.

```
+--[cyan neon border]------------------+
|                                       |
|  BUSHEL ZONE (flex-col-reverse)       |
|  [P] Unikernels: Rise of the...      |
|  [N] Memory-safe OCaml patterns       |
|                                       |
+=======================================+
|  >_ query|                  [K] ESC  |
+=======================================+
|                                       |
|  LINKS ZONE (flex-col)                |
|  ~ arxiv.org  Formal verification...  |
|  ~ github.com mirage/ocaml-dns        |
|                                       |
+--[magenta neon border]---------------+
```

- Bushel zone uses `flex-direction: column-reverse` to grow upward
- Each zone scrolls independently, max-height constrained
- Empty zones collapse to zero height
- Search input is always visible

## Visual Styling

- **Background**: near-black `#0a0a0f` with blue tint
- **Bushel border**: cyan `#00e5ff` with 8px glow shadow
- **Links border**: magenta `#ff00e5` with 8px glow shadow
- **Text**: light gray `#e0e0e0`, white for highlights
- **Prompt**: `>_` in cyan monospace
- **Kind badges**: monospace `[P]`, `[N]`, `[I]`, `[V]`, `[L]`
- **Key hints**: bordered key-cap style badges for ESC, Cmd+K
- **Dark-only**: modal always renders in dark cyberpunk style regardless of site theme

## Animations

**Bushel results (instant + glow pulse):**
- Render immediately on API response
- Each item gets a 300ms `glow-pulse` CSS keyframe animation on its kind-color left border
- 30ms stagger via `animation-delay` per item
- Pure CSS, no JS animation loop

**Link results (slide-and-fade):**
- Start at `opacity: 0; transform: translateX(12px)`
- CSS transition (200ms ease-out) triggered by adding `.visible` class
- JS adds `.visible` with 50ms stagger via `setTimeout`
- ~500ms cascade for 10 results

**Performance:**
- All animations use compositor-friendly properties (opacity, transform)
- 150ms input debounce
- `AbortController` cancels in-flight fetches on new input
- Max 20 results per zone

## Keyboard Navigation

- Arrow keys traverse both zones seamlessly (up from top link enters bottom bushel item)
- Tab jumps between zones
- Enter opens selected item (links open in new tab)
- Selected item gets brighter glow + background highlight

## Data Flow

No backend changes. Existing `/api/search?q=QUERY&limit=LIMIT` returns results
with `kind` field. JS partitions: `kind === "link"` goes to links zone, everything
else goes to bushel zone. Request limit increased to 40 from JS side (20 per zone).

Filter pills restyled to match cyberpunk theme. Selecting entry kinds shows results
only in top zone; selecting links shows only in bottom zone.

## Files to Modify

1. **`lib_component/nav.ml`** - Restructure search modal HTML: three-zone layout
   (bushel zone, search input, links zone), restyle filter pills, zone containers
2. **`lib_component/theme.ml`** - Cyberpunk CSS: neon borders, dark backgrounds,
   glow animations, keyframes, slide-and-fade transitions, monospace badges, key-caps
3. **`lib_component/scripts.ml`** - Rewrite result rendering: partition by kind,
   render into separate zones, glow-pulse for bushel, staggered `.visible` for links,
   cross-zone keyboard navigation

No changes to `arod_search.ml`, `arod_handlers.ml`, or other modules.
