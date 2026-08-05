# Search Bar Cyberpunk Redesign Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Redesign the arod search modal into a cyberpunk split layout where bushel items expand upward above the search input and external links slide in below it, with neon borders and animations.

**Architecture:** Single-API client-side split. The existing `/api/search` endpoint is unchanged. JavaScript partitions results by `kind === "link"` into two zones rendered in separate DOM containers. CSS handles all animations via compositor-friendly properties (opacity, transform).

**Tech Stack:** OCaml (Tw_html/Htmlit for HTML generation), vanilla JavaScript (ES5+), CSS3 keyframes/transitions.

---

### Task 1: Restructure search modal HTML (nav.ml)

**Files:**
- Modify: `avsm/arod/lib_component/nav.ml:227-295`

**Context:** The `search_modal` value (lines 227-295) builds the modal HTML. Currently it's: search input row -> filter pills -> single results area -> footer. We need to restructure it to: bushel zone -> search input row with filter pills -> links zone -> footer.

**Step 1: Rewrite the `search_modal` value**

Replace lines 227-295 with a new structure. The key changes:
- The modal gets a wrapper class `cyberpunk-search` that forces dark theme
- Search input row moves to the middle, with filter pills inline
- Two result zones: `search-bushel` (above input) and `search-links` (below input)
- Footer stays at the bottom
- Remove the separate filter pills row - integrate pills into the search input row to save space

```ocaml
let search_modal =
  let kbd cls txt =
    El.unsafe_raw (Printf.sprintf
      {|<kbd class="cyber-kbd %s">%s</kbd>|}
      cls txt)
  in
  El.div
    ~at:[
      At.id "search-modal-overlay";
      At.class' "search-modal-overlay items-center justify-center p-4";
    ]
    [
      El.div
        ~at:[At.class' "cyberpunk-search w-full max-w-2xl";
             At.v "role" "search"]
        [
          (* Bushel results zone - grows upward *)
          El.div
            ~at:[ At.id "search-bushel";
                  At.class' "cyber-zone cyber-zone-bushel" ]
            [];
          (* Search input row with integrated filter pills *)
          El.div
            ~at:[At.class' "cyber-input-row"]
            [
              El.span ~at:[At.class' "cyber-prompt"] [ El.txt ">_" ];
              El.input
                ~at:[
                  At.id "search-input";
                  At.type' "text";
                  At.v "placeholder" "search...";
                  At.autocomplete "off";
                  At.class' "cyber-input";
                ] ();
              (* Inline filter pills *)
              El.div
                ~at:[At.id "search-filters"; At.class' "cyber-filters"]
                [
                  search_filter_pill ~active:false "paper" "P";
                  search_filter_pill ~active:false "note" "N";
                  search_filter_pill ~active:false "project" "Pr";
                  search_filter_pill ~active:false "idea" "I";
                  search_filter_pill ~active:false "video" "V";
                  search_filter_pill ~active:false "link" "L";
                ];
              kbd "shrink-0" "esc";
            ];
          (* Links results zone - grows downward *)
          El.div
            ~at:[ At.id "search-links";
                  At.class' "cyber-zone cyber-zone-links" ]
            [];
          (* Footer *)
          El.div
            ~at:[At.class' "cyber-footer"]
            [
              El.span ~at:[At.id "search-count"; At.class' "cyber-count"] [];
              El.div ~at:[At.class' "cyber-hints"]
                [
                  El.span [ kbd "" "\xE2\x86\x91\xE2\x86\x93"; El.txt " nav" ];
                  El.span [ kbd "" "tab"; El.txt " zone" ];
                  El.span [ kbd "" "\xE2\x86\xB5"; El.txt " open" ];
                ];
            ];
        ];
    ]
```

**Step 2: Update `search_filter_pill` for compact cyberpunk style**

The pills now show short labels (P, N, Pr, I, V, L) and use `cyber-pill` class:

```ocaml
let search_filter_pill ~active kind label =
  El.button
    ~at:[
      At.class' ("cyber-pill" ^ (if active then " active" else ""));
      At.v "data-kind" kind;
      At.v "title" (match kind with
        | "paper" -> "Papers" | "note" -> "Notes" | "project" -> "Projects"
        | "idea" -> "Ideas" | "video" -> "Videos" | "link" -> "Links"
        | _ -> kind);
    ]
    [ El.txt label ]
```

**Step 3: Build and verify compilation**

Run: `dune build avsm/arod/`
Expected: Compiles successfully (CSS/JS changes come in later tasks, but HTML structure is valid)

**Step 4: Commit**

```bash
git add avsm/arod/lib_component/nav.ml
git commit -m "refactor(arod): restructure search modal HTML for split layout"
```

---

### Task 2: Cyberpunk CSS theme (theme.ml)

**Files:**
- Modify: `avsm/arod/lib_component/theme.ml:1293-1485` (search CSS section)
- Modify: `avsm/arod/lib_component/theme.ml:2248-2273` (filter pill CSS)

**Context:** Replace the existing search modal CSS with cyberpunk-themed styles. The modal uses class `cyberpunk-search` which forces a dark aesthetic regardless of site theme. Two zones get different neon border colors: cyan for bushel, magenta for links.

**Step 1: Replace the search modal CSS block (lines 1293-1485)**

Remove all existing `.search-modal-overlay` through `.sr-parent:hover` CSS and replace with:

```css
/* ===== Cyberpunk Search Modal ===== */
.search-modal-overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.7);
  backdrop-filter: blur(4px);
  z-index: 60;
  display: none;
}
.search-modal-overlay.active { display: flex; }

/* Force dark cyberpunk theme regardless of site mode */
.cyberpunk-search {
  --cyber-bg: #0a0a0f;
  --cyber-surface: #12121f;
  --cyber-border: #1e1e3a;
  --cyber-text: #e0e0e0;
  --cyber-muted: #6b6b8a;
  --cyber-cyan: #00e5ff;
  --cyber-magenta: #ff00e5;
  --cyber-cyan-dim: rgba(0, 229, 255, 0.15);
  --cyber-magenta-dim: rgba(255, 0, 229, 0.15);

  background: var(--cyber-bg);
  border: 1px solid var(--cyber-border);
  border-radius: 0.75rem;
  overflow: hidden;
  color: var(--cyber-text);
  font-family: ui-monospace, 'SF Mono', 'Cascadia Code', monospace;
  box-shadow: 0 0 40px rgba(0, 0, 0, 0.6);
}

/* --- Search input row --- */
.cyber-input-row {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.6rem 0.75rem;
  background: var(--cyber-surface);
  border-top: 1px solid var(--cyber-border);
  border-bottom: 1px solid var(--cyber-border);
  position: relative;
}
.cyber-input-row::after {
  content: '';
  position: absolute;
  bottom: -1px;
  left: 10%;
  right: 10%;
  height: 1px;
  background: linear-gradient(90deg, transparent, var(--cyber-cyan), transparent);
  animation: cyber-glow-line 2s ease-in-out infinite;
}
@keyframes cyber-glow-line {
  0%, 100% { opacity: 0.3; }
  50% { opacity: 0.8; }
}
.cyber-prompt {
  color: var(--cyber-cyan);
  font-weight: 700;
  font-size: 0.85rem;
  flex-shrink: 0;
}
.cyber-input {
  flex: 1;
  min-width: 0;
  background: transparent;
  border: none;
  outline: none;
  color: var(--cyber-text);
  font-size: 0.85rem;
  font-family: inherit;
  caret-color: var(--cyber-cyan);
}
.cyber-input::placeholder {
  color: var(--cyber-muted);
}

/* --- Filter pills (inline) --- */
.cyber-filters {
  display: flex;
  gap: 0.2rem;
  flex-shrink: 0;
}

/* --- Key-cap badges --- */
.cyber-kbd {
  display: inline-flex;
  align-items: center;
  padding: 0.1rem 0.35rem;
  background: var(--cyber-bg);
  border: 1px solid var(--cyber-border);
  border-radius: 3px;
  font-size: 0.65rem;
  color: var(--cyber-muted);
  font-family: inherit;
  line-height: 1.4;
}

/* --- Result zones --- */
.cyber-zone {
  overflow-y: auto;
  transition: max-height 0.2s ease-out;
  scrollbar-width: thin;
  scrollbar-color: var(--cyber-border) transparent;
}
.cyber-zone:empty {
  max-height: 0;
  padding: 0;
}
.cyber-zone-bushel {
  max-height: min(16rem, 35vh);
  display: flex;
  flex-direction: column-reverse;
  border-top: 1px solid var(--cyber-cyan);
  box-shadow: inset 0 1px 12px var(--cyber-cyan-dim);
  border-radius: 0.75rem 0.75rem 0 0;
}
.cyber-zone-bushel:empty {
  border-top-color: transparent;
  box-shadow: none;
}
.cyber-zone-links {
  max-height: min(16rem, 35vh);
  border-bottom: 1px solid var(--cyber-magenta);
  box-shadow: inset 0 -1px 12px var(--cyber-magenta-dim);
  border-radius: 0 0 0.75rem 0.75rem;
}
.cyber-zone-links:empty {
  border-bottom-color: transparent;
  box-shadow: none;
}

/* --- Individual result rows --- */
.cyber-result {
  padding: 0.35rem 0.75rem;
  cursor: pointer;
  border-left: 2px solid transparent;
  transition: background 0.08s, border-color 0.08s;
  display: flex;
  align-items: center;
  gap: 0.4rem;
  text-decoration: none;
  color: var(--cyber-text);
  font-size: 0.8rem;
  min-height: 1.8rem;
}
.cyber-result:hover,
.cyber-result.selected {
  background: var(--cyber-surface);
}

/* Bushel result selected state — cyan glow */
.cyber-zone-bushel .cyber-result.selected {
  border-left-color: var(--cyber-cyan);
  box-shadow: -2px 0 8px var(--cyber-cyan-dim);
}
/* Link result selected state — magenta glow */
.cyber-zone-links .cyber-result.selected {
  border-left-color: var(--cyber-magenta);
  box-shadow: -2px 0 8px var(--cyber-magenta-dim);
}

.cyber-result .cr-badge {
  font-size: 0.6rem;
  font-weight: 700;
  width: 1.3rem;
  text-align: center;
  flex-shrink: 0;
  text-transform: uppercase;
}
.cr-badge-paper { color: #3b82f6; }
.cr-badge-note { color: #10b981; }
.cr-badge-project { color: #8b5cf6; }
.cr-badge-idea { color: #f59e0b; }
.cr-badge-video { color: #ef4444; }

.cyber-result .cr-title {
  flex: 1;
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
.cyber-result .cr-title b {
  color: #fff;
  font-weight: 600;
}
.cyber-result .cr-date {
  font-size: 0.65rem;
  color: var(--cyber-muted);
  flex-shrink: 0;
  margin-left: auto;
}
.cyber-result .cr-domain {
  font-size: 0.65rem;
  color: var(--cyber-magenta);
  flex-shrink: 0;
  opacity: 0.8;
}
.cyber-result .cr-tags {
  display: flex;
  gap: 0.25rem;
  flex-shrink: 0;
}
.cyber-result .cr-tag {
  font-size: 0.6rem;
  color: var(--cyber-cyan);
  cursor: pointer;
  text-decoration: none;
  opacity: 0.7;
}
.cyber-result .cr-tag:hover {
  opacity: 1;
  text-decoration: underline;
}

/* --- Glow-pulse animation for bushel results --- */
@keyframes glow-pulse-cyan {
  0% { border-left-color: var(--cyber-cyan); box-shadow: -2px 0 12px var(--cyber-cyan-dim); }
  100% { border-left-color: transparent; box-shadow: none; }
}
.cyber-zone-bushel .cyber-result.glow-in {
  animation: glow-pulse-cyan 0.3s ease-out forwards;
}

/* Per-kind glow pulse colors */
@keyframes glow-pulse-blue {
  0% { border-left-color: #3b82f6; box-shadow: -2px 0 12px rgba(59, 130, 246, 0.3); }
  100% { border-left-color: transparent; box-shadow: none; }
}
@keyframes glow-pulse-green {
  0% { border-left-color: #10b981; box-shadow: -2px 0 12px rgba(16, 185, 129, 0.3); }
  100% { border-left-color: transparent; box-shadow: none; }
}
@keyframes glow-pulse-purple {
  0% { border-left-color: #8b5cf6; box-shadow: -2px 0 12px rgba(139, 92, 246, 0.3); }
  100% { border-left-color: transparent; box-shadow: none; }
}
@keyframes glow-pulse-amber {
  0% { border-left-color: #f59e0b; box-shadow: -2px 0 12px rgba(245, 158, 11, 0.3); }
  100% { border-left-color: transparent; box-shadow: none; }
}
@keyframes glow-pulse-red {
  0% { border-left-color: #ef4444; box-shadow: -2px 0 12px rgba(239, 68, 68, 0.3); }
  100% { border-left-color: transparent; box-shadow: none; }
}
.cyber-result.glow-paper { animation-name: glow-pulse-blue; }
.cyber-result.glow-note { animation-name: glow-pulse-green; }
.cyber-result.glow-project { animation-name: glow-pulse-purple; }
.cyber-result.glow-idea { animation-name: glow-pulse-amber; }
.cyber-result.glow-video { animation-name: glow-pulse-red; }

/* --- Slide-and-fade for link results --- */
.cyber-zone-links .cyber-result {
  opacity: 0;
  transform: translateX(12px);
  transition: opacity 0.2s ease-out, transform 0.2s ease-out;
}
.cyber-zone-links .cyber-result.visible {
  opacity: 1;
  transform: translateX(0);
}

/* --- Empty / no results states --- */
.cyber-empty {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 1.5rem 1rem;
  gap: 0.25rem;
  color: var(--cyber-muted);
  font-size: 0.8rem;
}

/* --- Footer --- */
.cyber-footer {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.35rem 0.75rem;
  border-top: 1px solid var(--cyber-border);
  font-size: 0.65rem;
}
.cyber-count {
  color: var(--cyber-muted);
}
.cyber-hints {
  display: flex;
  align-items: center;
  gap: 0.6rem;
  color: var(--cyber-muted);
}
```

**Step 2: Replace the filter pill CSS (lines 2248-2273)**

```css
/* Cyberpunk filter pills */
.cyber-pill {
  padding: 0.1rem 0.3rem;
  border-radius: 3px;
  border: 1px solid var(--cyber-border, #1e1e3a);
  color: var(--cyber-muted, #6b6b8a);
  background: none;
  cursor: pointer;
  font-size: 0.6rem;
  font-family: ui-monospace, 'SF Mono', monospace;
  font-weight: 600;
  white-space: nowrap;
  transition: border-color 0.1s, color 0.1s, background 0.1s;
}
.cyber-pill:hover {
  border-color: var(--cyber-cyan, #00e5ff);
  color: var(--cyber-text, #e0e0e0);
}
.cyber-pill.active {
  border-color: var(--cyber-cyan, #00e5ff);
  color: var(--cyber-cyan, #00e5ff);
  background: rgba(0, 229, 255, 0.1);
}
```

**Step 3: Build and verify**

Run: `dune build avsm/arod/`
Expected: Compiles

**Step 4: Commit**

```bash
git add avsm/arod/lib_component/theme.ml
git commit -m "style(arod): add cyberpunk neon CSS for search modal"
```

---

### Task 3: Rewrite search JavaScript (scripts.ml)

**Files:**
- Modify: `avsm/arod/lib_component/scripts.ml:321-694` (the `search_js` string)

**Context:** The `search_js` OCaml string contains ~370 lines of vanilla JS. It needs to be rewritten to: partition results into bushel vs links, render into separate zone containers, apply glow-pulse to bushel items (with staggered animation-delay), apply staggered slide-and-fade to link items, handle cross-zone keyboard navigation, and add AbortController for fetch cancellation.

**Step 1: Rewrite the `search_js` string**

Replace the entire `search_js` value. Key changes from the old code:

1. **New DOM references**: `bushelEl` and `linksEl` instead of single `resultsEl`
2. **Partition logic**: After fetch, split `data.results` into `bushelResults` (kind !== 'link') and `linkResults` (kind === 'link')
3. **Render bushel**: Build HTML for each entry with `.cyber-result .glow-in .glow-{kind}` classes, stagger `animation-delay`
4. **Render links**: Build HTML with `.cyber-result` (no `.visible` yet), then `setTimeout` stagger to add `.visible`
5. **Cross-zone navigation**: Arrow keys treat `allResults = bushelResults.concat(linkResults)` as a single linear list. Track `selectedZone` ('bushel' | 'links') and `selectedIndex` within zone. Tab switches zone.
6. **AbortController**: Cancel previous fetch on new input
7. **Limit**: Request `limit=40` from API

```javascript
// Search — cyberpunk split-zone search with animations
(function() {
  var toggleBtn = document.getElementById('search-toggle-btn');
  var overlay = document.getElementById('search-modal-overlay');
  if (!toggleBtn || !overlay) return;

  var input = document.getElementById('search-input');
  var bushelEl = document.getElementById('search-bushel');
  var linksEl = document.getElementById('search-links');
  var countEl = document.getElementById('search-count');
  var pills = overlay.querySelectorAll('.cyber-pill');

  var activeKinds = new Set();
  var bushelResults = [];
  var linkResults = [];
  var selectedZone = 'bushel'; // 'bushel' or 'links'
  var selectedIdx = -1;
  var debounceTimer = null;
  var abortCtrl = null;

  // Kind badge labels
  var kindBadge = {
    paper: 'P', note: 'N', project: 'Pr', idea: 'I', video: 'V', link: 'L'
  };
  var kindGlow = {
    paper: 'glow-paper', note: 'glow-note', project: 'glow-project',
    idea: 'glow-idea', video: 'glow-video'
  };
  var kindBadgeCls = {
    paper: 'cr-badge-paper', note: 'cr-badge-note', project: 'cr-badge-project',
    idea: 'cr-badge-idea', video: 'cr-badge-video'
  };

  function escapeHtml(s) {
    var d = document.createElement('div');
    d.appendChild(document.createTextNode(s));
    return d.innerHTML;
  }

  function urlDomain(url) {
    try { return url.replace(/^https?:\/\//, '').split('/')[0]; } catch(e) { return url; }
  }

  function openSearch() {
    overlay.classList.add('active');
    if (input) { input.value = ''; input.focus(); }
    bushelResults = [];
    linkResults = [];
    selectedZone = 'bushel';
    selectedIdx = -1;
    bushelEl.innerHTML = '';
    linksEl.innerHTML = '';
    if (countEl) countEl.textContent = '';
    document.body.style.overflow = 'hidden';
  }

  function closeSearch() {
    overlay.classList.remove('active');
    document.body.style.overflow = '';
    if (abortCtrl) { abortCtrl.abort(); abortCtrl = null; }
  }

  function renderBushel() {
    if (!bushelResults.length) { bushelEl.innerHTML = ''; return; }
    var html = '';
    for (var i = 0; i < bushelResults.length; i++) {
      var r = bushelResults[i];
      var sel = (selectedZone === 'bushel' && i === selectedIdx) ? ' selected' : '';
      var glow = kindGlow[r.kind] || '';
      var delay = (i * 30) + 'ms';
      var badgeCls = kindBadgeCls[r.kind] || '';

      html += '<a href="' + escapeHtml(r.url) + '" class="cyber-result glow-in ' + glow + sel + '" data-zone="bushel" data-idx="' + i + '" style="animation-delay:' + delay + '">';
      html += '<span class="cr-badge ' + badgeCls + '">' + (kindBadge[r.kind] || '?') + '</span>';
      html += '<span class="cr-title">' + escapeHtml(r.title) + '</span>';
      html += '<span class="cr-date">' + r.date + '</span>';
      html += '</a>';
    }
    bushelEl.innerHTML = html;
  }

  function renderLinks() {
    if (!linkResults.length) { linksEl.innerHTML = ''; return; }
    var html = '';
    for (var i = 0; i < linkResults.length; i++) {
      var r = linkResults[i];
      var sel = (selectedZone === 'links' && i === selectedIdx) ? ' selected' : '';

      html += '<a href="' + escapeHtml(r.url) + '" class="cyber-result' + sel + '" data-zone="links" data-idx="' + i + '" target="_blank" rel="noopener">';
      html += '<span class="cr-domain">~ ' + escapeHtml(urlDomain(r.url)) + '</span>';
      html += '<span class="cr-title">' + escapeHtml(r.title) + '</span>';
      html += '<span class="cr-date">' + r.date + '</span>';
      html += '</a>';
    }
    linksEl.innerHTML = html;

    // Stagger .visible class for slide-and-fade
    var linkEls = linksEl.querySelectorAll('.cyber-result');
    for (var j = 0; j < linkEls.length; j++) {
      (function(el, delay) {
        setTimeout(function() { el.classList.add('visible'); }, delay);
      })(linkEls[j], j * 50);
    }
  }

  function totalCount() { return bushelResults.length + linkResults.length; }

  function getActiveResults() {
    return selectedZone === 'bushel' ? bushelResults : linkResults;
  }

  function getActiveContainer() {
    return selectedZone === 'bushel' ? bushelEl : linksEl;
  }

  function clearSelection() {
    var prev = overlay.querySelector('.cyber-result.selected');
    if (prev) prev.classList.remove('selected');
  }

  function applySelection() {
    clearSelection();
    var container = getActiveContainer();
    var el = container.querySelector('[data-idx="' + selectedIdx + '"]');
    if (el) {
      el.classList.add('selected');
      el.scrollIntoView({ block: 'nearest' });
    }
  }

  function moveSelection(dir) {
    var results = getActiveResults();
    if (!results.length && !otherZoneHasResults()) return;

    var newIdx = selectedIdx + dir;

    // Cross-zone navigation
    if (newIdx < 0) {
      if (selectedZone === 'links' && bushelResults.length) {
        selectedZone = 'bushel';
        selectedIdx = bushelResults.length - 1;
        applySelection();
        return;
      }
      newIdx = results.length - 1; // wrap
    } else if (newIdx >= results.length) {
      if (selectedZone === 'bushel' && linkResults.length) {
        selectedZone = 'links';
        selectedIdx = 0;
        applySelection();
        return;
      }
      newIdx = 0; // wrap
    }

    selectedIdx = newIdx;
    applySelection();
  }

  function otherZoneHasResults() {
    return selectedZone === 'bushel' ? linkResults.length > 0 : bushelResults.length > 0;
  }

  function switchZone() {
    if (selectedZone === 'bushel' && linkResults.length) {
      selectedZone = 'links';
      selectedIdx = 0;
    } else if (selectedZone === 'links' && bushelResults.length) {
      selectedZone = 'bushel';
      selectedIdx = 0;
    }
    applySelection();
  }

  function buildQuery() {
    var q = (input ? input.value.trim() : '');
    var parts = [];
    activeKinds.forEach(function(k) { parts.push('kind:' + k); });
    if (q) parts.push(q);
    return parts.join(' ');
  }

  function doSearch() {
    var query = buildQuery();
    if (!query) {
      bushelResults = [];
      linkResults = [];
      selectedZone = 'bushel';
      selectedIdx = -1;
      bushelEl.innerHTML = '';
      linksEl.innerHTML = '';
      if (countEl) countEl.textContent = '';
      return;
    }

    // Cancel previous in-flight request
    if (abortCtrl) abortCtrl.abort();
    abortCtrl = new AbortController();

    fetch('/api/search?limit=40&q=' + encodeURIComponent(query), { signal: abortCtrl.signal })
      .then(function(r) { return r.json(); })
      .then(function(data) {
        var all = data.results || [];

        // Partition into bushel (entries) and links
        bushelResults = [];
        linkResults = [];
        for (var i = 0; i < all.length; i++) {
          if (all[i].kind === 'link') {
            if (linkResults.length < 20) linkResults.push(all[i]);
          } else {
            if (bushelResults.length < 20) bushelResults.push(all[i]);
          }
        }

        // Set initial selection
        if (bushelResults.length) {
          selectedZone = 'bushel';
          selectedIdx = 0;
        } else if (linkResults.length) {
          selectedZone = 'links';
          selectedIdx = 0;
        } else {
          selectedIdx = -1;
        }

        renderBushel();
        renderLinks();

        var total = bushelResults.length + linkResults.length;
        if (countEl) {
          countEl.textContent = total + ' result' + (total !== 1 ? 's' : '')
            + ' (' + bushelResults.length + ' entries, ' + linkResults.length + ' links)';
        }

        if (!total) {
          bushelEl.innerHTML = '<div class="cyber-empty">No results found</div>';
        }
      })
      .catch(function(e) {
        if (e.name === 'AbortError') return; // expected
        bushelResults = [];
        linkResults = [];
        bushelEl.innerHTML = '<div class="cyber-empty">Search failed</div>';
        linksEl.innerHTML = '';
      });
  }

  function debouncedSearch() {
    if (debounceTimer) clearTimeout(debounceTimer);
    debounceTimer = setTimeout(doSearch, 150);
  }

  if (input) {
    input.addEventListener('input', debouncedSearch);
  }

  // Kind filter pills
  pills.forEach(function(pill) {
    pill.addEventListener('click', function() {
      var kind = pill.dataset.kind;
      if (activeKinds.has(kind)) {
        activeKinds.delete(kind);
        pill.classList.remove('active');
      } else {
        activeKinds.add(kind);
        pill.classList.add('active');
      }
      doSearch();
    });
  });

  // Click handling for both zones
  function handleZoneClick(e) {
    var tagEl = e.target.closest('.cr-tag');
    if (tagEl) {
      e.preventDefault();
      e.stopPropagation();
      var tag = tagEl.dataset.tag;
      if (input) { input.value = '#' + tag; input.focus(); doSearch(); }
      return;
    }
    var result = e.target.closest('.cyber-result');
    if (result) {
      closeSearch();
      // Let the <a> default navigation happen
    }
  }
  bushelEl.addEventListener('click', handleZoneClick);
  linksEl.addEventListener('click', handleZoneClick);

  // Hover to select
  function handleHover(e) {
    var el = e.target.closest('.cyber-result');
    if (el) {
      var zone = el.dataset.zone;
      var idx = parseInt(el.dataset.idx);
      if (!isNaN(idx) && (zone !== selectedZone || idx !== selectedIdx)) {
        selectedZone = zone;
        selectedIdx = idx;
        applySelection();
      }
    }
  }
  bushelEl.addEventListener('mousemove', handleHover);
  linksEl.addEventListener('mousemove', handleHover);

  toggleBtn.addEventListener('click', openSearch);
  overlay.addEventListener('click', function(e) {
    if (e.target === overlay) closeSearch();
  });

  document.addEventListener('keydown', function(e) {
    if ((e.metaKey || e.ctrlKey) && e.key === 'k') {
      e.preventDefault();
      if (overlay.classList.contains('active')) closeSearch();
      else openSearch();
      return;
    }
    if (!overlay.classList.contains('active')) return;
    if (e.key === 'Escape') { closeSearch(); return; }
    if (e.key === 'Tab') {
      e.preventDefault();
      switchZone();
      return;
    }
    if (e.key === 'ArrowDown') {
      e.preventDefault();
      moveSelection(1);
      return;
    }
    if (e.key === 'ArrowUp') {
      e.preventDefault();
      moveSelection(-1);
      return;
    }
    if (e.key === 'Enter') {
      e.preventDefault();
      var results = getActiveResults();
      if (selectedIdx >= 0 && selectedIdx < results.length) {
        var url = results[selectedIdx].url;
        if (selectedZone === 'links') {
          window.open(url, '_blank', 'noopener');
        } else {
          window.location.href = url;
        }
        closeSearch();
      }
      return;
    }
  });

  // Global: clicking [data-tag] opens search with that tag
  document.addEventListener('click', function(e) {
    var tagEl = e.target.closest('[data-tag]');
    if (!tagEl) return;
    if (tagEl.classList.contains('tag-cloud-btn')) return;
    e.preventDefault();
    var tag = tagEl.dataset.tag;
    overlay.classList.add('active');
    if (input) {
      input.value = '#' + tag;
      input.focus();
      input.dispatchEvent(new Event('input'));
    }
    document.body.style.overflow = 'hidden';
  });

  // Global: clicking [data-kind] opens search with that kind filter
  document.addEventListener('click', function(e) {
    var kindEl = e.target.closest('[data-kind]');
    if (!kindEl) return;
    if (kindEl.closest('.cyber-filters')) return; // pill clicks handled above
    e.preventDefault();
    var kind = kindEl.dataset.kind;
    pills.forEach(function(pill) {
      if (pill.dataset.kind === kind) {
        activeKinds.add(kind);
        pill.classList.add('active');
      }
    });
    overlay.classList.add('active');
    if (input) { input.value = ''; input.focus(); }
    document.body.style.overflow = 'hidden';
    doSearch();
  });

  // On load: check for #tag=foo or #kind=foo in URL hash
  (function() {
    var hash = location.hash;
    if (hash && hash.indexOf('#tag=') === 0) {
      var tag = decodeURIComponent(hash.slice(5));
      setTimeout(function() {
        overlay.classList.add('active');
        if (input) {
          input.value = '#' + tag;
          input.focus();
          input.dispatchEvent(new Event('input'));
        }
        document.body.style.overflow = 'hidden';
      }, 100);
    } else if (hash && hash.indexOf('#kind=') === 0) {
      var kind = decodeURIComponent(hash.slice(6));
      setTimeout(function() {
        pills.forEach(function(pill) {
          if (pill.dataset.kind === kind) {
            activeKinds.add(kind);
            pill.classList.add('active');
          }
        });
        overlay.classList.add('active');
        if (input) { input.value = ''; input.focus(); }
        document.body.style.overflow = 'hidden';
        doSearch();
      }, 100);
    }
  })();
})();
```

**Step 2: Build and verify**

Run: `dune build avsm/arod/`
Expected: Compiles

**Step 3: Commit**

```bash
git add avsm/arod/lib_component/scripts.ml
git commit -m "feat(arod): rewrite search JS for split-zone cyberpunk layout"
```

---

### Task 4: Manual visual testing and polish

**Files:**
- Possibly tweak: `avsm/arod/lib_component/theme.ml` (CSS adjustments)
- Possibly tweak: `avsm/arod/lib_component/nav.ml` (HTML adjustments)
- Possibly tweak: `avsm/arod/lib_component/scripts.ml` (JS adjustments)

**Step 1: Start the dev server**

Run: `dune exec avsm/arod/bin/main.exe -- serve` (or however the local dev server starts)

**Step 2: Visual testing checklist**

1. Open browser, press Cmd+K — modal should appear with cyberpunk dark theme
2. Type a query that returns both entries and links — verify bushel zone appears above, links below
3. Verify bushel items appear instantly with kind-colored glow pulse animation
4. Verify link items slide in from right with staggered fade
5. Arrow keys navigate within and across zones
6. Tab switches between zones
7. Enter opens bushel item in same tab, link in new tab
8. Filter pills toggle correctly, restricting results to matching zone
9. Empty zones collapse (no blank space when no links match, or no entries match)
10. Esc closes modal

**Step 3: Fix any visual issues found**

Adjust CSS spacing, colors, animation timing as needed.

**Step 4: Commit polish**

```bash
git add -A avsm/arod/lib_component/
git commit -m "polish(arod): visual tweaks from search bar testing"
```

---

### Task 5: Final commit with all changes

**Step 1: Verify clean build**

Run: `dune build avsm/arod/`
Expected: No warnings, no errors

**Step 2: Verify no regressions**

Run: `dune runtest avsm/arod/` (if tests exist)

**Step 3: Review diff**

Run: `git diff HEAD~4..HEAD --stat`
Verify only the expected 3 files were modified.
