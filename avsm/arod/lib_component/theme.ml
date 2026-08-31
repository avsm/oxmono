(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Design tokens and theme constants for the Arod site.

    CSS custom properties for light/dark mode, typography, and spacing. *)

(** {1 Tailwind CDN Config}

    Injected as an inline script after the Tailwind CDN <script> tag. *)

(** {1 Theme Init Script}

    Tiny synchronous script for <head> that prevents FOUC by applying
    the .dark class before any CSS/rendering happens. *)

let theme_init_js = {|
(function(){
  var t = localStorage.getItem('theme');
  if (t === 'dark' || (!t && matchMedia('(prefers-color-scheme:dark)').matches)) {
    document.documentElement.classList.add('dark');
  }
})();
|}

(** {1 Custom CSS}

    Styles that can't be expressed purely via Tw utilities:
    - CSS custom properties for light/dark mode
    - Link underline styling with underline-offset
    - Blockquote green border
    - Sidenote CSS
    - Scrollbar-hide
    - TOC link gradient progress
    - Nav emphasis brackets *)

let custom_css = {|

/* CSS Custom Properties for light/dark theming */
:root {
  --color-bg: #fffffc;
  --color-surface: #f6f8fa;
  --color-surface-alt: #f3f4f6;
  --color-nav-from: #f8faf8;
  --color-nav-to: #f6f8f6;
  --color-text: #000000;
  --color-secondary: #555555;
  --color-muted: #777777;
  --color-faint: #999999;
  --color-dim: #444444;
  --color-link: #090c8d;
  --color-link-ul: #bbbbff;
  --color-border: #e5e7eb;
  --color-border-nav: #e0e2e0;
  --color-border-light: #dddddd;
  --color-border-faint: #cccccc;
  --color-accent: #22c55e;
  --color-st-avail: #22c55e;
  --color-st-discuss: #3b82f6;
  --color-st-ongoing: #f59e0b;
  --color-st-done: #8b5cf6;
  --color-st-expired: #6b7280;
  --color-sidenote-ref: #5b6abf;
  --color-highlight: #fde68a;
  --color-toc-bg: #e0e7ff;
  --color-bq-text: #4a4a4a;
  --color-weeknote-accent: #dde8d8;
  --idea-art: 0.12;
}

.dark {
  --color-bg: #0d1117;
  --color-surface: #161b22;
  --color-surface-alt: #1c2128;
  --color-nav-from: #0d1117;
  --color-nav-to: #111518;
  --color-text: #e6edf3;
  --color-secondary: #8b949e;
  --color-muted: #6e7681;
  --color-faint: #8b949e;
  --color-dim: #b1bac4;
  --color-link: #7dd3a0;
  --color-link-ul: #3d7a5488;
  --color-border: #30363d;
  --color-border-nav: #21262d;
  --color-border-light: #30363d;
  --color-border-faint: #21262d;
  --color-accent: #3fb950;
  --color-st-avail: #3fb950;
  --color-st-discuss: #58a6ff;
  --color-st-ongoing: #d29922;
  --color-st-done: #a371f7;
  --color-st-expired: #8b949e;
  --color-sidenote-ref: #7dd3a0;
  --idea-art: 0.2;
  --color-highlight: #634d15;
  --color-toc-bg: #1c2654;
  --color-bq-text: #b1bac4;
  --color-weeknote-accent: #253528;
}

/* Base element styles — in @layer base so Tailwind utilities can override */
@layer base {
  html {
    scroll-behavior: smooth;
    font-size: 110%;
  }
  body {
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    font-size: 0.88rem;
    line-height: 1.3;
  }
  code {
    font-family: ui-monospace, 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
    font-size: 0.78rem;
  }
  pre {
    font-size: 0.72rem;
    line-height: 1.5;
    background: var(--color-surface);
    border: 1px solid var(--color-border);
    border-radius: 4px;
    padding: 0.5rem 0.75rem;
    overflow-x: auto;
  }
  pre code {
    background: none;
    border: none;
    padding: 0;
    border-radius: 0;
  }
  pre code.hljs {
    padding: 0 !important;
    background: none !important;
  }
  :not(pre) > code {
    background: var(--color-surface-alt);
    padding: 0.15em 0.35em;
    border-radius: 3px;
    border: 1px solid var(--color-border);
  }
  a {
    color: var(--color-link);
    text-decoration: underline dotted;
    text-decoration-color: var(--color-link-ul);
    text-underline-offset: 2px;
  }
  a:hover {
    text-decoration-style: solid;
    text-decoration-color: var(--color-link);
  }
  blockquote {
    position: relative;
    border-left: 3px solid var(--color-accent);
    padding: 0.5rem 1rem;
    margin-left: 0;
    color: var(--color-bq-text);
    font-style: italic;
  }
  blockquote::before {
    content: "\201C";
    position: absolute;
    top: -0.2rem;
    left: 0.35rem;
    font-size: 2.5rem;
    line-height: 1;
    color: var(--color-accent);
    opacity: 0.18;
    font-style: normal;
    pointer-events: none;
  }
  blockquote cite {
    display: block;
    font-style: normal;
    font-size: 0.78rem;
    margin-top: 0.35rem;
    color: var(--color-muted);
    letter-spacing: 0.01em;
  }
  figcaption {
    font-style: italic;
    font-size: 0.78rem;
    color: var(--color-secondary);
    margin-top: 0.3rem;
    line-height: 1.4;
  }
}

/* Component/utility styles — in @layer components so they override base but
   can still be overridden by utilities */
@layer components {
  /* Enhanced code blocks */
  .code-block {
    position: relative;
    border: 1px solid var(--color-border);
    border-radius: 6px;
    overflow: hidden;
    margin: 0.5em 0;
  }
  .code-block pre {
    margin: 0;
    border: none;
    border-radius: 0;
    padding: 0.4rem 0.75rem 0.4rem 0;
  }
  .code-toolbar {
    position: absolute;
    top: 0.45rem;
    right: 0.4rem;
    display: flex;
    align-items: center;
    gap: 0.25rem;
    z-index: 1;
  }
  .code-copy {
    display: flex;
    align-items: center;
    padding: 0.15rem;
    border: none;
    background: none;
    color: var(--color-muted);
    cursor: pointer;
    border-radius: 3px;
    opacity: 0.4;
    transition: color 0.15s;
  }
  .code-copy:hover { color: var(--color-text); opacity: 0.8; }
  .code-copy.copied { color: var(--color-accent); opacity: 1; }

  a.no-underline, a.no-underline:hover {
    text-decoration: none;
  }
  .sidenote-ref {
    cursor: help;
  }
  .sidenote-number {
    color: var(--color-sidenote-ref);
  }
  /* CSS Text treats an atomic inline as an ideographic character for
     line breaking, so an inline-block marker permits a break between
     the ref text and the number. Keeping the marker inline removes that
     break opportunity, and setupSidenoteNumbers in scripts.ml inserts a
     U+2060 word joiner before the marker to forbid the break outright.
     Drop either half and the marker can wrap on to a line of its own. */
  .sidenote-toggle {
    display: inline;
    font-size: 0.6em;
    vertical-align: super;
    color: var(--color-sidenote-ref);
    opacity: 0.75;
    font-weight: 500;
    margin-left: 1px;
    font-variant-numeric: tabular-nums;
    transition: opacity 0.2s;
  }
  .sidenote-anchor.sidenote-active .sidenote-toggle {
    opacity: 1;
  }
  .sidenote-anchor.sidenote-active .sidenote-ref {
    color: var(--color-sidenote-ref);
  }
  .sidenote-anchor {
    position: relative;
  }
  /* Below the sidebar breakpoint a ref opens its inline note in place.
     The ref itself is named because .sidenote-ref sets cursor: help and
     is more specific over the <a> than .sidenote-anchor alone. */
  @media (max-width: 1023px) {
    .sidenote-anchor,
    .sidenote-anchor .sidenote-ref {
      cursor: pointer;
    }
  }
  /* Hide sidenotes until JS positions them, then fade in */
  .sidenote-hidden {
    opacity: 0;
  }
  .sidenote-visible {
    opacity: 1;
    transition: opacity 0.3s ease-in;
  }
  /* Contents box. Pinned below the sticky header while the article
     scrolls, so it sits over the sidenotes beneath it. The offset must
     match the header exactly or sidebar content shows through the gap;
     toc.js measures the header and sets --toc-top. */
  .toc-box {
    position: sticky;
    top: var(--toc-top, 3rem);
    z-index: 20;
  }
  /* Fade the column out under the pinned box instead of cutting it off.
     Only while pinned: at rest the box sits on the meta box below it. */
  .toc-box::after {
    content: "";
    position: absolute;
    left: 0;
    right: 0;
    top: 100%;
    height: 3rem;
    background: linear-gradient(to bottom, var(--color-bg) 30%, transparent);
    opacity: 0;
    transition: opacity 0.2s ease;
    pointer-events: none;
  }
  .toc-box.stuck::after {
    opacity: 1;
  }
  #toc-box .sidebar-meta-header {
    display: flex;
    align-items: center;
    gap: 0.25rem;
  }
  .toc-top {
    margin-left: auto;
    display: inline-flex;
    color: var(--color-secondary);
  }
  .toc-top:hover {
    color: var(--color-link);
  }
  .toc-list {
    display: flex;
    flex-direction: column;
    padding: 0.25rem;
    max-height: 40vh;
    overflow-y: auto;
  }
  .toc-link {
    position: relative;
    display: grid;
    grid-template-columns: minmax(1.1em, auto) 1fr;
    align-items: baseline;
    gap: 0.35em;
    padding: 0.1rem 0.3rem;
    border-radius: 3px;
    font-size: 0.72rem;
    color: var(--color-secondary);
    transition: color 0.15s ease;
  }
  .toc-link:hover {
    color: var(--color-link);
  }
  /* Subsections hang off a thread running down the left of the group.
     The run ends at the next h2, or at the end of the list, and that
     last row draws only the top half of the thread to close it off. */
  .toc-sub {
    padding-left: 1.55em;
    grid-template-columns: minmax(2.1em, auto) 1fr;
  }
  .toc-sub::before {
    content: "";
    position: absolute;
    left: 0.6em;
    top: 0;
    bottom: 0;
    width: 1px;
    background: var(--color-muted);
    opacity: 0.45;
  }
  .toc-sub:last-child::before,
  .toc-sub:has(+ .toc-link:not(.toc-sub))::before {
    bottom: auto;
    height: 50%;
  }
  .toc-sub::after {
    content: "";
    position: absolute;
    left: 0.6em;
    top: 50%;
    width: 0.5em;
    height: 1px;
    background: var(--color-muted);
    opacity: 0.45;
  }
  .toc-label {
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  /* Matches the section numbers the article headings carry */
  .toc-num {
    font-family: ui-monospace, 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
    font-variant-numeric: tabular-nums;
    text-align: right;
    color: var(--color-muted);
  }
  .toc-link.passed .toc-num,
  .toc-link.active .toc-num {
    color: var(--color-link);
  }
  .toc-link.passed {
    color: var(--color-link);
  }
  /* Only the section being read is filled, so the fill reads as a
     progress bar rather than as everything above the reader. Both the
     h2 and the h3 within it are active, so both fill. Mark it with an
     underline too: a heavier weight or a wider glyph would reflow the
     row as the reader scrolls past it. */
  .toc-link.active {
    color: var(--color-link);
    background: linear-gradient(to right, var(--color-toc-bg) 0%, var(--color-toc-bg) var(--progress, 0%), transparent var(--progress, 0%), transparent 100%);
  }
  .toc-link.active .toc-label {
    text-decoration: underline;
    text-decoration-thickness: 1px;
    text-underline-offset: 2px;
  }
  .text-body { font-size: 0.88rem; line-height: 1.45; }
  /* idea status colours are now Tailwind utilities (font-medium text-st-*) */
  /* Idea list items */
  .idea-list {
    display: flex;
    flex-direction: column;
    gap: 0.15rem;
    margin-top: 0.75rem;
  }
  .idea-row {
    display: flex;
    align-items: flex-start;
    gap: 0.5rem;
    padding: 0.35rem 0.5rem;
    border-radius: 3px;
    transition: background 0.1s;
  }
  .idea-row:hover {
    background: var(--color-surface);
  }
  .idea-dot {
    display: inline-flex;
    align-items: center;
    flex-shrink: 0;
    margin-top: 0.35rem;
  }
  .note-compact .idea-dot {
    margin-top: 0;
  }
  /* Idea list: allow titles to wrap */
  .idea-item .note-compact-title {
    white-space: normal;
    overflow: visible;
    text-overflow: unset;
  }
  .idea-row-content {
    min-width: 0;
    flex: 1;
  }
  .idea-row-title {
    font-weight: 500;
    text-decoration: none !important;
    color: var(--color-text) !important;
  }
  .idea-row-title:hover {
    color: var(--color-link) !important;
    text-decoration: underline dotted !important;
    text-decoration-color: var(--color-link-ul) !important;
  }
  .idea-row-meta {
    display: block;
    font-size: 0.85rem;
    color: var(--color-secondary);
  }
  /* hash-prefix opacity is now a Tailwind utility (opacity-50) */
  .tag-search-link, .kind-search-link {
    text-decoration: none;
    cursor: pointer;
    color: var(--color-accent);
  }
  .tag-search-link:hover, .kind-search-link:hover {
    text-decoration: underline;
  }
  /* Paper sidebar — filter rows and year jump */
  .paper-filter-row {
    display: flex;
    align-items: center;
    gap: 0.3rem;
    padding: 0.05rem 0;
    cursor: pointer;
  }
  .paper-filter-row input[type="checkbox"] {
    flex-shrink: 0;
  }
  .paper-filter-row:has(input:not(:checked)) {
    opacity: 0.4;
  }
  .paper-filter-label {
    color: var(--color-dim);
    flex: 1;
  }
  .paper-stat-count {
    color: var(--color-secondary);
    font-variant-numeric: tabular-nums;
    font-size: 0.68rem;
  }
  .paper-compact-authors {
    font-size: 0.75rem;
    color: var(--color-secondary);
    line-height: 1.4;
    font-weight: 400;
    margin-top: 0.05rem;
  }
  .paper-compact-authors a {
    color: var(--color-secondary) !important;
    font-weight: 400 !important;
    text-decoration: none !important;
  }
  .paper-compact-authors a:hover {
    color: var(--color-link) !important;
    text-decoration: underline dotted !important;
  }
  .paper-compact-links {
    font-size: 0.75rem;
    font-weight: 400;
  }
  .paper-compact-links a {
    font-weight: 400 !important;
    font-size: 0.75rem;
  }
  /* Footnote endnotes (cmarkit doc-endnotes) */
  [role="doc-endnotes"] {
    font-size: 0.82rem;
    line-height: 1.5;
    margin-top: 2rem;
    border-top: 1px solid var(--color-border);
    padding-top: 1rem;
    color: var(--color-secondary);
  }
  [role="doc-endnotes"] > ol {
    padding-inline-start: 1.5em;
  }
  [role="doc-endnotes"] > ol > li {
    margin-bottom: 0.3rem;
  }
  [role="doc-endnotes"] > ol > li > p {
    display: inline;
  }
  [role="doc-endnotes"] a.fn-label {
    text-decoration: none;
    padding-left: 0.3em;
    color: var(--color-muted);
  }
  [role="doc-endnotes"] a.fn-label:hover {
    color: var(--color-link);
  }
  /* Paper detail page */
  .paper-citation {
    color: var(--color-secondary);
    line-height: 1.5;
  }
  .paper-cite-authors {
    color: var(--color-dim);
  }
  .paper-cite-authors a {
    color: var(--color-dim) !important;
    font-weight: 400 !important;
    text-decoration: none !important;
  }
  .paper-cite-authors a:hover {
    color: var(--color-link) !important;
    text-decoration: underline dotted !important;
  }
  .paper-cite-venue {
    color: var(--color-secondary);
  }
  .paper-detail-actions {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 0.5rem;
    margin-bottom: 0.75rem;
  }
  .paper-action-pills {
    display: flex;
    flex-wrap: wrap;
    gap: 0.4rem;
  }
  .paper-action-pill {
    display: inline-flex;
    align-items: center;
    gap: 0.35rem;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.82rem;
    font-weight: 500;
    padding: 0.2rem 0.6rem;
    border-radius: 4px;
    text-decoration: none !important;
    transition: all 0.15s ease;
    border: 1px solid;
  }
  .paper-action-pill {
    opacity: 0.65;
  }
  .paper-action-pill:hover {
    opacity: 1;
  }
  .paper-action-pdf {
    color: #c0392b !important;
    border-color: #e8c4c0;
    background: #fdf5f4;
  }
  .paper-action-pdf:hover {
    background: #f9e0dd;
    border-color: #c0392b;
  }
  .paper-action-bib {
    color: #2874a6 !important;
    border-color: #bdd6e6;
    background: #f0f7fb;
  }
  .paper-action-bib:hover {
    background: #d6ebf5;
    border-color: #2874a6;
  }
  .paper-action-doi {
    color: #7d3c98 !important;
    border-color: #d5b8e2;
    background: #f9f3fc;
  }
  .paper-action-doi:hover {
    background: #eed9f7;
    border-color: #7d3c98;
  }
  .paper-action-url {
    color: #1a7a5a !important;
    border-color: #b8ddd0;
    background: #f0faf6;
  }
  .paper-action-url:hover {
    background: #d6f0e6;
    border-color: #1a7a5a;
  }
  @media (prefers-color-scheme: dark) {
    .paper-action-pdf {
      color: #e88e85 !important;
      border-color: #5a2520;
      background: #2d1614;
    }
    .paper-action-pdf:hover {
      background: #3d1e1a;
      border-color: #e88e85;
    }
    .paper-action-bib {
      color: #7cb8d9 !important;
      border-color: #1e4a64;
      background: #142a38;
    }
    .paper-action-bib:hover {
      background: #1c3a4e;
      border-color: #7cb8d9;
    }
    .paper-action-doi {
      color: #c49bd8 !important;
      border-color: #4a2660;
      background: #271435;
    }
    .paper-action-doi:hover {
      background: #351c49;
      border-color: #c49bd8;
    }
    .paper-action-url {
      color: #6ec4a0 !important;
      border-color: #1e5a40;
      background: #142e22;
    }
    .paper-action-url:hover {
      background: #1c3e30;
      border-color: #6ec4a0;
    }
  }
  .paper-detail-tags {
    display: flex;
    flex-wrap: wrap;
    gap: 0.3rem;
  }
  .paper-detail-tag {
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.78rem;
    color: var(--color-muted) !important;
    padding: 0.05rem 0.3rem;
    border: 1px solid var(--color-border);
    border-radius: 3px;
    line-height: 1.5;
    text-decoration: none !important;
    cursor: pointer;
  }
  .paper-detail-tag:hover {
    color: var(--color-accent) !important;
    border-color: var(--color-accent);
  }
  .detail-meta {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    margin-left: auto;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.72rem;
    color: var(--color-muted);
    line-height: 1.5;
  }
  .detail-meta a {
    color: var(--color-muted) !important;
    text-decoration: none !important;
  }
  .detail-meta a:hover {
    color: var(--color-accent) !important;
  }
  .detail-meta-sep {
    color: var(--color-border);
  }
  .detail-synopsis {
    font-size: 0.82rem;
    font-style: italic;
    line-height: 1.4;
    color: var(--color-secondary);
    margin-top: 0.5rem;
    padding: 0.4rem 0.6rem;
    background: var(--color-surface-alt);
    border-radius: 4px;
  }
  /* social-icon styles are now Tailwind utilities */
  .paper-abstract-section::after {
    content: "";
    display: table;
    clear: both;
  }
  .paper-detail-thumb {
    float: right;
    width: 50%;
    max-width: 50%;
    margin: 0 0 0.75rem 1rem;
  }
  .paper-detail-img {
    width: 100%;
    border: 1px solid var(--color-border);
    border-radius: 4px;
    box-shadow: 0 1px 4px rgba(0,0,0,0.08);
  }
  /* paper-full/short/preprint colours are now Tailwind utilities */
  .paper-item.note-compact {
    position: relative;
  }
  .paper-cls-icon {
    position: absolute;
    left: 0.15rem;
    top: 0.55rem;
    display: inline-flex;
    align-items: center;
  }
  .paper-jump-list {
    display: flex;
    flex-direction: column;
  }
  .paper-jump-link {
    display: flex;
    align-items: center;
    padding: 0.15rem 0;
    color: var(--color-dim) !important;
    text-decoration: none !important;
    transition: color 0.1s;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.72rem;
  }
  .paper-jump-link:hover {
    color: var(--color-link) !important;
  }
  .paper-jump-title {
    flex: 1;
    min-width: 0;
  }
  .paper-jump-count {
    color: var(--color-secondary);
    font-variant-numeric: tabular-nums;
    margin-left: 0.3rem;
    flex-shrink: 0;
  }
  /* Sidebar avatar thumbnails — text-sized */
  .sidebar-avatar-row {
    display: inline-flex;
    align-items: center;
    gap: 0.15rem;
    flex-wrap: nowrap;
    vertical-align: middle;
  }
  .sidebar-avatar-wrap {
    position: relative;
    display: inline-flex;
  }
  .sidebar-avatar-wrap-link {
    display: inline-flex;
  }
  .sidebar-avatar {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 0.9rem;
    height: 0.9rem;
    border-radius: 50%;
    overflow: hidden;
    border: 1.5px solid var(--color-border);
    box-shadow: 0 0 0 0.5px var(--color-surface-alt);
    background: var(--color-surface-alt);
    flex-shrink: 0;
    transition: border-color 0.15s, box-shadow 0.15s;
    cursor: pointer;
  }
  .sidebar-avatar-wrap:hover .sidebar-avatar {
    border-color: var(--color-accent);
    box-shadow: 0 0 0 1px var(--color-accent);
  }
  .sidebar-avatar-img {
    width: 100%;
    height: 100%;
    object-fit: cover;
    border-radius: 50%;
  }
  /* sidebar-avatar-initials styles are now Tailwind utilities */
  /* Contact popover card — hover-triggered */
  .contact-popover {
    display: none;
    position: absolute;
    left: 50%;
    bottom: calc(100% + 1px);
    transform: translateX(-50%);
    z-index: 50;
    background: var(--color-bg);
    border: 1px solid var(--color-border);
    border-radius: 5px;
    box-shadow: 0 4px 16px rgba(0,0,0,0.15);
    padding: 0.25rem 0.4rem;
    width: max-content;
    max-width: 14rem;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.72rem;
    line-height: 1.35;
  }
  /* Invisible bridge so mouse can travel from avatar to popover */
  .contact-popover::after {
    content: "";
    position: absolute;
    top: 100%;
    left: 0;
    width: 100%;
    height: 6px;
  }
  /* Small arrow on the bridge */
  .contact-popover::before {
    content: "";
    position: absolute;
    top: 100%;
    left: 50%;
    transform: translateX(-50%);
    border: 5px solid transparent;
    border-top-color: var(--color-border);
  }
  .sidebar-avatar-wrap:hover .contact-popover {
    display: block;
  }
  /* Sidenote contact — thumbnail, name, social icons */
  .sn-contact-row {
    display: inline-flex;
    align-items: center;
    gap: 0.2rem;
    white-space: nowrap;
  }
  .sn-contact-thumb {
    width: 0.9rem;
    height: 0.9rem;
    border-radius: 50%;
    object-fit: cover;
    flex-shrink: 0;
    border: 1.5px solid var(--color-border);
    box-shadow: 0 0 0 0.5px var(--color-surface-alt);
  }
  .sn-contact-socials {
    display: inline-flex;
    align-items: center;
    gap: 0.15rem;
    margin-left: 0.15rem;
  }
  .sn-social-icon {
    display: inline-flex;
    color: var(--color-muted);
    opacity: 0.7;
    text-decoration: none !important;
  }
  .sn-social-icon:hover {
    color: var(--color-link);
    opacity: 1;
  }
  .popover-row {
    display: flex;
    align-items: center;
    gap: 0.3rem;
  }
  .popover-photo {
    width: 1.8rem;
    height: 1.8rem;
    border-radius: 50%;
    object-fit: cover;
    border: 2px solid var(--color-border);
    box-shadow: 0 0 0 1px var(--color-surface-alt);
    flex-shrink: 0;
  }
  .popover-photo-initials {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 1.8rem;
    height: 1.8rem;
    border-radius: 50%;
    background: var(--color-surface-alt);
    border: 2px solid var(--color-border);
    font-size: 0.6rem;
    font-weight: 700;
    color: var(--color-muted);
    text-transform: uppercase;
    flex-shrink: 0;
  }
  /* popover-info, popover-name, popover-org base styles are now Tailwind utilities */
  .popover-socials {
    display: flex;
    align-items: center;
    gap: 0.3rem;
    margin-top: 0.15rem;
    padding-top: 0.15rem;
    border-top: 1px dashed var(--color-border);
  }
  .popover-social-link {
    color: var(--color-muted) !important;
    text-decoration: none !important;
    display: inline-flex;
    transition: color 0.1s;
  }
  .popover-social-link:hover {
    color: var(--color-link) !important;
  }
  .sidebar-meta-box {
    font-family: ui-monospace, 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
    font-size: 0.72rem;
    line-height: 1.5;
    border: 1px solid var(--color-border);
    border-left: 2px solid var(--color-accent);
    border-radius: 3px;
    background: var(--color-surface);
    overflow: visible;
    min-width: 0;
    overflow-wrap: break-word;
  }
  .sidebar-meta-header {
    padding: 0.3rem 0.5rem;
    background: var(--color-surface-alt);
    border-bottom: 1px solid var(--color-border);
    border-radius: 3px 3px 0 0;
    color: var(--color-secondary);
    overflow-wrap: break-word;
  }
  .sidebar-meta-prompt {
    color: var(--color-accent);
    font-weight: 600;
  }
  .sidebar-meta-body {
    padding: 0.35rem 0.5rem;
    font-family: system-ui, -apple-system, sans-serif;
  }
  .sidebar-meta-line {
    margin: 0;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    display: flex;
    align-items: center;
  }
  .sidebar-meta-line:has(.sidebar-avatar-row),
  .sidebar-meta-line:has(.sidebar-avatar-wrap) {
    overflow: visible;
  }
  .sidebar-meta-line:has(.sidebar-meta-tags),
  .sidebar-meta-line:has(.sidebar-meta-link),
  .sidebar-meta-line.sidebar-meta-wrap {
    white-space: normal;
    overflow: visible;
    align-items: flex-start;
  }
  .sidebar-meta-line:has(.sidebar-meta-link) > .sidebar-meta-val {
    min-width: 0;
  }
  .sidebar-meta-tags {
    display: flex;
    flex-wrap: wrap;
    gap: 0.2rem;
    align-items: center;
  }
  .sidebar-meta-tags .sidebar-tag {
    font-size: 0.65rem;
    color: var(--color-dim);
    padding: 0 0.2rem;
    border: 1px solid var(--color-border);
    border-radius: 2px;
    line-height: 1.5;
    white-space: nowrap;
    text-decoration: none;
    cursor: pointer;
  }
  .sidebar-meta-tags .sidebar-tag:hover {
    color: var(--color-accent);
    border-color: var(--color-accent);
  }
  .sidebar-meta-icon {
    display: inline-flex;
    align-items: center;
    color: var(--color-muted);
    margin-right: 0.3rem;
    flex-shrink: 0;
    vertical-align: middle;
  }
  /* sidebar-meta-val colour is now a Tailwind utility (text-dim) */
  .sidebar-meta-link {
    color: var(--color-dim) !important;
    text-decoration: underline dotted var(--color-border-faint) !important;
  }
  .sidebar-note-tags {
    margin: 0 0 0.3rem 0;
  }
  .sidebar-meta-synopsis {
    font-style: italic;
    color: var(--color-secondary);
    margin: 0 0 0.3rem 0;
    padding-bottom: 0.3rem;
    border-bottom: 1px dashed var(--color-border);
    white-space: normal;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.78rem;
    line-height: 1.4;
  }
  .sidebar-meta-link:hover {
    color: var(--color-link) !important;
    text-decoration-color: var(--color-link) !important;
    text-decoration-style: solid !important;
  }
  .sidebar-meta-links {
    margin-top: 0.25rem;
    padding-top: 0.25rem;
    border-top: 1px dashed var(--color-border);
    overflow: hidden;
    min-width: 0;
  }
  .sidebar-meta-linkline {
    margin: 0;
    display: flex;
    align-items: center;
    white-space: nowrap;
    overflow: hidden;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.72rem;
    min-width: 0;
  }
  .sidebar-link-type-icon {
    display: inline-flex;
    align-items: center;
    color: var(--color-muted);
    margin-right: 0.2rem;
    flex-shrink: 0;
  }
  .sidebar-link-title {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    min-width: 0;
  }
  .sidebar-meta-expand {
    display: block;
    width: 100%;
    margin-top: 0.15rem;
    padding: 0.15rem 0;
    background: none;
    border: none;
    color: var(--color-muted);
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.72rem;
    cursor: pointer;
    text-align: left;
    transition: color 0.15s;
  }
  .sidebar-meta-expand:hover {
    color: var(--color-link);
  }
  /* Links button in a box header rather than a row in its body. The
     title grows to fill, which puts the button hard against the right
     edge without a margin: Tailwind's unlayered button reset zeroes
     margins on buttons whatever this layer says. */
  #note-meta > .sidebar-meta-header {
    display: flex;
    align-items: center;
    gap: 0.3rem;
  }
  #note-meta > .sidebar-meta-header > .sidebar-meta-link {
    flex: 1 1 auto;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .links-modal-overlay {
    position: fixed;
    inset: 0;
    background: rgba(0,0,0,0.55);
    z-index: 60;
    display: none;
    align-items: center;
    justify-content: center;
    padding: 2rem;
  }
  .links-modal-overlay.active { display: flex; }
  .links-modal {
    background: var(--color-bg);
    border: 1px solid var(--color-border);
    border-radius: 6px;
    width: 100%;
    max-width: 40rem;
    max-height: 80vh;
    display: flex;
    flex-direction: column;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.82rem;
    box-shadow: 0 8px 30px rgba(0,0,0,0.25);
  }
  .links-modal-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 0.5rem 0.75rem;
    border-bottom: 1px solid var(--color-border);
    color: var(--color-secondary);
    font-size: 0.72rem;
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }
  .links-modal-close-btn {
    background: none;
    border: none;
    color: var(--color-muted);
    font-size: 1.1rem;
    cursor: pointer;
    line-height: 1;
    padding: 0 0.25rem;
  }
  .links-modal-close-btn:hover { color: var(--color-text); }
  .links-modal-body {
    overflow-y: auto;
    padding: 0.4rem 0;
  }
  .links-modal-row {
    display: flex;
    align-items: baseline;
    gap: 0.35rem;
    padding: 0.3rem 0.75rem;
    transition: background 0.1s;
  }
  .links-modal-row:hover {
    background: var(--color-surface);
  }
  .links-modal-icon {
    display: inline-flex;
    flex-shrink: 0;
    color: var(--color-muted);
  }
  .links-modal-type-icon {
    display: inline-flex;
    flex-shrink: 0;
    color: var(--color-muted);
    margin-right: 0.15rem;
  }
  .links-modal-link {
    color: var(--color-dim) !important;
    text-decoration: none !important;
    flex: 1;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .links-modal-link:hover {
    color: var(--color-link) !important;
    text-decoration: underline dotted !important;
  }
  .links-modal-date {
    color: var(--color-faint);
    font-size: 0.65rem;
    flex-shrink: 0;
    margin-left: auto;
  }
  .references-block {
    font-size: 0.78rem;
    line-height: 1.5;
  }
  .ref-item {
    margin-bottom: 0.3rem;
    display: flex;
    gap: 0.35rem;
    align-items: baseline;
  }
  .ref-item:last-child { margin-bottom: 0; }
  .ref-num {
    color: var(--color-link);
    font-weight: 600;
    flex-shrink: 0;
  }
  .ref-body {
    color: var(--color-dim);
  }
  .ref-doi {
    font-size: 0.72rem;
    color: var(--color-faint) !important;
    text-decoration: none !important;
    white-space: nowrap;
  }
  .ref-doi:hover {
    color: var(--color-link) !important;
    text-decoration: underline dotted !important;
  }
  .heading-number {
    color: var(--color-muted);
    font-weight: 400;
    font-variant-numeric: tabular-nums;
    text-decoration: none !important;
    transition: color 0.15s;
    margin-right: 0.15em;
  }
  .heading-number::after {
    content: "\2002\007C\2002";
    color: var(--color-border);
  }
  a.heading-number:hover { color: var(--color-link) !important; }
  /* Ensure floated images in content clear properly */
  main::after, .space-y-4::after, .space-y-3::after {
    content: "";
    display: table;
    clear: both;
  }
  .lightbox-trigger { cursor: zoom-in; }
  figure img {
    border: 1px solid var(--color-border);
    border-radius: 3px;
  }
  .float-img {
    margin: 0;
  }
  .float-img img {
    border: 2px solid var(--color-secondary);
    transition: filter 0.3s ease, border-color 0.3s ease, box-shadow 0.3s ease;
  }
  .float-img:hover img {
    border-color: var(--color-accent);
    filter: saturate(0.3) contrast(1.1);
    box-shadow: 0 0 8px rgba(34,197,94,0.3);
  }
  .lightbox-expand {
    position: absolute;
    bottom: 4px;
    left: 4px;
    width: 1.5rem;
    height: 1.5rem;
    background: rgba(0,0,0,0.45);
    color: white;
    border-radius: 4px;
    display: flex;
    align-items: center;
    justify-content: center;
    cursor: zoom-in;
    opacity: 0;
    pointer-events: none;
    transition: opacity 0.2s;
    text-decoration: none !important;
    z-index: 5;
    padding: 3px;
  }
  .lightbox-expand svg {
    width: 100%;
    height: 100%;
  }
  figure:hover .lightbox-expand,
  .relative:hover > .lightbox-expand {
    opacity: 0.7;
    pointer-events: auto;
  }
  .lightbox-expand:hover {
    opacity: 1 !important;
    background: rgba(0,0,0,0.75);
  }
  #lightbox-overlay {
    position: fixed;
    inset: 0;
    z-index: 70;
    background: rgba(0,0,0,0.85);
    display: none;
    align-items: center;
    justify-content: center;
    flex-direction: column;
    padding: 2rem;
  }
  #lightbox-overlay.active { display: flex; }
  .lightbox-content {
    max-width: 90vw;
    max-height: 85vh;
    display: flex;
    flex-direction: column;
    align-items: center;
  }
  .lightbox-img {
    max-width: 90vw;
    max-height: 75vh;
    object-fit: contain;
    border-radius: 4px;
    box-shadow: 0 4px 30px rgba(0,0,0,0.4);
  }
  .lightbox-below {
    margin-top: 0.75rem;
    text-align: center;
    max-width: 90vw;
  }
  .lightbox-caption {
    color: #ddd;
    font-size: 0.85rem;
    margin-bottom: 0.5rem;
  }
  .lightbox-downloads {
    display: flex;
    gap: 0.4rem;
    flex-wrap: wrap;
    justify-content: center;
  }
  .lightbox-dl {
    font-family: ui-monospace, 'SF Mono', monospace;
    font-size: 0.7rem;
    color: #aaa !important;
    text-decoration: none !important;
    background: rgba(255,255,255,0.1);
    padding: 0.15rem 0.4rem;
    border-radius: 3px;
    transition: background 0.15s, color 0.15s;
  }
  .lightbox-dl:hover {
    background: rgba(255,255,255,0.25);
    color: #fff !important;
  }
  .lightbox-close {
    position: fixed;
    top: 1rem;
    right: 1.5rem;
    color: #999;
    font-size: 2rem;
    background: none;
    border: none;
    cursor: pointer;
    line-height: 1;
    transition: color 0.15s;
    z-index: 71;
  }
  .lightbox-close:hover { color: #fff; }
  /* Project grid — two-column CSS Grid layout */
  .proj-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    column-gap: 1.25rem;
    align-items: start;
  }
  @media (max-width: 900px) {
    .proj-grid { grid-template-columns: 1fr; }
  }
  .proj-card {
    border: 1px solid var(--color-border);
    border-radius: 4px;
    margin-bottom: 1rem;
    overflow: hidden;
    font-family: ui-monospace, 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
    font-size: 0.78rem;
    line-height: 1.5;
    transition: border-color 0.15s;
  }
  .proj-card:hover {
    border-color: var(--color-accent);
  }
  .proj-card-header {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    padding: 0.4rem 0.6rem;
    border-bottom: 1px solid var(--color-border);
    background: var(--color-surface);
    font-size: 0.85rem;
  }
  .proj-card-prompt {
    color: var(--color-accent);
    font-weight: 600;
    flex-shrink: 0;
  }
  .proj-card-title {
    flex: 1;
    min-width: 0;
    font-weight: 600;
    color: var(--color-text) !important;
    text-decoration: none !important;
  }
  .proj-card-title:hover {
    color: var(--color-link) !important;
  }
  .proj-card-date {
    flex-shrink: 0;
    color: var(--color-secondary);
    font-variant-numeric: tabular-nums;
  }
  .proj-card-body {
    padding: 0.5rem 0.6rem;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.85rem;
    color: var(--color-secondary);
    line-height: 1.4;
  }
  .proj-card-body p { margin: 0 0 0.3em; }
  .proj-card-body a { color: var(--color-link); }
  .proj-card-thumb {
    float: right;
    margin: 0 0 0.3rem 0.5rem;
  }
  .proj-card-thumb img {
    width: 36px;
    height: 36px;
    object-fit: cover;
    border-radius: 3px;
  }
  .proj-detail-logo {
    float: right;
    width: 80px;
    height: 80px;
    object-fit: cover;
    border-radius: 50%;
    margin: 0 0 0.75rem 1rem;
    border: 2px solid var(--color-border);
  }
  .proj-card-tags {
    display: flex;
    flex-wrap: wrap;
    gap: 0.2rem;
    margin-top: 0.35rem;
  }
  .proj-card-tag {
    font-family: ui-monospace, 'SF Mono', monospace;
    font-size: 0.65rem;
    color: var(--color-muted);
    padding: 0 0.25rem;
    border: 1px solid var(--color-border);
    border-radius: 2px;
    line-height: 1.5;
    text-decoration: none;
    cursor: pointer;
  }
  .proj-card-tag:hover {
    color: var(--color-accent);
    border-color: var(--color-accent);
  }
  .proj-card-recent {
    border-top: 1px solid var(--color-border);
    padding: 0.35rem 0.6rem;
  }
  .proj-card-section-label {
    font-family: ui-monospace, 'SF Mono', monospace;
    font-size: 0.65rem;
    color: var(--color-muted);
    text-transform: uppercase;
    letter-spacing: 0.05em;
    margin-bottom: 0.15rem;
  }
  /* Project activity stream */
  .project-activity-list {
    display: flex;
    flex-direction: column;
    gap: 0.1rem;
  }
  .project-activity-row {
    display: flex;
    align-items: flex-start;
    gap: 0.35rem;
    padding: 0.3rem 0.5rem;
    border-radius: 3px;
    transition: background 0.1s;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.88rem;
  }
  .project-activity-row:hover {
    background: var(--color-surface);
  }
  #older-versions a {
    color: inherit;
  }
  .project-activity-icon {
    display: inline-flex;
    align-items: center;
    color: var(--color-muted);
    flex-shrink: 0;
    margin-top: 0.2rem;
  }
  .project-activity-content {
    flex: 1;
    min-width: 0;
  }
  .project-activity-header {
    display: flex;
    align-items: baseline;
    gap: 0.35rem;
  }
  .project-activity-title {
    flex: 1;
    min-width: 0;
    font-weight: 500;
    color: var(--color-text) !important;
    text-decoration: none !important;
  }
  .project-activity-title:hover {
    color: var(--color-link) !important;
    text-decoration: underline dotted !important;
    text-decoration-color: var(--color-link-ul) !important;
  }
  .project-activity-date {
    flex-shrink: 0;
    font-size: 0.78rem;
    color: var(--color-secondary);
    font-variant-numeric: tabular-nums;
  }
  .project-activity-detail {
    font-size: 0.82rem;
    color: var(--color-secondary);
    line-height: 1.4;
    display: -webkit-box;
    -webkit-line-clamp: 2;
    -webkit-box-orient: vertical;
    overflow: hidden;
  }
  /* Related stream — smaller variant at bottom of articles */
  .related-stream {
    margin-top: 2rem;
  }
  .related-stream .project-activity-row {
    font-size: 0.82rem;
    padding: 0.2rem 0.4rem;
  }
  .related-stream .project-activity-date {
    font-size: 0.72rem;
  }
  .related-stream .project-activity-detail {
    font-size: 0.76rem;
  }
  /* Feed activity detail — author + summary flowing together */
  .feed-activity-author {
    font-weight: 500;
  }
  /* Project entry rows — compact icon+link items */
  .project-entry-row {
    display: flex;
    align-items: center;
    gap: 0.35rem;
    padding: 0.1rem 0;
  }
  .project-entry-icon {
    display: inline-flex;
    align-items: center;
    color: var(--color-muted);
    flex-shrink: 0;
  }
  .project-entry-link {
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.72rem;
    color: var(--color-dim) !important;
    text-decoration: none !important;
    min-width: 0;
  }
  .project-entry-link:hover {
    color: var(--color-link) !important;
    text-decoration: underline dotted !important;
  }
  .project-entry-row {
    min-width: 0;
  }
  .project-read-more {
    display: inline-flex;
    align-items: center;
    gap: 0.2rem;
    font-size: 0.78rem;
    font-weight: 500;
    color: var(--color-accent) !important;
    text-decoration: none !important;
    margin-top: 0.25rem;
  }
  .project-read-more:hover {
    text-decoration: underline !important;
  }
  /* Compact note cards */
  .note-compact {
    position: relative;
    border-radius: 3px;
    transition: background 0.1s;
  }
  /* note-compact:hover bg is now a Tailwind utility (hover:bg-surface) */
  .note-compact-row {
    display: flex;
    align-items: baseline;
    gap: 0.4rem;
  }
  /* note-compact-title base styles are now Tailwind utilities */
  .note-compact-title:hover {
    color: var(--color-link) !important;
    text-decoration: underline dotted !important;
    text-decoration-color: var(--color-link-ul) !important;
  }
  /* note-compact-meta and note-compact-synopsis base styles are now Tailwind utilities */
  .note-compact-tags {
    display: flex;
    flex-wrap: wrap;
    gap: 0.2rem;
    margin-top: 0.05rem;
  }
  .note-tag-chip {
    font-size: 0.65rem;
    color: var(--color-muted) !important;
    padding: 0 0.2rem;
    border: 1px solid var(--color-border);
    border-radius: 2px;
    line-height: 1.5;
    text-decoration: none !important;
    cursor: pointer;
  }
  .note-tag-chip:hover {
    color: var(--color-accent) !important;
    border-color: var(--color-accent);
  }
  /* Quick post cards — smaller than perma size */
  .note-compact:not(.note-perma) .note-compact-title {
    font-size: 0.85rem !important;
  }
  /* Perma article cards — featured with more visual weight */
  .note-perma .note-compact-title {
    font-weight: 600 !important;
    font-size: 0.92rem !important;
  }
  .note-perma .note-compact-synopsis {
    display: block !important;
  }
  /* Slug-ent reference chip on compact cards */
  .note-compact-ref {
    display: flex;
    margin-top: 0.1rem;
    min-width: 0;
  }
  .note-compact-ref .link-backlink-chip {
    min-width: 0;
    max-width: 100%;
    font-style: italic;
  }
  .note-compact-ref-text {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  /* Synopsis for quick posts: smaller than heading */
  .note-compact:not(.note-perma) .note-compact-synopsis {
    font-size: 0.78rem;
    line-height: 1.35;
  }
  /* Weeknote navigation in sidebar infobox */
  .weeknote-nav {
    display: flex;
    justify-content: space-between;
    gap: 0.5rem;
    margin-bottom: 0.3rem;
    padding-bottom: 0.3rem;
    border-bottom: 1px dashed var(--color-border);
  }
  .weeknote-nav-link {
    font-size: 0.72rem !important;
    white-space: nowrap;
  }
  /* Weeknote navigation inline (in detail page header) */
  .weeknote-nav-inline {
    display: flex;
    gap: 0.4rem;
    margin-top: 0.2rem;
    margin-bottom: 0.2rem;
  }
  .weeknote-nav-chip {
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.78rem;
    color: var(--color-muted) !important;
    padding: 0.05rem 0.3rem;
    border: 1px solid var(--color-border);
    border-radius: 3px;
    line-height: 1.5;
    text-decoration: none !important;
  }
  .weeknote-nav-chip:hover {
    color: var(--color-accent) !important;
    border-color: var(--color-accent);
  }
  .paper-year-header {
    font-size: 0.75rem;
    font-weight: 500;
    color: var(--color-secondary);
    text-transform: uppercase;
    letter-spacing: 0.05em;
    margin-top: 0 !important;
    margin-bottom: 0.25rem !important;
    padding-bottom: 0.15rem;
    line-height: 1.3;
  }
  .note-month-header {
    font-size: 0.88rem;
    font-weight: 600;
    margin-top: 0 !important;
    margin-bottom: 0.25rem !important;
    border-bottom: 1px solid var(--color-border);
    padding-bottom: 0.15rem;
    line-height: 1.3;
  }
  .note-month-list {
    display: flex;
    flex-direction: column;
    gap: 0.1rem;
  }
  /* Journal-and-ledger split for the notes index. The journal stream of
     regular notes sits beside a weeknote ledger rail. On small screens the
     ledger comes first as a capped scrollable box so recent weeks stay
     visible without burying the journal. */
  .notes-split {
    display: flex;
    flex-direction: column;
    gap: 2rem;
  }
  @media (min-width: 1024px) {
    .notes-split {
      display: grid;
      grid-template-columns: 10rem minmax(0, 1fr);
      gap: 2.5rem;
      align-items: start;
    }
  }
  /* Extra room beneath the notes page section headers (month names in the
     journal, "Weeknotes" atop the rail). */
  .notes-journal .paper-year-header,
  .week-rail > .paper-year-header,
  .notes-feat > .paper-year-header {
    margin-bottom: 0.65rem !important;
  }
  /* Featured perma-article cards in the sidebar share the weeknote card
     anatomy: meta line, title, then image slice at the bottom. */
  .feat-list {
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
  }
  /* Spine colours live below the @layer block: Tailwind's unlayered
     border reset wins over any layered border declaration. */
  .feat-card {
    padding-left: 0.65rem;
    transition: border-color 0.15s;
  }
  .feat-slice-link {
    display: block;
  }
  .feat-synopsis {
    font-size: 0.7rem;
    line-height: 1.4;
    color: var(--color-secondary);
    margin-top: 0.15rem;
    display: -webkit-box;
    -webkit-line-clamp: 2;
    -webkit-box-orient: vertical;
    overflow: hidden;
  }
  .feat-card .feat-doi {
    color: var(--color-muted) !important;
    text-decoration: none !important;
  }
  .feat-card .feat-doi:hover {
    color: var(--color-accent) !important;
  }
  .week-rail-list {
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
  }
  /* Each weeknote is an open unit bound by an accent spine down its left
     edge: meta line, title, then the week's image slice. The spine border
     itself is declared below the @layer block. */
  .week-row {
    padding-right: 0.65rem;
    flex-shrink: 0;
    transition: border-color 0.15s;
  }
  .week-quiet {
    flex-shrink: 0;
    padding-right: calc(3px + 0.65rem);
  }
  .week-row:hover {
    border-color: var(--color-accent);
  }
  .week-slice-link {
    display: block;
  }
  .week-row-body {
    padding: 0 0 0.35rem 0;
  }
  .week-meta {
    font-size: 0.68rem;
    letter-spacing: 0.02em;
    color: var(--color-muted);
  }
  .week-range {
    color: var(--color-muted);
  }
  .week-current .week-range {
    color: var(--color-accent);
    font-weight: 500;
  }
  .week-title {
    display: block;
    font-size: 0.7rem;
    font-weight: 500;
    line-height: 1.35;
    margin-top: 0.05rem;
    color: var(--color-text) !important;
    text-decoration: none !important;
  }
  .week-quiet {
    font-size: 0.68rem;
    font-style: italic;
    color: var(--color-muted);
    padding: 0.05rem 0;
  }
  /* Year heatmap strip */
  .heatmap-strip {
    margin-bottom: 0.15rem;
  }
  .heatmap-grid {
    display: grid;
    grid-template-columns: repeat(12, 1fr);
    gap: 0.15rem;
  }
  .heatmap-cell {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.15rem;
    cursor: pointer;
    padding: 0.15rem 0;
    border-radius: 2px;
    transition: background 0.1s;
  }
  .heatmap-cell:hover {
    background: var(--color-surface-alt);
  }
  .heatmap-cell.heatmap-current {
    background: var(--color-surface-alt);
    outline: 1.5px solid var(--color-dim);
    outline-offset: -1px;
    border-radius: 3px;
  }
  .heatmap-cell.heatmap-current .heatmap-label {
    color: var(--color-text);
    font-weight: 700;
  }
  .heatmap-label {
    font-size: 0.5rem;
    color: var(--color-muted);
    line-height: 1;
    letter-spacing: -0.03em;
  }
  /* Heatmap circle — single element: colored bg (heatmap) + count inside */
  .heatmap-circle {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 1.15rem;
    height: 1.15rem;
    border-radius: 50%;
    font-size: 0.5rem;
    font-weight: 700;
    color: white;
    background: var(--color-border);
    line-height: 1;
    font-variant-numeric: tabular-nums;
    transition: background 0.15s, transform 0.1s;
  }
  .heatmap-cell:hover .heatmap-circle {
    transform: scale(1.15);
  }
  /* Green (idle) → orange → red (hot) gradient */
  .heatmap-cell[data-level="1"] .heatmap-circle { background: var(--color-accent); }
  .heatmap-cell[data-level="2"] .heatmap-circle { background: #b8a33a; }
  .heatmap-cell[data-level="3"] .heatmap-circle { background: #e08a30; }
  .heatmap-cell[data-level="4"] .heatmap-circle { background: #e04040; }
  .dark .heatmap-cell[data-level="1"] .heatmap-circle { background: var(--color-accent); }
  .dark .heatmap-cell[data-level="2"] .heatmap-circle { background: #a09030; }
  .dark .heatmap-cell[data-level="3"] .heatmap-circle { background: #c07828; }
  .dark .heatmap-cell[data-level="4"] .heatmap-circle { background: #c83838; }
  /* Level 0 — muted fill, no count */
  .heatmap-cell[data-level="0"] .heatmap-circle {
    color: var(--color-muted);
  }
  /* Past months with no posts — dashed hollow circle, en-dash inside */
  .heatmap-cell[data-state="empty"] .heatmap-circle {
    background: var(--color-surface-alt);
    border: 2px dashed var(--color-muted);
    box-sizing: border-box;
    color: var(--color-secondary);
    font-weight: 800;
  }
  .heatmap-cell[data-state="empty"] .heatmap-label {
    color: var(--color-secondary);
  }
  /* Future months — dotted hollow circle, double-dot inside */
  .heatmap-cell[data-state="future"] {
    cursor: default;
  }
  .heatmap-cell[data-state="future"] .heatmap-circle {
    background: none;
    border: 2px dotted var(--color-border-faint);
    box-sizing: border-box;
    color: var(--color-muted);
    font-size: 0.45rem;
    letter-spacing: -0.05em;
  }
  .heatmap-cell[data-state="future"] .heatmap-label {
    color: var(--color-muted);
    opacity: 0.5;
  }
  .heatmap-cell[data-state="future"]:hover {
    background: none;
  }
  .heatmap-cell[data-state="future"]:hover .heatmap-circle {
    transform: none;
  }
  .cal-divider {
    border-top: 1px dashed var(--color-border);
    margin: 0.35rem 0;
  }
  /* Notes calendar */
  .notes-calendar {
    font-family: ui-monospace, 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
  }
  .cal-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 0.3rem;
  }
  .cal-title {
    font-size: 0.72rem;
    font-weight: 600;
    color: var(--color-dim);
  }
  .cal-nav {
    background: none;
    border: none;
    color: var(--color-muted);
    cursor: pointer;
    font-size: 0.55rem;
    padding: 0.1rem 0.2rem;
    line-height: 1;
    transition: color 0.15s;
  }
  .cal-nav:hover {
    color: var(--color-link);
  }
  .cal-grid {
    display: grid;
    grid-template-columns: repeat(7, 1fr);
    grid-auto-rows: minmax(1.17rem, auto);
    gap: 0;
    text-align: center;
    font-size: 0.65rem;
    line-height: 1.8;
  }
  .cal-weekday {
    color: var(--color-muted);
    font-size: 0.6rem;
    font-weight: 600;
    line-height: 1.8;
  }
  .cal-day {
    display: flex;
    align-items: center;
    justify-content: center;
    border-radius: 2px;
    line-height: 1.8;
  }
  .cal-day-active {
    cursor: pointer;
    font-weight: 700;
    color: var(--color-link);
    transition: color 0.1s, background 0.15s;
  }
  .cal-day-active:hover {
    background: var(--color-surface-alt);
  }
  .cal-day-viewing {
    background: var(--color-link);
    color: var(--color-bg) !important;
    border-radius: 50%;
  }
  .cal-day-empty {
    color: var(--color-muted);
    font-size: 0.58rem;
  }
  /* Tag cloud */
  .tag-cloud {
    display: flex;
    flex-wrap: wrap;
    gap: 0.35rem 0.3rem;
    justify-content: space-between;
    font-family: system-ui, -apple-system, sans-serif;
  }
  .tag-cloud-btn {
    display: inline-flex;
    align-items: center;
    gap: 0.2rem;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.6rem;
    color: var(--color-dim);
    background: none;
    border: 1px solid var(--color-border);
    border-radius: 3px;
    padding: 0.15rem 0.25rem 0.15rem 0.35rem;
    cursor: pointer;
    transition: border-color 0.15s, color 0.15s, background 0.15s;
    line-height: 1.4;
  }
  .tag-cloud-btn:hover {
    border-color: var(--color-faint);
    color: var(--color-text);
  }
  .tag-cloud-btn.active {
    border-color: var(--color-accent);
    color: var(--color-accent);
    background: var(--color-surface);
  }
  /* tag-count base styles are now Tailwind utilities */
  .tag-cloud-btn:hover .tag-count {
    color: var(--color-dim);
    background: var(--color-border);
  }
  .tag-cloud-btn.active .tag-count {
    color: white;
    background: var(--color-accent);
  }
  /* Video grid — masonry two-column layout, JS reorders for left-to-right date flow */
  .vid-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    column-gap: 2rem;
    align-items: start;
  }
  @media (max-width: 900px) {
    .vid-grid { grid-template-columns: 1fr; }
  }
  /* Video card — terminal-style with thumbnail */
  .vid-card {
    border: 1px solid var(--color-border);
    border-radius: 4px;
    margin-bottom: 1.5rem;
    padding-bottom: 0.25rem;
    overflow: hidden;
    font-family: ui-monospace, 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
    font-size: 0.78rem;
    line-height: 1.5;
    transition: border-color 0.15s;
    box-shadow: 0 1px 3px rgba(0,0,0,0.06);
  }
  .vid-card:hover {
    border-color: var(--color-accent);
  }
  /* Video card header overrides (uses shared proj-card-* classes) */
  .vid-card .proj-card-header { font-size: 0.82rem; }
  .vid-card .proj-card-prompt { font-size: 0.7rem; }
  .vid-card .proj-card-title {
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  /* Video embed area — rescaled iframe within card at 75% width */
  .vid-card-embed {
    position: relative;
    overflow: hidden;
    display: flex;
    justify-content: center;
    padding: 0.5rem 0;
  }
  .vid-card-embed .video-center {
    position: relative;
    width: 75%;
    padding-bottom: 42.19%; /* 75% of 56.25% (16:9) */
    height: 0;
  }
  .vid-card-embed .video-center iframe {
    position: absolute;
    top: 0;
    left: 0;
    width: 100% !important;
    height: 100% !important;
    border: none;
    border-radius: 3px;
  }
  .vid-card-embed p { margin: 0; }
  .vid-card-embed figure { margin: 0; }
  .vid-card-embed figcaption { display: none; }
  /* Video card body */
  .vid-card-body {
    padding: 0.5rem 0.6rem;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.8rem;
    color: var(--color-secondary);
    line-height: 1.4;
  }
  .vid-card-desc {
    margin-bottom: 0.35rem;
  }
  .vid-card-desc p { margin: 0 0 0.3em; }
  .vid-card-desc a { color: var(--color-link); }
  /* Video card tags use shared .proj-card-tags class */
  .vid-card-refs {
    margin-top: 0.35rem;
    padding-top: 0.3rem;
    border-top: 1px dashed var(--color-border);
  }
  /* Video card entry rows use shared .project-entry-row classes */
  /* Contact inline row — avatar + name + social icons */
  .contact-inline-row {
    overflow: visible;
  }
  .contact-inline-socials {
    display: inline-flex;
    align-items: center;
    gap: 0.2rem;
    margin-left: 0.3rem;
    flex-shrink: 0;
  }
  .contact-social-icon {
    color: var(--color-muted) !important;
    text-decoration: none !important;
    display: inline-flex;
    align-items: center;
    transition: color 0.1s;
    opacity: 0.6;
  }
  .contact-social-icon:hover {
    color: var(--color-link) !important;
    opacity: 1;
  }
  /* Video embed in prose — add spacing below */
  .video-center {
    margin-bottom: 1.25rem;
  }
  /* Vertical video — float right on desktop, centered on mobile */
  .video-vertical {
    margin-bottom: 1.25rem;
    display: flex;
    justify-content: center;
  }
  @media (min-width: 768px) {
    .video-vertical {
      float: right;
      margin-left: 1.5rem;
      margin-bottom: 1rem;
      justify-content: flex-end;
    }
  }
  /* Video embed on detail page */
  .vid-embed .video-center {
    border: 1px solid var(--color-border);
    border-radius: 4px;
    overflow: hidden;
  }
  .vid-embed .video-vertical {
    border: 1px solid var(--color-border);
    border-radius: 4px;
    overflow: hidden;
  }
  .vid-embed iframe {
    display: block;
  }
}

/* Ideas index */
/* Unlayered: the cards, rows and search box draw 1px borders and the level
   rows are buttons, and Tailwind's unlayered preflight zeroes border-width
   and button padding inside @layer. */
/* Ideas index — filter band */
/* Not sticky. The nav header is sticky at the top of the viewport with a
   higher stacking order, so a band pinned there would scroll underneath it
   and disappear. */
.idea-band {
  background: var(--color-bg);
  border-bottom: 1px solid var(--color-border);
  padding: 0.5rem 0 0.7rem;
  margin-bottom: 1.1rem;
}
/* Two compact rows of checkboxes, first on the page. They are meant to be
   read at a glance, so a row wraps rather than scrolling and the whole of
   each label is the hit area. */
.idea-facet {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 0.15rem 0.45rem;
  margin-bottom: 0.25rem;
}
.idea-facet-label {
  flex-shrink: 0;
  width: 3.4rem;
  font-size: 0.7rem;
  font-weight: 600;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--color-muted);
}
.idea-box {
  display: inline-flex;
  align-items: center;
  gap: 0.3rem;
  font-size: 0.8rem;
  line-height: 1.5;
  padding: 0.05rem 0.4rem;
  border: 1px solid transparent;
  border-radius: 3px;
  color: var(--color-secondary);
  cursor: pointer;
  user-select: none;
}
.idea-box:hover { background: var(--color-surface); }
.idea-box.on {
  border-color: var(--color-border);
  background: var(--color-surface);
  color: var(--color-text);
}
/* Drawn rather than left to the browser, so the box carries the colour of the
   status it stands for and the row is the key to the bars in the contents. A
   level box has no status and falls back to the page accent. Ticked is the
   swatch filled and unticked is the same swatch outlined, which needs no tick
   glyph and so no colour that has to stay legible against five fills in two
   themes. */
.idea-box-in {
  appearance: none;
  -webkit-appearance: none;
  flex-shrink: 0;
  width: 0.8rem;
  height: 0.8rem;
  margin: 0;
  border: 1.5px solid var(--idea-status, var(--color-accent));
  border-radius: 3px;
  background: transparent;
  cursor: pointer;
}
.idea-box-in:checked { background: var(--idea-status, var(--color-accent)); }
.idea-box-in:focus-visible {
  outline: 2px solid var(--color-accent);
  outline-offset: 1px;
}
.idea-box-n {
  font-size: 0.72rem;
  color: var(--color-muted);
  font-variant-numeric: tabular-nums;
}
/* Pushed to the end of the row it sits in, so it never shifts a box along
   when it appears. */
.idea-clear {
  flex-shrink: 0;
  margin-left: auto;
  font: inherit;
  font-size: 0.78rem;
  color: var(--color-link);
  background: none;
  border: none;
  padding: 0;
  cursor: pointer;
  text-decoration: underline dotted;
}
.idea-band-part { margin-top: 0.9rem; }
.idea-band-label {
  font-size: 0.7rem;
  font-weight: 600;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--color-muted);
  margin: 0 0 0.3rem;
}
/* Ideas index — the contents. Two columns of project names, each with its
   ideas as a bar stacked by status. */
.idea-toc {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  column-gap: 1.5rem;
  row-gap: 1px;
}
@media (max-width: 640px) {
  .idea-toc { grid-template-columns: minmax(0, 1fr); }
}
.idea-toc-row {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 0.8rem;
  line-height: 1.6;
  padding: 0 0.35rem;
  border-left: 2px solid transparent;
  color: var(--color-secondary) !important;
  text-decoration: none !important;
}
.idea-toc-row:hover {
  background: var(--color-surface);
  border-left-color: var(--color-accent);
  color: var(--color-link) !important;
}
/* A filter that empties a project greys its line rather than dropping it, so
   the contents keeps its shape and a reader sees what was ruled out. */
.idea-toc-row.idea-toc-out { opacity: 0.3; }
.idea-toc-name {
  flex: 1;
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
/* The bar is scaled against the largest project on the page, so its length
   compares across the contents and its bands compare within one. The track
   behind it is the rest of that scale, which is what makes a short bar read
   as few rather than as narrow. */
.idea-toc-bar {
  flex-shrink: 0;
  display: flex;
  gap: 1px;
  width: 7rem;
  height: 0.45rem;
  border-radius: 2px;
  background: var(--color-border-faint);
  overflow: hidden;
}
.idea-toc-seg {
  flex-shrink: 0;
  min-width: 2px;
  background: var(--idea-status);
}
.idea-toc-total {
  flex-shrink: 0;
  width: 1.2rem;
  text-align: right;
  font-size: 0.75rem;
  color: var(--color-muted);
  font-variant-numeric: tabular-nums;
}
/* As a rail the band has a column to itself, so the two facets stack, each
   box takes a line of its own and the contents runs down one column instead
   of two. The rule on the left is what makes the column read as a rail
   rather than as a stray block of text. */
@media (min-width: 76rem) {
  .idea-band {
    border-bottom: none;
    border-left: 1px solid var(--color-border);
    padding: 0.15rem 0 0.7rem 1.25rem;
    margin-bottom: 0;
  }
  .idea-facet { display: block; margin-bottom: 1rem; }
  .idea-facet-label { display: block; width: auto; margin-bottom: 0.25rem; }
  .idea-box { display: flex; padding: 0.05rem 0.3rem; }
  /* Pushed to the right edge of the rail, so the counts line up down it. */
  .idea-box-n { margin-left: auto; }
  .idea-clear { margin-top: 0.35rem; }
  .idea-band-part { margin-top: 0; }
  .idea-toc { grid-template-columns: minmax(0, 1fr); row-gap: 0.2rem; }
  /* The name takes a line to itself and wraps, since a rail is too narrow to
     hold a project name and its bar side by side without cutting the name,
     and the name is the half a reader needs. */
  .idea-toc-row { flex-wrap: wrap; gap: 0.1rem 0.4rem; padding: 0.1rem 0.3rem; }
  .idea-toc-name {
    flex: 1 0 100%;
    white-space: normal;
    overflow: visible;
    line-height: 1.35;
  }
  .idea-toc-bar { flex: 1; width: auto; }
}
/* The way back from an idea to the list it belongs to. Quiet, above the
   title, and the arrow is what makes it read as a way back rather than as
   another link into the page. */
.idea-back {
  margin: 0 0 0.4rem;
  font-size: 0.78rem;
  line-height: 1.4;
}
.idea-back-link {
  display: inline-flex;
  align-items: center;
  gap: 0.3rem;
  color: var(--color-secondary) !important;
  text-decoration: none !important;
}
.idea-back-link:hover { color: var(--color-link) !important; }
/* Ideas index — project groups in one column, in project order. The page is
   served by the full width layout, so the cap is here rather than there. The
   ideas keep the measure they had, and the filter takes a rail beside them
   in room the cap left blank. Below the width that fits both, the rail goes
   back to being a band above the ideas, which is the order it reads in. */
[data-idea-index] { max-width: 48rem; }
@media (min-width: 76rem) {
  [data-idea-index] {
    display: grid;
    grid-template-columns: minmax(0, 1fr) 15rem;
    column-gap: 2rem;
    align-items: start;
    max-width: 65rem;
  }
  .idea-intro { grid-column: 1; grid-row: 1; }
  .idea-grid { grid-column: 1; grid-row: 2; }
  .idea-empty { grid-column: 1; grid-row: 3; }
  /* Spans the rows so the rail runs the length of the ideas rather than
     stopping level with the first of them. */
  .idea-band { grid-column: 2; grid-row: 1 / span 3; }
}
.idea-grid {
  display: grid;
  grid-template-columns: minmax(0, 1fr);
  align-items: start;
}
.idea-group {
  min-width: 0;
  margin: 0 0 2.25rem;
  /* The nav header is sticky, so a project jumped to from the contents would
     otherwise land underneath it. */
  scroll-margin-top: 4.5rem;
}
/* A filled bar, as a card header is on the projects page, since the two name
   the same thing. A rule alone is not enough to tell a project heading from
   the cards under it. It is told apart from them by three things a card has
   none of: it reaches past them on both sides, its fill is a step darker
   than the page, and its left edge is drawn in the text colour. That last is
   deliberately not a colour from the status palette, so the edge of a
   heading can never be read as the status of a card. */
.idea-group-head {
  position: relative;
  overflow: hidden;
  display: grid;
  grid-template-columns: auto minmax(0, 1fr) auto;
  align-items: baseline;
  column-gap: 0.45rem;
  font-size: 1rem;
  color: var(--color-secondary);
  background: var(--color-surface-alt);
  border: 1px solid var(--color-border);
  border-left: 3px solid var(--color-text);
  border-radius: 3px;
  padding: 0.45rem 0.7rem;
  margin: 0 0 0.85rem;
}
/* The reach past the cards costs the page gutter, which is only 0.5rem until
   the layout widens it. Below that the fill, the edge and the size are what
   tell the heading apart, and it sits flush with the cards. */
@media (min-width: 48rem) {
  .idea-group-head { margin-left: -0.55rem; margin-right: -0.55rem; }
}
/* A subtitle inside the heading bar rather than a caption loose beneath it.
   It takes the column the title is in, so it lines up under the title rather
   than under the prompt, with no indent of its own to keep in step. */
.idea-group-note {
  grid-column: 2;
  max-width: 34rem;
  margin: 0.12rem 0 0;
  font-size: 0.78rem;
  font-weight: 400;
  line-height: 1.4;
  color: var(--color-secondary);
}
/* The picture of the project washed across its heading, not an illustration
   beside the title. It is faded to a token that differs by theme, and masked
   away towards the left so it never reaches the start of the title. */
.idea-group-art {
  position: absolute;
  top: 0;
  right: 0;
  bottom: 0;
  width: 55%;
  opacity: var(--idea-art);
  /* Desaturated, so a bright photograph does not pull the eye off the words
     sitting on top of it. */
  filter: grayscale(0.4);
  pointer-events: none;
  -webkit-mask-image: linear-gradient(to left, #000 5%, transparent 92%);
  mask-image: linear-gradient(to left, #000 5%, transparent 92%);
}
.idea-group-art img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  display: block;
}
/* The wash is a positioned box, so the words need to be positioned too or
   they paint under it. */
.idea-group-prompt,
.idea-group-title,
.idea-group-tail,
.idea-group-note { position: relative; }
.idea-group-prompt {
  grid-column: 1;
  color: var(--color-accent);
  font-weight: 600;
}
/* One box for the counts and the chevron, so the bar keeps three columns
   however many counts a project has. */
.idea-group-tail {
  grid-column: 3;
  display: inline-flex;
  align-items: baseline;
  gap: 0.4rem;
}
/* The name wraps rather than truncating. A project heading that reads
   "TESSERA, a pixelwise geospatial foundation mo…" is the one thing on this
   page a reader cannot afford to lose. */
.idea-group-title {
  grid-column: 2;
  min-width: 0;
  font-size: 1.05rem;
  font-weight: 700;
  letter-spacing: -0.01em;
  color: var(--color-text) !important;
  text-decoration: none !important;
}
.idea-group-title:hover { color: var(--color-link) !important; }
/* Each count is a chip in the colour of the status it counts, so a heading
   reads against the same key as the filter row and the card borders. The
   plain colour above the mixed one is the fallback a browser without
   color-mix keeps. */
.idea-group-count {
  white-space: nowrap;
  padding: 0.02rem 0.4rem;
  border: 1px solid var(--color-border);
  border: 1px solid color-mix(in srgb, var(--idea-status) 38%, transparent);
  border-radius: 999px;
  background: color-mix(in srgb, var(--idea-status) 9%, var(--color-bg));
  font-size: 0.72rem;
  color: var(--color-secondary);
  font-variant-numeric: tabular-nums;
}
/* Finished work covers two statuses, so its count takes neither colour. The
   whole border is restated rather than only its colour: the rule above
   resolves to a colour that does not exist without a status, which drops the
   style with it and leaves no border at all. */
.idea-group-count-past {
  border: 1px solid var(--color-border);
  background: none;
}
/* Ideas index — the status of an idea as a colour a card can draw with. A
   card, the line of a finished idea and the count on a project heading all
   read from this palette, so none of them can disagree about a status. */
.idea-st-avail { --idea-status: var(--color-st-avail); }
.idea-st-discuss { --idea-status: var(--color-st-discuss); }
.idea-st-ongoing { --idea-status: var(--color-st-ongoing); }
.idea-st-done { --idea-status: var(--color-st-done); }
.idea-st-expired { --idea-status: var(--color-st-expired); }
/* Ideas index — a card for an idea open for takers. The plain border above
   the mixed one is the fallback a browser without color-mix keeps. */
.idea-card {
  border: 1px solid var(--color-border);
  border: 1px solid color-mix(in srgb, var(--idea-status) 40%, transparent);
  border-left: 3px solid var(--idea-status);
  border-radius: 3px;
  padding: 0.6rem 0.7rem;
  margin-bottom: 0.85rem;
  transition: border-color 0.15s;
}
.idea-card:hover { border-color: var(--idea-status); }
/* Two columns wide enough apart to read as separate things: prose on the
   left at the measure a line of text wants, and the level, the year and the
   supervisors in a fixed column on the right. Positioned so both sit over
   the picture rather than under it. */
.idea-card-body {
  display: grid;
  grid-template-columns: minmax(0, 1fr) 12rem;
  column-gap: 1.5rem;
  align-items: start;
}
.idea-card-title {
  display: block;
  grid-column: 1;
  font-size: 0.95rem;
  font-weight: 600;
  line-height: 1.3;
  color: var(--color-text) !important;
  text-decoration: none !important;
}
.idea-card-title:hover { color: var(--color-link) !important; }
/* Spans both rows so it keeps the top of the card whatever the summary does
   below the title. */
.idea-card-meta {
  grid-column: 2;
  grid-row: 1 / span 2;
  font-size: 0.78rem;
  line-height: 1.45;
  color: var(--color-secondary);
  margin: 0.15rem 0 0;
}
.idea-card-meta a { color: var(--color-link); }
.idea-card-discuss { color: var(--color-st-discuss); }
.idea-card-summary {
  grid-column: 1;
  font-size: 0.84rem;
  line-height: 1.5;
  color: var(--color-dim);
  margin: 0.35rem 0 0;
}
/* Too narrow for a second column, so the three fall back to the one order
   they read in as a block. */
@media (max-width: 56rem) {
  .idea-card-body { grid-template-columns: minmax(0, 1fr); }
  .idea-card-title, .idea-card-meta, .idea-card-summary {
    grid-column: 1;
    grid-row: auto;
  }
}
/* Ideas index — an idea no longer on offer, on one line that links straight
   to it. It runs over the tracks a card runs over, so a line and a card put
   their facts in the same place. */
.idea-past-card {
  position: relative;
  display: grid;
  grid-template-columns: minmax(0, 1fr) 12rem;
  column-gap: 1.5rem;
  align-items: baseline;
  border: 1px solid var(--color-border);
  border: 1px solid color-mix(in srgb, var(--idea-status) 22%, transparent);
  border-left: 2px solid var(--idea-status);
  border-radius: 3px;
  padding: 0.3rem 0.45rem;
  margin-bottom: 0.35rem;
  font-size: 0.84rem;
  line-height: 1.45;
  transition: border-color 0.15s;
}
/* The history starts a little clear of the last open card, so the two kinds
   of entry do not run together. */
.idea-card + .idea-past-card { margin-top: 0.35rem; }
.idea-past-card:hover {
  border-color: color-mix(in srgb, var(--idea-status) 45%, transparent);
  border-left-color: var(--idea-status);
}
.idea-past-card:hover .idea-past-title { color: var(--color-link) !important; }
.idea-past-card:hover .idea-past-open { color: var(--color-link); }
.idea-past-line { grid-column: 1; min-width: 0; }
.idea-past-title {
  color: var(--color-text) !important;
  text-decoration: none !important;
}
/* The box that makes the whole row the link. It is on the title rather than
   on the row, so the row stays a plain element and the students named in the
   facts can be links of their own inside it. Those sit above this box by
   being positioned themselves, which paints them later. */
.idea-past-title::after {
  content: "";
  position: absolute;
  inset: 0;
}
.idea-past-meta {
  grid-column: 2;
  padding-right: 1rem;
  font-size: 0.76rem;
  line-height: 1.4;
  color: var(--color-secondary);
}
.idea-past-meta a {
  position: relative;
  color: var(--color-link);
}
/* Pinned to the corner rather than given a track, so the row runs over the
   two columns a card has and not three. */
.idea-past-open {
  position: absolute;
  top: 0.3rem;
  right: 0.45rem;
  display: inline-flex;
  color: var(--color-muted);
  pointer-events: none;
}
/* Too narrow for a second column, so the facts fall back under the title,
   which is what an open card does at this width. */
@media (max-width: 56rem) {
  .idea-past-card { grid-template-columns: minmax(0, 1fr); }
  .idea-past-meta { grid-column: 1; }
}
.idea-empty {
  font-size: 0.88rem;
  color: var(--color-dim);
  padding: 0.75rem 0;
}

/* Search page */
/* Unlayered: the section cards, rows and form draw 1px borders, and
   Tailwind's unlayered preflight zeroes border-width inside @layer. */
.sp-page { max-width: 72rem; margin: 0 auto; }
.sp-form { display: flex; align-items: center; gap: 0.5rem; border: 1px solid var(--color-border-faint); border-radius: 6px; padding: 0.4rem 0.7rem; margin-bottom: 0.9rem; }
.sp-form:focus-within { border-color: var(--color-accent); }
.sp-prompt { color: var(--color-accent); font-family: ui-monospace, 'SF Mono', monospace; font-weight: 600; font-size: 0.85rem; }
.sp-form input { flex: 1; border: 0; outline: 0; background: transparent; color: var(--color-text); font: inherit; font-size: 1rem; }
.sp-grid { display: grid; grid-template-columns: minmax(0, 1fr) 20rem; gap: 2rem; row-gap: 0.9rem; }
@media (max-width: 56rem) { .sp-grid { grid-template-columns: minmax(0, 1fr); } }
.sp-count { grid-column: 1 / -1; font-family: ui-monospace, 'SF Mono', monospace; font-size: 0.75rem; color: var(--color-muted); }
.sp-sort { display: inline-flex; border: 1px solid var(--color-border); border-radius: 999px; overflow: hidden; background: var(--color-bg); }
.sp-sort-opt { font-size: 0.68rem; padding: 0.05rem 0.55rem; color: var(--color-muted); text-decoration: none !important; }
.sp-sort-opt.on { background: var(--color-accent); color: #fff; }
.sp-sort-opt:not(.on):hover { color: var(--color-text); }
.sp-spin { display: none; width: 0.95rem; height: 0.95rem; border: 2px solid var(--color-border); border-top-color: var(--color-accent); border-radius: 50%; flex-shrink: 0; }
.sp-spin.busy { display: inline-block; animation: sp-rotate 0.7s linear infinite; }
@keyframes sp-rotate { to { transform: rotate(360deg); } }
@media (prefers-reduced-motion: reduce) { .sp-spin.busy { animation: none; } }
.sp-link[data-href] { cursor: pointer; }
.sp-link:hover .sp-t, .sp-link .sp-t:hover { text-decoration: underline !important; }
.sp-via:hover { text-decoration: underline !important; color: var(--color-link) !important; }
/* A hit's media block: the entry image washed into the background with
   the kind icon as a corner badge, or the icon alone on a tinted square.
   Unlayered so the fixed crop beats the Tailwind preflight img reset. */
.sp-media {
  position: relative;
  width: 3rem;
  height: 3rem;
  flex-shrink: 0;
  border: 1px solid var(--color-border);
  border-radius: 6px;
  overflow: hidden;
  display: inline-flex;
  align-items: center;
  justify-content: center;
}
.sp-media img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  display: block;
  opacity: 0.45;
  filter: sepia(0.5) saturate(0.6);
  transition: opacity 0.15s, filter 0.15s;
}
.sp-work:hover .sp-media img { opacity: 1; filter: none; }
.sp-media-badge {
  position: absolute;
  right: 2px;
  bottom: 2px;
  display: inline-flex;
  padding: 2px;
  border-radius: 4px;
  background: var(--color-bg);
  border: 1px solid var(--color-border);
  line-height: 0;
}
.sp-media-solo {
  background: color-mix(in srgb, currentColor 12%, var(--color-surface));
}
@media (prefers-reduced-motion: reduce) { .sp-media img { transition: none; } }
/* Each tier is a card: a surface header strip naming the tier and how it
   is ordered, hairline-separated rows, and a footer strip for more. */
.sp-sec { margin-bottom: 1.25rem; border: 1px solid var(--color-border-light); border-radius: 8px; background: var(--color-bg); overflow: hidden; }
.sp-sec-h { display: flex; align-items: baseline; gap: 0.6rem; padding: 0.4rem 0.75rem; background: var(--color-surface); border-bottom: 1px solid var(--color-border); }
.sp-eyebrow { font-size: 0.66rem; text-transform: uppercase; letter-spacing: 0.1em; color: var(--color-dim); font-weight: 600; }
.sp-note { font-size: 0.7rem; color: var(--color-muted); }
.sp-sec-h .sp-n { margin-left: auto; padding: 0 0.5rem; border: 1px solid var(--color-border); border-radius: 999px; background: var(--color-bg); }
.sp-n { font-family: ui-monospace, 'SF Mono', monospace; font-size: 0.68rem; color: var(--color-faint); font-variant-numeric: tabular-nums; }
.sp-rows .sp-hit + .sp-hit { border-top: 1px solid var(--color-border); }
.sp-gotos { display: flex; flex-wrap: wrap; gap: 0.4rem; padding: 0.6rem 0.75rem; }
.sp-goto { display: inline-flex; align-items: center; gap: 0.35rem; padding: 0.25rem 0.6rem; border: 1px solid var(--color-border); border-radius: 6px; font-size: 0.82rem; text-decoration: none !important; color: var(--color-text) !important; }
.sp-goto .sp-t { font-weight: 500; }
.sp-goto .sp-sub { font-family: ui-monospace, 'SF Mono', monospace; font-size: 0.68rem; color: var(--color-muted); }
.sp-goto:hover, .sp-goto.selected { border-color: var(--color-accent); background: var(--color-surface); }
.sp-hit { text-decoration: none !important; color: inherit !important; }
.sp-work, .sp-link { display: flex; gap: 0.55rem; padding: 0.5rem 0.75rem; border-left: 2px solid transparent; }
.sp-work:hover, .sp-work.selected, .sp-link:hover, .sp-link.selected { background: var(--color-surface); border-left-color: var(--color-accent); }
.sp-ic, .sp-fav { width: 1.3rem; height: 1.3rem; display: inline-flex; align-items: center; justify-content: center; flex-shrink: 0; margin-top: 0.1rem; color: var(--color-secondary); }
.sp-fav img { width: 16px; height: 16px; border-radius: 3px; }
.sp-ic-paper { color: #3b82f6; } .sp-ic-note { color: #10b981; } .sp-ic-project { color: #8b5cf6; }
.sp-ic-idea { color: #f59e0b; } .sp-ic-video { color: #ef4444; } .sp-ic-weekly { color: #14b8a6; }
.sp-body { flex: 1; min-width: 0; display: flex; flex-direction: column; gap: 0.1rem; }
.sp-line { display: flex; align-items: baseline; gap: 0.5rem; min-width: 0; }
.sp-line .sp-t { font-weight: 500; font-size: 0.88rem; flex: 1; min-width: 0; }
.sp-link .sp-t { text-decoration: none !important; color: var(--color-text) !important; font-size: 0.78rem; overflow-wrap: anywhere; display: -webkit-box; -webkit-line-clamp: 2; -webkit-box-orient: vertical; overflow: hidden; }
.sp-d { font-size: 0.7rem; color: var(--color-muted); flex-shrink: 0; white-space: nowrap; }
.sp-snip { font-size: 0.78rem; color: var(--color-secondary); line-height: 1.4; overflow: hidden; display: -webkit-box; -webkit-line-clamp: 2; -webkit-box-orient: vertical; }
.sp-snip b, .sp-t b { background: var(--color-highlight); font-weight: 600; border-radius: 2px; }
.sp-tags { display: flex; flex-wrap: wrap; gap: 0.25rem; }
.sp-tag { font-size: 0.66rem; padding: 0.05rem 0.45rem; border: 1px solid var(--color-border); border-radius: 999px; color: var(--color-secondary); cursor: pointer; }
.sp-tag:hover { border-color: var(--color-accent); color: var(--color-text); }
.sp-meta { display: block; font-size: 0.7rem; color: var(--color-secondary); }
.sp-dom { color: var(--color-muted); }
.sp-via { text-decoration: none !important; color: var(--color-secondary); }
.sp-via svg { vertical-align: -0.12em; }
.sp-via-in { color: var(--color-muted); }
.sp-more { display: block; width: 100%; text-align: left; padding: 0.35rem 0.75rem; font: inherit; font-size: 0.76rem; color: var(--color-link); background: var(--color-surface); border: 0; border-top: 1px solid var(--color-border); cursor: pointer; }
.sp-more:hover { text-decoration: underline; }
.sp-rail { font-size: 0.82rem; }
.sp-facets { display: flex; flex-wrap: wrap; gap: 0.3rem; padding: 0.5rem 0.75rem 0.15rem; }
.sp-facets:last-of-type { padding-bottom: 0.5rem; }
.sp-f { display: inline-flex; gap: 0.3rem; align-items: baseline; font: inherit; font-size: 0.74rem; padding: 0.1rem 0.5rem; border: 1px solid var(--color-border); border-radius: 999px; color: var(--color-secondary); cursor: pointer; background: var(--color-bg); }
.sp-f:hover, .sp-f.on { border-color: var(--color-accent); color: var(--color-text); }
.sp-years { display: flex; align-items: flex-end; gap: 2px; height: 2.2rem; margin: 0.4rem 0.75rem 1.3rem; }
.sp-year { flex: 1; background: var(--color-border-faint); border-radius: 1px 1px 0 0; position: relative; min-height: 2px; }
.sp-year.hot { background: var(--color-accent); }
.sp-year span { position: absolute; bottom: -0.95rem; left: 0; font-family: ui-monospace, 'SF Mono', monospace; font-size: 0.58rem; color: var(--color-faint); }
.sp-empty { padding: 2rem 1rem; text-align: center; color: var(--color-secondary); font-size: 0.85rem; grid-column: 1 / -1; }

/* Weeknote ledger slice — unlayered so the fixed crop height wins over the
   Tailwind preflight img { height: auto } reset */
.week-slice {
  display: block;
  width: 100%;
  height: 2.9rem;
  object-fit: cover;
  opacity: 0.75;
  filter: sepia(0.7) saturate(0.7);
  transition: opacity 0.15s, filter 0.15s;
  border: 1px solid var(--color-border);
  border-radius: 4px;
}
.week-row:hover .week-slice {
  opacity: 1;
  filter: none;
}
.feat-card:hover .week-slice {
  opacity: 1;
  filter: none;
}
/* Accent spines binding each ledger and featured unit — unlayered so they
   survive the Tailwind preflight border reset */
.week-row {
  border-right: 3px solid var(--color-weeknote-accent);
}
.feat-card {
  border-left: 3px solid color-mix(in srgb, var(--color-accent) 45%, var(--color-weeknote-accent));
}
.week-row:hover {
  border-right-color: var(--color-accent);
}
.feat-card:hover {
  border-left-color: var(--color-accent);
}
/* The dark weeknote accent is a background tint and vanishes as a line,
   so lift the spine towards the foreground in dark mode. */
.dark .week-row {
  border-right-color: color-mix(in srgb, var(--color-weeknote-accent) 45%, var(--color-secondary));
}
.dark .feat-card {
  border-left-color: color-mix(in srgb, var(--color-accent) 45%, var(--color-secondary));
}
.dark .week-row:hover {
  border-right-color: var(--color-accent);
}
.dark .feat-card:hover {
  border-left-color: var(--color-accent);
}
/* Light source images glare against dark cards, so dim them further */
.dark .week-slice {
  opacity: 0.55;
}
.dark .week-row:hover .week-slice,
.dark .feat-card:hover .week-slice {
  opacity: 0.9;
}

/* Unlayered — Tailwind's button reset is unlayered too, and would
   otherwise win over anything set inside @layer components */
.sidebar-header-btn {
  display: inline-flex;
  align-items: center;
  flex: 0 0 auto;
  padding: 0;
  border: none;
  background: none;
  color: var(--color-muted);
  cursor: pointer;
  transition: color 0.15s;
}
.sidebar-header-btn:hover {
  color: var(--color-link);
}
/* Unlayered — wins over Tailwind utility classes */
main a:not(.no-underline):not(.heading-anchor):not(.lightbox-trigger) {
  color: var(--color-link);
  text-decoration: underline dotted;
  text-decoration-color: var(--color-link-ul);
  text-underline-offset: 2px;
}
main a:not(.no-underline):not(.heading-anchor):not(.lightbox-trigger):hover {
  text-decoration-style: solid;
  text-decoration-color: var(--color-link);
}
/* Paper list: allow titles to wrap */
.paper-item .note-compact-title {
  white-space: normal;
  overflow: visible;
  text-overflow: unset;
}
/* Paper list entries: tone down links since there are many per entry */
main .paper-item a:not(.no-underline):not(.heading-anchor):not(.lightbox-trigger) {
  color: var(--color-text);
  text-decoration: underline;
  text-decoration-color: var(--color-border);
}
main .paper-item a:not(.no-underline):not(.heading-anchor):not(.lightbox-trigger):hover {
  text-decoration-color: var(--color-text);
}
/* Compact the action links bar in paper list items */
.paper-item .flex.items-center.gap-4 {
  font-size: 0.72rem;
  gap: 0.5rem;
  margin-top: 0.15rem;
}
.paper-item .flex.items-center.gap-4 svg {
  width: 12px;
  height: 12px;
}
.ref-backlink {
  color: var(--color-link);
  text-decoration: none;
}
.ref-backlink:hover {
  text-decoration: underline dotted;
  text-decoration-color: var(--color-link-ul);
}
/* Nav bar */
.nav-bg {
  background: linear-gradient(to bottom, var(--color-nav-from), var(--color-nav-to));
}
.nav-prompt {
  font-family: ui-monospace, 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
  color: var(--color-accent);
  font-weight: 400;
  font-size: 0.85em;
  letter-spacing: -0.05em;
}
.nav-border {
  border-bottom: 1px solid var(--color-border-nav);
  box-shadow: 0 1px 2px rgba(0,0,0,0.03);
}
.nav-caret {
  color: var(--color-accent);
}
/* Mobile menu */
.mobile-menu {
  display: none;
  position: fixed;
  inset: 0;
  z-index: 100;
}
.mobile-menu.open {
  display: flex;
}
.mobile-menu-backdrop {
  position: absolute;
  inset: 0;
  background: rgba(0,0,0,0.3);
  transition: opacity 0.2s;
}
.mobile-menu-panel {
  position: relative;
  width: 16rem;
  max-width: 80vw;
  height: 100%;
  background: var(--color-bg);
  border-right: 1px solid var(--color-border);
  box-shadow: 4px 0 12px rgba(0,0,0,0.08);
  overflow-y: auto;
  animation: mobile-menu-slide 0.2s ease-out;
}
@keyframes mobile-menu-slide {
  from { transform: translateX(-100%); }
  to { transform: translateX(0); }
}
.mobile-nav-link {
  font-size: 0.88rem;
  border-radius: 6px;
}
@media (min-width: 768px) {
  .mobile-menu { display: none !important; }
}
/* Social box */
.social-box-body {
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
}
.social-box-link {
  display: flex;
  align-items: flex-start;
  gap: 0.4rem;
  padding: 0.15rem 0;
  color: var(--color-dim) !important;
  text-decoration: none !important;
  transition: color 0.1s;
}
.social-box-link > svg {
  margin-top: 0.15rem;
  flex-shrink: 0;
}
.social-box-link:hover {
  color: var(--color-link) !important;
}
.social-box-label {
  font-family: system-ui, -apple-system, sans-serif;
  font-size: 0.72rem;
  min-width: 0;
}
.social-group { margin-bottom: 0.35rem; }
.social-group:last-child { margin-bottom: 0; }
.social-group-label {
  font-size: 0.6rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  color: var(--color-muted);
  margin-bottom: 0.1rem;
}
.social-box-service {
  font-size: 0.7rem;
  padding-left: 0.25rem;
  opacity: 0.5;
}
.social-box-address {
  font-family: system-ui, -apple-system, sans-serif;
  font-size: 0.65rem;
  color: var(--color-dim);
  padding-left: 1.4rem;
  line-height: 1.4;
}
/* Feed dropdown */
.feed-dropdown-wrap {
  position: relative;
}
.feed-dropdown {
  display: none;
  position: fixed;
  z-index: 110;
  background: var(--color-bg);
  border: 1px solid var(--color-border);
  border-radius: 6px;
  box-shadow: 0 4px 16px rgba(0,0,0,0.12);
  min-width: 10rem;
  font-family: system-ui, -apple-system, sans-serif;
  font-size: 0.78rem;
  overflow: hidden;
}
.feed-dropdown.open {
  display: block;
}
.feed-dropdown-header {
  padding: 0.35rem 0.6rem;
  font-size: 0.65rem;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  color: var(--color-muted);
  border-bottom: 1px solid var(--color-border);
}
.feed-dropdown-item {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  padding: 0.4rem 0.6rem;
  color: var(--color-dim) !important;
  text-decoration: none !important;
  transition: background 0.1s;
}
.feed-dropdown-item:hover {
  background: var(--color-surface);
  color: var(--color-link) !important;
}
.feed-dropdown-desc {
  margin-left: auto;
  font-size: 0.65rem;
  color: var(--color-muted);
}
.feed-dropdown-divider {
  border-top: 1px solid var(--color-border);
  margin: 0;
}
.page-title {
  border-left: 3px solid var(--color-accent);
  padding-left: 0.6rem;
  margin-top: 0 !important;
}
/* Links page */
.link-list {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}
.link-group {
  padding: 0.35rem 0;
}
.link-group-header {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  padding-bottom: 0.25rem;
  border-bottom: 1px solid var(--color-border);
  margin-bottom: 0.2rem;
}
.link-group-title {
  font-size: 0.88rem;
  font-weight: 500;
  color: var(--color-text) !important;
}
.link-group-title:hover {
  color: var(--color-link) !important;
}
.link-row {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  padding: 0.15rem 0;
  min-width: 0;
}
.link-row:hover {
  background: var(--color-surface);
  border-radius: 3px;
}
.link-kind-badge {
  flex-shrink: 0;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 1.1rem;
  font-size: 0.65rem;
  color: var(--color-muted);
  text-transform: uppercase;
  font-weight: 600;
  letter-spacing: 0.02em;
}
.link-kind-badge svg {
  color: var(--color-secondary);
  opacity: 0.7;
}
.link-kind-github svg { color: var(--color-text); opacity: 0.8; }
.link-kind-contact svg { color: var(--color-accent); opacity: 0.8; }
.link-kind-arxiv svg { color: #b31b1b; opacity: 0.9; }
.link-kind-doi svg { color: #fcb425; opacity: 0.9; }
.link-kind-rfc svg { color: #0076a8; opacity: 0.9; }
.link-favicon {
  flex-shrink: 0;
  width: 14px;
  height: 14px;
  border-radius: 2px;
  object-fit: contain;
  background: white;
  padding: 1px;
}
.link-contact-thumb {
  flex-shrink: 0;
  width: 14px;
  height: 14px;
  border-radius: 50%;
  object-fit: cover;
}
.link-label {
  flex: 1;
  min-width: 0;
  font-size: 0.82rem;
  color: var(--color-text) !important;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.link-label:hover {
  color: var(--color-link) !important;
}
.link-label-secondary {
  color: var(--color-secondary) !important;
  font-weight: 400;
}
.link-domain-hint {
  flex-shrink: 0;
  font-size: 0.75rem;
  color: var(--color-muted);
  white-space: nowrap;
  margin-left: auto;
}
.link-backlink-chip {
  font-size: 0.75rem;
  color: var(--color-muted) !important;
  text-decoration: none !important;
  display: inline-flex;
  align-items: center;
  gap: 0.15rem;
}
.link-backlink-chip:hover {
  color: var(--color-link) !important;
}
/* Feeds page */
.feed-list {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
}
.feed-card {
  padding: 0.5rem;
  border-radius: 3px;
  transition: background 0.1s;
}
.feed-card:hover {
  background: var(--color-surface);
}
.feed-card-header {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 0.25rem;
}
.feed-card-name {
  font-weight: 500;
  color: var(--color-text) !important;
}
.feed-card-name:hover {
  color: var(--color-link) !important;
}
.feed-card-feeds {
  padding-left: 2rem;
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
}
.feed-entry-row {
  display: flex;
  align-items: baseline;
  gap: 0.35rem;
  font-size: 0.82rem;
}
.feed-url {
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
/* feed-type-badge base styles are now Tailwind utilities */
/* Feed items (planet-style entries) */
.feed-item {
  border-radius: 3px;
  transition: background 0.1s;
}
.feed-item:hover {
  background: var(--color-surface);
}
.feed-item-source {
  margin-top: 0.15rem;
}
.feed-item-mentions {
  display: flex;
  flex-direction: column;
  gap: 0.1rem;
  margin-top: 0.1rem;
}
.feed-item-mentions .link-backlink-chip {
  display: inline-flex;
  align-items: center;
  gap: 0.25rem;
}
/* Blogroll sidebar */
.feed-blogroll-row {
  gap: 0.35rem;
}
.feed-blogroll-avatar {
  width: 16px;
  height: 16px;
  border-radius: 50%;
  object-fit: cover;
  border: 1.5px solid var(--color-border);
}
.feed-blogroll-avatar-initials {
  width: 16px;
  height: 16px;
  border-radius: 50%;
  background: var(--color-surface-alt);
  color: var(--color-secondary);
  display: inline-flex;
  align-items: center;
  justify-content: center;
  font-size: 0.5rem;
  font-weight: 600;
  border: 1.5px solid var(--color-border);
}
.feed-blogroll-badges {
  margin-left: auto;
  display: flex;
  gap: 0.2rem;
}
.feed-blogroll-badges .feed-type-badge {
  font-size: 0.6rem;
  text-decoration: none !important;
}
/* People modal — compact variant of links-modal */
#people-modal-overlay .links-modal {
  max-width: 22rem;
}
#people-modal-overlay .links-modal-body {
  padding: 0.3rem 0.5rem;
}
#people-modal-overlay .feed-blogroll-row {
  padding: 0.2rem 0;
  gap: 0.3rem;
}
#people-modal-overlay .feed-blogroll-badges {
  margin-left: 0;
}
#people-modal-overlay .sidebar-meta-val {
  flex: none;
}
/* Network page */
.network-timeline {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}
.network-month {
  margin-bottom: 1.5rem;
}
.network-month-header {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  border-bottom: 1px solid var(--color-border);
  padding-bottom: 0.35rem;
  margin-bottom: 0.35rem;
}
.network-month-title {
  font-size: 0.88rem;
  font-weight: 600;
  margin: 0 !important;
  line-height: 1.3;
  white-space: nowrap;
}
.network-month-people {
  display: flex;
  align-items: center;
  flex-wrap: wrap;
  padding-left: 5px;
  row-gap: 2px;
}
.network-avatar-wrap {
  margin-left: -5px;
  position: relative;
  z-index: 1;
  transition: z-index 0s, transform 0.15s;
  text-decoration: none !important;
}
.network-avatar-wrap:hover {
  z-index: 10;
  transform: scale(1.15);
}
.network-avatar {
  width: 20px;
  height: 20px;
  border-radius: 50%;
  object-fit: cover;
  border: 1.5px solid var(--color-border);
  box-shadow: 0 0 0 1px var(--color-surface-alt);
  filter: sepia(0.6) saturate(0.7);
}
.network-avatar-wrap:hover .network-avatar {
  filter: none;
}
.network-avatar-initials {
  width: 20px;
  height: 20px;
  border-radius: 50%;
  background: var(--color-surface-alt);
  color: var(--color-secondary);
  display: inline-flex;
  align-items: center;
  justify-content: center;
  font-size: 0.5rem;
  font-weight: 600;
  border: 1.5px solid var(--color-border);
}
.network-month-body {
  display: flex;
  flex-direction: column;
  gap: 0.1rem;
}
.network-feed-item {
  border-radius: 3px;
  transition: background 0.1s;
  overflow: hidden;
}
.network-feed-item:hover {
  background: var(--color-surface);
}
.network-feed-avatar {
  float: left;
  width: 32px;
  height: 32px;
  border-radius: 50%;
  object-fit: cover;
  border: 1px solid var(--color-border);
  margin-right: 0.4rem;
  margin-top: 0.1rem;
}
@media (max-width: 768px) {
  .network-feed-avatar {
    width: 1.2em;
    height: 1.2em;
    margin-top: 0.15em;
    margin-right: 0.25rem;
  }
}
.network-feed-headline {
  font-size: 0.85rem;
  line-height: 1.4;
}
.network-feed-name {
  font-size: 0.78rem;
  color: var(--color-secondary);
  text-decoration: none !important;
  white-space: nowrap;
}
.network-feed-name:hover {
  color: var(--color-link) !important;
}
.network-feed-summary {
  font-size: 0.78rem;
  color: var(--color-secondary);
}
@media (min-width: 769px) {
  .network-feed-headline {
    display: -webkit-box;
    -webkit-box-orient: vertical;
    -webkit-line-clamp: 2;
    overflow: hidden;
  }
}
.network-feed-item .feed-item-mentions {
  clear: left;
}
@media (min-width: 769px) {
  .network-feed-item .feed-item-mentions {
    padding-left: 2.4rem;
  }
}
.network-blogroll-avatar {
  width: 24px;
  height: 24px;
  border-radius: 50%;
  object-fit: cover;
  border: 1.5px solid var(--color-border);
  box-shadow: 0 0 0 0.5px var(--color-surface-alt);
}
.network-blogroll-initials {
  width: 24px;
  height: 24px;
  border-radius: 50%;
  background: var(--color-surface-alt);
  color: var(--color-secondary);
  display: inline-flex;
  align-items: center;
  justify-content: center;
  font-size: 0.55rem;
  font-weight: 600;
  border: 1.5px solid var(--color-border);
}
/* Mobile listing adjustments */
@media (max-width: 768px) {
  /* Hide type icons from listing items */
  .paper-cls-icon {
    display: none;
  }
  /* Tighten heading number divider */
  .heading-number::after {
    content: "\2009\007C\2009";
  }
}
|}
