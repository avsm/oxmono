(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Design tokens and theme constants for the Arod site.

    CSS custom properties for light/dark mode, typography, and spacing. *)

(** {1 Tailwind CDN Config}

    Injected as an inline script after the Tailwind CDN <script> tag. *)

let tailwind_config = {|
    tailwind.config = {
      darkMode: 'class',
      theme: {
        extend: {
          fontFamily: {
            sans: ['system-ui', '-apple-system', 'sans-serif'],
            serif: ['Georgia', 'serif'],
          },
          colors: {
            bg: 'var(--color-bg)',
            text: 'var(--color-text)',
            link: 'var(--color-link)',
            'link-underline': 'var(--color-link-ul)',
            secondary: 'var(--color-secondary)',
            surface: 'var(--color-surface)',
            'surface-alt': 'var(--color-surface-alt)',
            muted: 'var(--color-muted)',
            faint: 'var(--color-faint)',
            dim: 'var(--color-dim)',
            accent: 'var(--color-accent)',
            'border-color': 'var(--color-border)',
            'st-avail': 'var(--color-st-avail)',
            'st-discuss': 'var(--color-st-discuss)',
            'st-ongoing': 'var(--color-st-ongoing)',
            'st-done': 'var(--color-st-done)',
            'st-expired': 'var(--color-st-expired)',
          },
          fontSize: {
            'body': ['0.88rem', '1.45'],
          },
          typography: {
            DEFAULT: {
              css: {
                '--tw-prose-body': 'var(--color-text)',
                '--tw-prose-headings': 'var(--color-text)',
                '--tw-prose-links': 'var(--color-link)',
                '--tw-prose-bold': 'var(--color-text)',
                '--tw-prose-counters': 'var(--color-secondary)',
                '--tw-prose-bullets': 'var(--color-secondary)',
                '--tw-prose-quotes': 'var(--color-bq-text)',
                '--tw-prose-quote-borders': 'var(--color-accent)',
                '--tw-prose-code': 'var(--color-text)',
                '--tw-prose-pre-bg': 'var(--color-surface)',
                '--tw-prose-pre-code': 'var(--color-text)',
                fontSize: '0.88rem',
                lineHeight: '1.45',
                maxWidth: 'none',
                p: { marginTop: '0', marginBottom: '1.1em' },
                '[class~="lead"]': { fontSize: '1rem' },
                ol: { marginTop: '0.5em', marginBottom: '1.1em', paddingLeft: '1.5em' },
                ul: { marginTop: '0.5em', marginBottom: '1.1em', paddingLeft: '1.5em' },
                li: { marginTop: '0.2em', marginBottom: '0.2em' },
                'ol > li::marker': { color: 'var(--color-secondary)' },
                'ul > li::marker': { color: 'var(--color-secondary)' },
                a: { color: 'var(--color-link)', fontWeight: 'inherit', textDecoration: 'underline dotted', textDecorationColor: 'var(--color-link-ul)', textUnderlineOffset: '2px' },
                'a:hover': { textDecorationStyle: 'solid', textDecorationColor: 'var(--color-link)' },
                blockquote: { fontStyle: 'italic', color: 'var(--color-bq-text)', borderLeftColor: 'var(--color-accent)' },
                'blockquote p:first-of-type::before': { content: 'none' },
                'blockquote p:last-of-type::after': { content: 'none' },
                h1: { fontSize: '1.25rem', marginTop: '1rem', marginBottom: '0.75rem' },
                h2: { fontSize: '1.125rem', marginTop: '1.25rem', marginBottom: '0.5rem' },
                h3: { fontSize: '1rem', marginTop: '1rem', marginBottom: '0.375rem' },
                h4: { fontSize: '0.88rem', marginTop: '0.75rem', marginBottom: '0.25rem' },
                code: { fontSize: '0.78rem', fontWeight: '400' },
                'code::before': { content: 'none' },
                'code::after': { content: 'none' },
                pre: { fontSize: '0.72rem', lineHeight: '1.5', backgroundColor: 'var(--color-surface)', border: '1px solid var(--color-border)', borderRadius: '4px', padding: '0.5rem 0.75rem', marginTop: '0.5em', marginBottom: '0.5em' },
                img: { marginTop: '0', marginBottom: '0' },
                hr: { borderColor: 'var(--color-border)' },
              },
            },
          },
        }
      }
    }
|}

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
  --color-st-done: #6b7280;
  --color-st-expired: #ef4444;
  --color-sidenote-ref: #5b6abf;
  --color-highlight: #fde68a;
  --color-toc-bg: #e0e7ff;
  --color-bq-text: #4a4a4a;
  --color-perma-accent: #c8d4e8;
  --color-quickpost-accent: #e0e0e0;
  --color-weeknote-accent: #dde8d8;
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
  --color-st-done: #8b949e;
  --color-st-expired: #f85149;
  --color-sidenote-ref: #7dd3a0;
  --color-highlight: #634d15;
  --color-toc-bg: #1c2654;
  --color-bq-text: #b1bac4;
  --color-perma-accent: #2d3a52;
  --color-quickpost-accent: #2a2e33;
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
  .scrollbar-hide {
    -ms-overflow-style: none;
    scrollbar-width: none;
  }
  .scrollbar-hide::-webkit-scrollbar {
    display: none;
  }
  .sidenote-ref {
    cursor: help;
  }
  .sidenote-number {
    color: var(--color-sidenote-ref);
  }
  /* Sidenote toggle: mobile = clickable circle, desktop = discrete superscript */
  .sidenote-toggle {
    display: inline-block;
    margin-left: 1px;
    font-variant-numeric: tabular-nums;
    transition: color 0.2s;
  }
  /* Mobile: circled number badge */
  @media (max-width: 1023px) {
    .sidenote-toggle {
      width: 1rem;
      height: 1rem;
      font-size: 0.55rem;
      line-height: 1rem;
      text-align: center;
      background: var(--color-surface-alt);
      border: 1px solid var(--color-border);
      border-radius: 9999px;
      color: var(--color-muted);
      font-weight: 500;
      cursor: pointer;
      vertical-align: baseline;
      position: relative;
      top: -0.35em;
    }
    .sidenote-toggle:hover {
      background: var(--color-surface);
      border-color: var(--color-faint);
    }
    .dark .sidenote-toggle {
      background: color-mix(in srgb, var(--color-sidenote-ref) 15%, transparent);
      border-color: transparent;
      color: color-mix(in srgb, var(--color-sidenote-ref) 70%, transparent);
    }
    .dark .sidenote-toggle:hover {
      background: color-mix(in srgb, var(--color-sidenote-ref) 25%, transparent);
      color: var(--color-sidenote-ref);
    }
  }
  /* Desktop: plain superscript number */
  @media (min-width: 1024px) {
    .sidenote-toggle {
      font-size: 0.6em;
      vertical-align: super;
      color: var(--color-sidenote-ref);
      opacity: 0.75;
      pointer-events: none;
      font-weight: 500;
    }
  }
  .sidenote-anchor {
    position: relative;
  }
  /* Hide sidenotes until JS positions them, then fade in */
  .sidenote-hidden {
    opacity: 0;
  }
  .sidenote-visible {
    opacity: 1;
    transition: opacity 0.3s ease-in;
  }
  .toc-link {
    background: linear-gradient(to right, var(--color-toc-bg) 0%, var(--color-toc-bg) var(--progress, 0%), transparent var(--progress, 0%), transparent 100%);
    transition: all 0.15s ease;
  }
  .toc-link.passed {
    background: var(--color-toc-bg);
    color: var(--color-link);
  }
  .toc-link.active {
    color: var(--color-link);
    font-weight: 500;
  }
  .text-body { font-size: 0.88rem; line-height: 1.45; }
  /* idea status colours are now Tailwind utilities (font-medium text-st-*) */
  /* Idea list page — project cards */
  .idea-project-section {
    border-bottom: 1px solid var(--color-border);
    padding-bottom: 1rem;
  }
  .idea-project-section:last-child { border-bottom: none; }
  .idea-project-brief {
    font-size: 0.75rem;
    color: var(--color-secondary);
    line-height: 1.4;
    margin: 0.4rem 0 0.5rem;
  }
  .idea-project-brief p { margin: 0 0 0.3em; }
  .idea-project-brief a { color: var(--color-link); }
  .idea-project-brief ul, .idea-project-brief ol {
    list-style-position: outside;
    padding-left: 1.2em;
    margin: 0.2em 0;
  }
  .idea-project-brief li { margin: 0; }
  .idea-project-section > .proj-card-header {
    font-family: ui-monospace, 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
    background: var(--color-surface);
    border-radius: 4px;
  }
  .idea-project-thumb {
    float: right;
    margin: 0 0 0.3rem 0.5rem;
  }
  .idea-project-thumb img {
    width: 36px;
    height: 36px;
    object-fit: cover;
    border-radius: 3px;
  }
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
  /* Idea sidebar — filter rows with inline stats */
  .idea-filter-row {
    display: flex;
    align-items: center;
    gap: 0.3rem;
    padding: 0.05rem 0;
    cursor: pointer;
  }
  .idea-filter-row input[type="checkbox"] {
    flex-shrink: 0;
  }
  .idea-filter-row:has(input:not(:checked)) {
    opacity: 0.4;
  }
  .idea-filter-label {
    color: var(--color-dim);
    flex: 1;
  }
  .idea-stat-count {
    color: var(--color-secondary);
    font-variant-numeric: tabular-nums;
    font-size: 0.68rem;
  }
  /* Idea sidebar — project jump list */
  .idea-jump-list {
    display: flex;
    flex-direction: column;
  }
  .idea-jump-link {
    display: flex;
    align-items: center;
    padding: 0.15rem 0;
    color: var(--color-dim) !important;
    text-decoration: none !important;
    transition: color 0.1s;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 0.72rem;
  }
  .idea-jump-link:hover {
    color: var(--color-link) !important;
  }
  .idea-jump-title {
    flex: 1;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .idea-jump-count {
    color: var(--color-secondary);
    font-variant-numeric: tabular-nums;
    margin-left: 0.3rem;
    flex-shrink: 0;
  }
  .idea-proj-bar {
    flex-direction: row;
    height: 0.45rem;
    border-radius: 2px;
    flex-shrink: 0;
    margin-left: 0.35rem;
    max-width: 4rem;
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
  .search-modal-overlay {
    position: fixed;
    inset: 0;
    background: rgba(0,0,0,0.5);
    z-index: 60;
    display: none;
  }
  .search-modal-overlay.active { display: flex; }
  .search-modal {
    box-shadow: 0 16px 48px rgba(0,0,0,0.2);
  }
  .dark .search-modal {
    box-shadow: 0 16px 48px rgba(0,0,0,0.5);
  }
  /* Search filter pills — see unlayered rules below @layer for .active state */
  /* Search results area */
  .search-results-area {
    max-height: min(24rem, 60vh);
  }
  .search-empty-state {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    padding: 2rem 1rem;
    gap: 0.25rem;
  }
  .search-no-results {
    padding: 2rem 1rem;
    text-align: center;
    font-size: 0.85rem;
    color: var(--color-secondary);
  }
  /* Individual search result */
  .search-result {
    padding: 0.4rem 1rem;
    cursor: pointer;
    transition: background 0.08s;
    border-left: 2px solid transparent;
  }
  .search-result.selected {
    background: var(--color-surface);
    border-left-color: var(--color-accent);
  }
  .search-result:hover {
    background: var(--color-surface);
  }
  .search-result .sr-row {
    display: flex;
    gap: 0.5rem;
    align-items: flex-start;
  }
  .search-result .sr-thumb {
    width: 2.5rem;
    height: 2.5rem;
    border-radius: 4px;
    overflow: hidden;
    flex-shrink: 0;
    border: 1px solid var(--color-border);
    margin-top: 0.1rem;
  }
  .search-result .sr-thumb img {
    width: 100%;
    height: 100%;
    object-fit: cover;
    display: block;
  }
  .search-result .sr-body {
    flex: 1;
    min-width: 0;
  }
  .search-result .sr-main {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    text-decoration: none !important;
    color: var(--color-text) !important;
    min-width: 0;
  }
  .search-result .sr-icon-wrap {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 1.35rem;
    height: 1.35rem;
    border-radius: 3px;
    flex-shrink: 0;
    color: var(--color-secondary);
  }
  .sr-icon-paper { color: #3b82f6; }
  .sr-icon-note { color: #10b981; }
  .sr-icon-project { color: #8b5cf6; }
  .sr-icon-idea { color: #f59e0b; }
  .sr-icon-video { color: #ef4444; }
  .sr-icon-link { color: var(--color-secondary); }
  .search-result .sr-title {
    font-weight: 500;
    font-size: 0.85rem;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    flex: 1;
  }
  .search-result .sr-date {
    font-size: 0.72rem;
    color: var(--color-muted);
    font-variant-numeric: tabular-nums;
    flex-shrink: 0;
    margin-left: auto;
    font-family: ui-monospace, 'SF Mono', monospace;
  }
  .search-result .sr-snippet {
    font-size: 0.78rem;
    color: var(--color-secondary);
    line-height: 1.4;
    overflow: hidden;
    text-overflow: ellipsis;
    display: -webkit-box;
    -webkit-line-clamp: 1;
    -webkit-box-orient: vertical;
    padding-left: 1.75rem;
    margin-top: 0.1rem;
  }
  .search-result .sr-snippet b {
    color: var(--color-text);
    font-weight: 600;
  }
  /* Search result tags */
  .search-result .sr-tags {
    display: flex;
    flex-wrap: wrap;
    gap: 0.25rem;
    padding-left: 1.75rem;
    margin-top: 0.15rem;
  }
  .search-result .sr-tag {
    font-size: 0.65rem;
    color: var(--color-accent);
    cursor: pointer;
    text-decoration: none;
  }
  .search-result .sr-tag:hover {
    text-decoration: underline;
  }
  /* Link URL + parent entries row */
  .search-result .sr-links {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 0.25rem;
    padding-left: 1.75rem;
    margin-top: 0.2rem;
  }
  .search-result .sr-url {
    display: inline-flex;
    align-items: center;
    gap: 0.2rem;
    font-size: 0.72rem;
    color: var(--color-link) !important;
    text-decoration: none !important;
    padding: 0.05rem 0.35rem;
    border: 1px solid var(--color-link);
    border-radius: 3px;
    transition: background 0.1s;
    opacity: 0.8;
  }
  .search-result .sr-url:hover {
    background: var(--color-surface);
    opacity: 1;
  }
  .search-result .sr-parent {
    display: inline-flex;
    align-items: center;
    gap: 0.2rem;
    font-size: 0.72rem;
    color: var(--color-muted) !important;
    text-decoration: none !important;
    padding: 0.05rem 0.3rem;
    border: 1px solid var(--color-border);
    border-radius: 3px;
    transition: color 0.1s, border-color 0.1s;
    max-width: 16rem;
  }
  .search-result .sr-parent span {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .search-result .sr-parent:hover {
    color: var(--color-link) !important;
    border-color: var(--color-link);
  }
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
  /* Weeknote cards — .plan entries */
  .note-weeknote .note-compact-title {
    font-weight: 500 !important;
    font-size: 0.85rem !important;
  }
  .note-weeknote .note-compact-meta {
    font-size: 0.72rem;
  }
  .note-weeknote:hover .note-compact-title {
    color: var(--color-link) !important;
  }
  /* Quick post cards — between perma and weeknote size */
  .note-compact:not(.note-perma):not(.note-weeknote) .note-compact-title {
    font-size: 0.85rem !important;
  }
  /* Perma article cards — subtle accent background */
  /* Note type icon — positioned in left gutter with radial glow */
  /* Note type icon — vertically centered with the heading row */
  .note-type-icon {
    position: absolute;
    left: 0.35rem;
    top: 0.55rem;
    display: inline-flex;
    align-items: center;
  }
  .note-weeknote .note-type-icon {
    background: radial-gradient(circle, var(--color-weeknote-accent) 30%, transparent 70%);
    padding: 0.3rem;
    margin: -0.3rem;
  }
  .note-perma .note-type-icon {
    background: radial-gradient(circle, var(--color-perma-accent) 30%, transparent 70%);
    padding: 0.3rem;
    margin: -0.3rem;
  }
  .note-compact:not(.note-perma):not(.note-weeknote) .note-type-icon {
    background: radial-gradient(circle, var(--color-quickpost-accent) 30%, transparent 70%);
    padding: 0.3rem;
    margin: -0.3rem;
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
  /* Synopsis for weeknotes and quick posts: smaller than heading */
  .note-weeknote .note-compact-synopsis {
    font-size: 0.62rem;
  }
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
  /* Idea heatmap — stacked status bar instead of circle */
  .idea-status-bar {
    display: flex;
    width: 1.15rem;
    height: 1.15rem; /* default, overridden by JS */
    align-self: flex-end;
    border-radius: 3px;
    overflow: hidden;
    gap: 1px;
    transition: transform 0.1s;
  }
  .heatmap-cell:hover .idea-status-bar {
    transform: scale(1.15);
  }
  .idea-status-bar > span {
    min-width: 1px;
  }
  .bar-available { background: var(--color-st-avail); }
  .bar-discussion { background: var(--color-st-discuss); }
  .bar-ongoing { background: var(--color-st-ongoing); }
  .bar-completed { background: var(--color-st-done); }
  .bar-expired { background: var(--color-st-expired); }
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

/* Search filter pills — unlayered so they win over Tailwind resets */
.search-filter-pill {
  padding: 0.15rem 0.45rem;
  border-radius: 9999px;
  border: 1px solid var(--color-border);
  color: var(--color-secondary);
  background: none;
  cursor: pointer;
  font-size: 0.75rem;
  white-space: nowrap;
}
.search-filter-pill:hover {
  border-color: var(--color-faint);
  color: var(--color-text);
  background: var(--color-surface);
}
.search-filter-pill.active {
  border-color: var(--color-accent);
  color: #fff;
  background: var(--color-accent);
  font-weight: 600;
}
.dark .search-filter-pill.active {
  color: #fff;
  background: var(--color-accent);
}

/* These need higher specificity than layered rules */
#nav-notes.emphasized {
  color: var(--color-link);
}
#toc-row.visible {
  opacity: 1;
  max-height: 2rem;
  overflow-x: auto;
  overflow-y: hidden;
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
  .note-type-icon,
  .paper-cls-icon {
    display: none;
  }
  /* Tighten heading number divider */
  .heading-number::after {
    content: "\2009\007C\2009";
  }
}
|}
