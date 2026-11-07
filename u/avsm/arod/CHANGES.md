# Changes

- Serve the site stylesheet as one cached file instead of inlining it in
  every page, and build it through Tailwind so its rules sit between the
  preflight reset and the utility classes. Borders on sidebar boxes, cards,
  chips, pills and code blocks that the reset had been dropping are drawn
  again, paper action pills follow the site theme toggle rather than the
  system scheme, and smooth scrolling honours reduced-motion preferences.

- Ship the highlight.js light and dark themes inside the site stylesheet
  instead of fetching two stylesheets from a CDN on every page.

- Give sidenotes and their links readable contrast, and replace hard-coded
  grey utilities with theme colours so the footer and menu rules match dark
  mode.

- Show the OCaml compiler version in the live dashboard headline and on
  the analytics page.

- Draw a live dashboard under `arod serve` when standard error is a
  terminal: request rates, latencies, status classes, cache outcomes, top and
  slowest paths, agents, visitors, process load and the last day from the
  access log. It redraws only when requests arrive, settling for a minute
  afterwards, so an idle server draws nothing. `--theme` picks its colours
  and `--no-dashboard` disables it.

- Allow pull to replace an empty Sortal data directory with its checkout link.
  Explain how to reuse an existing checkout when the paths differ.

- Keep shared embedding settings, key paths and MLX model directories portable
  in setup exports.

- Cache immutable link and network pagination indexes and reduce temporary
  search-ranking allocations.

- Reduce allocations in query parsing and backlink lookups.

- Simplify pull readiness reporting and require regular files when checking
  whether paper PDFs are present.

- Add `arod pull` to scan and export public setup, restore website repositories
  and assets, and prompt for service authentication on a new machine.
