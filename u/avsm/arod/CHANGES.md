# Changes

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
