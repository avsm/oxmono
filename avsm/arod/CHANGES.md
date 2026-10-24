# Changes

- Keep shared embedding settings, key paths and MLX model directories portable
  in setup exports.

- Cache immutable link and network pagination indexes and reduce temporary
  search-ranking allocations.

- Reduce allocations in query parsing and backlink lookups.

- Simplify pull readiness reporting and require regular files when checking
  whether paper PDFs are present.

- Add `arod pull` to scan and export public setup, restore website repositories
  and assets, and prompt for service authentication on a new machine.
