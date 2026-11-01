# OxMono import

Imported 0install-solver.2.18 from the SHA256-verified archive recorded
in `OXMONO.json`. Retained implementation sources are unchanged.

Only the generic solver library is included. The 0install application, UI
and OUnit tests are omitted. Package metadata removes the unused test
dependency. Consumer tests exercise the solver with opam dependency graphs.

On refresh, compare all retained implementation files with the verified release,
preserve this scope, then force `dune runtest avsm/ox` and build
`@avsm/ox/all` with the OxCaml `release-check` profile.

The retained sources compile with OxCaml without mode annotations or behavior
patches. Solver state is mutable. Ox uses it within one Eio domain and does not
claim that solver contexts or callbacks are portable across domains.
