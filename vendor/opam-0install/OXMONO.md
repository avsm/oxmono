# OxMono import

Imported opam-0install.0.6.0 from the SHA256-verified archive recorded
in `OXMONO.json`. Retained implementation sources are unchanged.

Only the solver functor, model and signatures are included. Directory and
switch contexts and the CLI are omitted. The library depends on opam-format
instead of opam-state. The Dune stanza and package dependency list are
restricted to those retained modules. Ox supplies oi's repository context.

On refresh, compare all retained implementation files with the verified release,
preserve this scope, then force `dune runtest avsm/ox` and build
`@avsm/ox/all` with the OxCaml `release-check` profile.

The retained sources compile with OxCaml without mode annotations or behavior
patches. Solver state is mutable. Ox uses it within one Eio domain and does not
claim that solver contexts or callbacks are portable across domains.
