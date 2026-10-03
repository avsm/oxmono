# OxMono import

Imported opam-core.2.5.2 at `ad0564f20c6306bc83085fe663cab903fe887208`.
`OXMONO.json` records the release archive URL and checksums. Every retained
source was compared against the checksum-verified release archive on
2026-10-03. Sources match except for the adaptations below.

## Scope and adaptations

Core and format libraries and shell build support only. No opam CLI, state, repository or solver libraries. OxCaml eta expansions in opamConsole.ml, opamProcess.ml and get_version.ml. See OXMONO.md.

## Refresh

1. Fetch the recorded release and the proposed replacement. Compare their
   library sources and dependency declarations.
2. Preserve the documented build scope and compiler adaptations. Verify the
   downloaded archive checksum and record its commit in `OXMONO.json` and
   `../upstreams.json`.
3. Build `@bleeding/oi-libs/all` with `release-check` and run
   `dune runtest --profile release-check --force bleeding/oi-libs`.
   Use the `5.2.0+ox` switch. The HTTP regressions need loopback sockets.
4. Run the explicit vendor test aliases listed in
   `../../bleeding/oi-libs/OXMONO.md`. Recursive vendor aliases skip tests.

## OxCaml patches

- `shell/get_version.ml`: eta-expand `print_endline` for `Scanf.sscanf`.
- `src/core/opamConsole.ml`: eta-expand the Unix printing branches.
- `src/core/opamProcess.ml`: eta-expand `Unix.create_process_env` and
  `Buffer.add_string` to preserve the upstream global callback signatures.
- `dune-project`: declare only opam-core and opam-format at version 2.5.2.
- `src/dune`: remove the absent crowbar subtree declaration.
- `src/core/dune`: omit the Windows opam CLI helper install stanza.

The compiler diagnosed each eta expansion. No opam algorithm was changed.
`bleeding/oi-libs/test/compat/opam_probe.ml` compares canonical effective
opam output, MD5/SHA256/SHA512 hashes and child environment behavior against
opam-format 2.5.2 built with stock OCaml. `opam.expected` is that baseline.
The fixture does not prove equivalence of every opam API or Windows behavior.
