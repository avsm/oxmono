# dune-rpc-eio: the dune RPC client as its own library

Extract okit's dune RPC client into a library and opam package of its own,
named `dune-rpc-eio`, so that any Eio program can drive a dune server. okit
keeps its tools and builds them on the new library.

## What moves

Four modules leave `okit/` for a new top-level directory `dune_rpc_eio/`:

- `csexp.ml(i)`, canonical s-expressions over `Eio.Buf_read`.
- `pp_text.ml(i)`, rendering dune's serialised `Pp.t` message trees.
- `wire.ml(i)`, the RPC wire format: packets, requests, decoders.
- `dune_rpc.ml(i)`, the session: spawn or attach, request, relaunch, stop.
  It is renamed `session.ml(i)`, so the module is `Session`.

Everything else in `okit/` stays: `proto`, `server`, `client`, `toolbox`,
`status`, `report`, `merlin`, `project`. `project.ml` goes on using `Csexp`
for `dune describe --format csexp` output, through the new library.

## Naming

The library is wrapped as `Dune_rpc_eio`, giving `Dune_rpc_eio.Csexp`,
`Dune_rpc_eio.Pp_text`, `Dune_rpc_eio.Wire` and `Dune_rpc_eio.Session`. A
top-level module named `Dune_rpc` is not an option, because the upstream
`dune-rpc` package already exports one and a program linking both would
collide. `Session` also removes the stutter `Dune_rpc_eio.Dune_rpc` would
have. The session's main type stays `Session.t` with `start`, `build`,
`runtest`, `promote`, `stop` and `trace`, and the record type `Session.build`
is unchanged.

## The library must not say "okit"

The moved code names okit in two ways, and both go:

- `Wire.initialize` sends the client id `okit`. It becomes
  `Wire.initialize ~client`, and `Session.start` gains `?client`, defaulting
  to `dune-rpc-eio`, which it passes down. The `Session.start` call in
  `okit/server.ml` passes `~client:"okit"`.
- Error messages such as "the dune server okit started" and "okit cannot
  start a server" speak for the library now. Rewrite them around "this
  session" or "the session", keeping their content: which server, which
  workspace, what to run by hand. The target-refusal wording from the alias
  change does not name okit and is kept word for word.

Log output moves onto a source of the library's own,
`Logs.Src.create "dune-rpc-eio"`, used where `notify/log` messages go to
debug today.

## Packaging

`dune_rpc_eio/dune`:

    (library
     (name dune_rpc_eio)
     (public_name dune-rpc-eio)
     (libraries eio eio.unix logs unix))

A new package stanza in `dune-project`:

    (package
     (name dune-rpc-eio)
     (synopsis "Client for dune's RPC server using Eio")
     (description
      "Speaks dune's RPC protocol over its build socket: spawn or attach to a \
       dune server, build paths and aliases, run tests, read diagnostics and \
       promote files, from any Eio program.")
     (depends (ocaml (>= 5.2.0)) (eio (>= 1.4)) (logs (>= 0.7.0))))

No `available:` restriction, since the library is portable. The `humpty`
package adds `(dune-rpc-eio (= :version))` to its depends, and `okit/dune`
adds `dune-rpc-eio` to its libraries while dropping nothing else it uses.

## okit after the move

okit modules that used the moved code take one alias line each at the top of
the `.ml`, in the existing style:

    module Csexp = Dune_rpc_eio.Csexp
    module Wire = Dune_rpc_eio.Wire
    module Dune_rpc = Dune_rpc_eio.Session

`server.ml` keeps reading as it does today through the `Dune_rpc` alias.
`.mli` files write the full path instead, so `report.mli` says
`Dune_rpc_eio.Wire.Diagnostic.t` and `Dune_rpc_eio.Session.build`. The
toolbox and status wording is untouched.

## Tests

- `test/okit_wire.ml` becomes `test/rpc_wire.ml`, a pure unit test of the
  library alone, with `(libraries dune-rpc-eio eio)` and aliases pointing at
  `Dune_rpc_eio`. Its cases are unchanged apart from the initialize packet,
  which now pins the bytes for a chosen `~client`.
- `test/okit_dune.ml` keeps its name, since it still tests okit's project
  map, and its stanza gains `dune-rpc-eio`. References move to
  `Dune_rpc_eio.Session`. Cases that pin error wording follow the
  de-okitified messages.
- `okit_tools`, `okit_server`, `okit_client`, `okit_proto`, `okit_merlin`
  are unchanged beyond whatever the aliases already absorb.

## Documentation

- ARCH.md: the okit file map loses the four moved files and a short new
  section names the `dune_rpc_eio/` package, what it holds and that okit
  builds on it. The paragraphs added in the last two changes move their file
  paths from `okit/` to `dune_rpc_eio/`. The sentence in `csexp.mli` naming
  `Dune_rpc` now names `Session`.
- The comment in `session.ml` naming `test/okit_wire.ml` as the pin of the
  mid-value message now names `test/rpc_wire.ml`.
- CHANGES.md: one entry, the dune RPC client is now the `dune-rpc-eio`
  package and library, usable outside okit.
- The wire-format reference stays
  `docs/superpowers/plans/2026-08-02-dune-rpc-protocol.md`.

## Approaches set aside

A separate repository was set aside for now, since okit depends on the
library and one tree keeps the tests honest. Publishing later is a matter of
moving the directory. An unwrapped library was set aside for the module
collision above.

## Verification

`dune build`, `dune runtest` and `dune build @fmt` all clean. `dune build
@doc` builds, since the library is now public API. The live tests in
`test/okit_dune.ml` and `test/okit_tools.ml` prove the session still spawns,
builds and reports through the new layering.
