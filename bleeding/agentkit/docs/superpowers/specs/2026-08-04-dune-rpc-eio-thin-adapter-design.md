# dune-rpc-eio as a thin adapter over dune-rpc

`dune_rpc_eio/` reimplements the `dune-rpc` opam package: canonical
s-expressions, dune's `Pp.t` message trees, and every request and response
codec, in 1755 lines. Upstream's `dune-rpc-lwt` does none of that. It is 141
lines, because it supplies an Lwt `Fiber` and an `Lwt_io` `Chan` and applies
`Client.Make` and `Where.Make`.

Make `dune-rpc-eio` the same kind of library for Eio, and move the policy that
is okit's rather than upstream's back into okit.

## What the library becomes

One module, `dune_rpc_eio.ml(i)`, of about 160 lines. `csexp.ml(i)`,
`pp_text.ml(i)`, `wire.ml(i)` and `session.ml(i)` are deleted.

- `Fiber`, the functor argument. Eio is direct style, so `'a t = 'a`.
  `Ivar` is `Eio.Promise`, `fork_and_join_unit` is `Eio.Fiber.pair`,
  `parallel_iter` forks under a switch of its own, `finalize` is `Fun.protect`,
  and `O.( let* )` and `O.( let+ )` apply their argument. `collect_errors`
  re-raises `Eio.Cancel.Cancelled`, since a fiber ending is not a fault to
  report.
- `Chan`, a socket with an `Eio.Buf_read.t`. `read` drives the `csexp`
  package's `Parser.Lexer` over the buffer and is `None` at end of input,
  `write` uses `Eio.Buf_write`, and `close` closes the socket.
- `Client = Dune_rpc.Private.Client.Make (Fiber) (Chan)`.
- `Where = Dune_rpc.V1.Where.Make (Fiber) (…)`, over `Eio.Path`, which finds
  the socket from the build directory and the environment.
- `connect ~sw ~net : Dune_rpc.V1.Where.t -> Chan.t`.

`Client` is exported at `Dune_rpc.Private.Client.S` and not at
`Dune_rpc.V1.Client.S`. `V1` makes `Request.t` abstract, so a client held to
that signature can send only the requests `V1` declares, and `build` is not one
of them. Dune declares `build` in `src/dune_rpc_impl/decl.ml` and its own
`dune rpc build` reaches it the same way. `Private.Client.S` is a superset of
the `V1` one, so nothing is lost.

The library's dependencies become `dune-rpc`, `csexp` and `eio`. It no longer
needs `logs`, which only the session used. It keeps `unix`, because `Where.Make`
takes a `read_file` and an `analyze_path` over plain paths with no capability to
thread through, which is how the Lwt adapter does it too.

## What moves to okit

`okit/session.ml(i)`, about 330 lines against today's 780. It keeps what
upstream has no opinion on:

- Spawning `dune build --passive-watch-mode`, or attaching to a server that is
  already there, and telling a stale socket from a live one.
- The 30 second window for a socket, the 2 second grace once the spawned child
  has exited, and the tail of the child's output that names a refusal.
- Relaunching a server this session owns, and not one it merely attached to.
- Target validation: `@check` and `@@check` in dune's command-line spelling,
  turned into dep-specs, with a hand-written dep-spec refused.
- The trace callback and the mutex that serialises calls.

It loses what upstream does: the initialize and version-menu handshake, packet
encoding and decoding, request id allocation, `notify/log` and `notify/abort`
handling, and `required_menu`.

`Client.connect_with_menu` is scoped, taking `~f:(t -> 'a)`, and `Session.start`
is not. `start` forks a fiber on the session's switch which runs it, resolves a
promise carrying the client, and parks on a stop promise. A response whose
`Response.Error.kind` is `Connection_dead` replaces the hand-rolled detection of
a server that went away.

The custom `build` declaration must go through `connect_with_menu
~private_menu`, not plain `connect`. Version negotiation only offers the methods
in the menu, so a `build` declared but not offered is dropped by the server and
`prepare_request` then fails.

`build` is declared once, in okit, mirroring dune:

    let build =
      let open Dune_rpc.Private in
      Decl.Request.make
        ~method_:(Method.Name.of_string "build")
        ~generations:
          [ Decl.Request.make_current_gen ~req:(Conv.list Conv.string)
              ~resp:Build_outcome_with_diagnostics.sexp_v2 ~version:2 ]

`Session.t`, `Session.build` and the calls `start`, `build`, `runtest`,
`promote`, `stop` and `trace` keep their names and signatures. The element type
of `build.diagnostics` becomes `Dune_rpc.Private.Diagnostic.t`. `okit/server.ml`
changes only its alias line.

## Other okit modules

- `report.ml` takes `Dune_rpc.Private.Diagnostic.t`. `to_text` moves here, about
  25 lines, and renders `Diagnostic.message`, a `unit Pp.t`, through `Pp.to_fmt`.
  That is what `pp_text.ml` walked by hand. `about` reads the location through
  `Loc.start`, whose `Lexing.position` carries the file name. No `stdune`
  dependency is needed: `Dune_rpc.Private.Loc.t` is a concrete record of two
  `Lexing.position` values, and `Diagnostic.t` is a concrete record too.
- `project.ml` uses the `csexp` package for `dune describe --format csexp`.
  Its `atom`, `to_list` and `field` helpers are about 12 lines and become local
  to the file, since nothing else needs them. `Csexp.parse_string` replaces
  `Csexp.of_string`.

`okit/dune` gains `csexp`, `dune-rpc` and `pp`, and keeps `dune-rpc-eio`. It
does not gain `stdune`.

## Packaging

The `dune-rpc-eio` stanza in `dune-project` drops `logs` from its depends and
gains `dune-rpc` and `csexp`. The `humpty` package gains `dune-rpc`, `csexp` and
`pp`, since okit now names them itself. Both `.opam` files are regenerated by
the build. A lower bound of 3.24 on `dune-rpc` matches the build v2 and
diagnostics v2 the session already required.

## Tests

- `test/rpc_wire.ml` tested codecs that are now upstream's, and is replaced by
  `test/rpc_eio.ml`: the adapter against a stub server over a socket pair,
  checking that a request round-trips and that a reply cut off part way through
  reads as a disconnect rather than as bytes that were wrong.
- `test/okit_dune.ml` keeps every case. Its stub servers build packets by hand
  through `Wire.Packet`, which becomes a local helper of about 20 lines over the
  `csexp` package. Cases that pin wording follow whatever the messages become.
- No other test changes.

## Prose

Every surviving comment and doc-comment is cut to the density `CLAUDE.md` asks
for. `session.mli` goes from 140 lines to about 55: what each call does and what
a caller must know. The reasoning about the spawn race, the grace and the
relaunch stays as short comments in the `.ml`, beside the invariant each one
keeps, rather than as narration in the interface.

## Documentation

- `ARCH.md`: the `dune_rpc_eio/` section says the library is an Eio counterpart
  to `dune-rpc-lwt` and lists what it supplies. The session, the wire format and
  the s-expression reader leave the file map for okit or for upstream.
- `CHANGES.md`: one entry, that `dune-rpc-eio` is now a thin Eio adapter over
  the `dune-rpc` package rather than a reimplementation of it.
- `docs/superpowers/plans/2026-08-02-dune-rpc-protocol.md` described the wire
  format this change stops implementing. It stays, marked as a record of what
  the protocol looks like on the wire, not as a specification okit follows.

## Verification

`dune build`, `dune runtest` and `dune build @fmt` clean. `dune build @doc`
clean, since the library is public API. `DS4_LIVE=1 dune runtest` is not needed,
because nothing here touches the FFI, the session lifetime of the engine or the
agent loop. The live cases in `test/okit_dune.ml` and `test/okit_tools.ml` run
against a real dune and prove the session still spawns, builds and reports.
