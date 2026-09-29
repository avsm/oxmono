# Reusing csexp and dune-rpc in okit: assessment and actions

An audit of whether `okit/csexp.ml` and `okit/dune_rpc.ml` should be replaced
by the opam packages of the same names. Verified against the installed
packages, csexp 1.5.2 and dune-rpc 3.24.1, on 2026-08-03.

## Findings

### dune-rpc is not reusable

The published `dune-rpc` library has no `build` and no `runtest` request.
`Dune_rpc.V1.Request` offers `ping`, `diagnostics`, `format_dune_file`,
`promote` and `build_dir`, and the private API offers the same set. A
recursive grep over the installed library finds no occurrence of `runtest` at
all. The two methods okit exists to call are registered inside dune's own
binary, under `src/dune_rpc_impl/`, and are not part of the package.

Adopting the package would also cost:

- An Eio instantiation of `Dune_rpc.V1.Client.Make`, whose `Fiber` argument
  wants `Ivar`, `fork_and_join_unit` and `parallel_iter`. The only published
  instantiation is `dune-rpc-lwt`.
- The dependency set `stdune` (pinned `= version` of dune), `dyn`,
  `ordering`, `pp`, `xdg` and `ocamlc-loc`, version-locked to dune releases.

None of that buys the calls okit makes. ARCH.md already records the decision
to speak the protocol directly, and this audit confirms it holds at 3.24.1.
The wire reference is `docs/superpowers/plans/2026-08-02-dune-rpc-protocol.md`.

### csexp stays as a local adaptation

Upstream csexp's own documentation says: "If you are using fancy input
sources, simply copy the parser and adapt it." `okit/csexp.ml` is that
adaptation for `Eio.Buf_read`, and it carries three things the package does
not have:

- A streaming `read` on `Eio.Buf_read.t`, taking one value from a stream that
  carries many.
- A 16 MiB atom-length cap, refusing a corrupt or hostile length prefix
  before waiting on the stream for it.
- A documented error contract. Only `Failure` escapes, messages begin
  `csexp:`, and `Dune_rpc.await` matches the exact message
  `csexp: input ended mid-value` to tell a server that died mid-write from
  bytes that are wrong. `test/okit_wire.ml` pins that message.

The package would supply only the type declaration and the encoder, about 15
lines. The reader, the cap, `field` and `pp` stay local either way. Dune's
module aliasing means okit's own `Csexp` shadows the library's module inside
the okit library, so adopting the package also forces a rename of okit's
module across `wire.ml`, `dune_rpc.ml`, `pp_text.ml` and the tests. The churn
exceeds the saving.

### Nothing else overlaps

The session lifecycle in `dune_rpc.ml` (spawn or attach, socket wait, tails,
relaunch), the okitd line protocol in `proto.ml` (jsont), and the merlin
child in `merlin.ml` have no counterpart in either package.

## Actions

Documentation only. No code changes and no new dependencies.

1. ARCH.md, the paragraph beginning "okit speaks dune's RPC protocol itself":
   replace the clause about dune's client with the verified fact. The
   published dune-rpc library has no `build` or `runtest` request, since dune
   registers those inside its own binary, so no client built on the package
   can ask for a build. Keep the rest of the paragraph.
2. ARCH.md, same section: one sentence noting that `okit/csexp.ml` is the
   adaptation upstream csexp's documentation recommends for other input
   sources, with an atom cap the package does not have.
3. `okit/csexp.mli`, module doc: one or two sentences saying the module is a
   deliberate Eio adaptation rather than a substitute for the csexp package,
   and why (streaming read, atom cap, the error contract Dune_rpc matches
   on).
4. No CHANGES.md entry, since no behaviour changes.

Prose follows the norms in CLAUDE.md: manpage density, complete sentences, no
em-dashes, no clause-joining semicolons.

## Verification

`dune build`, `dune runtest` and `dune build @fmt` must all be clean.
