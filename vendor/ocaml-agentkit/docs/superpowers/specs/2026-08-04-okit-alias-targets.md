# Alias targets for okit's build over dune RPC

okit refuses an alias target such as `@check`, and ARCH.md says dune's RPC
resolves a target as a path alone. That is wrong. A build target string may
also be a dep-spec s-expression naming an alias. Ali Caglayan pointed this out
and it is verified below against a live dune 3.24.2 passive server.

## Verified server behaviour, dune 3.24.2

Each target atom of a `build` v2 request is one of:

- A path relative to the workspace root: `.`, `lib`, `src/foo.exe`.
- `(alias <path>)`. The alias in exactly that directory, the root when the
  path has no directory. This is `@@name` in dune's CLI. `(alias check)` at
  the root fails with `No rule found for alias check` when the alias is
  defined only in subdirectories.
- `(alias_rec <path>)`. The alias in that directory and every directory
  below, which is `@name` in dune's CLI. `(alias_rec check)`,
  `(alias_rec runtest)`, `(alias_rec default)`, `(alias lib/runtest)` and
  `(alias_rec lib/runtest)` all succeed.

Paths and alias forms mix freely in one request. The string `@check` is read
as a path and fails with `Don't know how to build @check`. A malformed
s-expression such as `(alias` is answered with a `Code_error`, dune's own
internal failure, which names nothing useful. A client must therefore
validate before sending.

## Design

okit keeps dune's CLI spelling and writes the s-expression itself. In
`Dune_rpc.build`, each target is translated:

- `@name` becomes `(alias_rec name)`.
- `@@name` becomes `(alias name)`.
- Anything else not starting with `(` is a path and passes through.
- A target starting with `(` is refused, telling the caller to write `@name`.
  Passing raw dep-specs through would hand a malformed one to the server,
  whose `Code_error` answer explains nothing.
- An alias name must be non-empty and free of whitespace, parentheses and
  double quotes, since those would corrupt the constructed s-expression.
  A bad name is refused with the form to write.

`Wire.build` stays a passthrough of strings. The `runtest` method and the
`test` tool are unchanged. A follow-up could replace the `runtest` method
with `build [(alias_rec runtest)]` and shrink the required menu, but that is
a separate change and not part of this one.

## Changes

1. `okit/dune_rpc.ml`: replace `path_targets` with the translation above,
   applied in `build` before the guard. Refusal messages name the right form.
2. `okit/dune_rpc.mli`: rewrite the `build` doc. A target is a path or an
   alias written `@name` (that directory and below, as `dune build @name`)
   or `@@name` (that directory alone). A directory scope is written into the
   name, as in `@lib/runtest`. Raw `(alias ...)` strings are refused.
3. `okit/wire.mli`: the `build` doc says the server takes a path or the
   dep-spec forms `(alias <path>)` and `(alias_rec <path>)`, and that a
   malformed s-expression is answered with a Code_error, which is why
   callers validate before sending.
4. `okit/toolbox.ml` and `okit/toolbox.mli`: the build tool's parameter
   description and doc now offer aliases, e.g. `@check`, alongside paths.
5. `test/okit_tools.ml`: the case asserting `@check` is refused becomes a
   case asserting a raw `(alias check)` target is refused with a message
   naming `@check`. If the live workspace allows, also assert `@check`
   builds.
6. `test/okit_dune.ml`: update the header comment (targets are paths or
   aliases). The case at line 589 asserting `@check` errors becomes:
   `@check` builds with `ok = true`, a raw `(alias check)` target is
   refused, and `@` alone is refused. Add a `@@nosuch` build expecting
   `ok = false` if cheap to do.
7. `test/okit_wire.ml`: the `build bytes` case stays, since `Wire.build`
   remains a passthrough. Add one case encoding a translated alias target,
   e.g. targets `["(alias_rec check)"]`, pinning the atom form on the wire.
8. `ARCH.md`: rewrite the paragraph beginning "Dune's RPC resolves a target
   as a path alone". A build target is a path or an alias dep-spec, okit
   takes the CLI spelling and writes the s-expression itself, and a
   malformed dep-spec is refused locally because the server answers one with
   a bare Code_error. The following sentence about `test` changes to say the
   `runtest` method asks for the same alias by name.
9. `docs/superpowers/plans/2026-08-02-dune-rpc-protocol.md`: correct the
   build-targets bullet with the verified grammar and the Code_error
   behaviour, noting verification against dune 3.24.2.
10. `CHANGES.md`: one entry, the build tool now takes aliases written
    `@name` or `@@name`.

Prose follows CLAUDE.md norms: manpage density, complete sentences, no
em-dashes, no clause-joining semicolons.

## Verification

`dune build`, `dune runtest` and `dune build @fmt` all clean. The live dune
tests in `test/okit_dune.ml` and `test/okit_tools.ml` spawn a real dune and
are the ones that prove the alias path works end to end.
