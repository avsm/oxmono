# Working in this repository

Norms for anyone, human or otherwise, changing this code. See `ARCH.md` for how
the repository is put together.

## Prose

Aim for the density of a POSIX manual page. Say what a reader needs to act on
and stop. Leave out history, alternatives that were not taken, and detail that
serves the author rather than the reader. Write complete sentences. Do not use
em-dashes, and do not join two clauses with a semicolon. Prefer a full stop.

Document an OCaml value as `[foo x y] is ...` or `[foo x y] does ...`, naming
its arguments. Say what it does and what a caller must know, not how it works.

A comment earns its place by explaining something the code cannot: why a
constraint exists, what breaks without it, which invariant is being kept. Do not
restate the code.

## Changelog

One or two lines per entry in `CHANGES.md`, describing the change a user would
notice. Group entries by the commit that made them.

## Building

    dune build
    dune runtest
    dune build @fmt

All three must be clean before a commit, scoped to the packages the change
touches. A repo-wide build fails in packages unrelated to most work, so use
package aliases such as `dune build @avsm/arod/all @avsm/arod/runtest`. In
the `5.2.0+ox` switch `ocamlformat` is not installed, so the OCaml half of
`@fmt` cannot run. Match the surrounding formatting by hand and keep lines
within 80 columns. Force test runs with `--force` when verifying, since a
cached pass proves nothing about the change.

## OxCaml portability

Handlers and anything they capture are checked at `@ portable`. Rules that
were each learned from a compiler error, so trust them over intuition:

- A floating `@@ portable` applies only to declarations after it. Put it
  before the first `val` or `type` in the `.mli`.
- A value crosses into a portable closure only if its type carries a kind
  such as `immutable_data`. Guards for kinds must capture a module-level
  value inside a `@ portable` closure and use it. A parameter-shaped
  ascription proves nothing.
- A mode on an optional parameter takes effect only with an explicit type
  at the definition site, as in `?(f : ty @ portable = default)`. A
  cross-unit partial application also needs a result mode in the `.mli`.
- `external` values annotate in type position:
  `external f : ty @@ portable = "%prim"`.
- A module-level stdlib `Set`, `Map`, `Hashtbl` or array is unreadable from
  portable code even through `MakePortable`, because their `t` carries no
  kind. Escapes, cheapest first: build with `of_list []` at the use site,
  replace the table with a match, use a sorted immutable structure such as
  `Bushel.Smap` or an iarray via `basement`.
- Build-once mutable state that handlers read becomes an immutable
  structure reachable through the value the handler captures. The mutable
  original stays in the loader.
- Some libraries cannot be annotated. A record field holding closures that
  capture a formatter, a channel or a handle is structural, not effort. A
  compiled `Re.re` mutates internal caches on execution. Precompute at
  startup instead, or keep the env closure.
- Write `.mli` claims about portability and behaviour from a compiler probe
  or a test, never from memory. Reviews in this tree have repeatedly caught
  docs stating the inverse of the code.

## Vendored libraries

`vendor/` is declared in the root `dune` as `(vendored_dirs vendor)`. A
vendored `public_name` shadows the opam switch for the whole workspace, and
shadowing one package can force another in verbatim when an installed
dependent would otherwise link the switch copy. Vendored `runtest` aliases
are inert, so the guard test for a vendored patch lives outside `vendor/`,
usually in `avsm/arod/test/`. Every vendored copy carries a README naming
the upstream version, each patch hunk with its provenance, and a
re-vendoring checklist that verifies through consumer aliases only. Prove a
patched copy behaves identically with a differential against the pristine
sources before relying on it.

## Behaviour evidence

`avsm/arod/test/test_md_golden.ml` byte-compares real renders against golden
files. Never regenerate a golden to make a test pass, since a golden diff
means the change altered behaviour. For changes on the serving path, run
`avsm/arod/test/render_capture.sh` before and after and byte-compare. Its
header documents the route set and the one clock-stamped file that differs
between identical runs.

## Commits

Work on a branch. One commit per self-contained change, with a one-line message
in the imperative and no trailers or sign-off. Keep a mechanical change, such as
a reformat, out of the commit that changes behaviour.
