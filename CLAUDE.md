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

All three must be clean before a commit.

## Commits

Work on a branch. One commit per self-contained change, with a one-line message
in the imperative and no trailers or sign-off. Keep a mechanical change, such as
a reformat, out of the commit that changes behaviour.
