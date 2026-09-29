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

All three must be clean before a commit. Formatting is `ocamlformat` 0.29.0, and
the version is pinned, so a reformat that touches unrelated lines belongs in its
own commit.

`DS4_LIVE=1 dune runtest --force -j1` adds tests that load a real model. Run
them when changing how a command drives the agent, or the okitd or numptyd
split.

## Commits

Work on a branch. One commit per self-contained change, with a one-line message
in the imperative and no trailers or sign-off. Keep a mechanical change, such as
a reformat, out of the commit that changes behaviour.

## Correctness

Prefer fixing the documentation to accepting sloppy input. When a caller gets an
argument wrong, say what the right form is rather than guessing what was meant.

A tool or a library must not silence a failure. Reporting an empty result where
an operation was refused sends the caller looking for a way around a wall it
cannot see.

Test the property that matters, which is often a negative one. That a sandbox
refuses to escape is worth more than that it reads a file.

Verify against the real model, not only the compiler, when changing anything the
model exercises. Run one model at a time, since a model of this size will not
load twice at once.

## The engine

The DS4 engine and its OCaml bindings are the `ds4` package, which lives in
the `ocaml-deepseek` repository. A change to the engine, the FFI, the agent
loop or the plain tools belongs there, not here.
