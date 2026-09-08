# Hacking on ocaml-matrix

Conventions for code and documentation in `lib/`. `STATUS.md` says what the
tree does; `TODO.md` says what it does not; `ROADMAP.md` orders the latter.

## Interface documentation

An `.mli` is the manual page for its module. Write it as one: a reader with
the Matrix specification and this file alone should be able to use the
module. The reference implementation is matrix-rust-sdk, but the reader does
not have it open.

- **Module header.** One sentence saying what the module is for, then at
  most one short paragraph a user needs before calling anything: the model it
  assumes, the one thing it does not do. Naming the matrix-rust-sdk
  counterpart in a single clause is fine; nothing further about Rust.
- **Values.** The first sentence states what the call does, in the
  indicative: `[join c ~room_id] joins the room and returns its id.` Then only
  what changes behaviour: an optional argument whose effect is not obvious
  from its name, what the result holds, what fails and how (`@raise`, or the
  `Error.t` cases returned). One to three sentences is the norm.
- **Types.** One line per constructor or field whose name does not say it
  all; nothing for the rest.
- **Tense and voice.** Present tense, full sentences, no "we". No history:
  never "now", "still", "used to", "was", "recently", "as of", and never the
  name of an agent or a commit. A doc comment describes what is, not how it
  came to be.
- **Do not restate the type**, do not list `@param` for arguments whose
  names suffice, do not describe the implementation. Cite the spec section or
  MSC once where behaviour follows it non-obviously.
- **Sections.** `{1 …}` headings only for a module with more than about eight
  values, grouped by what the user is trying to do.
- **References.** Use `{!Module.value}` so odoc links. `dune build @doc` must
  not warn more than it does on `main`; every warning there is a
  cross-package root odoc cannot see, never an in-tree reference.

## Implementation comments

A comment in an `.ml` earns its place by saying *why*: an invariant, a spec
quirk, a deliberate deviation, an interoperability trap (with the reference
that settles it), an ordering constraint the types do not enforce.

Delete comments that narrate what the code does, restate a name, repeat the
`.mli`, or tell the story of a change. Delete banner rules and section
headings where the order of definitions already reads. Delete commented-out
code.

## Dead code

Dead is: an internal helper with no caller; a public value with no caller in
`lib/`, `example/` or `test/` that is not an endpoint binding, a codec a
caller needs for a documented type, or part of the E2EE surface `STATUS.md`
describes. An unused endpoint binding stays; the library is the API surface.
Duplicated helpers move to the module that owns the concept. A removal
carries its `matrix-chat.eio` wrapper, its test rows and its `STATUS.md` line with
it in the same commit.

## Before committing

```sh
dune build --root .                        # warnings are errors
dune build --root . @test/runtest --force  # hermetic suites
dune build --root . @integration --force   # with test/integration/synapse.sh up
dune build --root . @doc 2>&1 | grep -c Warning   # not above the baseline
```
