# Bytesrw 0.4.0 for OxCaml

This directory is based on upstream Bytesrw `v0.4.0`, commit
`6f2931e63b47ab854475f0759c38ceb6b689ef91`. It vendors the four pure-OCaml
top-level modules `Bytesrw`, `Bytesrw_fmt`, `Bytesrw_utf`, and `Bytesrw_hex`.
Optional compression, hashing, cryptography, Unix, sysrandom, and command-line
sublibraries are not included.

The local Dune build depends on Base because its bytes and buffer operations
accept the modes needed by this port.

## OxCaml changes

The existing port keeps byte-slice inputs at `local` where they are only
borrowed, marks verified accessors `zero_alloc`, uses `exclave_` for local slice
construction, and replaces closure-captured references in hot loops with
`let mutable`. `Bytesrw_utf` also uses an unboxed `char#` helper. The slice
formatters retain the local fix that prints a truncation ellipsis correctly for
slices whose first offset is not zero.

The portable surface is intentionally selective. The operations needed to
construct and run Jsont and Httpz media streams are portable, including slice
construction and buffering, reader construction/read/string conversion, and
writer construction/write/string conversion. A concrete reader or writer is
still mutable, domain-bound stream state; portability applies to the operation
closures that a codec captures, not to moving an active stream between
domains. Error-formatting and tracing APIs retain their upstream modes.

Internal buffering uses `Base.Buffer`, allowing the exported reader and writer
operations to satisfy those portable signatures.

## Audited unsafe boundary

`Bytes.Slice.eod` is the one portable assertion. Its record contains
`Bytes.empty`, whose type is structurally mutable even though its zero-length
storage cannot be modified. The singleton is promoted once with
`Obj.magic_portable`; internal domain-local uses recover the ordinary view with
`Obj.magic_uncontended`. No non-empty mutable byte buffer is promoted.

## Upstream behaviour

Bytesrw 0.4.0 already contains the upstream fixes for comparing a slice's last
byte and for `Reader.of_slice` with a non-zero starting offset. The local
formatter-offset fix remains a behavioural delta and is covered by the Httpz
Bytesrw tests.

## Validation

The workspace's Bytesrw, Media, Fetch, and Proffer tests exercise the local and
portable surfaces; Jsont's four-domain test runs the streaming codec
concurrently with a fresh reader and writer on each domain.

```sh
opam exec --switch=5.2.0+ox -- dune build @all
opam exec --switch=5.2.0+ox -- dune runtest
opam exec --switch=5.2.0+ox -- dune build --profile release-check @all
```

## Re-vendoring

1. Copy the four `src/bytesrw*` module pairs from the new upstream tag.
2. Reapply and re-audit the local, zero-allocation, loop, formatter, Base, and
   selective-portability changes above.
3. Check whether upstream now includes the formatter-offset fix and remove the
   local delta if so.
4. Update the version, commit, and checked date here, in `bytesrw.opam`, and in
   the root `VENDORED.md`.
5. Run all three commands in the validation section.
