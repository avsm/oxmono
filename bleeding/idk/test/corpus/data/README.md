# Vendored JSContact fixtures

The `*.txt` files in this directory are the JSContact test fixtures of
**calcard**, Stalwart Labs' Rust implementation of iCalendar, JSCalendar, vCard
and JSContact. They are vendored verbatim so that `test/corpus` needs no
checkout of another project.

| | |
|---|---|
| Upstream | <https://github.com/stalwartlabs/calcard> |
| Path | `resources/jscontact/` |
| Commit | `2d7e73f1cd1e8e462b6f9953b574d35e1e848bde` (2026-08-25) |
| Version | calcard 0.3.13 |
| Licence | Apache-2.0 OR MIT, at the user's option; MIT is taken here, see `LICENSE` |
| Modified | No. Copied byte for byte. |

calcard's `resources/SOURCES.txt` records third-party provenance for its
iCalendar and vCard fixtures. It records none for `resources/jscontact/`, so
these files are Stalwart Labs' own work, drawn from the examples of RFC 9553,
RFC 9555, RFC 6350, RFC 6715 and RFC 9554.

## What they are

Each file is a sequence of blocks. A block introduced by `> test` is a source
and one introduced by `> convert` is what it converts to; either may be a vCard
or a run of JSContact object members. `test/corpus/test_corpus.ml` takes the
JSContact blocks alone, splices each into a minimal Card, and puts it through a
decode, encode, decode cycle.

## Refreshing them

    cp /path/to/calcard/resources/jscontact/*.txt test/corpus/data/

then update the commit and version above. To check against a working tree
without copying, point `JSCONTACT_CORPUS` at it:

    JSCONTACT_CORPUS=../calcard/resources/jscontact dune runtest test/corpus
