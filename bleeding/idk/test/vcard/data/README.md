# Vendored vCard fixtures

`vcard4.txt` holds the vCard inputs of the test suite of **vcard4**, a Rust
parser for RFC 6350, so that `test/vcard` can check this library against the
inputs another implementation accepts.

| | |
|---|---|
| Upstream | <https://github.com/tmpfs/vcard4> |
| Path | `tests/*.rs`, the raw string literals holding a `BEGIN:VCARD` |
| Commit | `f040a2d67e72f3dfa7cf4808ed6e098bc6aa346d` (2026-02-07) |
| Version | vcard4 0.7.3 |
| Licence | MIT OR Apache-2.0, at the user's option; MIT is taken here |
| Modified | Extracted from the test sources, otherwise byte for byte |

The inputs of `errors.rs` are left out, since they are the cards that
implementation rejects.

## What they are

Each block starts with a `> ` line naming the test file it came from and holds
one vCard. `test/vcard/test_vcard.ml` reads every block, checks that it
parses, writes it back and checks that the result reads as the same card.
