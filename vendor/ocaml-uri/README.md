uriz -- an OxCaml-native RFC 3986 URI library
---------------------------------------------

`uriz` is an OCaml implementation of the
[RFC 3986](http://tools.ietf.org/html/rfc3986) specification for parsing URIs
and URLs. It is derived from [ocaml-uri](https://github.com/mirage/ocaml-uri)
and keeps its authorship lineage, but the API has diverged far enough that it
ships under its own name: consumers of `uri` must be ported deliberately.

**It targets the [OxCaml](https://oxcaml.org) compiler** (tested with
`ocaml-variants.5.2.0+ox`) and does not build on stock OCaml. The module is
`Uriz`, with the scanner exposed as `Uriz.Raw`.

## Design

A parsed URI is one canonical, percent-encoded string plus an index of
component spans held in a single flat record of immediates.

- `to_string` hands back the stored string: no work, no allocation.
- `equal`, `compare` and `hash` are string operations on that canonical form.
  Parsing applies RFC 3986 §6.2.2 syntax-based normalization, so equivalent
  references have identical text.
- Parsing allocates once — the record — because the canonical string is the
  caller's string, shared rather than copied, whenever the input is already
  canonical. Only non-canonical input costs a second allocation.
- `Uriz.Raw` exposes the scanner underneath: it returns an unboxed record of
  span offsets and is statically checked `[@zero_alloc]`, so hot paths can
  index a URI without allocating at all.
- `Uriz.of_string_canonical` parses onto the stack and refuses to normalize,
  so a parse/inspect/drop cycle is provably heap-free — the record lives in
  the caller's frame and the canonical string *is* the input.
- `Uriz.resolve__local` and `Uriz.normalize__local` compose their result text
  in the caller's region, so reference resolution can run with no heap traffic
  either. `Uriz.globalize` copies one back onto the heap when you want to keep
  it.

Errors are explicit. `Uriz.of_string` returns `t or_null` and never invents an
empty URI for input it could not parse.

Read-only operations accept their arguments at mode `local`, the producers are
mode-polymorphic (`__local` variants via `ppx_template`), and every export is
`portable`.

Measured on the same machine against `uri` 4.4; the first four rows come from
`bench/qbench.ml` and the `resolve` rows from `bench/compare.ml`:

| operation             | ocaml-uri 4.4 (angstrom) | uriz            |
| --------------------- | ------------------------ | --------------- |
| `of_string`           | 1780 ns, 11.9 KB         | 147 ns, 136 B   |
| `of_string_canonical` | —                        | 150 ns, **0 B** |
| `to_string`           | 410 ns, 1.2 KB           | 1.6 ns, 0 B     |
| `Raw.parse`           | —                        | 98 ns, 0 B      |
| `resolve`             | 365 ns, 193 w            | 145 ns, 30 w    |
| `resolve__local`      | —                        | 156 ns, **0 w** |

## Installation

You can build the source code locally via the [dune](https://github.com/ocaml/dune)
build system.

    opam install uriz --deps-only
    eval `opam config env`
    dune build
    dune runtest

## Usage

One findlib package is installed:

- `uriz` — the `Uriz` module, with the scanner available as `Uriz.Raw`.

The `uri-re`, `uri-sexp`, `uri-bench`, `uri.services` and `uri.services_full`
packages of ocaml-uri 4.x are gone.

## Contact

- Issues: <https://github.com/mirage/ocaml-uri/issues>
- E-mail: <mirageos-devel@lists.xenproject.org>
- API Documentation: <https://ocaml.org/p/uri/latest/doc/index.html>
