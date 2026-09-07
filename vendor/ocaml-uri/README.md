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
`portable`. `Uriz.t` has the `immutable_data` kind, so a URI parsed once at
module level, or a `Uriz.t or_null` returned by `of_string`, can be read from
a portable closure on any domain.

Historical measurements on the same machine against `uri` 4.4; the first four
rows come from `bench/qbench.ml`. The comparison benchmark for the `resolve`
rows is preserved in commit `50fca1889`; it was retired to remove its dependency
on opam `uri`.

| operation             | ocaml-uri 4.4 (angstrom) | uriz            |
| --------------------- | ------------------------ | --------------- |
| `of_string`           | 1780 ns, 11.9 KB         | 147 ns, 136 B   |
| `of_string_canonical` | —                        | 150 ns, **0 B** |
| `to_string`           | 410 ns, 1.2 KB           | 1.6 ns, 0 B     |
| `Raw.parse`           | —                        | 98 ns, 0 B      |
| `resolve`             | 365 ns, 193 w            | 145 ns, 30 w    |
| `resolve__local`      | —                        | 156 ns, **0 w** |

## Compatibility with Uri

Compatibility is a goal where it preserves URI meaning. The monorepo no longer
depends on opam `uri`; `Uriz` is the shared implementation. The main mappings are:

| Former operation | Uriz operation |
| --- | --- |
| `Uri.of_string` | `of_string` returns `This uri` or `Null`; `of_string_exn` raises for invalid input. |
| `Uri.with_query'` | `with_query_params`, replacing the entire query from decoded pairs. |
| `Uri.get_query_param` | `find_query ~plus_as_space:true`, returning `string or_null`. |
| `Uri.verbatim_query` | `query`, retaining encoded text and distinguishing absent from empty. |
| `Uri.canonicalize` for HTTP(S) | `canonicalize`, removing dot segments/default ports and supplying an empty path's `/`. |
| `Uri.resolve "" base reference` | `resolve ~base reference`. |

`with_query_params` keeps repeated keys, empty values and ordering, and rebuilds
once. `add_query_params` appends pairs; `set_query_params` replaces only named
keys. They encode keys and values independently so `+`, `&`, `=`, `;`, `,` and
percent escapes in supplied data cannot turn into query structure. Comma and
semicolon escaping follows Uri's query-value convention. Decoding `+` as a
space is explicit because it belongs to form-style queries, not generic URIs.

Parsing and `to_string` preserve encoded reserved characters. RFC 3986
[section 2.2](https://www.rfc-editor.org/rfc/rfc3986.html#section-2.2) distinguishes
these from their literal spellings: `%2B` and `+`, for example, need not identify
the same resource. `canonicalize` therefore keeps such escapes too. HTTP
signatures read encoded path and query components directly rather than decoding
and rebuilding the signed target. Malformed references are rejected instead of
silently coerced.

Arod's saved annotation keys deliberately retain their older comparison rules
at the storage boundary. That application-specific equivalence is tested in
`avsm/arod/test/test_feed_annotations.ml`; it is not the identity of `Uriz.t`.
The Uri-dependent corpus differential was retired after its original 4,659-URL
run found no link-classification changes; its source is in commit `50fca1889`.

The migration passed the workspace `release-check` build with switch
`5.2.0+ox`, all 315 Uriz tests, the affected HTTP/Fetch/APub/ATP/OpenAPI and
application regression suites, and 19 saved-annotation checks. Dune's external
dependency report contains no `uri` or Cohttp dependency. Syndic's optional
live-feed suite still needs `ocplib-json-typed`; its local regression passes.

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
