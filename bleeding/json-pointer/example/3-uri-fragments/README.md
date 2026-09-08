# `3-uri-fragments`

RFC 6901 defines a second representation for pointers embedded in URI
fragments. It first applies JSON Pointer escaping, then percent-encodes bytes
that are not allowed literally in the fragment.

```ocaml
let pointer =
  Json_pointer.of_tokens [ "catalog"; "snow man"; "100%"; "caf\u{00e9}" ]

let fragment = Json_pointer.to_uri_fragment pointer
let decoded = Json_pointer.of_uri_fragment fragment
```

Run the [complete example](uri_fragments.ml):

```console
$ cd example/3-uri-fragments
$ opam install --deps-only --yes .
$ dune exec --root . ./uri_fragments.exe
pointer: /catalog/snow man/100%/café
fragment content: /catalog/snow%20man/100%25/caf%C3%A9
complete URI: document.json#/catalog/snow%20man/100%25/caf%C3%A9
round trip: true
```

Both functions operate on fragment content: `to_uri_fragment` does not prepend
`#`, and `of_uri_fragment` does not expect one. This keeps URI construction in
the URI library or application layer. Result-returning parsing is available as
`Json_pointer.of_uri_fragment_result`.

## Next steps

- [`4-json-patch`](../4-json-patch#readme) moves from reading JSON to changing
  it.
- [`5-jsont-codecs`](../5-jsont-codecs#readme) composes pointers with typed
  codecs.

## Folders and files

- [`uri_fragments.ml`](uri_fragments.ml) is the complete program.
- [`dune`](dune) describes its executable.
- [`dune-project`](dune-project) and
  [`3-uri-fragments.opam`](3-uri-fragments.opam) make this a standalone project.

[Up to the tutorial index](../#readme)
