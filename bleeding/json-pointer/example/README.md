# Tutorial

These numbered examples form a step-by-step `json-pointer` tutorial. Each one
is a complete Dune project with a small executable and a README that explains
the new idea. Start with [`1-pointers`](1-pointers#readme), or jump directly to
the topic you need.

- [`1-pointers`](1-pointers#readme) — parse pointers, inspect their tokens, and
  build them without manual escaping.
- [`2-evaluation`](2-evaluation#readme) — resolve required and optional values
  while letting the JSON value determine whether a token is a member or index.
- [`3-uri-fragments`](3-uri-fragments#readme) — convert pointers to and from
  their percent-encoded URI fragment representation.
- [`4-json-patch`](4-json-patch#readme) — apply the six RFC 6902 operations as
  pure transformations.
- [`5-jsont-codecs`](5-jsont-codecs#readme) — extract and update typed values by
  composing pointers with Jsont codecs.
- [`6-jmap`](6-jmap#readme) — evaluate RFC 8620 result-reference wildcards and
  decode their results.

## Running an example

Each directory has its own `.opam` file. Once `json-pointer` is available in
your opam switch, run an example like this:

```console
$ cd example/1-pointers
$ opam install --deps-only --yes .
$ dune exec --root . ./pointers.exe
```

When working from this source checkout before the package is released, install
the checkout once with `opam install .` from the repository root, then use the
same commands.

The examples use `Jsont_bytesrw` only to turn JSON text into `Jsont.json`
values and back. All pointer behavior comes from `json-pointer` itself.

For a single-page reference with more detail, see the
[odoc tutorial](../doc/tutorial.mld).
