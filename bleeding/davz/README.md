# davz

`davz` provides immutable WebDAV protocol values and bounded XML codecs:
property fragments, multistatus responses, request XML, hrefs, lock discovery,
opaque tokens and DAV If conditions. It has no HTTP transport or Eio dependency.
The public module is `Davz`; link the Dune library `davz`.

Read the [specification](SPEC.md) for the supported RFC 4918 subset, resource
limits and extension roadmap. The [public interface](lib/davz.mli) is the API
reference. A Fetch-backed client is available as
[`proffer.dav`](../proffer/dav/README.md).

```sh
opam exec --switch=5.2.0+ox -- dune runtest bleeding/davz/test
```

This repository's Xmlm includes fixes for CDATA attribute whitespace,
whitespace serialization and reserved namespace bindings. Preserve those patches
when updating the vendor; [Xmlm's notes](../../vendor/xmlm/README.md) describe
them. `davz` additionally rejects duplicate expanded attribute names, DTDs,
unresolved entities, trailing XML documents and configured resource-limit excess.

The namespace tests generate prefix collisions, default namespace combinations,
shadowing and detached fragments, check that repeated serialization stays
stable, and exercise a 32,000-declaration document at its exact node limit.
Inherited attributes use hash lookups, with synthesized attributes charged to
the node budget. An explicit benchmark checks scaling without adding unstable
timing assertions to `runtest`:

```sh
opam exec --switch=5.2.0+ox -- dune exec bleeding/davz/test/bench_namespaces.exe
```
