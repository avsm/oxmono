# OxMono import

Imported `60ba9ed9ac113ddb5da38ee6d42a2f6ab24d27c3` from `../ocaml-jscontact` on 2026-09-08.
The source checkout is unchanged. [oxmono/upstream.json](oxmono/upstream.json)
records the base for future merges.

## Local adaptations

Packages `idk`, `jscontact`, `vcard`, `ical`, `carddav` and `caldav` retain
the upstream directory layout. Checkout-only vendoring declarations are disabled.
Pure interfaces expose compiler-checked portable functions and immutable kinds.
Jsont defaults use factories, enum module signatures expose portable values,
and retained callbacks carry portable modes. Portable map functors, fresh empty
maps, eta expansions and immutable registry lists preserve the computations.
The clients use `fetch-httpz` and `Fetch_dav.Objects` from the HTTPz import.

The combined offline import run passed 957 Alcotest tests, plus JSON Pointer
cram checks and DAV protocol/client checks. Live mail-server oracles were skipped.
Jsonm matched its pristine decoder in 5013 token, error and position comparisons.

## Validation and refresh

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check @bleeding/idk/all
opam exec --switch=5.2.0+ox -- dune runtest --force --profile release-check bleeding/idk
```

Export `git diff --binary BASE..NEW` from the source checkout and apply with
`git apply --directory=bleeding/idk`. Resolve against the recorded base,
retain the adaptations above, run the scoped checks and update the revision.
Upstream CI and documentation describe the standalone build.

## Known upstream limitations

`Fetch_dav.Objects.add` treats a supplied name as a raw href segment. Use
unreserved ASCII member names until percent-encoding of reserved characters
such as `?`, `#` and `%` is fixed upstream. The import preserves this behavior.
Typed sync also interprets missing multiget entries as removals, so consumers
requiring strict partial-failure detection should inspect the underlying DAV
responses. These are follow-up review items, not guarantees of the typed API.
