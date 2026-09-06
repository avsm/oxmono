# Read typed JSON

This example decodes a todo, handles a missing item, posts JSON and reads JSON Lines.

[json.ml](json.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/f-json/json.exe
```

`Fetch.Json.v` builds a media codec from a Jsont description.
`Fetch.read_as` returns a decoded value for a successful HTTP status or the
response for another status. `Fetch.encode` supplies request headers and
a body. `Fetch.Json.lines` and `Fetch.decode_seq` decode a sequence.

The [Proffer JSON example](../../proffer/e-json/README.md) shows
the server-side codec operations. This client uses its own local fixture
routes, so that example server does not need to be running.

Continue with [render Markdown](../g-markdown/README.md), or return to the
[Fetch examples](../../README.md#fetch).
