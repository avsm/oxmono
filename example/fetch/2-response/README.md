# Inspect a response

This example prints the status, URL, headers and body returned by `/json`.

[response.ml](response.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/2-response/response.exe
```

`Fetch.get` returns after the response headers arrive. `Fetch.body` is an
Eio source, copied here to standard output. The switch bounds the lifetime
of the response resources. `Fetch.header` decodes an individual field;
`Fetch.headers` exposes all fields.

Continue with [send request bodies](../3-post/README.md), or return to the
[Fetch examples](../../README.md#fetch).
