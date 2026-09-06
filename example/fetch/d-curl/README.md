# Use libcurl

This example makes a request with the Curl backend.

[curl.ml](curl.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/d-curl/curl.exe
```

`Fetch_curl.std ~sw env` uses libcurl for connection reuse, HTTP/2, TLS and
content decoding. The switch owns the client's resources. With no argument,
the example starts the local server. Supplying a URL makes it contact that
address instead.

```sh
dune exec --profile release-check ./example/fetch/d-curl/curl.exe -- https://example.com/
```

Continue with [use https](../e-https/README.md), or return to the
[Fetch examples](../../README.md#fetch).
