# Set fallback responses

This example adds a fallback response, a permanent redirect and a response header.

[error.ml](error.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/6-error/error.exe
```

With the server running, send requests from another terminal.

```sh
curl -i http://127.0.0.1:8765/missing
curl -i http://127.0.0.1:8765/old
```

`Site.with_fallback` handles an unmatched path with a custom 404 response.
`moved` redirects `/old` to `/`. `Site.with_headers` adds the `Server` field
to responses from the site.

Stop the server with Ctrl-C before running another server example.

Continue with [set cache policy](../7-cache/README.md), or return to the
[Proffer examples](../../README.md#proffer).
