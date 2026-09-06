# Attach credentials

This example supplies a bearer token for `/secret` and default request headers.

[credentials.ml](credentials.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/7-credentials/credentials.exe
```

`Fetch.with_credentials` scopes the token to the supplied base URL. The
example uses `~allow_insecure:true` because its server uses plaintext
loopback HTTP. Remote credentials require HTTPS. `Fetch.with_headers` adds
a user agent and an `X-Example` field, which `/headers` echoes.

`Fetch.read` also returns the body of the initial 401 response.

Continue with [retain cookies](../8-cookies/README.md), or return to the
[Fetch examples](../../README.md#fetch).
