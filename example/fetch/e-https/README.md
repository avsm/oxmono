# Use HTTPS

This example makes an HTTPS request with the pure OCaml backend.

[https.ml](https.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/e-https/https.exe
```

`Fetch_httpz.std` uses the operating system's trust anchors to verify the
server certificate. With no argument, the example requests
`https://example.com/` and requires Internet access. An argument replaces
that URL. This example does not start `Localhost`.

```sh
dune exec --profile release-check ./example/fetch/e-https/https.exe -- https://example.com/
```

Continue with [read typed json](../f-json/README.md), or return to the
[Fetch examples](../../README.md#fetch).
