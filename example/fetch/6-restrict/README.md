# Restrict a client

This example limits a client to the local server and then makes it read-only.

[restrict.ml](restrict.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/6-restrict/restrict.exe
```

`Fetch.restrict ~under:[ base ]` rejects requests outside the allowed URL
prefix. `Fetch.read_only` rejects writes. The program prints successful
local reads and the reasons for denied requests. The request to
`example.com` is denied before any network connection.

Continue with [attach credentials](../7-credentials/README.md), or return to the
[Fetch examples](../../README.md#fetch).
