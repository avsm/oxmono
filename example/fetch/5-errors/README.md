# Handle failures

This example distinguishes HTTP error responses from URL, connection and timeout failures.

[errors.ml](errors.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/5-errors/errors.exe
```

A missing path returns an ordinary 404 response. An invalid URL or failed
connection raises `Eio.Io` with a `Fetch.E` error. The `/slow` handler takes
five seconds; `Eio.Time.with_timeout` stops that request after one second.
The connection-failure check assumes nothing is listening on loopback port 1.

Eio's outer timeout takes seconds as a float. Fetch's client timeout
options take `Duration.t`.

Continue with [restrict a client](../6-restrict/README.md), or return to the
[Fetch examples](../../README.md#fetch).
