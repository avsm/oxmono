# Pace requests

This example runs six requests with a concurrency limit and a delay between starts.

[limits.ml](limits.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/a-limits/limits.exe
```

`~max_concurrent:2` allows at most two requests in flight per origin.
`~min_interval:(Fetch.Duration.of_ms 500)` separates request starts by at
least half a second. Printed completion times vary with scheduling.

Continue with [stream bodies](../b-stream/README.md), or return to the
[Fetch examples](../../README.md#fetch).
