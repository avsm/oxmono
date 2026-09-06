# Retry temporary failures

This example retries `/flaky` until its third request succeeds.

[retry.ml](retry.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/9-retry/retry.exe
```

The server answers the first two attempts with 503 and `Retry-After: 1`.
The default client retries and prints the successful body. A second client
uses `Fetch.Retry.v ~max_retries:0 ()` and returns the next 503 body.
Retries require an eligible method and a replayable body.

Continue with [pace requests](../a-limits/README.md), or return to the
[Fetch examples](../../README.md#fetch).
