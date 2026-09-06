# Stream a response

This example writes response bytes as they become available.

[stream.ml](stream.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/8-stream/stream.exe
```

With the server running, send requests from another terminal.

```sh
curl --no-buffer http://127.0.0.1:8765/countdown
curl http://127.0.0.1:8765/alphabet
```

`Resp.stream` supplies a sink to the response producer. The countdown
writes one line per second. Curl's `--no-buffer` option displays each line
as it arrives. The alphabet response declares its 27-byte length; the
countdown has no declared length.

Stop the server with Ctrl-C before running another server example.

Continue with [protect routes](../9-auth/README.md), or return to the
[Proffer examples](../../README.md#proffer).
