# Pass application state

This example counts requests using state supplied through `~env`.

[counter.ml](counter.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/4-counter/counter.exe
```

With the server running, send requests from another terminal.

```sh
curl http://127.0.0.1:8765/
curl http://127.0.0.1:8765/
```

The responses contain counts of one and two. Each handler receives the
same environment record. Restarting the process resets the counter.

Stop the server with Ctrl-C before running another server example.

Continue with [read forms and query parameters](../5-form/README.md), or return to the
[Proffer examples](../../README.md#proffer).
