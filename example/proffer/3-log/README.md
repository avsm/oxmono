# Log requests

This example reports each completed request through `on_event`.

[log.ml](log.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/3-log/log.exe
```

With the server running, send requests from another terminal.

```sh
curl 'http://127.0.0.1:8765/echo/hello?source=example'
```

The server prints the remote address, method, path, status and elapsed
microseconds. The log uses `event.path`, which excludes the query string.
The response body is unchanged.

Stop the server with Ctrl-C before running another server example.

Continue with [pass application state](../4-counter/README.md), or return to the
[Proffer examples](../../README.md#proffer).
