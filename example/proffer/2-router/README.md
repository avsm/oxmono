# Match request paths

This example matches literal paths and captures strings, integers and remaining path segments.

[router.ml](router.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/2-router/router.exe
```

With the server running, send requests from another terminal.

```sh
curl http://127.0.0.1:8765/echo/hello
curl http://127.0.0.1:8765/square/12
curl http://127.0.0.1:8765/files/a/b.txt
```

`s` matches a literal segment. `str` and `int` pass a captured value to the handler.
`rest` captures the remaining segments as a list. Paths without a matching
route return 404.

Stop the server with Ctrl-C before running another server example.

Continue with [log requests](../3-log/README.md), or return to the
[Proffer examples](../../README.md#proffer).
