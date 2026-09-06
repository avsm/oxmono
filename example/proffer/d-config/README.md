# Set server limits

This example sets connection limits and typed timeouts, and requests an unused port.

[config.ml](config.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/d-config/config.exe
```

With the server running, send requests from another terminal.

```sh
curl http://127.0.0.1:PORT/
```

Replace `PORT` with the port printed by the server. `~port:0` lets the
operating system choose it. The configuration allows 100 connections,
five seconds to receive a request and ten seconds of idle time. Timeout
fields use `Duration.t`.

`on_listening` receives the bound address when a program needs the chosen
port. The Fetch examples' local server uses this callback.

Stop the server with Ctrl-C before running another server example.

Continue with [serve typed json](../e-json/README.md), or return to the
[Proffer examples](../../README.md#proffer).
