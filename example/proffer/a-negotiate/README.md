# Select a representation

This example selects HTML, JSON or plain text from the request's `Accept` field.

[negotiate.ml](negotiate.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/a-negotiate/negotiate.exe
```

With the server running, send requests from another terminal.

```sh
curl -H 'Accept: text/plain' http://127.0.0.1:8765/
curl -H 'Accept: application/json' http://127.0.0.1:8765/
curl -i -H 'Accept: image/png' http://127.0.0.1:8765/
```

`Negotiate.v` chooses a handler from the offered representations. An
unsupported `Accept` value receives 406. The example offers HTML first,
which is the default when the client supplies no preference.

Stop the server with Ctrl-C before running another server example.

Continue with [compose sites](../b-mount/README.md), or return to the
[Proffer examples](../../README.md#proffer).
