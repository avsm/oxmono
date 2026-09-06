# Serve typed JSON

This example stores todo records through a JSON API and exports them as JSON Lines.

[json.ml](json.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/e-json/json.exe
```

With the server running, send requests from another terminal.

```sh
curl http://127.0.0.1:8765/todos
curl -H 'Content-Type: application/json' --data '{"id":2,"title":"read the examples"}' http://127.0.0.1:8765/todos
curl http://127.0.0.1:8765/todos/2
curl http://127.0.0.1:8765/todos/export
```

`Jsont` describes the record. `Json.v` produces its media codec, and
`Json.lines` produces the sequence codec. `with_body` decodes a submitted
record before calling the handler. `Resp.encode` writes a typed response.
The store exists only for the lifetime of the process.

The [Fetch JSON example](../../fetch/f-json/README.md) shows
client-side use of the same codec operations.

Stop the server with Ctrl-C before running another server example.

Continue with [serve Markdown and HTML](../f-markdown/README.md), or return to the
[Proffer examples](../../README.md#proffer).
