# Serve Markdown and HTML

This example serves a Cmarkit document as HTML or Markdown.

[markdown.ml](markdown.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/f-markdown/markdown.exe
```

With the server running, send requests from another terminal.

```sh
curl -H 'Accept: text/html' http://127.0.0.1:8765/
curl -H 'Accept: text/markdown' http://127.0.0.1:8765/
```

`Markdown.html` and `Markdown.markdown` encode the same document.
`Negotiate.encode` selects a codec using the request's `Accept` field.
The [Fetch Markdown example](../../fetch/g-markdown/README.md)
reads a document and renders it as HTML.

Stop the server with Ctrl-C before running another server example.

Return to the [Proffer examples](../../README.md#proffer).
