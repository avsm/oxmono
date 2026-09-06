# Render Markdown

This example reads `/about` as a Cmarkit document and prints its HTML representation.

[markdown.ml](markdown.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/g-markdown/markdown.exe
```

`Fetch.Markdown.markdown` decodes the response. `Fetch.Markdown.html`
encodes the resulting document as HTML. Both codecs are included in Fetch.
The [Proffer Markdown example](../../proffer/f-markdown/README.md)
serves a document using the corresponding server codecs.

Return to the [Fetch examples](../../README.md#fetch).
