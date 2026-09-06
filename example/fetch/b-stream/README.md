# Stream bodies

This example downloads four MiB to a temporary-directory file and streams an eight KiB upload.

[stream.ml](stream.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/b-stream/stream.exe
```

`Eio.Flow.copy` reads the response as it writes the file. `Fetch.stream`
supplies an upload from an Eio source; `~length` declares the byte count.
The upload is kept small because the Proffer backend buffers request bodies.

The program writes `fetch-tutorial.bin` in the system temporary directory,
replacing that file if it exists. It prints the saved path and the received
upload size. A streamed body cannot be replayed for a retry or redirect.

Continue with [test client code in memory](../c-mock/README.md), or return to the
[Fetch examples](../../README.md#fetch).
