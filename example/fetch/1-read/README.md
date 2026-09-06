# Read the Proffer response

Start the [Proffer greeting server](../../proffer/1-hello/README.md)
and leave it running. This client requests its root path on port 8765.

[read.ml](read.ml) contains the complete program.
[dune](dune) declares its library dependencies.

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let client = Fetch_httpz.std env in
  print_string (Fetch.read client "http://127.0.0.1:8765/")
```

From another terminal at the repository root, run the client in the
configured switch.

```sh
dune exec --profile release-check ./example/fetch/1-read/read.exe
```

```text
Hello from Proffer!
```

`Fetch_httpz.std` creates a client with cookies, retries and per-origin
request limits. `Fetch.read` sends a GET request and returns its body as a
string, regardless of status, with a default limit of 16 MiB. Replace the
URL to contact another server.

This example does not use `Localhost`. Later examples use the repository's
[Localhost library](../../localhost/README.md) to start a server automatically.
Stop the greeting server with Ctrl-C when finished.

Continue with [response inspection](../2-response/README.md), or return to
the [Fetch examples](../../README.md#fetch).
