# Serve a response

This program serves `GET /` on loopback port 8765. Start it before the
[first Fetch example](../../fetch/1-read/README.md).

[hello.ml](hello.ml) contains the complete program.
[dune](dune) declares its library dependencies.

```ocaml
open Proffer
open Proffer.Route

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Hello from Proffer!\n") ]

let () =
  Eio_main.run @@ fun env ->
  Proffer_httpz.run env ~env:() site
```

`get root` matches the root path. Its handler receives the application
environment, the request and a response callback. `Resp.text` sends a text
response through that callback. `~env:()` supplies an empty application
environment. `Eio_main.run` supplies the networking and clock capabilities.

From the repository root, run the server in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/1-hello/hello.exe
```

Leave it running. In another terminal, request the root path.

```sh
curl http://127.0.0.1:8765/
```

```text
Hello from Proffer!
```

Run the [Fetch client](../../fetch/1-read/README.md) against this
server, or stop it with Ctrl-C and continue with
[path matching](../2-router/README.md).
