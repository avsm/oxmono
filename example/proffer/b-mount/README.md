# Compose sites

This example mounts an API site beneath `/api/v1`.

[mount.ml](mount.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/b-mount/mount.exe
```

With the server running, send requests from another terminal.

```sh
curl http://127.0.0.1:8765/api/v1
curl http://127.0.0.1:8765/api/v1/users/alice
```

`Site.mount ~at:[ "api"; "v1" ]` adds a path prefix to the mounted routes.
The API defines its own root and `/users/:name` routes. The outer site
adds a `Server` header to their responses.

Stop the server with Ctrl-C before running another server example.

Continue with [test a site in memory](../c-mock/README.md), or return to the
[Proffer examples](../../README.md#proffer).
