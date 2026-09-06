# Protect routes

This example requires an HTTP Basic credential beneath `/admin`.

[auth.ml](auth.ml) contains the complete program.
[dune](dune) declares its library dependencies.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/proffer/9-auth/auth.exe
```

With the server running, send requests from another terminal.

```sh
curl -i http://127.0.0.1:8765/admin
curl --user alice:secret http://127.0.0.1:8765/admin/settings
```

`Site.with_auth` applies the credential check to the configured path
prefix. A request without the credential receives 401 and a Basic challenge.
The example credential is `alice:secret`. The root path remains public.
This example uses plaintext loopback HTTP. Credentials sent to a remote
server require HTTPS.

Stop the server with Ctrl-C before running another server example.

Continue with [select a representation](../a-negotiate/README.md), or return to the
[Proffer examples](../../README.md#proffer).
