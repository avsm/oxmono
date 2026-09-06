# Retain cookies

This example stores the session cookie set by `/login` and sends it to `/account`.

[cookies.ml](cookies.ml) contains the complete program.
[dune](dune) declares its library dependencies.

The program starts its own Proffer server through the example-only
[Localhost library](../../localhost/README.md). No separate server is required.

From the repository root, run the program in the configured switch.

```sh
dune exec --profile release-check ./example/fetch/8-cookies/cookies.exe
```

`Fetch_cookies.Jar.in_memory` creates the jar. `Fetch_cookies.with_jar`
attaches it to a client. The login response redirects to `/account`, so the
cookie is used on the next request. A client created with `~cookies` set to `` `Off ``
receives an unauthorized response. The example creates no cookie file.

Continue with [retry temporary failures](../9-retry/README.md), or return to the
[Fetch examples](../../README.md#fetch).
