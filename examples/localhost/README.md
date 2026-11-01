# Local server for the Fetch examples

`Localhost` is defined in [localhost.ml](localhost.ml). Its [dune](dune)
file declares a private library named `localhost`, linked by the Fetch
examples through `(libraries ... localhost)`. It is built from this checkout
and is not an installed Fetch module.

`Localhost.run env f` starts a Proffer server on an unused loopback port and
calls `f` with its base URL. The server stops when the callback finishes or
raises. Lines beginning with `server:` are its request log.

```ocaml
Localhost.run env @@ fun base ->
print_string (Fetch.read client (base ^ "/hello"))
```

The site provides greetings and headers, request-body echoing, forms,
redirects, cookies, credentials, delayed and retryable responses, and JSON
and Markdown documents. The paths used by each example are defined in
[localhost.ml](localhost.ml).

The server uses the route and handler structure from
[Proffer's first example](../proffer/1-hello/README.md).
Its selected port is reported through `on_listening`, as described in
[server configuration](../proffer/d-config/README.md).

The first Fetch example instead uses the manually started greeting server.
`c-mock` uses no server, and `e-https` contacts the URL supplied on its command
line or its public default. `d-curl` uses this local server only when no URL
is supplied. Return to the [Fetch examples](../README.md#fetch).
