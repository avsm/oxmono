# Use a mock without a server

This program calls the message endpoint against an in-memory Fetch backend.
It needs no credentials, profile, Docker, or network access.

```sh
dune exec example/r-mock/mock.exe
```

Expected output:

```text
POST /api/v1/messages
Sent message 42
```

The [backend function](mock.ml) prints the request's method and path, then
returns a small Zulip success response. `Fetch_mock.client` turns that function
into a Fetch client, which is injected into the transport:

```ocaml
let transport = Transport.of_fetch (Fetch_mock.client backend) in
```

`Auth.create` and `Client.create` supply the normal request configuration. The
`zulip.example` address and `mock-key` are dummy values; every request is handled
by the function above. No connection is opened to that address.

The remaining code calls `Messages.send_channel` and matches its result. The
client still encodes the form and decodes the response's message ID through
the usual implementation. Only HTTP delivery has been replaced.

Try changing the response to an API error:

```ocaml
Fetch_mock.respond ~status:403
  ~headers:(Http.Header.of_list [ ("content-type", "application/json") ])
  {|{"result":"error","code":"FORBIDDEN","msg":"Not allowed"}|} request
```

The `Error` branch will print the failure. A mock supplies whatever behavior
you describe; it does not prove a real Zulip server accepts a request. The
[Docker suite](../../test/integration/README.md) tests that separately.

[All examples](../README.md) · [Source](mock.ml) ·
[Transport API](../../lib/zulip_eio/transport.mli) · [Build file](dune)
