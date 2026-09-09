# WebSockets

`httpz.websocket` provides RFC 6455 version-13 handshakes and a bounded
connection implementation. It has no Cstruct, Eio or socket dependency. HTTP
parsing, TLS, endpoint selection and deadlines belong to the caller.
Compression, extensions and HTTP/2 extended CONNECT are unsupported.

`Handshake.request` takes 16 bytes from a cryptographic random generator.
Send its fields with an HTTP/1.1 GET and Host. Pass the response status and
all fields to `Handshake.verify`. Preserve bytes read beyond the HTTP header
when transferring the connection to `create`. Never treat an unsuccessful
upgrade as a WebSocket connection. The library does not follow redirects.

On the server, check authorization and Origin policy, then pass all request
fields, including duplicates, to `Handshake.accept`. Proffer removes
Connection after parsing, so add a synthetic `Connection: Upgrade` field only
when `Req.connection_upgrade req` is true. Send the returned response fields
with status 101. Proffer's `Resp.upgrade` already supplies Connection and
Upgrade, so remove those two fields from the returned list before supplying
the remaining fields as `~headers`. Proffer lends the upgraded socket through
`Body.Socket`, including input already buffered after the request header.
The [Proffer integration test](../../proffer/test/test_httpz.ml) contains a
compiled echo handler and a TCP handshake/data/close exchange.

Once upgraded, a single-fiber server can use:

```ocaml
module W = Httpz_websocket

let echo socket =
  let ws = W.create ~role:Server
      ~read:(Proffer.Body.Socket.read socket)
      ~write:(Proffer.Body.Socket.write_sub socket)
      ~with_write_lock:(fun f -> f ()) () in
  while W.receive ws ~f:(fun kind bytes ~off ~len ->
    W.send ws kind bytes ~off ~len) do
    ()
  done
```

For concurrent sending and receiving in Eio, serialize every frame using a
mutex, including automatic pong and close replies. The callback is local, so
use the mutex's low-level operations:

```ocaml
let mutex = Eio.Mutex.create () in
let with_write_lock f =
  Eio.Mutex.lock mutex;
  match f () with
  | () -> Eio.Mutex.unlock mutex
  | exception exn -> Eio.Mutex.unlock mutex; raise exn
in
(* Pass with_write_lock to W.create. *)
```

Keep the connection on one domain and run exactly one receiver. All I/O must
have bounded timeouts. Cancellation or a transport failure invalidates the
connection. Suspended operations recheck failure when their callbacks return.
Close the underlying transport in the owner's cleanup scope to interrupt
I/O already in progress.
`close` sends the closing frame. Continue receiving under a deadline until
the peer closes, then release the transport. Pings still receive pongs while
waiting for the peer's close. A server acknowledges client-only close code
1010 with code 1000 and no reason. Other valid close payloads are echoed.
A protocol error invalidates the connection and requires transport teardown.
It does not emit another frame.

Receive storage grows up to `max_message`, default 16 MiB. Fragmentation is
bounded separately, default 1024 frames per message. Control frames use a
125-byte buffer. Complete messages are lent to the callback until it returns.
Copy the slice if it must outlive that callback. UTF-8 is checked after
assembly, allowing codepoints to span fragment boundaries.

Server sends borrow their payload. Client sends preserve the caller's payload
and mask through reusable 4 KiB scratch storage, obtaining a fresh four-byte
key from the supplied random source for each frame. Callers must not mutate
or reuse an outgoing payload until `send` returns. Frame parsing returns an
unboxed record. Parsing, header writing and masking have compiler-checked
`[@zero_alloc]` contracts. This implementation targets 64-bit OxCaml.

The [protocol tests](test/test_websocket.ml) cover RFC handshake and masking
vectors, extended length boundaries, wrong-direction masks, malformed control
frames, fragmentation, interleaved pings, UTF-8, close, bounds and transport
failure across suspended Eio fibers. They are deterministic local tests, not
an Autobahn conformance run.

```sh
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force \
  bleeding/httpz/websocket/test
```

Protocol reference: [RFC 6455](https://www.rfc-editor.org/rfc/rfc6455),
sections 4 through 8. No extension is negotiated, and reserved bits or
unsolicited extension responses are rejected.
