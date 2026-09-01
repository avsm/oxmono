# `d-config`

<br>

The server has a few limits that protect it from slow or misbehaving clients.
This example changes them, and lets the operating system choose the port:

```ocaml
let config =
  { Proffer_httpz.default_config with
    max_connections = 100;
    request_timeout = 5.0;
    idle_timeout = 10.0 }

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~port:0 ~config ~env:() site
```

<pre><code><b>$ cd proffer/example/d-config</b>
<b>$ dune exec ./config.exe</b>
Running at http://localhost:43295
</code></pre>

<br>

Start from `default_config` and change only the fields you need, so that any
setting added in a later release keeps a sensible default.

- `max_connections` is how many clients may be connected at once. Further
  connections wait in the operating system's queue until a slot frees up.
- `request_timeout` is how long a client has to send a complete request. A
  client that dawdles past it gets `408 Request Timeout` and is disconnected.
- `idle_timeout` is how long a connection may sit open between requests
  before the server closes it.
- `backlog` is the length of that operating system queue.

The defaults allow 512 connections, 15 seconds per request and 75 seconds of
idleness.

<br>

Port 0 asks the operating system for any free port. The "Running at" line
comes from the default `on_listening` callback, which is given the address
actually bound once the socket is ready. Pass your own `on_listening` to learn
the port programmatically, which is handy in tests where several servers run
side by side. To listen somewhere other than the loopback interface, or on a
Unix socket, pass `addr` instead of `port`, as in
`` ~addr:(`Unix "/tmp/proffer.sock") ``.

<br>

The server accepts requests of up to about 32 KiB, head and body together,
and answers a larger one with `413 Payload Too Large`. It is designed for
sites that receive forms and API calls rather than large uploads.

<br>

**Next steps:**

- [**`e-json`**](../e-json#readme) builds a JSON API from typed values.
- [**`f-markdown`**](../f-markdown#readme) serves a Markdown document as HTML
  or as source.

<br>

[Up to the tutorial index](../#readme)
