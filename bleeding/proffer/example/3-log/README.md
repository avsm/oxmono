# `3-log`

<br>

It is useful to see what a server is doing. Proffer's backend can call a
function of your choice each time it finishes answering a request. This example
prints who asked for what, the status they got, and how long it took:

```ocaml
let log (event : Proffer_httpz.event) =
  Printf.printf "%s %s %s -> %d in %d us\n%!"
    event.remote_addr
    (Method.to_string event.meth)
    event.path
    (Status.code event.status)
    event.duration_us

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~on_event:log ~env:() site
```

<pre><code><b>$ cd proffer/example/3-log</b>
<b>$ dune exec ./log.exe</b>
Running at http://localhost:8765
</code></pre>

<br>

Make a few requests, and a line appears for each one:

<pre><code>127.0.0.1:43010 GET / -> 200 in 38 us
127.0.0.1:43026 GET /echo/hi -> 200 in 10 us
127.0.0.1:43040 GET /nope -> 404 in 8 us
</code></pre>

The `path` field is the target with the query removed. `event.target` has the
whole thing, query included, but a query is whatever the client put there --
tokens and session identifiers among it -- so an access log is better off with
the path. For the same reason the `Authorization`, `Proxy-Authorization` and
`Cookie` values in `event.request_headers` read as `<redacted>`; handlers still
see the real ones.

The `event` record also carries the request headers, the content type and
size of the response, and any `X-Cache` value the handler set. Nothing is printed for
you, so the format and the destination are entirely yours: write to the
terminal, to a file, or to whatever logging library you already use.

Remember that `on_error` is a separate callback. It reports problems such as
an exception raised inside a handler, which `on_event` does not see.

<br>

**Next steps:**

- [**`4-counter`**](../4-counter#readme) shares state between requests.
- [**`5-form`**](../5-form#readme) reads submitted forms.

<br>

[Up to the tutorial index](../#readme)
