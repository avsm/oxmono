# `a-negotiate`

<br>

A browser wants HTML, a script wants JSON, and a terminal wants plain text.
Clients say which they prefer in an `Accept` header, and this example serves
the same greeting in all three forms from one route:

```ocaml
let greeting =
  Negotiate.v
    [ (`Html, fun () _request respond ->
        Resp.html respond "<!doctype html>\n<h1>Good morning, world!</h1>\n");
      (`Json, fun () _request respond ->
        Resp.media respond "application/json"
          "{\"greeting\": \"Good morning, world!\"}\n");
      (`Other "text/plain", fun () _request respond ->
        Resp.text respond "Good morning, world!\n") ]

let site =
  Site.of_routes [ get root greeting ]
```

<pre><code><b>$ cd proffer/example/a-negotiate</b>
<b>$ dune exec ./negotiate.exe</b>
Running at http://localhost:8765
</code></pre>

<br>

`Negotiate.v` turns a list of handlers, one per media type, into a single
handler. It reads the client's `Accept` header, works out the client's order
of preference, and runs the first handler whose type appears in that order.
The types `` `Html``, `` `Json``, `` `Markdown`` and `` `Xml`` are named for
convenience, and `` `Other`` takes any full media type.

<pre><code><b>$ curl -H 'Accept: application/json' http://localhost:8765/</b>
{"greeting": "Good morning, world!"}
<b>$ curl -H 'Accept: text/plain' http://localhost:8765/</b>
Good morning, world!
<b>$ curl -H 'Accept: text/plain;q=0.5, application/json' http://localhost:8765/</b>
{"greeting": "Good morning, world!"}
</code></pre>

When the client sends no `Accept` header, or nothing it lists is available,
the first handler in the list is used. That is why HTML comes first here:

<pre><code><b>$ curl -i http://localhost:8765/</b>
HTTP/1.1 200 OK
Date: Wed, 02 Sep 2026 09:32:02 GMT
Vary: Accept
Content-Type: text/html; charset=utf-8
Content-Length: 46
Connection: keep-alive

&lt;!doctype html&gt;
&lt;h1&gt;Good morning, world!&lt;/h1&gt;
</code></pre>

The response also gains a `Vary: Accept` header, which tells caches that the
answer depends on that request header and must not be reused for a client
that asked for something else.

<br>

**Next steps:**

- [**`b-mount`**](../b-mount#readme) builds a site from smaller sites.
- [**`c-mock`**](../c-mock#readme) tests a site without opening a network
  connection.

<br>

[Up to the tutorial index](../#readme)
