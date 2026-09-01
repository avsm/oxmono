# `1-hello`

<br>

This is the smallest Proffer server. It answers every request for its home page
with the same friendly message:

```ocaml
open Proffer
open Proffer.Route

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Good morning, world!\n") ]

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~env:() site
```

<pre><code><b>$ cd proffer/example/1-hello</b>
<b>$ dune exec ./hello.exe</b>
Running at http://localhost:8765
</code></pre>

If you open [http://localhost:8765](http://localhost:8765) in a browser, or
ask for it with curl, you get the greeting back as plain text:

<pre><code><b>$ curl -i http://localhost:8765/</b>
HTTP/1.1 200 OK
Date: Wed, 02 Sep 2026 09:31:36 GMT
Content-Type: text/plain; charset=utf-8
Content-Length: 21
Connection: keep-alive

Good morning, world!
</code></pre>

<br>

The program has two halves. The first half describes the site. A site is a
list of routes, and each route pairs a path with a *handler*, the function
that produces the response. Here there is one route, `get root`, which matches
a `GET` request for the root path `/`.

A handler is called with three things: the application's own state (we have
none yet, so it is `()`), the request, and a function named `respond`. The
handler describes its response by handing it to `respond` exactly once.
`Resp.text` is a shortcut that builds a plain-text response and passes it on,
so the handler does not need to write any headers itself. There are similar
shortcuts for HTML, JSON and other kinds of content, which later examples use.

The second half runs the site. `Eio_main.run` starts Eio, the concurrency
library Proffer is built on, and hands over `stdenv`, which holds the
network and the clock. `Proffer_httpz.run` takes that, the application's own
state for `env`, and the site, and serves it until the program is stopped. By
default it listens on the loopback interface at port 8765, prints where it
can be reached once the socket is ready, and reports anything that goes wrong
while serving a request on standard error. Each of those can be changed
through an optional argument, which [**`d-config`**](../d-config#readme)
shows.

You will see this second half again in almost every example. Only the site in
the first half changes.

<br>

**Next steps:**

- The next example, [**`2-router`**](../2-router#readme), serves different
  pages at different paths.
- [**`3-log`**](../3-log#readme) prints a line for every request.

<br>

[Up to the tutorial index](../#readme)
