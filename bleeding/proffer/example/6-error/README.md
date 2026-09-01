# `6-error`

<br>

Two things apply to a whole site rather than to one route: what to do when
nothing matches, and headers that every response should carry. This example
sets both, and also redirects an old address permanently:

```ocaml
let not_found () request respond =
  Resp.text respond ~status:Not_found
    ("Sorry! There is nothing at " ^ Req.path request ^ "\n")

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Good morning, world!\n");
      post (s "echo") (fun () request respond ->
        Resp.text respond (Req.body request));
      moved (s "old") "/" ]
  |> Site.with_fallback not_found
  |> Site.with_headers [ ("Server", "proffer-tutorial") ]
```

<pre><code><b>$ cd proffer/example/6-error</b>
<b>$ dune exec ./error.exe</b>
Running at http://localhost:8765
</code></pre>

<br>

`Site.with_fallback` replaces the plain `404 Not Found` that a site sends by
default. The fallback is an ordinary handler, so it can look at the request and
use any of the `Resp` functions. Every `Resp` shortcut accepts a `status`, and
here it is set to `Not_found` so that browsers and search engines still learn
that the page does not exist:

<pre><code><b>$ curl -i http://localhost:8765/nothing/here</b>
HTTP/1.1 404 Not Found
Date: Wed, 02 Sep 2026 09:31:37 GMT
Server: proffer-tutorial
Content-Type: text/plain; charset=utf-8
Content-Length: 41
Connection: keep-alive

Sorry! There is nothing at /nothing/here
</code></pre>

The fallback only runs when no route matches the path at all. A path that
exists under a different method still gets `405 Method Not Allowed`, as
`GET /echo` does here.

<br>

`Site.with_headers` adds the given headers to every response, including the
fallback and the responses Proffer generates itself, such as that 405. The
`Server` header above came from it. A few headers are managed by the backend
and cannot be set this way: `Content-Length`, `Transfer-Encoding` and
`Connection`.

<br>

`moved` is a route that answers with `301 Moved Permanently` and a fixed
`Location`. It is the tidy way to keep old links working after a page moves.
`found` does the same with a `302 Found`, which tells clients the move is
temporary. For a redirect whose target depends on the request, use an ordinary
`get` route with `Resp.redirect`.

<pre><code><b>$ curl -i http://localhost:8765/old</b>
HTTP/1.1 301 Moved Permanently
Date: Wed, 02 Sep 2026 09:31:37 GMT
Location: /
Server: proffer-tutorial
Content-Length: 0
Connection: keep-alive
</code></pre>

<br>

**Next steps:**

- [**`7-cache`**](../7-cache#readme) tells browsers what they may keep.
- [**`8-stream`**](../8-stream#readme) sends a response piece by piece.

<br>

[Up to the tutorial index](../#readme)
