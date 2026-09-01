# `7-cache`

<br>

Browsers and proxies can keep a copy of a response and reuse it, if the server
tells them how long it stays good for. This example serves a logo that never
changes, a report that is expensive to produce, and a clock that must never be
kept:

```ocaml
type env = { clock : float Eio.Time.clock_ty Eio.Resource.t; cache : Cache.t }

let logo = "<!doctype html>\n<h1>The logo never changes</h1>\n"
let logo_etag = Etag.strong (Digest.to_hex (Digest.string logo))

let expensive_report env () =
  Eio.Time.sleep env.clock 2.0;
  Printf.sprintf "Report generated at %.0f\n" (Eio.Time.now env.clock)

let site =
  Site.of_routes
    [ get (s "logo") (fun _env _request respond ->
        Resp.html respond ~etag:logo_etag
          ~cache:(Cache_control.public ~max_age:(`Days 365) ~immutable:true ())
          logo);

      get (s "report") (fun env _request respond ->
        let body, etag =
          Cache.memoize env.cache ~now:(Eio.Time.now env.clock) ~key:"report"
            (expensive_report env)
        in
        Resp.media respond ~etag "text/plain" body);

      get (s "clock") (fun env _request respond ->
        Resp.media respond ~cache:Cache_control.no_store "text/plain"
          (Printf.sprintf "%.0f\n" (Eio.Time.now env.clock))) ]
```

<pre><code><b>$ cd proffer/example/7-cache</b>
<b>$ dune exec ./cache.exe</b>
Running at http://localhost:8765
</code></pre>

<br>

The `cache` argument of a response sets its `Cache-Control` header. The
`Cache_control` module builds the common policies without you having to spell
out the directives. `public ~max_age` lets anyone keep the response for that
long, `private'` limits that to the visitor's own browser, and `no_store`
forbids keeping it at all. Durations are written as `` `Secs``, `` `Hours``
or `` `Days``.

The `etag` argument attaches an *entity tag*, a short string that changes
whenever the content does. Here it is a digest of the page. A client that
already has the page can send the tag back in an `If-None-Match` header, and
Proffer compares the two for you. When they match it answers
`304 Not Modified` with no body, and the handler's response is discarded
before it goes on the wire:

<pre><code><b>$ curl -i http://localhost:8765/logo</b>
HTTP/1.1 200 OK
Date: Wed, 02 Sep 2026 09:31:55 GMT
Content-Type: text/html; charset=utf-8
Cache-Control: public, max-age=31536000, immutable
ETag: "45bdb6004d54f4c4f22f8bcac7806547"
Content-Length: 48
Connection: keep-alive

&lt;!doctype html&gt;
&lt;h1&gt;The logo never changes&lt;/h1&gt;
<b>$ curl -i http://localhost:8765/logo -H 'If-None-Match: "45bdb6004d54f4c4f22f8bcac7806547"'</b>
HTTP/1.1 304 Not Modified
Date: Wed, 02 Sep 2026 09:31:55 GMT
Cache-Control: public, max-age=31536000, immutable
ETag: "45bdb6004d54f4c4f22f8bcac7806547"
Connection: keep-alive
</code></pre>

A `last_modified` time works the same way with `If-Modified-Since`.

<br>

The report is a different kind of caching: it is the server that remembers
the result. `Cache.memoize` looks up a key in a cache created at startup and
calls the function only when the entry is missing or older than the cache's
time to live, ten seconds here. It returns the body together with an entity
tag it computed, ready to pass to the response. The first request takes two
seconds and the next ones are immediate:

<pre><code><b>$ time curl http://localhost:8765/report</b>
Report generated at 1788341517

real	0m2.005s
<b>$ time curl http://localhost:8765/report</b>
Report generated at 1788341517

real	0m0.005s
</code></pre>

The clock is passed in through `env`, as in [**`4-counter`**](../4-counter#readme),
so that handlers can ask for the time and sleep without touching anything
global.

<br>

**Next steps:**

- [**`8-stream`**](../8-stream#readme) sends a response piece by piece.
- [**`9-auth`**](../9-auth#readme) protects part of the site with a password.

<br>

[Up to the tutorial index](../#readme)
