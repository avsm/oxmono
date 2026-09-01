# `d-curl`

<br>

Everything so far used `fetch-httpz`, the backend written entirely in OCaml.
The `fetch-curl` backend drives libcurl instead, and the same code runs on
either. This example is [**`1-read`**](../1-read#readme) on libcurl, and it
also accepts a URL of your own on the command line:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = Fetch_curl.std ~sw env in
  if Array.length Sys.argv > 1 then print_string (Fetch.read client Sys.argv.(1))
  else
    Localhost.run env @@ fun base ->
    print_string (Fetch.read client (base ^ "/hello"))
```

<pre><code><b>$ cd fetch/example/d-curl</b>
<b>$ dune exec ./curl.exe</b>
server: GET /hello -> 200
Hello from the local server!
<b>$ dune exec ./curl.exe -- https://example.com/</b>
&lt;!doctype html&gt;&lt;html lang="en"&gt;&lt;head&gt;&lt;title&gt;Example Domain&lt;/title&gt;...
</code></pre>

<br>

`Fetch_curl.std` needs a switch, because a libcurl client keeps connections
open between requests and runs a little machinery in the background to drive
them. Both are released when the switch finishes. Apart from that, the
client is used exactly as before, and every wrapper from the earlier
examples applies to it unchanged.

libcurl brings a few things with it. HTTPS works out of the box, checking
certificates against the system's trust store, which is why the second
command needs no setup. Connections are reused across requests to the same
site, and HTTP/2 is used where a site offers it. Many small requests to one
site are noticeably faster this way. The cost is a dependency on a C
library, and a client that must be used from the domain that created it.

`Fetch_curl.v` exposes libcurl's own settings, such as a proxy, connection
limits per host, and a transfer timeout, for programs that need them.

<br>

Choose `fetch-httpz` for a pure OCaml build, for control over TLS, or when
each request opens its own connection anyway. Choose `fetch-curl` for
throughput to a busy API, or when HTTPS should just work. Since the code is
the same, it is easy to change your mind.

<br>

**Next steps:**

- [**`e-https`**](../e-https#readme) adds HTTPS to the pure OCaml backend.

<br>

[Up to the tutorial index](../#readme)
