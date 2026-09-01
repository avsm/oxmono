# `e-https`

<br>

The pure OCaml backend includes TLS for `https://`. Its standard constructor
uses the operating system's certificate store and verifies both DNS names and
IP-address certificate identities:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let client = Fetch_httpz.std env in
  let url = if Array.length Sys.argv > 1 then Sys.argv.(1) else "https://example.com/" in
  print_string (Fetch.read client url)
```

<pre><code><b>$ cd fetch/example/e-https</b>
<b>$ dune exec ./https.exe</b>
&lt;!doctype html&gt;&lt;html lang="en"&gt;&lt;head&gt;&lt;title&gt;Example Domain&lt;/title&gt;...
</code></pre>

<br>

`Fetch_httpz.std` installs `Httpz_tls.system`, which uses the system trust
store and a fresh explicit random-number generator for each client connection.
A client that only talks `http://` performs no TLS handshake.

<br>

Applications can still replace the policy. Pass
`~https:(Httpz_tls.client ~authenticator)` for private trust roots or pinning,
pass any other `Fetch_httpz.https` implementation for a different TLS stack,
or pass `~https:Fetch_httpz.no_https` to make the standard constructor reject
HTTPS explicitly. The bare `Fetch_httpz.v` constructor also leaves TLS absent
unless a wrapper is supplied.

<br>

**Next steps:**

- [**`f-json`**](../f-json#readme) reads and writes JSON as typed values.
- [**`g-markdown`**](../g-markdown#readme) fetches a Markdown document and
  renders it.

<br>

[Up to the tutorial index](../#readme)
