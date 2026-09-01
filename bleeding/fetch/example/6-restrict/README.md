# `6-restrict`

<br>

A client can be narrowed so that it reaches only certain sites, or only
reads. This example makes a client that can talk to the local server and
nothing else, then one that can only make safe requests:

```ocaml
let denied f =
  try f () with Eio.Io (Fetch.E (Fetch.Denied reason), _) ->
    Printf.printf "Denied: %s\n" reason

let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in

  let local = Fetch.restrict ~under:[ base ] client in
  print_string (Fetch.read local (base ^ "/hello"));
  denied (fun () -> ignore (Fetch.read local "http://example.com/"));

  let read_only = Fetch.read_only local in
  print_string (Fetch.read read_only (base ^ "/hello"));
  denied (fun () ->
    Eio.Switch.run @@ fun sw ->
    ignore (Fetch.post ~sw read_only ~body:(String "hi") (base ^ "/echo")))
```

<pre><code><b>$ cd fetch/example/6-restrict</b>
<b>$ dune exec ./restrict.exe</b>
server: GET /hello -> 200
Hello from the local server!
Denied: url http://example.com/ not permitted
server: GET /hello -> 200
Hello from the local server!
Denied: method POST not permitted by a read-only client
</code></pre>

<br>

`Fetch.restrict` returns a new client that accepts only requests matching the
given rules, and passes those on to the original. `under` is a list of URL
prefixes. A request is allowed when it is on the same site as one of them,
meaning the same scheme, host and port, and its path starts with the prefix's
path. So `https://api.example.com` allows the whole of that site, while
`https://api.example.com/v2` allows only paths beneath `/v2`. `methods` limits
the request methods in the same way. A request outside the rules is refused
with `Denied` before anything is sent.

`Fetch.read_only` is a shorthand for allowing only `GET`, `HEAD` and
`OPTIONS`, the methods that are not supposed to change anything.

<br>

The point of narrowing is that a client can be handed to code you do not
fully trust, or simply to code that has no business reaching anywhere else.
The narrowed client gives no way back to the original, so a library given
`local` cannot reach `example.com` however it tries, and the rules apply to
every redirect as well. Narrowing can be repeated, and each step can only
take away.

For rules that a prefix cannot express, such as allowing every subdomain of a
host, `restrict` also takes a `filter` function that sees each request and
decides.

<br>

**Next steps:**

- [**`7-credentials`**](../7-credentials#readme) attaches a token to requests
  for one site.
- [**`8-cookies`**](../8-cookies#readme) remembers cookies between requests.

<br>

[Up to the tutorial index](../#readme)
