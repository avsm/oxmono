# `4-redirect`

<br>

Pages move. When a server answers with a redirect, Fetch follows it and
returns the page at the new address. This example fetches an old address and
sees where it ended up, then asks for the same address without following:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in
  Eio.Switch.run @@ fun sw ->

  let response = Fetch.get ~sw client (base ^ "/old") in
  Printf.printf "Status %d from %s\n%!" (Fetch.status response) (Fetch.url response);
  Eio.Flow.copy (Fetch.body response) (Eio.Stdenv.stdout env);

  let response = Fetch.get ~sw ~redirects:0 client (base ^ "/old") in
  Printf.printf "Status %d pointing at %s\n" (Fetch.status response)
    (Option.get (Fetch.header Fetch.Header.location response))
```

<pre><code><b>$ cd fetch/example/4-redirect</b>
<b>$ dune exec ./redirect.exe</b>
server: GET /old -> 301
server: GET /hello -> 200
Status 200 from http://127.0.0.1:38535/hello
Hello from the local server!
server: GET /old -> 301
Status 301 pointing at /hello
</code></pre>

<br>

The server's log shows the two requests behind the first `get`: the old
address answered `301 Moved Permanently`, and Fetch asked for `/hello` in its
place. `Fetch.url` reports the final address, which is how you find out that a
redirect happened.

Up to ten redirects are followed by default, and the `redirects` argument
changes that. With zero, the redirect itself is returned. Its `Location`
header holds the new address, read here with the `location` codec.

<br>

Fetch follows the rules browsers use. A `303 See Other`, the redirect a
server sends after a form is submitted, turns the request into a `GET` and
drops the body. A `307` or `308` repeats the request as it was. A redirect
from an `https://` address to a plain `http://` one is refused rather than
followed, and when a redirect leads to a different site, any credentials and
cookies attached to the request are left behind. The next examples show where
those come from.

<br>

**Next steps:**

- [**`5-errors`**](../5-errors#readme) handles missing pages, unreachable
  servers and slow ones.
- [**`6-restrict`**](../6-restrict#readme) limits where a client may go.

<br>

[Up to the tutorial index](../#readme)
