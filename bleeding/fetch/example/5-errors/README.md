# `5-errors`

<br>

Things go wrong in a few different ways, and this example provokes each of
them: a page that does not exist, a server that is not running, an address
that is not a URL, and a server that takes too long.

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.v (Eio.Stdenv.net env) () in

  Eio.Switch.run (fun sw ->
    let response = Fetch.get ~sw client (base ^ "/nothing") in
    Printf.printf "A missing page is an ordinary response with status %d.\n"
      (Fetch.status response));

  (try ignore (Fetch.read client "http://127.0.0.1:1/")
   with Eio.Io (Fetch.E (Fetch.Connection_failure _), _) ->
     print_endline "Nothing is listening on port 1.");

  (try ignore (Fetch.read client "not a url")
   with Eio.Io (Fetch.E (Fetch.Invalid_url reason), _) ->
     Printf.printf "That is not a URL: %s.\n" reason);

  match
    Eio.Time.with_timeout (Eio.Stdenv.clock env) 1.0 (fun () ->
      Ok (Fetch.read client (base ^ "/slow")))
  with
  | Ok body -> print_string body
  | Error `Timeout -> print_endline "Gave up waiting after one second."
```

<pre><code><b>$ cd fetch/example/5-errors</b>
<b>$ dune exec ./errors.exe</b>
server: GET /nothing -> 404
A missing page is an ordinary response with status 404.
Nothing is listening on port 1.
That is not a URL: not an absolute URL (missing scheme) ("not a url").
Gave up waiting after one second.
</code></pre>

<br>

A response is a response, whatever its status. A `404`, a `500` or any other
error status is returned normally, and it is up to you to check
`Fetch.status`. Fetch only raises an exception when there is no response to
give: the URL is malformed, the server cannot be reached, the connection
fails during the exchange, or a policy on the client refuses the request.

Those exceptions are all `Eio.Io`, the exception Eio uses for input and
output failures, carrying a `Fetch.E` value that says what kind of failure it
was. The example catches two kinds by name. Every kind is listed under
`Fetch.error`; the ones you will meet most are `Connection_failure`,
`Invalid_url`, `Tls_failure` for a certificate problem, and `Denied`, which
[**`6-restrict`**](../6-restrict#readme) introduces. An `Eio.Io` you do not
catch prints a readable description, including the request that failed.

<br>

Fetch has no timeout of its own. Instead, wrap a request in
`Eio.Time.with_timeout`, as the example does, or any other Eio cancellation.
When the time runs out the request is abandoned, the connection is dropped,
and you get `` Error `Timeout `` back. This works for any operation, however
many requests it makes, which is more useful than a per-request setting.

<br>

This example builds its client with `Fetch_httpz.v` rather than `std`. That is
the bare backend without retries or cookies, so that the failed connection is
reported at once. With `std`, a failed connection would be tried again a few
times first, as [**`9-retry`**](../9-retry#readme) explains.

<br>

**Next steps:**

- [**`6-restrict`**](../6-restrict#readme) limits where a client may go.
- [**`7-credentials`**](../7-credentials#readme) attaches a token to
  requests.

<br>

[Up to the tutorial index](../#readme)
