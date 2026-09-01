# `9-retry`

<br>

Servers have bad moments. The local server's `/flaky` page fails twice with
`503 Service Unavailable` and then succeeds on the third try. This example
fetches it with the default client, and then with one told never to retry:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->

  let client = Fetch_httpz.std env in
  print_string (Fetch.read client (base ^ "/flaky"));

  let impatient = Fetch_httpz.std ~retry:(Fetch.Retry.v ~max_retries:0 ()) env in
  print_string (Fetch.read impatient (base ^ "/flaky"))
```

<pre><code><b>$ cd fetch/example/9-retry</b>
<b>$ dune exec ./retry.exe</b>
server: GET /flaky -> 503
server: GET /flaky -> 503
server: GET /flaky -> 200
Third time lucky!
server: GET /flaky -> 503
Try again later.
</code></pre>

<br>

The first `read` made three requests, as the server's log shows, and
returned the successful one. Nothing in the program asked for that: the `std`
client retries on its own. It does so for the statuses that mean "not right
now", which are 429, 500, 502, 503 and 504, and for a connection that could
not be made. It waits a little longer before each attempt, half a second,
then one second, then two, and honours a `Retry-After` header when the server
sends one, as the flaky page does.

Only requests that are safe to repeat are retried. A `GET` is; a `POST` is
not, since the server may have acted on the first attempt even though the
reply was lost. A request whose body is streamed from a file cannot be
replayed either.

<br>

`Fetch.Retry.v` builds a policy with any of the defaults changed:
`max_retries` for how many further attempts to make, `status_forcelist` for
which statuses count as temporary, `allowed_methods` for which methods may be
repeated, and `backoff_factor` and `backoff_max` for the waiting. Pass the
policy as `retry` to `std`, or use it with `Fetch.with_retry` to add retrying
to any client.

The retried request also passes through the cookie jar and the rate limit
again each time, so a fresh attempt is a proper one.

<br>

**Next steps:**

- [**`a-limits`**](../a-limits#readme) spaces requests out.
- [**`b-stream`**](../b-stream#readme) handles large bodies.

<br>

[Up to the tutorial index](../#readme)
