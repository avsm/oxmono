# `a-limits`

<br>

A program that fires off requests as fast as it can is a nuisance to the site
on the other end, and may be cut off for it. This example starts six requests
at once, with a client that allows at most two in flight and at least half a
second between starts:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let clock = Eio.Stdenv.clock env in
  let client = Fetch_httpz.std ~max_concurrent:2 ~min_interval:0.5 env in
  let started = Eio.Time.now clock in
  Eio.Fiber.List.iter
    (fun i ->
      ignore (Fetch.read client (base ^ "/hello"));
      Printf.printf "Request %d finished after %.1f seconds\n%!" i
        (Eio.Time.now clock -. started))
    [ 1; 2; 3; 4; 5; 6 ]
```

<pre><code><b>$ cd fetch/example/a-limits</b>
<b>$ dune exec ./limits.exe</b>
server: GET /hello -> 200
Request 1 finished after 0.0 seconds
server: GET /hello -> 200
Request 2 finished after 0.5 seconds
server: GET /hello -> 200
Request 3 finished after 1.0 seconds
server: GET /hello -> 200
Request 4 finished after 1.5 seconds
server: GET /hello -> 200
Request 5 finished after 2.0 seconds
server: GET /hello -> 200
Request 6 finished after 2.5 seconds
</code></pre>

<br>

`Eio.Fiber.List.iter` runs the six requests concurrently, so without a limit
they would all finish at about the same moment. The client instead lets them
through one every half second. Both limits are counted per site, so a client
talking to two sites has a separate budget for each, and requests queue
quietly until it is their turn rather than failing.

`max_concurrent` caps how many requests to one site are in progress at once.
The `std` client allows six, which is what browsers do. `min_interval` sets
the smallest gap in seconds between the start of one request and the next,
and is unset by default. The same settings are available as
`Fetch.with_limits` for a client built by hand, where they can also be
confined to a `scope` of URL prefixes.

These limits are about being polite to a site. How many connections the
backend keeps open is a separate matter, which
[**`d-curl`**](../d-curl#readme) touches on.

<br>

**Next steps:**

- [**`b-stream`**](../b-stream#readme) handles large bodies.
- [**`c-mock`**](../c-mock#readme) tests code that makes requests.

<br>

[Up to the tutorial index](../#readme)
