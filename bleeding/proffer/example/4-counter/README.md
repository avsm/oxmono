# `4-counter`

<br>

Most sites need some state that outlives a single request: a database
connection, a configuration, or in this example simply a counter. Proffer
passes such state to every handler as its first argument. This example counts
the requests it has seen and reports the total:

```ocaml
type env = { count : int ref }

let site =
  Site.of_routes
    [ get root (fun env _request respond ->
        incr env.count;
        Resp.text respond (Printf.sprintf "Saw %d request(s)!\n" !(env.count))) ]

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~env:{ count = ref 0 } site
```

<pre><code><b>$ cd proffer/example/4-counter</b>
<b>$ dune exec ./counter.exe</b>
Running at http://localhost:8765
</code></pre>

<pre><code><b>$ curl http://localhost:8765/</b>
Saw 1 request(s)!
<b>$ curl http://localhost:8765/</b>
Saw 2 request(s)!
</code></pre>

<br>

The first argument of a handler has been `()` until now. It can be any type
you like, and the type of the site records what it is. The value itself is
supplied when the server starts, through the `env` argument of
`Proffer_httpz.run`. The site is written without knowing that value, which
keeps handlers easy to test: the same site can be run
with a different `env` under [**`c-mock`**](../c-mock#readme).

This example keeps a plain reference. That is safe because the server runs
all of its handlers in one domain, so two of them never touch the counter at
the same instant. State shared with other domains needs the usual care.

<br>

**Next steps:**

- [**`5-form`**](../5-form#readme) reads forms and query strings.
- [**`6-error`**](../6-error#readme) customises the "not found" page.

<br>

[Up to the tutorial index](../#readme)
