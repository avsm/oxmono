# `1-read`

<br>

This is the simplest request. It fetches a page and prints it:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in
  print_string (Fetch.read client (base ^ "/hello"))
```

<pre><code><b>$ cd fetch/example/1-read</b>
<b>$ dune exec ./read.exe</b>
server: GET /hello -> 200
Hello from the local server!
</code></pre>

<br>

The program has three lines that matter. The first starts Eio, the
concurrency library Fetch is built on, and hands over `env`, which holds the
capabilities of the machine such as the network and the clock. The second
starts the tutorial's [local server](../#the-local-server) and passes its
address as `base`. Replace `base ^ "/hello"` with any `http://` address and
the example still works.

The third line makes a *client*. A client is the value that has the authority
to make requests, and everything in this tutorial starts by making one.
`Fetch_httpz.std` makes a client with the defaults most programs want. It
remembers cookies, retries requests that fail for temporary reasons, and
avoids sending too many requests to one site at once. Later examples look at
each of these.

Finally, `Fetch.read` sends a `GET` request for the URL and returns the body
as a string. It is the right call when a page is small and all you need is
its contents. The body is limited to 16 MiB, which can be changed with its
`limit` argument. Whatever the status of the response, `read` returns the
body, so a "not found" page comes back just as a successful one does.

<br>

**Next steps:**

- The next example, [**`2-response`**](../2-response#readme), looks at the
  status and headers of a response as well as the body.
- [**`3-post`**](../3-post#readme) sends data to a server.

<br>

[Up to the tutorial index](../#readme)
