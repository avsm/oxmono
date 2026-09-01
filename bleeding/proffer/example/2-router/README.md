# `2-router`

<br>

A site usually has more than one page. This example serves a greeting at the
root, echoes back a word from the path, squares a number, and lists the parts
of any path under `/files`:

```ocaml
let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Good morning, world!\n");

      get (s "echo" / str) (fun word () _request respond ->
        Resp.text respond (word ^ "\n"));

      get (s "square" / int) (fun n () _request respond ->
        Resp.text respond (string_of_int (n * n) ^ "\n"));

      get (s "files" / rest) (fun segments () _request respond ->
        Resp.text respond (String.concat " / " segments ^ "\n")) ]
```

<pre><code><b>$ cd proffer/example/2-router</b>
<b>$ dune exec ./router.exe</b>
</code></pre>

<br>

A path is written as a chain of pieces joined with `/`. Each piece matches
one segment of the path, that is, one part between slashes:

- `s "echo"` matches the fixed word `echo`.
- `str` matches any segment and passes it to the handler as a string.
- `int` matches a segment that is a whole number and passes it as an `int`.
- `root` is the empty path, so on its own it matches `/`.

A path matches the whole request path, so `s "echo" / str` matches
`/echo/foo` and not `/echo/foo/bar`. To accept any number of further
segments, end the chain with `rest`, which passes them to the handler as a
list of strings. Nothing can follow `rest`, and the compiler enforces that.

The values captured along the way become the leading arguments of the handler,
in the order they appear in the path. So the handler for `s "echo" / str`
receives `word` first, and then the three usual arguments from
[**`1-hello`**](../1-hello#readme). The compiler checks that the number and
types of these arguments agree with the path.

<pre><code><b>$ curl http://localhost:8765/echo/foo</b>
foo
<b>$ curl http://localhost:8765/square/12</b>
144
<b>$ curl http://localhost:8765/files/css/site.css</b>
css / site.css
</code></pre>

<br>

Routes are tried in order, and the first one that matches wins. When no route
matches, the response is `404 Not Found`. That also happens when a segment
fails to convert, so `/square/twelve` is not found because `twelve` is not a
number:

<pre><code><b>$ curl -i http://localhost:8765/square/twelve</b>
HTTP/1.1 404 Not Found
Date: Wed, 02 Sep 2026 09:31:36 GMT
Content-Type: text/plain; charset=utf-8
Content-Length: 10
Connection: keep-alive

Not Found
</code></pre>

When the path matches a route but the method does not, the response is
`405 Method Not Allowed`, with an `Allow` header listing the methods that
would have worked. Notice that a `get` route also answers `HEAD` requests. The
handler runs as usual, and the body is left out of the response.

<pre><code><b>$ curl -i -X POST http://localhost:8765/echo/foo</b>
HTTP/1.1 405 Method Not Allowed
Date: Wed, 02 Sep 2026 09:31:36 GMT
Allow: GET, HEAD
Content-Type: text/plain; charset=utf-8
Content-Length: 19
Connection: keep-alive

Method Not Allowed
</code></pre>

<br>

Besides `get` there is `post`, which [**`5-form`**](../5-form#readme) uses, and
`route`, which takes any method. If a segment needs a conversion of your own,
`conv` takes a function from a string to an optional value, and the route
matches only when the function returns `Some`.

<br>

**Next steps:**

- [**`3-log`**](../3-log#readme) prints a line for every request the server
  handles.
- [**`4-counter`**](../4-counter#readme) shares state between handlers.

<br>

[Up to the tutorial index](../#readme)
