# `3-post`

<br>

This example sends data to the server three ways: as a plain body, as an HTML
form, and as a file upload.

```ocaml
let print_body env response =
  Eio.Flow.copy (Fetch.body response) (Eio.Stdenv.stdout env)

let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in
  Eio.Switch.run @@ fun sw ->

  print_endline "A plain body:";
  Fetch.post ~sw client (base ^ "/echo")
    ~headers:Fetch.Header.[ content_type, media "text/plain" ]
    ~body:(String "Hello, server!\n")
  |> print_body env;

  print_endline "A form:";
  let headers, body =
    Fetch.Form.urlencoded [ ("name", "alice"); ("colour", "blue") ]
  in
  Fetch.post ~sw ~headers ~body client (base ^ "/form") |> print_body env;

  print_endline "A file upload, as the server receives it:";
  let headers, body =
    Fetch.Form.multipart
      [ Fetch.Form.field "name" "alice";
        Fetch.Form.file ~name:"avatar" ~filename:"avatar.txt"
          ~content_type:"text/plain" "Not really a picture.\n" ]
  in
  Fetch.post ~sw ~headers ~body client (base ^ "/echo") |> print_body env
```

<pre><code><b>$ cd fetch/example/3-post</b>
<b>$ dune exec ./post.exe</b>
A plain body:
server: POST /echo -> 200
Hello, server!
A form:
server: POST /form -> 200
name = alice
colour = blue
A file upload, as the server receives it:
server: POST /echo -> 200
--form8c154fafab786e4aa93f28b3ba48b2914ac3375497c88867x0
Content-Disposition: form-data; name="name"

alice
--form8c154fafab786e4aa93f28b3ba48b2914ac3375497c88867x0
Content-Disposition: form-data; name="avatar"; filename="avatar.txt"
Content-Type: text/plain

Not really a picture.

--form8c154fafab786e4aa93f28b3ba48b2914ac3375497c88867x0--
</code></pre>

<br>

`Fetch.post` takes a `body`. The simplest body is `String`, which holds the
bytes in memory. The local server's `/echo` page sends back whatever it
receives, with the same content type.

The `headers` argument is a list of headers to send. Each entry pairs a codec
from `Fetch.Header` with a value of the matching type, so `content_type` takes
a media type built by `media` rather than a string you might mistype. The
list is written with `Fetch.Header.[ ... ]` so that entries of different
types can sit side by side. For a header without a codec, `raw "Name" "value"`
sends it as is.

Fetch works out `Host` and `Content-Length` for you, and refuses a request
that sets them by hand.

<br>

Web forms are sent in one of two encodings, and `Fetch.Form` builds both.
`urlencoded` takes a list of fields and is what a browser sends for a simple
form. `multipart` is used when a form includes files. It takes a list of
parts: `field` for a text field, `file` for a file held in memory, and
`stream` for a file read from disk as the request is sent. Each returns the
`Content-Type` header and the body together, ready to pass to `post`, since
the two must agree.

The echoed multipart body above is what a web server sees when a browser
uploads a file, with each part separated by a boundary line that Fetch chose.
The boundary is derived from the parts and from a fresh random salt, so it
differs on every request; pass `~boundary` to `multipart` to fix it.

A `urlencoded` body is what a browser sends for a form without files. It is
the encoding of `Fetch.Media.form`, so a server that answers in it &mdash; an
OAuth token endpoint, say &mdash; is read with
`Fetch.read_as client Fetch.Media.form url`.

<br>

**Next steps:**

- [**`4-redirect`**](../4-redirect#readme) follows redirects.
- [**`b-stream`**](../b-stream#readme) sends and receives large bodies.

<br>

[Up to the tutorial index](../#readme)
