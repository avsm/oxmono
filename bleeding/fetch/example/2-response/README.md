# `2-response`

<br>

Most programs need more than the body. This example fetches a page with
`Fetch.get` and prints its status, the address it came from, its headers, and
then the body:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in
  Eio.Switch.run @@ fun sw ->
  let response = Fetch.get ~sw client (base ^ "/json") in
  Printf.printf "Status: %d\n" (Fetch.status response);
  Printf.printf "Fetched from: %s\n" (Fetch.url response);
  (match Fetch.header Fetch.Header.content_type response with
   | Some { media; _ } -> Printf.printf "Media type: %s\n" media
   | None -> print_endline "No media type given");
  print_endline "Headers:";
  Http.Header.to_list (Fetch.headers response)
  |> List.iter (fun (name, value) -> Printf.printf "  %s: %s\n" name value);
  print_string "Body: ";
  flush stdout;
  Eio.Flow.copy (Fetch.body response) (Eio.Stdenv.stdout env)
```

<pre><code><b>$ cd fetch/example/2-response</b>
<b>$ dune exec ./response.exe</b>
server: GET /json -> 200
Status: 200
Fetched from: http://127.0.0.1:45847/json
Media type: application/json
Headers:
  Date: Wed, 02 Sep 2026 09:35:13 GMT
  Content-Type: application/json
  Content-Length: 50
  Connection: close
Body: {"greeting": "hello", "from": "the local server"}
</code></pre>

<br>

`Fetch.get` returns a response as soon as the headers have arrived. The body
has not been read yet, and `Fetch.body` gives it to you as a *flow*, a stream
of bytes that you read at your own pace. Here it is simply copied to the
terminal. Because the body may still be in transit, `get` needs a *switch*,
`sw`, which is how Eio groups resources that should be released together. The
connection is closed when the switch finishes, at the end of the
`Eio.Switch.run` block.

`Fetch.status` is the numeric status code and `Fetch.headers` gives all the
headers. `Fetch.url` is the address the response actually came from, which
matters once redirects are involved.

<br>

`Fetch.header` reads a single header as a typed value rather than a string.
Here `Content-Type` comes back as a record with the media type and its
parameters already separated. The `Fetch.Header` module has a codec like this
for most standard headers: `content_length` gives an integer, `etag` an
entity tag, `links` a list of links, and so on. A header that is absent or
malformed reads as `None`, so your code never has to parse anything itself.
The same codecs are used to set headers on a request, which
[**`3-post`**](../3-post#readme) shows.

There are also `Fetch.head`, `Fetch.delete`, `Fetch.put`, `Fetch.patch` and
`Fetch.options`, each taking the same arguments as `get`.

<br>

**Next steps:**

- [**`3-post`**](../3-post#readme) sends a body with a request.
- [**`4-redirect`**](../4-redirect#readme) follows redirects.

<br>

[Up to the tutorial index](../#readme)
