# `g-markdown`

<br>

Codecs are not only for JSON. This example fetches a page written in
Markdown, parses it with [Cmarkit](https://erratique.ch/software/cmarkit),
and prints it rendered as HTML:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in
  match Fetch.read_as client (Fetch.Markdown.markdown ()) (base ^ "/about") with
  | Ok document ->
    print_string (Fetch.Media.encode (Fetch.Markdown.html ()) document)
  | Error r -> Printf.printf "The server said %d\n" (Fetch.status r)
```

<pre><code><b>$ cd fetch/example/g-markdown</b>
<b>$ dune exec ./markdown.exe</b>
server: GET /about -> 200
&lt;h1&gt;About&lt;/h1&gt;
&lt;p&gt;This server is written with &lt;em&gt;Proffer&lt;/em&gt;.&lt;/p&gt;
</code></pre>

<br>

`Fetch.Markdown` provides two codecs for Cmarkit documents.
`markdown` reads and writes `text/markdown`, so `Fetch.read_as` with it asks
the server for Markdown and returns a parsed document rather than a string.
`html` renders a document as `text/html`. It only encodes, since a rendered
page cannot be turned back into its source, and here it is used directly
through `Fetch.Media.encode` to produce the output.

The document in between is an ordinary Cmarkit value, so it can be walked,
edited or rendered to other formats before anything is printed.

<br>

Any format with a string form can be made into a codec with
`Fetch.Media.of_strings`, which takes the two conversion functions and the
media type, and then works with every function this example and
[**`f-json`**](../f-json#readme) used.

<br>

That's the end of the tutorial! The full interface is documented in
[`fetch.mli`](../../lib/fetch.mli),
[`fetch_httpz.mli`](../../httpz/fetch_httpz.mli),
[`fetch_curl.mli`](../../curl/fetch_curl.mli) and
[`media.mli`](../../../httpz/media/lib/media.mli). The server library that
the local server is built with has a tutorial of its own in
[`proffer/example`](../../../proffer/example#readme).

<br>

[Up to the tutorial index](../#readme)
