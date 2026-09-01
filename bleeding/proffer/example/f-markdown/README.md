# `f-markdown`

<br>

A page written in Markdown can be shown to a browser as HTML and given to
other programs as its source. This example serves one document both ways,
choosing by what the client asks for:

```ocaml
let page =
  Cmarkit.Doc.of_string
    {|# Good morning, world!

This page is written in *Markdown*.

- A browser is shown it as HTML.
- A client that asks for `text/markdown` gets the source.
|}

let site =
  Site.of_routes
    [ get root (fun () request respond ->
        Negotiate.encode respond request
          [ Markdown.html (); Markdown.markdown () ]
          page) ]
```

<pre><code><b>$ cd proffer/example/f-markdown</b>
<b>$ dune exec ./markdown.exe</b>
Running at http://localhost:8765
</code></pre>

<br>

The document is parsed once, at startup, with
[Cmarkit](https://erratique.ch/software/cmarkit). `Markdown` provides two
codecs for such documents: `markdown` reads and writes
the source as `text/markdown`, and `html` renders it as `text/html`. The
HTML codec only goes one way, since a rendered page cannot be turned back
into its source, and that is all a response needs.

`Negotiate.encode` takes one value and a list of codecs, and responds with
the first codec the client accepts, in the client's order of preference.
The first codec in the list is used when the client expresses no
preference, which is why HTML comes first. A browser gets the rendered page:

<pre><code><b>$ curl -i http://localhost:8765/</b>
HTTP/1.1 200 OK
Date: Wed, 02 Sep 2026 10:24:02 GMT
Vary: Accept
Content-Type: text/html; charset=utf-8
Content-Length: 207
Connection: keep-alive

&lt;h1&gt;Good morning, world!&lt;/h1&gt;
&lt;p&gt;This page is written in &lt;em&gt;Markdown&lt;/em&gt;.&lt;/p&gt;
&lt;ul&gt;
&lt;li&gt;A browser is shown it as HTML.&lt;/li&gt;
&lt;li&gt;A client that asks for &lt;code&gt;text/markdown&lt;/code&gt; gets the source.&lt;/li&gt;
&lt;/ul&gt;
</code></pre>

A client that asks for Markdown gets the source, written back out by
Cmarkit, which is why the exclamation mark comes back escaped:

<pre><code><b>$ curl -H 'Accept: text/markdown' http://localhost:8765/</b>
# Good morning, world\!

This page is written in *Markdown*.

- A browser is shown it as HTML.
- A client that asks for `text/markdown` gets the source.
</code></pre>

As in [**`a-negotiate`**](../a-negotiate#readme), the response carries
`Vary: Accept` so that a cache keeps the two forms apart.

<br>

The HTML codec renders in Cmarkit's safe mode, which drops any raw HTML and
unsafe links found in the document. That makes it fine to render Markdown
written by visitors. Pass `~safe:false` to `Markdown.html` for
documents you wrote yourself and want to embed HTML in.

<br>

That's the end of the tutorial! The full interface is documented in
[`proffer.mli`](../../lib/proffer.mli),
[`proffer_httpz.mli`](../../httpz/proffer_httpz.mli) and
[`media.mli`](../../../httpz/media/lib/media.mli). Proffer's companion
client library has a tutorial of its own in
[`fetch/example`](../../../fetch/example#readme).

<br>

[Up to the tutorial index](../#readme)
