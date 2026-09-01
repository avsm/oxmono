# Proffer tutorial

Proffer is a library for writing web servers. You describe the pages your site
serves, and a backend takes care of the network. These examples introduce its
features one at a time. Each one is a complete program with a README that
explains what it does and how to try it. Start at
[**`1-hello`**](1-hello#readme), or pick whatever interests you from the list.

- [**`1-hello`**](1-hello#readme) &nbsp;&mdash;&nbsp; the smallest Proffer
  server answers every request with the same greeting.
- [**`2-router`**](2-router#readme) &nbsp;&mdash;&nbsp; different pages for
  different paths, with parts of the path passed to your code.
- [**`3-log`**](3-log#readme) &nbsp;&mdash;&nbsp; prints a line for every
  request the server handles.
- [**`4-counter`**](4-counter#readme) &nbsp;&mdash;&nbsp; shares application
  state between requests.
- [**`5-form`**](5-form#readme) &nbsp;&mdash;&nbsp; reads submitted forms and
  query strings, and redirects afterwards.
- [**`6-error`**](6-error#readme) &nbsp;&mdash;&nbsp; a custom "not found"
  page, permanent redirects, and a header on every response.
- [**`7-cache`**](7-cache#readme) &nbsp;&mdash;&nbsp; tells browsers what they
  may keep, and avoids repeating expensive work.
- [**`8-stream`**](8-stream#readme) &nbsp;&mdash;&nbsp; sends a response a
  piece at a time while it is being produced.
- [**`9-auth`**](9-auth#readme) &nbsp;&mdash;&nbsp; protects part of the site
  with a password.
- [**`a-negotiate`**](a-negotiate#readme) &nbsp;&mdash;&nbsp; serves the same
  page as HTML, JSON or plain text depending on what the client asks for.
- [**`b-mount`**](b-mount#readme) &nbsp;&mdash;&nbsp; builds a site from
  smaller sites.
- [**`c-mock`**](c-mock#readme) &nbsp;&mdash;&nbsp; tests a site without
  opening a network connection.
- [**`d-config`**](d-config#readme) &nbsp;&mdash;&nbsp; connection limits,
  timeouts, and letting the system pick a port.
- [**`e-json`**](e-json#readme) &nbsp;&mdash;&nbsp; a JSON API whose requests
  and responses are typed values.
- [**`f-markdown`**](f-markdown#readme) &nbsp;&mdash;&nbsp; serves one Markdown
  document as HTML or as source.

That's it for the tutorial! The complete interface is documented in
[`proffer.mli`](../lib/proffer.mli) and
[`proffer_httpz.mli`](../httpz/proffer_httpz.mli).

<br>

## Running the examples

The examples are built along with the rest of this repository. From the
repository root, install the dependencies once and build:

<pre><code><b>$ opam install --deps-only --with-test ./*.opam</b>
<b>$ dune build</b>
</code></pre>

Then go into any example's directory and run it. Every server in this tutorial
listens on port 8765, so stop one before starting the next.

<pre><code><b>$ cd proffer/example/1-hello</b>
<b>$ dune exec ./hello.exe</b>
Running at http://localhost:8765
</code></pre>
