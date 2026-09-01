# Fetch tutorial

Fetch is a library for making HTTP requests. You ask for a URL and get a
response back, and you can shape a client so that the code you hand it to can
reach only the places you intend. These examples introduce its features one
at a time. Each one is a complete program with a README that explains what it
does and what it prints. Start at [**`1-read`**](1-read#readme), or pick
whatever interests you from the list.

- [**`1-read`**](1-read#readme) &nbsp;&mdash;&nbsp; the simplest request
  fetches a page as a string.
- [**`2-response`**](2-response#readme) &nbsp;&mdash;&nbsp; looks at the
  status, headers and body of a response.
- [**`3-post`**](3-post#readme) &nbsp;&mdash;&nbsp; sends data: a plain body, a
  form, and a file upload.
- [**`4-redirect`**](4-redirect#readme) &nbsp;&mdash;&nbsp; follows redirects,
  or chooses not to.
- [**`5-errors`**](5-errors#readme) &nbsp;&mdash;&nbsp; what happens when a
  page is missing, a server is down, or too slow.
- [**`6-restrict`**](6-restrict#readme) &nbsp;&mdash;&nbsp; a client that can
  only reach some sites, or only read.
- [**`7-credentials`**](7-credentials#readme) &nbsp;&mdash;&nbsp; attaches a
  token to requests for one site, and headers to every request.
- [**`8-cookies`**](8-cookies#readme) &nbsp;&mdash;&nbsp; remembers cookies
  between requests, like a browser.
- [**`9-retry`**](9-retry#readme) &nbsp;&mdash;&nbsp; tries again when a
  server is temporarily unavailable.
- [**`a-limits`**](a-limits#readme) &nbsp;&mdash;&nbsp; spaces requests out so
  a site is not overwhelmed.
- [**`b-stream`**](b-stream#readme) &nbsp;&mdash;&nbsp; downloads and uploads
  large bodies without holding them in memory.
- [**`c-mock`**](c-mock#readme) &nbsp;&mdash;&nbsp; tests code that makes
  requests, with no network at all.
- [**`d-curl`**](d-curl#readme) &nbsp;&mdash;&nbsp; the same program on the
  libcurl backend, with HTTPS built in.
- [**`e-https`**](e-https#readme) &nbsp;&mdash;&nbsp; HTTPS with the pure
  OCaml backend.
- [**`f-json`**](f-json#readme) &nbsp;&mdash;&nbsp; reads and writes JSON as
  typed values.
- [**`g-markdown`**](g-markdown#readme) &nbsp;&mdash;&nbsp; fetches a Markdown
  document and renders it.

That's it for the tutorial! The complete interface is documented in
[`fetch.mli`](../lib/fetch.mli), [`fetch_httpz.mli`](../httpz/fetch_httpz.mli)
and [`fetch_curl.mli`](../curl/fetch_curl.mli).

<br>

## Running the examples

The examples are built along with the rest of this repository. From the
repository root, install the dependencies once and build:

<pre><code><b>$ opam install --deps-only --with-test ./*.opam</b>
<b>$ dune build</b>
</code></pre>

Then go into any example's directory and run it:

<pre><code><b>$ cd fetch/example/1-read</b>
<b>$ dune exec ./read.exe</b>
server: GET /hello -> 200
Hello from the local server!
</code></pre>

<br>

## The local server

Every example except `c-mock` and `e-https` talks to a small web server that runs
inside the same program, so that the tutorial works without a network
connection and prints the same thing every time. The server is written with
Proffer, the server library in this repository, and lives in
[`localhost/localhost.ml`](localhost/localhost.ml). It serves a handful of
paths such as `/hello`, `/login` and `/flaky` that the examples need.

`Localhost.run env` starts it on a free port and passes the base URL, such as
`http://127.0.0.1:45847`, to the function you give it. The server prints one
line for every request it handles, beginning with `server:`, so you can see
what a client did as well as what it got back. When the function returns,
the server stops.

You do not need to understand the server to follow the tutorial. Everything
the examples show works just the same against any web site.
