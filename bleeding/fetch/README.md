# Fetch

Fetch makes HTTP requests through a client capability. Middleware applies
redirects, retries, cookies and access policies. `fetch-httpz` uses an OCaml
HTTP/1.1 transport. `fetch-curl` uses libcurl for connection reuse and HTTP/2.
`fetch.mock` supplies responses in memory.

Start with the [server and client examples](../../example/README.md).
The [first Fetch lesson](../../example/fetch/1-read/README.md)
contains a complete program and its run command.

The main library includes JSON, JSON Lines, Markdown and HTML codecs.
Timeout and delay values use the external `Duration.t`. Add `duration` to
your Dune libraries when constructing these values.

The [public interface](lib/fetch.mli) describes the API. The
[repository guide](../../HTTPZ.md) describes library selection, setup
and backend limits. All examples are under the top-level `example/` directory.
