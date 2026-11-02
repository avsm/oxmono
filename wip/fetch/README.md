# Fetch

Fetch makes HTTP requests through a client capability. Middleware applies
redirects, retries, cookies and access policies. `fetch-httpz` uses an OCaml
HTTP/1.1 transport. `fetch-curl` uses libcurl for connection reuse and HTTP/2.
`fetch.mock` supplies responses in memory.

Start with the [server and client examples](../../examples/README.md).
The [first Fetch lesson](../../examples/fetch/1-read/README.md)
contains a complete program and its run command.

The main library includes JSON, JSON Lines, Markdown and HTML codecs.
Timeout and delay values use the external `Duration.t`. Add `duration` to
your Dune libraries when constructing these values.

The [public interface](lib/fetch.mli) describes the API. The
[repository guide](../../docs/http.md) describes library selection, setup
and backend limits. All examples are under the top-level `example/` directory.

The optional [`fetch.dav`](dav/README.md) library adds WebDAV client operations
over an existing Fetch capability, using the `httpz.dav` protocol codecs.

Request bodies can borrow local strings through `fetch`, `post`, `put`,
`patch` and `with_response`. Middleware receives `request @ local` and must
consume or copy its body before returning. HTTPZ sends the borrowed body
before returning the response. Curl and macOS copy in-memory uploads into
owned storage for asynchronous processing.

`Middleware.globalize` creates an owned request snapshot. Its implementation
uses `ppx_globalize`, preserving global metadata and copying borrowed body
storage. `Fetch_mock.client` provides owned snapshots for retaining tests.
`Fetch_mock.client_local` lends requests without copying.

URLs, typed headers, security metadata and returned responses remain owned.
The URL and security metadata survive in the response, and existing header
codecs require global values. These interfaces do not promise an allocation-
free HTTP exchange.
