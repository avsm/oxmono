# HTTP libraries in OxMono

This repository provides HTTP servers, clients and protocol libraries for
OxCaml.

- **Proffer** defines sites, routes and response handlers.
- **Fetch** makes requests and applies redirects, retries, cookies and access policies.
- **Httpz** parses and writes HTTP/1.1 and supplies URI, media, JSON and TLS libraries.

Proffer and Fetch use Eio for network I/O. Both include JSON, JSON Lines,
Markdown and HTML codecs, and provide mock backends for tests.

## Design

Proffer includes routing, typed body codecs, content negotiation and response
caching. A handler can attach a `Cache_control` policy and an `Etag` to a
response. Proffer handles conditional GET requests and HEAD responses
consistently across its live and mock backends. `Cache.memoize` retains
generated bodies for a specified lifetime. The
[cache example](../examples/proffer/7-cache/README.md) shows both HTTP cache policy
and caching within the server.

Fetch treats a client as a capability that grants authority to make HTTP
requests. A library accepts a client from its caller and can narrow it with
`Fetch.restrict` to selected origins, path prefixes and methods.
`Fetch.read_only` allows GET, HEAD and OPTIONS. Restrictions compose by
intersection and apply to redirect hops as well as initial requests.

For example, an API library can restrict its supplied client to
`https://api.example.com/v1/`, attach credentials scoped to that API, then
pass a read-only client to a component that only lists records. That component
can add restrictions but cannot remove those already imposed through the
client. Libraries can therefore share HTTP access without each receiving
unrestricted network access or constructing its own transport. The same
library can accept a mock client in tests. The
[restriction](../examples/fetch/6-restrict/README.md) and
[credential](../examples/fetch/7-credentials/README.md) examples show these policies.

Fetch and Proffer separate their frontend APIs from the backends that perform
I/O. A library can depend on `fetch` and accept a client from its caller. The
application chooses `fetch-httpz` for the OCaml HTTP/1.1 transport or
`fetch-curl` for libcurl. Code using the common Fetch interface keeps the same
requests, restrictions and codecs with either backend. `fetch.mock` supplies
responses in tests.

Proffer routes, handlers and response policies are also independent of the
transport. `proffer-httpz` serves a site over Eio, while `proffer.mock`
dispatches requests to the same site in memory.

## Examples

Each [example](../examples/README.md) contains a complete program, its Dune
dependencies and commands to run it.

### Proffer

- [Greeting server](../examples/proffer/1-hello/README.md) serves a text response at `/`.
- [Routing](../examples/proffer/2-router/README.md) matches paths and captures strings, integers and remaining path segments.
- [Request logging](../examples/proffer/3-log/README.md) reports completed requests through `on_event`.
- [Application state](../examples/proffer/4-counter/README.md) shares a request counter between handlers through `~env`.
- [Form handling](../examples/proffer/5-form/README.md) reads URL-encoded forms, multipart files and query parameters.
- [Fallback handling](../examples/proffer/6-error/README.md) provides a custom error page, a permanent redirect and response headers.
- [Caching](../examples/proffer/7-cache/README.md) sets cache headers and ETags, and caches a generated response in server memory.
- [Streaming](../examples/proffer/8-stream/README.md) writes response bytes as they become available, with or without a declared length.
- [Authentication](../examples/proffer/9-auth/README.md) protects routes beneath `/admin` with HTTP Basic credentials.
- [Content negotiation](../examples/proffer/a-negotiate/README.md) selects HTML, JSON or plain text from the request's `Accept` field.
- [Site composition](../examples/proffer/b-mount/README.md) mounts a group of API routes beneath `/api/v1`.
- [Mock dispatch](../examples/proffer/c-mock/README.md) exercises handlers without opening a network connection.
- [Server configuration](../examples/proffer/d-config/README.md) sets connection limits and timeouts, and selects an unused port.
- [JSON API](../examples/proffer/e-json/README.md) decodes submitted records, serves typed responses and exports JSON Lines.
- [Markdown response](../examples/proffer/f-markdown/README.md) serves a document as HTML or Markdown.

### Fetch

Start the greeting server before running the first client. Most later clients
start their own server through the shared
[Localhost library](../examples/localhost/README.md). The mock example needs no
server, and the HTTPS example contacts a public URL by default.

- [Reading a response](../examples/fetch/1-read/README.md) retrieves the greeting from the first Proffer server.
- [Response inspection](../examples/fetch/2-response/README.md) reads status, URL, headers and a streaming body.
- [POST submission](../examples/fetch/3-post/README.md) sends plain text, URL-encoded forms and multipart bodies.
- [Redirect following](../examples/fetch/4-redirect/README.md) follows a redirect and reports the final URL.
- [Error handling](../examples/fetch/5-errors/README.md) distinguishes HTTP error responses from URL, connection and timeout failures.
- [Client restriction](../examples/fetch/6-restrict/README.md) narrows a client to one origin, then denies methods outside its read-only policy.
- [Credential scoping](../examples/fetch/7-credentials/README.md) scopes a bearer token to an API and supplies default request headers.
- [Cookie storage](../examples/fetch/8-cookies/README.md) retains a login cookie and sends it on a later request.
- [Retry policy](../examples/fetch/9-retry/README.md) repeats a temporarily failing request until it succeeds.
- [Request pacing](../examples/fetch/a-limits/README.md) bounds concurrency and spaces out request starts.
- [Body streaming](../examples/fetch/b-stream/README.md) downloads to a file and uploads from a flow.
- [Mock client](../examples/fetch/c-mock/README.md) tests client code with supplied responses and no network connection.
- [Curl transport](../examples/fetch/d-curl/README.md) makes requests through libcurl.
- [HTTPS transport](../examples/fetch/e-https/README.md) uses the OCaml transport with system trust anchors.
- [Typed JSON](../examples/fetch/f-json/README.md) decodes records, handles unsuccessful responses, posts JSON and reads JSON Lines.
- [Markdown decoding](../examples/fetch/g-markdown/README.md) reads a document and renders it as HTML.

## Repository layout

- [`wip/proffer/`](../wip/proffer) contains the server library and its backends.
- [`wip/fetch/`](../wip/fetch) contains the client library, middleware and backends.
- [`wip/httpz/`](../wip/httpz) contains the protocol and supporting libraries.
- [`example/`](../examples/README.md) contains the Proffer and Fetch lessons and their shared local server.

Public interfaces are documented in `.mli` files beside their implementations.
The examples are separate executables and are not installed library modules.

## Build

Select an OxCaml switch. The commands below use `5.2.0+ox`.

```sh
eval "$(opam env --switch=5.2.0+ox)"
dune build --profile release-check @wip/httpz/all @wip/fetch/all @wip/proffer/all @example/all
```

The [repository setup](../README.md#setup) describes the workspace dependencies.

## Start a server

The [first Proffer example](../examples/proffer/1-hello/README.md) serves a
greeting on loopback port 8765.

```ocaml
open Proffer
open Proffer.Route

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Hello from Proffer!\n") ]

let () =
  Eio_main.run @@ fun env ->
  Proffer_httpz.run env ~env:() site
```

Its Dune dependencies are `(libraries proffer proffer-httpz eio_main)`.
From the repository root, run it in one terminal and leave it running.

```sh
dune exec --profile release-check ./examples/proffer/1-hello/hello.exe
```

`get root` matches `GET /`. `Resp.text` produces a text response. The handler
receives the application state supplied by `~env`, which is `()` here.
The server uses plaintext HTTP by default.

## Make a request

The [first Fetch example](../examples/fetch/1-read/README.md) calls that server.

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let client = Fetch_httpz.std env in
  print_string (Fetch.read client "http://127.0.0.1:8765/")
```

Its Dune dependencies are `(libraries fetch fetch-httpz eio_main)`.
From another terminal at the repository root, run the client.

```sh
dune exec --profile release-check ./examples/fetch/1-read/read.exe
```

```text
Hello from Proffer!
```

`Fetch.read` returns the response body regardless of status, with a default
limit of 16 MiB. `Fetch.get` exposes status, headers and a streaming body.
`Fetch.read_as` decodes a successful response using a media codec.

Stop the server with Ctrl-C when finished. Continue through the
[examples](../examples/README.md), which introduce server features before client
features. Most later Fetch examples start their own server through
[`Localhost`](../examples/localhost/README.md). That module comes from
`examples/localhost/localhost.ml` and the private Dune library `localhost`.

## Select libraries

Declare each library whose modules a program uses in Dune's `(libraries ...)`
field. A library name such as `httpz.uri` is an OCamlfind subpackage.

| Use | Dune libraries |
| --- | --- |
| Serve HTTP/1.1. | `proffer proffer-httpz` |
| Make HTTP/1.1 requests with an OCaml transport. | `fetch fetch-httpz` |
| Make requests with libcurl and HTTP/2. | `fetch fetch-curl` |
| Test server handlers in memory. | `proffer proffer.mock` |
| Use WebDAV through an existing Fetch client. | `fetch.dav` (or `proffer.dav`) |
| Test client code in memory. | `fetch fetch.mock` |

The `httpz` opam package installs all the supporting libraries below.
Applications select the Findlib libraries they use. Media codecs include JSON
and Markdown support and depend on the HTTP wire library.

| Library | Entry module | Purpose |
| --- | --- | --- |
| `httpz` | `Httpz` | HTTP/1.1 parsing and writing. |
| `httpz.uri` | `Httpz_uri` | URI parsing, resolution and templates. |
| `httpz.media` | `Httpz_media` | Typed codecs, forms, multipart, SSE, JSON and Markdown. |
| `httpz.dav` | `Httpz_dav` | Bounded WebDAV protocol values and XML codecs. |
| `httpz.tls` | `Httpz_tls` | Eio TLS client and server flows. |
| `httpz.websocket` | `Httpz_websocket` | WebSocket handshakes and bounded byte messages. |
| `httpz.cookie` | `Cookie` | Cookie parsing and writing. |
| `httpz.cookie.jar` | `Cookie_jar` | Client cookie storage and persistence. |
| `httpz.punycode` | `Punycode` | Punycode encoding and decoding. |
| `httpz.punycode.idna` | `Punycode_idna` | Domain-name normalization; a subset of IDNA. |
| `httpz.pubsuffix` | `Pubsuffix` | Public suffix and registrable-domain lookup. |

`httpz.uri` adapts the shared portable Uriz implementation. The HTTP wire library owns
shared syntax and diagnostic helpers. `httpz.media` depends on it and includes
`Json` for bounded JSON and JSON Lines, and `Markdown` for Markdown and HTML.
Fetch and Proffer expose the same codecs as `Media`, `Json` and `Markdown`.

Timeouts, retry delays, pacing and cache lifetimes use the external
`Duration.t`. Add `duration` to your Dune libraries when using it directly.
For example, `Duration.of_ms 500` is half a second.

## Scope

`fetch-httpz` opens a connection for each request and supports HTTP/1.1.
`fetch-curl` uses libcurl 7.83 or later for connection reuse, HTTP/2 and
content decoding. Both support HTTPS with system trust anchors.

The Proffer HTTP/1.1 backend buffers each request, including its headers and
body, in approximately 32 KiB. It supports streaming responses. Fetch
supports streamed uploads and downloads; the server examples use small
uploads to stay within the Proffer backend's limit.

The native libraries target 64-bit little-endian systems. The
[Proffer interface](../wip/proffer/lib/proffer.mli),
[Fetch interface](../wip/fetch/lib/fetch.mli) and
[Httpz interface](../wip/httpz/lib/httpz.mli) describe the public APIs.

OxMono also includes `fetch-main` for platform selection, `fetch-macos` for
NSURLSession, `fetch-cmdliner` for command-line configuration and
`fetch-signature` for HTTP Message Signatures. Server applications use Proffer
and the `proffer-httpz` backend.

WebDAV protocol values live in [`httpz.dav`](../wip/httpz/dav/README.md).
The [`fetch.dav` client](../wip/fetch/dav/README.md) supplies scoped operations
and explicit locks; `proffer.dav` re-exports it.
