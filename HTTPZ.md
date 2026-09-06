# HTTP libraries in OxMono

This repository provides HTTP servers, clients and protocol libraries for
OxCaml.

- **Proffer** defines sites, routes and response handlers.
- **Fetch** makes requests and applies redirects, retries, cookies and access policies.
- **Httpz** parses and writes HTTP/1.1 and supplies URI, media, JSON and TLS libraries.

Proffer and Fetch use Eio for network I/O. Both include JSON, JSON Lines,
CommonMark and HTML codecs, and provide mock backends for tests.

## Design

Proffer includes routing, typed body codecs, content negotiation and response
caching. A handler can attach a `Cache_control` policy and an `Etag` to a
response. Proffer handles conditional GET requests and HEAD responses
consistently across its live and mock backends. `Cache.memoize` retains
generated bodies for a specified lifetime. The
[cache example](example/proffer/7-cache/README.md) shows both HTTP cache policy
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
[restriction](example/fetch/6-restrict/README.md) and
[credential](example/fetch/7-credentials/README.md) examples show these policies.

## Repository layout

- [`bleeding/proffer/`](bleeding/proffer/) contains the server library and its backends.
- [`bleeding/fetch/`](bleeding/fetch/) contains the client library, middleware and backends.
- [`bleeding/httpz/`](bleeding/httpz/) contains the protocol and supporting libraries.
- [`example/`](example/README.md) contains the Proffer and Fetch lessons and their shared local server.

Public interfaces are documented in `.mli` files beside their implementations.
The examples are separate executables and are not installed library modules.

## Build

Select an OxCaml switch. The commands below use `5.2.0+ox`.

```sh
eval "$(opam env --switch=5.2.0+ox)"
dune build --profile release-check @bleeding/httpz/all @bleeding/fetch/all @bleeding/proffer/all @example/all
```

The [repository setup](README.md#setup) describes the workspace dependencies.

## Start a server

The [first Proffer example](example/proffer/1-hello/README.md) serves a
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
dune exec --profile release-check ./example/proffer/1-hello/hello.exe
```

`get root` matches `GET /`. `Resp.text` produces a text response. The handler
receives the application state supplied by `~env`, which is `()` here.
The server uses plaintext HTTP by default.

## Make a request

The [first Fetch example](example/fetch/1-read/README.md) calls that server.

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let client = Fetch_httpz.std env in
  print_string (Fetch.read client "http://127.0.0.1:8765/")
```

Its Dune dependencies are `(libraries fetch fetch-httpz eio_main)`.
From another terminal at the repository root, run the client.

```sh
dune exec --profile release-check ./example/fetch/1-read/read.exe
```

```text
Hello from Proffer!
```

`Fetch.read` returns the response body regardless of status, with a default
limit of 16 MiB. `Fetch.get` exposes status, headers and a streaming body.
`Fetch.read_as` decodes a successful response using a media codec.

Stop the server with Ctrl-C when finished. Continue through the
[examples](example/README.md), which introduce server features before client
features. Most later Fetch examples start their own server through
[`Localhost`](example/localhost/README.md). That module comes from
`example/localhost/localhost.ml` and the private Dune library `localhost`.

## Select libraries

Declare each library whose modules a program uses in Dune's `(libraries ...)`
field. A library name such as `httpz.uri` is an OCamlfind subpackage.

| Use | Dune libraries |
| --- | --- |
| Serve HTTP/1.1. | `proffer proffer-httpz` |
| Make HTTP/1.1 requests with an OCaml transport. | `fetch fetch-httpz` |
| Make requests with libcurl and HTTP/2. | `fetch fetch-curl` |
| Test server handlers in memory. | `proffer proffer.mock` |
| Test client code in memory. | `fetch fetch.mock` |

The `httpz` opam package installs all the supporting libraries below.
Their OCamlfind dependencies determine what a program links. URI operations,
media codecs and bounded Jsont readers can be used independently.

| Library | Entry module | Purpose |
| --- | --- | --- |
| `httpz` | `Httpz` | HTTP/1.1 parsing and writing. |
| `httpz.uri` | `Httpz_uri` | URI parsing, resolution and templates. |
| `httpz.media` | `Httpz_media` | Typed codecs, forms, multipart bodies and SSE writers. |
| `httpz.jsont` | `Httpz_jsont` | Jsont readers with a nesting limit. |
| `httpz.media.jsont` | `Httpz_media_jsont` | JSON and JSON Lines codecs. |
| `httpz.media.cmarkit` | `Httpz_media_cmarkit` | CommonMark and HTML codecs. |
| `httpz.tls` | `Httpz_tls` | Eio TLS client and server flows. |
| `httpz.cookie` | `Cookie` | Cookie parsing and writing. |
| `httpz.cookie.jar` | `Cookie_jar` | Client cookie storage and persistence. |
| `httpz.punycode` | `Punycode` | Punycode encoding and decoding. |
| `httpz.punycode.idna` | `Punycode_idna` | Domain-name normalization; a subset of IDNA. |
| `httpz.pubsuffix` | `Pubsuffix` | Public suffix and registrable-domain lookup. |
| `httpz.route` | `Httpz_route` | Route matching for the lower-level server API. |
| `httpz.eio_server` | `Httpz_eio_server` | Eio server API with static-file support. |

`httpz.uri` uses Base and the OxCaml standard libraries. Its URI type is shared
with the workspace's `Uriz.t`. The HTTP wire library does not link media, JSON,
CommonMark, Eio or TLS. `httpz.media` and `httpz.jsont` do not require the
wire parser. Fetch and Proffer include the media adapters for applications
that need the complete client or server library.

Timeouts, retry delays, pacing and cache lifetimes use the external
`Duration.t`. Fetch and Proffer re-export it as `Fetch.Duration` and
`Proffer.Duration`. For example, `Fetch.Duration.of_ms 500` is half a second.

## Scope

`fetch-httpz` opens a connection for each request and supports HTTP/1.1.
`fetch-curl` uses libcurl 7.83 or later for connection reuse, HTTP/2 and
content decoding. Both support HTTPS with system trust anchors.

The Proffer HTTP/1.1 backend buffers each request, including its headers and
body, in approximately 32 KiB. It supports streaming responses. Fetch
supports streamed uploads and downloads; the server examples use small
uploads to stay within the Proffer backend's limit.

The native libraries target 64-bit little-endian systems. The
[Proffer interface](bleeding/proffer/lib/proffer.mli),
[Fetch interface](bleeding/fetch/lib/fetch.mli) and
[Httpz interface](bleeding/httpz/lib/httpz.mli) describe the public APIs.

OxMono also includes `fetch-main` for platform selection, `fetch-macos` for
NSURLSession, `fetch-cmdliner` for command-line configuration and
`fetch-signature` for HTTP Message Signatures. `httpz.eio_server` provides
the separate server API that supports static files.
