# HTTP libraries in OxMono

OxMono includes three HTTP libraries for OxCaml:

- **Fetch** makes HTTP requests, with streaming, retries, redirects, cookies
  and controls over which URLs a client can access.
- **Proffer** serves HTTP applications. You define routes and handlers;
  a backend manages connections and sends their responses.
- **Httpz** provides HTTP/1.1 parsing and writing, plus independently usable
  libraries for URIs, media formats, JSON, TLS and cookies.

Fetch and Proffer use Eio for network I/O. Both include JSON, JSON Lines,
CommonMark and HTML codecs, and provide mock backends for tests that need no
network. You can also use the smaller Httpz libraries on their own.

## Choose a client or server

| Task | Libraries to use |
| --- | --- |
| Make HTTP/1.1 requests with a pure OCaml transport | `fetch` and `fetch-httpz` |
| Make requests using libcurl, with connection reuse and HTTP/2 | `fetch` and `fetch-curl` |
| Serve an HTTP/1.1 application | `proffer` and `proffer-httpz` |
| Test client code in memory | `fetch` and `fetch.mock` |
| Test routes in memory | `proffer` and `proffer.mock` |

The `httpz` opam package supplies the smaller libraries below. Fetch and
Proffer have separate packages for their transport backends. OxMono also
includes `fetch-main` for platform selection, `fetch-macos` for NSURLSession,
`fetch-cmdliner` for CLI configuration and `fetch-signature` for HTTP Message
Signatures.

## Build from source

Use an OxCaml switch with the workspace dependencies installed. From the
OxMono root:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  @bleeding/httpz/all @bleeding/fetch/all @bleeding/proffer/all
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check \
  bleeding/httpz bleeding/fetch bleeding/proffer
```

The workspace contains portable dependencies under `vendor/`, including the
corrected Cmarkit Markdown parser. It uses the installed `duration` package.

The following examples can each be placed in their own directory inside this
checkout. Each needs an OCaml source file and a `dune` file alongside it.

## Make a request

Save this as `client.ml`:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let client = Fetch_httpz.std env in
  print_string (Fetch.read client "https://example.com/")
```

Use this `dune` file:

```lisp
(executable
 (name client)
 (libraries fetch fetch-httpz eio_main))
```

Run it with `opam exec --switch=5.2.0+ox -- dune exec ./client.exe` from that
directory. `Fetch_httpz.std` supports HTTP and HTTPS, verifying server
certificates against the operating system's trust anchors.

`Fetch.read` returns the response body regardless of HTTP status, with a
16 MiB default size limit. Use `Fetch.get` to inspect status and headers, or
`Fetch.read_as` to decode a successful response into an OCaml value. The
[Fetch interface](bleeding/fetch/lib/fetch.mli) describes these operations, streaming,
credentials, retries and testing.

## Serve a response

Save this as `server.ml`:

```ocaml
open Proffer
open Proffer.Route

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "hello\n") ]

let () =
  Eio_main.run @@ fun env ->
  Proffer_httpz.run env ~env:() site
```

Use this `dune` file:

```lisp
(executable
 (name server)
 (libraries proffer proffer-httpz eio_main))
```

Run `opam exec --switch=5.2.0+ox -- dune exec ./server.exe` from that directory,
then visit `http://localhost:8765/`. The route answers `GET /` with `hello`.
The `~env:()` argument supplies the application state passed to handlers;
replace it with your own value when the application needs shared state.

The server listens on loopback and speaks plaintext HTTP by default. Pass
`~port` to choose another port. To serve HTTPS, pass
`~tls:(Httpz_tls.server config)` with a `Tls.Config.server` built from your
certificate and private key, and add `httpz.tls` and `tls` to the program's
Dune dependencies. The [Proffer interface](bleeding/proffer/lib/proffer.mli) describes
routing, forms, caching, authentication, streaming responses and testing.

## Work with typed bodies

A media codec pairs a content type with functions that encode and decode one
OCaml type. Fetch and Proffer share codecs, so a format definition can serve
both sides of an API. For example, this codec represents a JSON array of
strings:

```ocaml
let greetings = Fetch.Json.v (Jsont.list Jsont.string)
let json = Fetch.Media.encode greetings ["hello"; "world"]
let decoded = Proffer.Media.decode greetings json
(* decoded is Ok ["hello"; "world"] *)
```

This example uses `(libraries fetch proffer jsont)`. Pass the same codec to
`Fetch.read_as` to read a response, `Proffer.Resp.encode` to send one, or
`Proffer.Route.with_body` to decode an incoming request.

`Fetch.read_as` returns `Ok value` for a decoded 2xx response and
`Error response` for other HTTP statuses. A decoding failure raises
`Fetch.Decode_failure`. On the server, `with_body` answers an unsupported
content type with 415 and malformed content with 400. JSON codecs limit
nesting depth; request and response byte limits apply separately.

The `Json.lines` modules provide JSON Lines codecs, and the `Markdown`
modules provide CommonMark and HTML codecs. For event streams, `Fetch.Sse`
reads server-sent events and `Proffer.Sse` writes them.

## Set timeouts and delays

Timeouts, retry delays, request pacing and cache lifetimes use `Duration.t`
from the `duration` package. Construct values with explicit units, such as
`Fetch.Duration.of_sec 30` or `Proffer.Duration.of_ms 500`. Fetch and Proffer
re-export the same module, so durations can be shared between them. Code using
`Duration` directly declares `(libraries duration)`.

`Fetch_httpz.std` accepts `~connect_timeout` and `~idle_timeout`.
`Fetch.Retry.v` accepts `~backoff_factor` and `~backoff_max`, and
`Proffer_httpz.default_config` supplies server timeout defaults. See the
[client interface](bleeding/fetch/httpz/fetch_httpz.mli) and
[server configuration interface](bleeding/proffer/httpz/proffer_httpz.mli).

## Use the smaller Httpz libraries

One `httpz` opam package installs all the libraries below. Choose what your
program links through Dune's `(libraries ...)` field, using the Findlib names
in the first column. The opam package's installation dependencies cover all
of its libraries; the choice here controls the libraries linked into your
program.

| Findlib library | OCaml module | Purpose |
| --- | --- | --- |
| `httpz` | `Httpz` | HTTP/1.1 wire parsing and serialization |
| `httpz.uri` | `Httpz_uri` | URI parsing, resolution, templates and IP classification |
| `httpz.media` | `Httpz_media` | Typed codecs, URL-encoded forms, multipart forms and SSE writers |
| `httpz.jsont` | `Httpz_jsont` | Jsont readers with a JSON nesting limit |
| `httpz.media.jsont` | `Httpz_media_jsont` | JSON and JSON Lines media codecs |
| `httpz.media.cmarkit` | `Httpz_media_cmarkit` | CommonMark and HTML media codecs |
| `httpz.route` | `Httpz_route` | Routing directly over protocol spans |
| `httpz.eio_server` | `Httpz_eio_server` | Eio connection handling and static files |
| `httpz.tls` | `Httpz_tls` | Eio TLS client and server flows |
| `httpz.punycode` | `Punycode` | Punycode encoding and decoding |
| `httpz.punycode.idna` | `Punycode_idna` | NFC normalization and Punycode for domain names; a subset of IDNA |
| `httpz.pubsuffix` | `Pubsuffix` | Public suffix and registrable-domain lookup |
| `httpz.cookie` | `Cookie` | Cookie parsing and serialization |
| `httpz.cookie.jar` | `Cookie_jar` | Concurrent client cookie storage and persistence |

`httpz.uri` shares its URI type with the portable `uriz` library and uses
Base and the OxCaml standard libraries for local allocation. The HTTP wire
library does not link media,
JSON, Markdown, Eio or TLS. `httpz.media` and `httpz.jsont` can each be used
without the HTTP wire library.

Declare each library whose modules your code uses. URI-only code needs
`(libraries httpz.uri)`; code using `Httpz_jsont` and `Jsont` needs
`(libraries httpz.jsont jsont)`.

### Build a URI from a template

`Httpz_uri` parses RFC 3986 URI references. Accessors such as `encoded_path`
preserve percent encoding; `decoded_path` explicitly decodes it.
`Httpz_uri.Template` supports all four RFC 6570 template levels, including
strings, lists and key/value pairs:

```ocaml
let template =
  Httpz_uri.Template.of_string_exn
    "https://api.example.test{/path*}{?accountId}"

let () =
  match Httpz_uri.Template.expand_uri_assoc template
          [ "path", `List [ "jmap"; "download" ];
            "accountId", `String "a/b" ] with
  | Ok uri -> print_endline (Httpz_uri.to_string uri)
  | Error _ -> failwith "URI template expansion failed"
```

With `(libraries httpz.uri)`, this prints:

```text
https://api.example.test/jmap/download?accountId=a%2Fb
```

For parsers working directly with input buffers, `Httpz_uri.Scanner` provides
component offsets without copying component strings.

## OxCaml interfaces

The libraries preserve local allocation, unboxed values and portable callbacks.
URI producer functions have local variants, and `Httpz_uri.Scanner` returns
unboxed spans. Proffer routes can capture portable media codecs and Jsont
schemas. The `release-check` build checks the declared zero-allocation
contracts.

Generating API documentation requires an OxCaml-compatible `odoc`; stock
`odoc` cannot read this compiler's metadata.

## Scope and further reading

The native target is 64-bit little-endian OCaml, validated on Linux x86-64.
The Proffer server buffers the complete request head and body in about 32 KiB, so it
is intended for small requests. Streaming uploads, static-file serving,
conditional writes and browser cookie-policy enforcement are outside its
scope. Fetch supports streaming bodies; HTTP/2 is available through
`fetch-curl`, using libcurl 7.83.0 or later for parsing, decompression and
connection reuse.

See the interfaces under [`httpz/`](bleeding/httpz/), [`fetch/`](bleeding/fetch/) and
[`proffer/`](bleeding/proffer/) for the APIs.

The HTTP libraries use the [ISC license](bleeding/httpz/LICENSE.md). The embedded
Public Suffix List data carries its MPL-2.0 notice.
