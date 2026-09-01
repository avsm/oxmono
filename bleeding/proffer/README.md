# proffer - Declarative HTTP serving for OxCaml

Proffer is a declarative HTTP server library. A handler describes a response
and a backend owns the wire. Dispatch, conditional requests, and HEAD behavior
are shared by the live `proffer-httpz` backend and `proffer.mock` tests.

## Libraries

| Library | Entry point | Purpose |
| --- | --- | --- |
| `proffer` | `Proffer` | Responses, routes, sites, and typed codecs |
| `proffer.mock` | `Proffer_mock` | In-memory requests with no sockets |
| `proffer-httpz` | `Proffer_httpz` | Eio HTTP/1.1 server using `httpz` |

## Quick start

```ocaml
open Proffer
open Proffer.Route

let site =
  Site.of_routes
    [ get root (fun () _req respond -> Resp.text respond "index")
    ; get (s "hello" / str) (fun who () _req respond ->
        Resp.text respond ("hello " ^ who))
    ]
let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~env:() site
```

A path is a chain of segments joined by `( / )`, starting from `root` or a
literal `s "name"`, and ending in `rest` to capture whatever remains. Captures
become curried handler arguments. A GET route also answers HEAD.

Request and response bodies can be typed values. `Route.with_body` obtains a
`Media.t` codec through a callback (which may simply return a captured
module-level codec), decodes the body, and answers 415 or 400 when it cannot.
`Resp.encode` responds with a value through one, and `Negotiate.encode`
chooses among several by the Accept field. `Json` and `Markdown` provide
codecs for Jsont descriptions and Cmarkit documents directly in the main
library.

The live backend serves plaintext by default. TLS is an optional listener
policy: after constructing a `Tls.Config.server` from the site's certificate
and private key, pass `~tls:(Httpz_tls.server config)` to
`Proffer_httpz.run`. The handshake and subsequent HTTP first-byte wait are
both bounded, independently, by `config.first_byte_timeout`.

`proffer.mock` runs the same dispatcher without opening a socket:

```ocaml
let response =
  Proffer_mock.request site () Httpz.Method.Get "/hello/world"
in
assert (Proffer_mock.body response = "hello world")
```

## Tutorial

[`example/`](example#readme) is a step-by-step tutorial made of small complete
programs, from a one-route server through routing, forms, caching, streaming,
authentication and in-memory testing.

## Requirements

Route constructors take their handler at `portable`, so proffer needs the
OxCaml compiler. A handler therefore cannot capture domain-bound state, and a
site is portable by construction. `Media.t` codecs and Jsont descriptions are
portable and may be captured directly. Mutable or otherwise domain-bound state
reaches a handler through the `'env` argument, which the mode system does not
constrain; build that state as a record of closures, one per domain.

## Build

```sh
dune build
dune runtest
```

## License

ISC. See the [license](LICENSE.md).
