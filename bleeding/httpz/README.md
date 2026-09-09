# HTTPz

HTTPz supplies the bounded HTTP/1.1 parser and writer used by Fetch and Proffer.
Server applications use `proffer` and `proffer-httpz`; HTTP clients use `fetch`
with `fetch-httpz` or `fetch-curl`.

The [HTTP guide](../../HTTPZ.md) explains library selection and backend limits.
The [examples](../../example/README.md) provide complete client and server
programs. [HTTPZ_SYNC.md](../../HTTPZ_SYNC.md) records synchronization with the
standalone OxCaml checkout and intentional monorepo adaptations.

## Libraries

| Findlib library | Entry module | Purpose |
| --- | --- | --- |
| `httpz` | `Httpz` | Bounded HTTP/1.1 parsing, framing and serialization |
| `httpz.uri` | `Httpz_uri` | Shared Uriz values, URI templates and IP classification |
| `httpz.media` | `Httpz_media` | Typed media, JSON, Markdown, multipart and forms |
| `httpz.dav` | `Httpz_dav` | Bounded WebDAV protocol values and XML codecs |
| `httpz.tls` | `Httpz_tls` | Eio TLS client and server flows |
| `httpz.websocket` | `Httpz_websocket` | WebSocket handshakes, byte framing and bounded connections |
| `httpz.cookie` | `Cookie` | Cookie parsing and serialization |
| `httpz.cookie.jar` | `Cookie_jar` | Cookie persistence and request selection |
| `httpz.punycode` | `Punycode` | Punycode conversion |
| `httpz.punycode.idna` | `Punycode_idna` | Domain-name normalization |
| `httpz.pubsuffix` | `Pubsuffix` | Public suffix and registrable-domain lookup |

The [wire interface](lib/httpz.mli) exposes borrowed spans, unboxed parse results
and bounded buffers. The [URI interface](uri/httpz_uri.mli) shares its value type
with the monorepo's `Uriz.t`, preserving existing client type compatibility.

`httpz-pubsuffix` queries the bundled suffix data and `httpz-cookiecat` reads
cookie jars. Component provenance and data refresh instructions are in
[VENDORED.md](VENDORED.md).

## Build and test

Run these commands from the monorepo root:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check @bleeding/httpz/all
opam exec --switch=5.2.0+ox -- dune build --profile release-check @bleeding/httpz/runtest
opam exec --switch=5.2.0+ox -- dune build --profile release-check @bleeding/httpz/fuzz/fuzz
```

The retained benchmarks measure parser throughput, HTTP field operations and
chunking. Proffer's benchmarks cover routing and response dispatch. The former
Core_bench executable was disabled by an incompatible dependency closure and
has been removed.

[SECURITY.md](SECURITY.md) is a historical audit snapshot. Consult the sync
record and current regression tests for fixes applied since that snapshot.
