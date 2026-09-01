# httpz - Zero-Allocation HTTP/1.1 Parser for OxCaml

A high-performance HTTP/1.1 parser and serializer achieving zero heap
allocations using OxCaml's unboxed types (`int16#`, `int64#`, `char#`, `float#`)
and local allocations.

Will soon have io_uring on Linux.

## Features

- **Zero heap allocations**: Parsing, response writing and routing allocate nothing on the heap — parser results are stack-allocated using OxCaml unboxed records and local lists, and the router matches literal segments against the buffer in place. Materialising a value as a `string` (a captured path segment, a header value, a body) allocates, necessarily.
- **Unboxed integers throughout**: Uses `int16#` for offsets/counts and `int64#` for content lengths - no boxing overhead
- **Caller-owned buffers**: Parsing and response writing operate on plain `bytes` buffers supplied by the caller; the library never allocates a buffer for you
- **HTTP/1.1 support**: Methods, headers, chunked transfer encoding, keep-alive, `Expect: 100-continue`, range requests, ETags, HTTP-dates
- **Trie-based routing**: Type-safe segment patterns with O(path depth) dispatch, matched against the parse buffer without materialising segments
- **Eio server runtime**: The core parser and router drive an Eio static file
  server with range and conditional GET support

## Libraries

The core code is split into three libraries plus one executable:

| Directory       | Library name         | Entry point           | Purpose |
|-----------------|----------------------|-----------------------|---------|
| `lib/`          | `httpz`              | `Httpz`               | Protocol types, request parsing, response writing |
| `route/`        | `httpz.route`        | `Httpz_route`         | Segment routing with span-keyed trie dispatch |
| `eio_server/`   | `httpz.eio_server`   | `Httpz_eio_server`    | Eio connection lifecycle, chunked bodies, 100-continue, `Static` file serving |

| Executable            | CLI style          | Built from                    |
|-----------------------|--------------------|-------------------------------|
| `httpz-eio-server`    | `cmdliner`         | `bin/httpz_eio_server.ml`     |

`httpz.route` is shared by `httpz.eio_server` for dispatch.

The `httpz` opam package additionally owns the namespaced support libraries
`httpz.punycode`, `httpz.punycode.idna`, `httpz.pubsuffix`, `httpz.cookie`,
and `httpz.cookie.jar`.

`lib` has no dependency on the server or I/O layers, so it can be embedded in
any event loop. The modules it exposes are:

- `Scan` - SIMD byte-class scans (CR, SP-or-CR, token characters) over the parse buffer
- `Buf_read` / `Buf_write` - reading and writing primitives, parse `status`, security `limits`
- `Span` - unboxed `#{ off : int16#; len : int16# }` references into the parse buffer
- `Method`, `Version`, `Header_name`, `Header` - protocol enumerations and header records
- `Req` - the parsed request record, with cached content headers
- `Target` - request-target splitting into path and query, plus zero-allocation
  segment matching and query-parameter lookup
- `Res` - status codes and response header/chunk writers
- `Chunk` - chunked transfer-encoding parsing, including trailers
- `Etag`, `Date`, `Range` - RFC 7232/7231/7233 conditional and range support
- `Parser`, `Err` - the low-level request-line/header parser and its error signalling

## Architecture

httpz achieves zero-allocation parsing through:

1. **Unboxed records** (`#{...}`): Request, span, header state and target types are stack-allocated
2. **Unboxed primitives**: `int16#` for buffer offsets, `int64#` for content lengths, `char#` for byte comparisons
3. **Local lists** (`@ local`): Header list grows on the stack, not heap
4. **Span-based parsing**: Strings are referenced by offset+length into the input buffer
5. **Caller-owned `bytes` buffers**: 32KB (`Httpz.buffer_size`) buffers, allocated once and reused across requests
6. **Explicitly threaded position**: The parser carries no mutable state; the read position is passed and returned, so no parser record is ever allocated
7. **Word-at-a-time scans**: `Scan` and `Span` compare eight bytes at a time
   with portable SWAR operations

`int16#` offsets are what fix the buffer size at 32KB: a whole request head must
fit within a single `int16#`-addressable buffer.

Content-related headers are folded into the request record while headers are
being scanned, and are *excluded* from the returned header list:

| Header | Field on `Req.t` |
|--------|------------------|
| `Content-Length` | `req.#content_length` (`-1L` when absent) |
| `Transfer-Encoding: chunked` | `req.#is_chunked` |
| `Connection` | `req.#keep_alive` |
| `Expect: 100-continue` | `req.#expect_continue` |

The parser also enforces RFC 7230 security requirements: Content-Length
overflow limits, bare-CR detection (request smuggling, Section 3.5), rejection
of ambiguous framing (both `Content-Length` and `Transfer-Encoding`), and the
HTTP/1.1 `Host` header requirement.

## Parsing a request

```ocaml
let i16 = Httpz.Buf_read.i16

(* Allocate once, reuse for every request on the connection *)
let buf = Bytes.create Httpz.buffer_size

let handle (len : int) =
  let #(status, req, headers) =
    Httpz.parse buf ~len:(i16 len) ~limits:Httpz.default_limits
  in
  match status with
  | Httpz.Buf_read.Complete ->
    Printf.printf "%s %s %s\n"
      (Httpz.Method.to_string req.#meth)
      (Httpz.Span.to_string buf req.#target)
      (Httpz.Version.to_string req.#version);
    (* Cached content headers - no list search needed *)
    if req.#is_chunked then read_chunked_body ();
    (* Other headers are found by typed name *)
    (match Httpz.Header.find headers Httpz.Header_name.Host with
     | Some hdr -> use_host (Httpz.Span.to_string buf hdr.Httpz.Header.value)
     | None -> ())
  | Httpz.Buf_read.Partial ->
    read_more_and_retry ()
  | s ->
    (* Invalid_method, Bare_cr_detected, Ambiguous_framing, ... *)
    reject (Httpz.Buf_read.status_to_string s)
```

Comparisons against spans avoid materialising strings entirely:

```ocaml
if Httpz.Span.equal buf req.#target "/health" then respond_ok ()
```

## Writing a response

Response writers append to a `bytes` buffer at an `int16#` offset and return the
new offset:

```ocaml
let i16 = Httpz.Buf_write.i16
let to_int = Httpz.Buf_write.to_int

let write_headers buf body =
  let off =
    Httpz.Res.write_status_line buf ~off:(i16 0)
      Httpz.Res.Success Httpz.Version.Http_1_1
  in
  let off =
    Httpz.Res.write_header_name buf ~off Httpz.Header_name.Content_type "text/html"
  in
  let off = Httpz.Res.write_content_length buf ~off (String.length body) in
  let off = Httpz.Res.write_connection buf ~off ~keep_alive:true in
  let off = Httpz.Res.write_crlf buf ~off in
  to_int off  (* write buf[0..off] to the socket, then the body *)
```

`Res` also provides `write_transfer_encoding_chunked`, `write_chunk_header`,
`write_chunk_footer` and `write_final_chunk` for responses of unknown length.

## Routing

`httpz.route` provides segment-based routing. Path patterns are typed, so
captured segments arrive as a tuple in the handler, and dispatch walks a trie of
literal segments:

```ocaml
open Httpz_route

let routes =
  of_list
    [ get_ [] (fun _ctx respond -> html respond "<h1>Welcome</h1>")
    ; get_ [ "api"; "status" ] (fun _ctx respond -> json respond {|{"ok":true}|})
    ; get ("users" / seg root) (fun (user_id, ()) _ctx respond ->
        plain respond (Printf.sprintf "user %s" user_id))
    ; get ("static" / tail) (fun path _ctx respond ->
        plain respond (String.concat "/" path))
    ]
```

Pattern vocabulary: `root` matches `/`, `"lit" / rest` matches a literal
segment, `seg rest` captures one segment, and `tail` captures all remaining
segments. `( / )` is left-associative, as every OCaml operator starting with
`/` must be, so chained literals need parentheses on the right —
`"api" / ("v1" / seg root)`. `get_`/`post_` take a plain segment list when nothing is captured, and
the `_h` variants (`get_h`, `post_h`, ...) additionally require named headers,
which are passed to the handler as `string option`s.

The `ctx` argument gives access to the request: `meth`, `is_head`, `path`,
`query`, `query_param`, `body` (a zero-copy `Span.t`), `body_string` and
`content_length`. HEAD requests are automatically matched against GET routes;
the `_gen` response helpers (`html_gen`, `json_gen`, ...) skip body generation
entirely for HEAD.

Responses are written through a `respond` callback whose header list is
`local_`, so no header list is heap-allocated. Bodies are `Empty`, `String`,
`Bigstring` (for zero-copy file serving) or `Stream`.

## Eio server

`httpz.eio_server` supplies the connection loop: parsing, keep-alive, chunked request
bodies, `Expect: 100-continue` and response writing.

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let sock =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~backlog:128 ~reuse_addr:true addr
  in
  Eio.Net.run_server sock
    ~on_error:(fun exn -> prerr_endline (Printexc.to_string exn))
    (fun flow client_addr ->
       Httpz_eio_server.handle_client
         ~routes
         ~on_request:(fun (info @ local) ->
           Printf.printf "%s %s -> %s (%dus)\n%!"
             (Httpz.Method.to_string info.meth)
             info.path
             (Httpz.Res.status_to_string info.status)
             info.duration_us)
         ~on_error:(fun exn -> prerr_endline (Printexc.to_string exn))
         flow client_addr)
```

`on_request` receives a `Httpz_eio_server.request_info` mixed block passed `@ local`,
so per-request logging metadata is stack-allocated: the timestamp is a `float#`
and optional fields use `or_null` rather than `option`, avoiding the `Some`
boxes.

## Performance

Measured on OxCaml `5.2.0+ox` (`5.2.0minus-39`) with `make bench`. Figures are
from two runs that agreed to within 2%.

### Build the release profile, always

**Benchmark and deploy with `--profile release`.** Dune's default `dev` profile
passes `-opaque` for every module that has an `.mli`, which disables flambda2's
cross-module inlining. The per-byte primitives in `Buf_read`, `Scan` and `Span`
then become indirect — and in some cases unknown-arity `caml_apply` — calls from
`Parser` and `Header_name`, executed once per scanned byte. Parsing runs about
twice as slow, and the penalty grows with header count. Confirm which you are
getting with:

```bash
dune rules --profile dev _build/default/lib/.httpz.objs/native/httpz__Parser.cmx \
  | grep -c opaque   # 1 in dev, 0 in release
```

`dune build -p`, and therefore opam, already builds in release, so consumers of
the published package get the fast code. `make bench` uses release explicitly.

### httpz vs httpe

`httpe` is a conventional Eio `Buf_read` parser, vendored unchanged under
`bench/vendor/` so both parsers are measured in the same process and run.

| Request Size  | httpz (ns/op) | httpe (ns/op) | Ratio        | Allocation      |
|---------------|---------------|---------------|--------------|-----------------|
| Small (35B)   | 56            | 275           | 4.9x faster  | 95x fewer words |
| Medium (439B) | 300           | 1,870         | 6.2x faster  | 401x fewer      |
| Large (1155B) | 667           | 4,400         | 6.6x faster  | 832x fewer      |

**Throughput** (small requests): ~17M requests/sec.

Numbers are medians of five runs. They are not comparable with those published
before August 2026: the harness used to copy the request into the parse buffer
with a bounds-checked byte loop on every iteration, which at ~0.8ns/byte was
about two thirds of the reported time for the large request — so it measured
the copy more than the parser, and understated httpz against `httpe`, which
wraps its input without copying. It now blits.

Against the same fixed harness, the vectorised token scan and the
four-then-two-byte tail in `Span` are worth **-24% on medium and -20% on
large** requests. The 35-byte request is about **8% slower**: it is too short
for either to pay for itself, and that shows up directly because there is
almost nothing else in the measurement.

### Detailed Timings

From `core_bench`. The `mWd/Run` column reads 3.00w for every parsing and
serialization benchmark, which is exactly the figure reported by the
`httpz_noop` baseline — that is, zero words attributable to httpz itself.

| Operation | Time | Heap Allocations |
|-----------|------|------------------|
| Parse minimal request (35B) | 58.7ns | 0 words |
| Parse simple request (4 headers) | 136ns | 0 words |
| Parse browser request (10 headers) | 323ns | 0 words |
| Parse 50 headers | 1.10μs | 0 words |
| Write status line | 7.1ns | 0 words |
| Write full response headers | 19.6ns | 0 words |
| Read body (100B / 1KB / 10KB) | 110ns / 122ns / 164ns | 0 words |

Parsing and serialization are genuinely zero-allocation: all values are
stack-allocated via unboxed records and an explicitly threaded position. Body
access used to be the exception, allocating 4 words per call; the culprit was a
boxed tuple inside an `option` that flambda2 unboxed only halfway, and it now
returns an unboxed tuple.

The body timings above are dominated by the harness copy, not by httpz — the
parser does not touch the body — so they moved with the blit fix described
above rather than because body access got faster.

### Beyond the parser

`bench_alloc` covers the paths `core_bench` does not: routing, the
conditional-request modules and chunked decoding. Per operation, measured with
`Gc.minor_words`:

| Operation | Time | Heap Allocations |
|-----------|------|------------------|
| Dispatch `/api/v1/health` (literal) | 51ns | 0 words |
| Dispatch `/users/12345` (one capture) | 47ns | 5 words |
| Dispatch `/static/a/b/c.css` (tail) | 61ns | 15 words |
| `Date.format` | 30ns | 5 words |
| `Date.write_http_date` | 4.6ns | 0 words |
| `Etag.parse_match_header` (3 tags) | 53ns | 0 words |
| `Range.parse_string` | 29ns | 0 words |
| `Chunk.parse` x4 | 51ns | 0 words |

Routing a path made only of literals allocates nothing: segments are compared
against the parse buffer in place, and a string is built only where a `seg` or
`tail` actually captures one — which is what the 5 and 15 words above are. The
`ctx` passed to handlers is stack-allocated, which is why it is `local`.

### Where the time goes

The scans that dominate parsing are vectorised or word-at-a-time:

| Primitive | Technique |
|---|---|
| `Scan.find_cr`, `Scan.find_sp_or_cr` | SWAR "haszero", 8 bytes per step |
| `Scan.skip_token` | 256-byte table derived from the scalar predicate |
| `Span.equal`, `Span.equal_caseless` | 8 bytes per step, then 4 and 2 for the tail — most header names are 4-17 bytes, so the tail is where the time was; case folding is a branchless `upper_mask` |
| `Span.split_on_char` | 8 bytes per step via the SWAR "haszero" trick, resolving the lowest marked byte |
| `Header_name.of_span` | switch on name length and folded first byte, leaving at most three candidates |
| `Res.write_status_line` | one memcpy from a static per-status string, rather than a digit loop and five copies |
| `Buf_write.int` | two digits per division from a 200-byte table |
| `Date.write_http_date` | `civil_from_days` plus table writes; no `gmtime`, no `Printf` |

The case-folding paths test for a high bit before folding a word. That guard is
not redundant even though the literal being compared is normally ASCII: the
literal is caller-supplied, and without the test the range comparisons carry
into the neighbouring byte and report a false match. `test_scan.ml` fails with
several hundred mismatches if it is removed.

The scanner uses unboxed integer operations but no architecture-specific SIMD,
so native and bytecode builds select the same implementation. It is
differentially tested against byte-loop reference implementations.

## Installation

Requires the OxCaml compiler from https://oxcaml.org/

```sh
dune build
dune runtest
```

## Static File Server

The Eio static file server ships as a `cmdliner` binary:

```bash
dune exec bin/httpz_eio_server.exe
dune exec bin/httpz_eio_server.exe -- -d /var/www -p 3000
dune exec bin/httpz_eio_server.exe -- --help
```

It provides MIME type detection, directory traversal protection, automatic
`index.html` for directories, `Range` requests with 206/416, ETags and
`If-None-Match` conditional GET, and keep-alive. Specifically:

| | |
|---|---|
| Built on | `Httpz_route` + `Httpz_eio_server.Static` |
| `If-None-Match` parsing | full RFC 7232 via `Httpz.Etag` (`*`, `W/`, comma lists) |
| Percent-decoded paths | yes |
| `HEAD` | headers only, file never opened |
| Large files | streamed in 64KB chunks above 1MB |
| Configurable index names | yes |
| Concurrency | Eio fibers |

It does not implement `multipart/byteranges` for multiple ranges in one
request, `If-Modified-Since`, or `If-Range`; only the first range requested is
served.

## Running Benchmarks

```bash
make bench          # both of the below, in the release profile

# Comparative benchmark (httpz vs httpe)
dune exec --profile release bench/bench_compare.exe

# Detailed httpz benchmarks with core_bench
dune exec --profile release bench/bench_httpz.exe -- -quota 2
```

The `--profile release` is not optional: see [Performance](#performance) for
why the `dev` profile roughly halves parsing throughput.

## RFC Compliance

`spec/RFC_COMPLIANCE.md` tracks the implementation against RFC 7230-7235, with
the RFC texts themselves vendored under `spec/` for reference.

## License

ISC. See the [repository license](LICENSE.md).
