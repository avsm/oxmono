# proffer - Declarative HTTP serving for OxCaml

A proffer handler returns a description of a response rather than writing one.
A backend consumes that description and owns the wire, so dispatch, conditional
requests and HEAD are decided once and behave the same everywhere. The core
depends on the stdlib alone, holding no sockets and no buffers.

## Libraries

| Library         | Entry point     | Purpose                                                             |
|-----------------|-----------------|---------------------------------------------------------------------|
| `proffer`       | `Proffer`       | Requests, responses, typed routing, sites and the compiled dispatcher |
| `proffer.mock`  | `Proffer_mock`  | Runs a compiled site against synthetic requests, with no sockets      |
| `proffer-httpz` | `Proffer_httpz` | Serves a compiled site over HTTP/1.1 on the httpz parser and Eio      |

## Quick start

```ocaml
open Proffer
open Proffer.Route

type env = { greet : string -> string }

let site =
  Site.of_routes
    [ get nil (fun _env _req -> Resp.text "index")
    ; get (s "hello" / str /? nil) (fun who env _req ->
        let html = env.greet who in
        Resp.html ~etag:(`Strong (Digest.to_hex (Digest.string html))) html)
    ; get (s "notes" / int /? nil) (fun n _env _req ->
        Resp.text (Printf.sprintf "note %d" n))
    ; post (s "hello" /? nil) (fun _env req ->
        match Req.form_param req "who" with
        | Some who -> Resp.see_other ("/hello/" ^ who)
        | None -> Resp.bad_request ())
    ]

let compiled = Compiled.compile site
let env = { greet = (fun who -> "<h1>hello " ^ who ^ "</h1>") }

let () =
  Eio_main.run @@ fun stdenv ->
  Eio.Switch.run @@ fun sw ->
  Proffer_httpz.run ~sw
    ~net:(Eio.Stdenv.net stdenv)
    ~clock:(Eio.Stdenv.clock stdenv)
    ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8380))
    ~on_error:(fun exn -> prerr_endline (Printexc.to_string exn))
    ~env compiled
```

A pattern is a chain of fragments joined by `( / )` and closed with `( /? ) nil`,
or with `( /* ) rest` to capture every remaining segment. Each capture arrives as
a curried handler argument, so `s "hello" / str /? nil` demands a handler of type
`string -> env -> Req.t -> Resp.t`. The converters are `str`, `int` and
`conv ~name parse`.

## What the core decides for every backend

- Dispatch. The decoded path segments and the method select a route. A GET route
  also answers HEAD. A path that matches only under other methods gives 405 with
  an `Allow` field, and a path that matches nothing gives the site's fallback.
- Conditional GET per RFC 9110. A response carrying an ETag is checked against
  `If-None-Match`, including the comma-separated list and `*` forms, under weak
  comparison. Failing that, and only when `If-None-Match` is absent, a response
  carrying `Last-Modified` is checked against `If-Modified-Since`. A 304 carries
  the validators, `Cache-Control` and `Vary` of the full response, and never runs
  a delayed body generator.
- HEAD. The body is emptied and the content length is kept, so a backend sends an
  accurate `Content-Length` with no body.
- Cache policy. `Cache_control.t` is typed data, serialised to its header value
  when the policy is described rather than once per response.

`Resp.v` rejects a malformed response at construction. It raises
`Invalid_argument` for a header name that is not a token, a header value
containing CR, LF or NUL, and an entity-tag opaque value containing a double
quote.

Backend authors work through `Proffer.Backend`, which exposes the outcome type
and the `handle` function the two shipped backends call. Ordinary users never
name it.

## Testing a site

`proffer.mock` dispatches a synthetic request through the same code a socket
backend runs, so a test asserts on the real routing, conditional and HEAD
behaviour without opening a socket.

```ocaml
let () =
  let r = Proffer_mock.request compiled env `GET "/hello/world" in
  assert (Proffer_mock.status r = `OK);
  assert (Proffer_mock.body r = "<h1>hello world</h1>");
  let etag = Option.get (Proffer_mock.header r "etag") in
  let r = Proffer_mock.request compiled env `GET "/hello/world"
            ~headers:[ ("If-None-Match", etag) ] in
  assert (Proffer_mock.status r = `Not_modified)
```

A streaming body is run to completion into a buffer, so `body` and
`content_length` report what a client would have received. Pass `?on_error` to
observe an exception a handler raised on its way to a 500.

## Requirements

Route constructors take their handler at `portable`, so proffer needs the OxCaml
compiler. A handler therefore cannot capture domain-bound state, and a compiled
site is portable by construction. State a handler needs reaches it through the
`'env` argument, which the mode system does not constrain. Build that value as a
record of closures, one per domain.

## Building

```bash
dune build
dune runtest
```

## License

ISC. See `LICENSE.md`.
