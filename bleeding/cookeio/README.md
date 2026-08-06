# Cookeio - HTTP Cookie Management for OCaml

Cookeio implements the [RFC 6265](https://datatracker.ietf.org/doc/html/rfc6265)
cookie model for both sides of the protocol, with the SameSite, cookie-prefix
and Partitioned extensions of
[RFC 6265bis](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis)
and CHIPS.

The `cookeio` library is the pure cookie model:

- a **client** parses `Set-Cookie` with `Cookeio.parse_set_cookie`, which
  enforces the full §5.2/§5.3 storage rules — the `Domain` attribute must
  domain-match the setting host, public suffixes are refused (via the
  Mozilla Public Suffix List), `Max-Age` takes precedence over `Expires`,
  missing paths take the §5.1.4 default, and `__Secure-`/`__Host-` names
  must carry the attributes they promise — and serializes stored cookies
  with `Cookeio.cookie_header`;
- a **server** parses `Cookie` with `Cookeio.parse_cookie_header` and emits
  `Set-Cookie` with `Cookeio.set_cookie_header`.

The `cookeio.jar` library is a client-side jar built on Eio: storage per
§5.3, retrieval per §5.4, the §6.1 bounds (4096-byte cookies, 50 per
domain, 3000 in all, LRU eviction), refusal of plaintext cookies that
would shadow `Secure` ones, and persistence in the curl-compatible
Netscape cookies.txt format, including curl's `#HttpOnly_` marking.

## Usage

A client storing and sending cookies:

```ocaml
Eio_main.run @@ fun env ->
let clock = Eio.Stdenv.clock env in
let jar = Cookeio_jar.of_file ~clock Eio.Path.(Eio.Stdenv.cwd env / "cookies.txt") in
(* store each Set-Cookie value a response carried *)
(match Cookeio_jar.set jar ~host:"example.com" ~path:"/" ~https:true
         "session=abc123; Secure; HttpOnly; SameSite=Strict" with
 | Ok () -> ()
 | Error reason -> Format.eprintf "cookie refused: %s@." reason);
(* build the Cookie header for the next request *)
match Cookeio_jar.header_for jar ~host:"example.com" ~path:"/api" ~https:true with
| Some header -> Format.printf "Cookie: %s@." header
| None -> ()
```

A server reading and setting cookies:

```ocaml
let pairs = Cookeio.parse_cookie_header "session=abc123; theme=dark" in
let cookie =
  Cookeio.v ~domain:"example.com" ~path:"/" ~name:"session" ~value:"abc123"
    ~secure:true ~http_only:true ~expiry:`Session ~now ()
in
let header = Cookeio.set_cookie_header cookie
```

The `fetch.cookies` library attaches a `Cookeio_jar` to a
[fetch](../fetch/) HTTP client as a middleware.

## History

The client side of this library was reworked inside fetch (as a vendored
`Cookie` module) and folded back here, replacing the original parser,
which lacked the domain-match check on `Set-Cookie`, default paths,
domain canonicalization and the prefix rules. The earlier server-side
delta jar was removed with its last consumer; the server role is now
served by the pure functions in `Cookeio`.
