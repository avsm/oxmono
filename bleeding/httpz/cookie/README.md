# HTTP cookies

The `httpz.cookie` library implements HTTP cookie parsing, formatting, and
matching for both clients and servers. The `httpz.cookie.jar` library adds a
concurrent Eio client jar with optional persistence in curl's Netscape
`cookies.txt` format. Both libraries are installed by `opam install httpz`.

The implementation follows [RFC 6265](https://www.rfc-editor.org/rfc/rfc6265.html)
and supports the `SameSite`, `__Secure-`, `__Host-`, and `Partitioned`
extensions described by
[RFC 6265bis](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis)
and
[Cookies Having Independent Partitioned State](https://datatracker.ietf.org/doc/html/draft-cutler-httpbis-partitioned-cookies).
It uses `httpz.pubsuffix` to reject cookies scoped to a public suffix; domain
matching separately prevents a server from claiming an unrelated domain.

## Client use

```ocaml
Eio_main.run @@ fun env ->
let clock = Eio.Stdenv.clock env in
let path = Eio.Path.(Eio.Stdenv.cwd env / "cookies.txt") in
let jar = Cookie_jar.of_file ~clock path in
match
  Cookie_jar.set jar ~host:"example.com" ~path:"/" ~https:true
    "session=abc123; Secure; HttpOnly; SameSite=Strict"
with
| Error reason -> Format.eprintf "Cookie rejected: %s@." reason
| Ok () ->
    match
      Cookie_jar.header_for jar ~host:"example.com" ~path:"/api" ~https:true
    with
    | None -> ()
    | Some value -> Format.printf "Cookie: %s@." value
```

The jar limits an individual cookie's name and value together to 4096 bytes,
and its name, value, path, and domain together to 8192 bytes. Thus Path and
Domain have up to 4096 additional bytes beyond a maximum-sized name/value
pair. Each domain is limited to 50 cookies and the whole jar to 3000.
Least-recently-used entries are evicted when a limit is reached. The
`Partitioned` attribute is preserved,
but the jar does not partition storage by top-level site. Session cookies in a
persistent jar are written with expiry zero and therefore survive a file
round-trip.

Persistent saves use a unique private temporary file in the jar's directory
and atomically rename it over the target after the write closes. The directory
must therefore be writable. This provides atomic visibility to concurrent
readers, but does not promise persistence across sudden power loss because the
file and directory are not synchronized with `fsync`.

## Server use

```ocaml
let request_cookies =
  Cookie.parse_cookie_header "session=abc123; theme=dark"

let response_cookie ~now =
  Cookie.v ~domain:"example.com" ~path:"/" ~name:"session" ~value:"abc123"
    ~secure:true ~http_only:true ~expiry:`Session ~now ()

let set_cookie_value ~now = Cookie.set_cookie_header (response_cookie ~now)
```

The `fetch.cookies` library integrates a `Cookie_jar` with a Fetch client.

The installed `httpz-cookiecat FILE` command prints the contents of a
Netscape-format cookie file for inspection. It exits nonzero, rather than
printing an empty jar, when `FILE` cannot be opened.
