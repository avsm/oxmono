# Cookie jars

```ocaml
# #require "fetch";;
# #require "fetch.mock";;
# #require "fetch.cookies";;
# #require "eio.mock";;
```

```ocaml
let () = Printexc.record_backtrace false
open Fetch
module Cookies = Fetch_cookies

let site (req : Middleware.request) =
  Fmt.pr "> GET %s (cookie: %s)@."
    (Middleware.Url.to_string req.url)
    (Option.value (Http.Header.get req.headers "cookie") ~default:"-");
  match Middleware.Url.path_segments req.url with
  | [ "login" ] ->
    Fetch_mock.respond ~status:302
      ~headers:(Http.Header.of_list
                  [ "Set-Cookie", "sid=abc123; Path=/; HttpOnly";
                    "Set-Cookie", "theme=dark; Path=/";
                    "Location", "/account" ]) "" req
  | [ "admin"; _ ] ->
    Fetch_mock.respond ~headers:(Http.Header.of_list
                                      [ "Set-Cookie", "admin=1; Path=/admin" ]) "ok" req
  | _ -> Fetch_mock.respond "ok" req

let run fn = Eio_mock.Backend.run_full @@ fun env -> fn env
```

## Login flow: Set-Cookie captured on the 302, sent from then on

The redirect hop already carries the session cookie set by the 302:

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  let t = Cookies.with_jar jar (Fetch_mock.client site) in
  ignore (Fetch.read t "https://shop.example/login");
  ignore (Fetch.read t "https://shop.example/basket");;
> GET https://shop.example/login (cookie: -)
> GET https://shop.example/account (cookie: sid=abc123; theme=dark)
> GET https://shop.example/basket (cookie: sid=abc123; theme=dark)
- : unit = ()
```

## Path scoping

A cookie with `Path=/admin` is not sent to other paths:

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  let t = Cookies.with_jar jar (Fetch_mock.client site) in
  ignore (Fetch.read t "https://shop.example/admin/panel");
  ignore (Fetch.read t "https://shop.example/admin/users");
  ignore (Fetch.read t "https://shop.example/public");;
> GET https://shop.example/admin/panel (cookie: -)
> GET https://shop.example/admin/users (cookie: admin=1)
> GET https://shop.example/public (cookie: -)
- : unit = ()
```

## Host-only cookies do not travel to other hosts

One jar, one client, two hosts — a host-only cookie stays with the exact host
that set it (each redirect hop consults the jar for its own URL):

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  let t = Cookies.with_jar jar (Fetch_mock.client site) in
  ignore (Fetch.read t "https://shop.example/login");
  ignore (Fetch.read t "https://other.example/page");
  ignore (Fetch.read t "https://shop.example/basket");;
> GET https://shop.example/login (cookie: -)
> GET https://shop.example/account (cookie: sid=abc123; theme=dark)
> GET https://other.example/page (cookie: -)
> GET https://shop.example/basket (cookie: sid=abc123; theme=dark)
- : unit = ()
```

A redirect chain rechecks host, `Domain`, path and `Secure` rules at each
hop, applying replacements before the next request. Here an unrelated host
cannot claim the API's cookies, and an expired replacement removes `plain`.
Ports do not partition cookies; an explicit `Domain=example.test` could share
cookies between these sibling hosts.

```ocaml
# run @@ fun env ->
  let server (req : Middleware.request) =
    Fmt.pr "> GET %s (cookie: %s)@."
      (Middleware.Url.to_string req.url)
      (Option.value (Http.Header.get req.headers "cookie") ~default:"-");
    let headers =
      match Middleware.Url.host req.url, Middleware.Url.path_segments req.url with
      | "evil.example.test", [ "start" ] ->
          [ "Set-Cookie", "sid=evil; Path=/";
            "Set-Cookie", "sid=evil-domain; Domain=api.example.test; Path=/";
            "Location", "https://api.example.test/seed" ]
      | "api.example.test", [ "seed" ] ->
          [ "Set-Cookie", "secure=1; Secure; Path=/private";
            "Set-Cookie", "plain=1; Path=/private";
            "Location", "/public" ]
      | "api.example.test", [ "public" ] ->
          [ "Location", "http://api.example.test/private" ]
      | "api.example.test", [ "private" ] ->
          [ "Location", "https://api.example.test/private/expire" ]
      | "api.example.test", [ "private"; "expire" ] ->
          [ "Set-Cookie", "plain=; Max-Age=0; Path=/private";
            "Location", "/private/done" ]
      | _ -> []
    in
    if headers = [] then Fetch_mock.respond "ok" req
    else Fetch_mock.respond ~status:302 ~headers:(Http.Header.of_list headers) "" req
  in
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  let t = Cookies.with_jar jar (Fetch_mock.client server) in
  Fetch.with_response ~allow_downgrade:true t `GET
    "https://evil.example.test/start" (fun _ -> ());;
> GET https://evil.example.test/start (cookie: -)
> GET https://api.example.test/seed (cookie: -)
> GET https://api.example.test/public (cookie: -)
> GET http://api.example.test/private (cookie: plain=1)
> GET https://api.example.test/private/expire (cookie: plain=1; secure=1)
> GET https://api.example.test/private/done (cookie: secure=1)
- : unit = ()
```

## Secure cookies stay off plaintext connections

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "https://shop.example/" "token=s3cret; Secure; Path=/";
  let t = Cookies.with_jar jar (Fetch_mock.client site) in
  ignore (Fetch.read t "https://shop.example/page");
  ignore (Fetch.read t "http://shop.example/page");;
> GET https://shop.example/page (cookie: token=s3cret)
> GET http://shop.example/page (cookie: -)
- : unit = ()
```

## `Domain=` may widen to a parent, never to another domain

A host can scope a cookie to a domain it belongs to, but not to one it
does not
([RFC 6265 §5.3](https://www.rfc-editor.org/rfc/rfc6265#section-5.3),
step 6), and never to a public suffix:

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "https://a.shop.example/" "ok=1; Domain=shop.example; Path=/";
  Cookies.Jar.set jar "https://evil.example/" "hijack=1; Domain=shop.example; Path=/";
  Cookies.Jar.set jar "https://shop.co.uk/" "super=1; Domain=co.uk; Path=/";
  (Cookies.Jar.header_for jar "https://shop.example/",
   Cookies.Jar.header_for jar "https://other.co.uk/");;
- : string option * string option = (Some "ok=1", None)
```

A `Domain` naming the public suffix that the request host *is* — a
shared-hosting name such as `s3.amazonaws.com` — is ignored rather than
honoured
([RFC 6265 §5.3](https://www.rfc-editor.org/rfc/rfc6265#section-5.3),
step 5), so the cookie stays on that host and no site beneath it sees it:

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "https://s3.amazonaws.com/"
    "sid=x; Domain=s3.amazonaws.com; Path=/";
  (Cookies.Jar.header_for jar "https://s3.amazonaws.com/",
   Cookies.Jar.header_for jar "https://evil-bucket.s3.amazonaws.com/");;
- : string option * string option = (Some "sid=x", None)
```

An address has no subdomains and no public suffix, so a `Domain` on one
can only restate the host. The URL layer has already folded every
`inet_aton(3)` spelling to the dotted quad the jar stores:

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "http://127.1/" "a=1; Domain=127.0.0.1; Path=/";
  Cookies.Jar.set jar "http://0x7f.1/" "b=1; Domain=1; Path=/";
  (Cookies.Jar.header_for jar "http://127.0.0.1/",
   Cookies.Jar.header_for jar "http://0xc0.0.0.1/");;
- : string option * string option = (Some "a=1", None)
```

A cookie arriving over plaintext cannot replace a `Secure` one:

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "https://shop.example/" "sid=SECURE; Secure; Path=/";
  Cookies.Jar.set jar "http://shop.example/" "sid=SPOOFED; Path=/";
  Cookies.Jar.header_for jar "https://shop.example/";;
- : string option = Some "sid=SECURE"
```

The `__Secure-` and `__Host-` name prefixes claim the cookie was stored
with those protections. The
[RFC 6265bis storage algorithm](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis#section-5.7)
requires `Secure` for both, plus host-only scope and `Path=/` for
`__Host-`. A `Set-Cookie` that does not honour the claim is rejected, as
is any prefixed cookie arriving over plaintext:

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "https://shop.example/" "__Host-sid=1; Secure; Path=/";
  Cookies.Jar.set jar "https://shop.example/" "__Secure-tok=2; Secure; Path=/";
  Cookies.Jar.set jar "https://shop.example/" "__Secure-a=x; Path=/";
  Cookies.Jar.set jar "https://shop.example/" "__Host-b=x; Path=/";
  Cookies.Jar.set jar "https://shop.example/" "__Host-c=x; Secure; Domain=shop.example; Path=/";
  Cookies.Jar.set jar "https://shop.example/" "__Host-d=x; Secure";
  Cookies.Jar.set jar "http://shop.example/" "__Host-e=x; Secure; Path=/";
  Cookies.Jar.header_for jar "https://shop.example/";;
- : string option = Some "__Host-sid=1; __Secure-tok=2"
```

## Expiry against the jar's clock

`Max-Age=10` is set, then virtual time passes (the mock wall clock is linked to
the auto-advancing monotonic clock):

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "https://shop.example/" "temp=1; Max-Age=10; Path=/";
  let t = Cookies.with_jar jar (Fetch_mock.client site) in
  ignore (Fetch.read t "https://shop.example/a");
  Eio.Time.Mono.sleep env#mono_clock 60.;
  ignore (Fetch.read t "https://shop.example/b");;
> GET https://shop.example/a (cookie: temp=1)
+mock time is now 60
> GET https://shop.example/b (cookie: -)
- : unit = ()
```

## Cookie storage details

A `Domain=` attribute in any case matches later requests — domains are
canonicalized once, at parse time:

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "https://a.shop.example/" "case=1; Domain=Shop.Example; Path=/";
  Cookies.Jar.header_for jar "https://a.shop.example/page";;
- : string option = Some "case=1"
```

`Max-Age` beats `Expires` regardless of attribute order
([RFC 6265 §5.3](https://www.rfc-editor.org/rfc/rfc6265#section-5.3),
step 3). Here a stale `Expires` follows a live `Max-Age`:

```ocaml
# run @@ fun env ->
  Eio.Time.sleep env#clock 10.0;
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "https://shop.example/"
    "o=1; Max-Age=100000; Expires=Thu, 01 Jan 1970 00:00:01 GMT; Path=/";
  Cookies.Jar.header_for jar "https://shop.example/";;
+mock time is now 10
- : string option = Some "o=1"
```

A cookie set without `Path=` gets the request's *default path*: everything up
to but not including the last `/`, as defined by
[RFC 6265 §5.1.4](https://www.rfc-editor.org/rfc/rfc6265#section-5.1.4).
A cookie from `/a/b` therefore covers `/a` and its siblings, not just `/a/b`:

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "https://shop.example/a/b" "p=1";
  (Cookies.Jar.header_for jar "https://shop.example/a/other",
   Cookies.Jar.header_for jar "https://shop.example/b");;
- : string option * string option = (Some "p=1", None)
```

An empty value is a legal cookie and is sent as `name=`:

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "https://shop.example/" "flag=; Path=/";
  Cookies.Jar.header_for jar "https://shop.example/";;
- : string option = Some "flag="
```

`Max-Age=0` (or any already-expired arrival) deletes the stored cookie:

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "https://shop.example/" "sid=abc; Path=/";
  let before = Cookies.Jar.header_for jar "https://shop.example/" in
  Cookies.Jar.set jar "https://shop.example/" "sid=; Path=/; Max-Age=0";
  (before, Cookies.Jar.header_for jar "https://shop.example/");;
- : string option * string option = (Some "sid=abc", None)
```

The
[RFC 6265bis secure-shadowing rule](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis#section-5.7)
also covers a nearby path, not just an exact collision: plaintext HTTP
cannot plant `sid` at `/x/` beside the Secure `sid` at `/`:

```ocaml
# run @@ fun env ->
  let jar = Cookies.Jar.in_memory ~clock:env#clock () in
  Cookies.Jar.set jar "https://shop.example/" "sid=SECURE; Secure; Path=/";
  Cookies.Jar.set jar "http://shop.example/x/" "sid=SPOOFED; Path=/x/";
  Cookies.Jar.header_for jar "https://shop.example/x/page";;
- : string option = Some "sid=SECURE"
```

## Persistence via an fs capability

A file-backed jar retains a destination `Eio.Path.t`. Saving creates a private
temporary sibling and atomically renames it over the destination, so the jar
requires create, rename, and remove authority over the containing directory.
The capability must remain valid for the jar's lifetime. This uses the real
scheduler and a file in the test's sandbox directory (Netscape cookies.txt
format, curl-compatible):

```ocaml
# #require "eio_main";;
# Eio_main.run @@ fun env ->
  let file = Eio.Path.(Eio.Stdenv.cwd env / "cookies.txt") in
  let clock = Eio.Stdenv.clock env in
  let jar = Cookies.Jar.of_file ~clock file in
  Cookies.Jar.set jar "https://shop.example/" "sid=abc123; Path=/; HttpOnly";
  (* Show the data rows (the file also has a "# Netscape HTTP Cookie File"
     comment header, which would confuse this toplevel transcript). An
     HttpOnly cookie is written with curl's "#HttpOnly_" line prefix,
     and survives the round-trip below. *)
  Eio.Path.load file
  |> String.split_on_char '\n'
  |> List.iter (fun l ->
       if l <> "" && not (String.starts_with ~prefix:"# " l) then
         print_endline ("| " ^ l));;
| #HttpOnly_shop.example	FALSE	/	FALSE	0	sid	abc123
- : unit = ()
# Eio_main.run @@ fun env ->
  let file = Eio.Path.(Eio.Stdenv.cwd env / "cookies.txt") in
  let clock = Eio.Stdenv.clock env in
  let jar = Cookies.Jar.of_file ~clock file in
  Cookies.Jar.header_for jar "https://shop.example/basket";;
- : string option = Some "sid=abc123"
```
