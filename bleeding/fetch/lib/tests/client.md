# The client capability: narrowing, appending, redirects

```ocaml
# #require "fetch";;
# #require "fetch.mock";;
# #require "eio.mock";;
```

```ocaml
open Fetch

(* A mock server that reports each request it receives (method, URL, and
   the Authorization header if present) and answers 200 "ok". *)
let echo (req : Middleware.request) =
  Fmt.pr "> %s %s@." (Http.Method.to_string req.meth) (Middleware.Url.to_string req.url);
  (match Http.Header.get req.headers "authorization" with
   | Some v -> Fmt.pr ">   authorization: %s@." v
   | None -> ());
  Fetch_mock.respond "ok" req

(* Where a request is going, read off the parsed URL. A URL that reaches
   a client has been validated, so it has a host. *)
let host (req : Middleware.request) = Option.get (Uri.host (Middleware.Url.to_uri req.url))
let is_https (req : Middleware.request) = Uri.scheme (Middleware.Url.to_uri req.url) = Some "https"

let run fn = Eio_mock.Backend.run @@ fun () -> Eio.Switch.run @@ fun sw -> fn sw
```

## Basic requests

```ocaml
# run @@ fun sw ->
  let t = Fetch_mock.client echo in
  let resp = Fetch.get ~sw t "https://example.com/hi" in
  status resp;;
> GET https://example.com/hi
- : int = 200
```

`read` drains the body to a string:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fetch.read (Fetch_mock.client echo) "https://example.com/hi";;
> GET https://example.com/hi
- : string = "ok"
```

A body over `~limit` raises the documented exception (not a wrapped
`Failure`):

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let big (req : Middleware.request) = Fetch_mock.respond (String.make 64 'x') req in
  Fetch.read ~limit:16 (Fetch_mock.client big) "https://example.com/big";;
Exception: Eio__Buf_read.Buffer_limit_exceeded.
```

## Narrowing

`restrict ~under` names URL prefixes, the language every `scope` below
is written in too. An entry with no path covers the whole origin, and a
request outside it is refused without touching the network:

```ocaml
# let api = Fetch.restrict (Fetch_mock.client echo)
    ~under:[ "https://api.example.com" ];;
val api : plain = Eio.Resource.T (<poly>, <abstr>)
# Eio_mock.Backend.run @@ fun () ->
  Fetch.read api "https://api.example.com/v1/users";;
> GET https://api.example.com/v1/users
- : string = "ok"
# Eio_mock.Backend.run @@ fun () ->
  Fetch.read api "https://evil.example.com/v1/users";;
Exception:
Eio.Io Http Denied "url https://evil.example.com/v1/users not permitted",
  GET https://evil.example.com/v1/users
```

An entry with a path narrows further, and intersects the other axes:

```ocaml
# let readonly = Fetch.restrict (Fetch_mock.client echo)
    ~methods:[ `GET; `HEAD ] ~under:[ "https://example.com/public" ];;
val readonly : plain = Eio.Resource.T (<poly>, <abstr>)
# Eio_mock.Backend.run @@ fun () ->
  Fetch.read readonly "https://example.com/public/page";;
> GET https://example.com/public/page
- : string = "ok"
# run @@ fun sw ->
  status
    (Fetch.post ~sw readonly ~body:(String "x") "https://example.com/public/page");;
Exception:
Eio.Io Http Denied "method POST not permitted",
  POST https://example.com/public/page
# Eio_mock.Backend.run @@ fun () ->
  Fetch.read readonly "https://example.com/private/page";;
Exception:
Eio.Io Http Denied "url https://example.com/private/page not permitted",
  GET https://example.com/private/page
```

The match is segment by whole segment, so a longer name that merely
starts with the prefix is not under it:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fetch.read readonly "https://example.com/publicity";;
Exception:
Eio.Io Http Denied "url https://example.com/publicity not permitted",
  GET https://example.com/publicity
```

Traversal cannot escape a prefix either, since the URL is canonicalized
before it is matched:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Fetch.read readonly "https://example.com/public/../private/page";;
Exception:
Eio.Io Http Denied "url https://example.com/private/page not permitted",
  GET https://example.com/private/page
```

An entry is a URL, so its path is percent-encoded like any other one.
`https://example.com/api%20v1` scopes the decoded segment `api v1`:

```ocaml
# let spaced = Fetch.restrict (Fetch_mock.client echo)
    ~under:[ "https://example.com/api%20v1" ];;
val spaced : plain = Eio.Resource.T (<poly>, <abstr>)
# Eio_mock.Backend.run @@ fun () ->
  Fetch.read spaced "https://example.com/api%20v1/users";;
> GET https://example.com/api%20v1/users
- : string = "ok"
```

An entry is parsed where it is written, so a mistyped address fails at
the wrapper rather than on the first request:

```ocaml
# Fetch.restrict (Fetch_mock.client echo) ~under:[ "api.example.com" ];;
Exception:
Invalid_argument
 "Fetch.restrict: prefix \"api.example.com\" is not a URL: not an absolute URL (missing scheme)".
```

A query cannot be a prefix of anything, so an entry carrying one is
refused as well, wherever it is written:

```ocaml
# Fetch.with_credentials ~scope:[ "https://api.example.com/v1?tenant=acme" ]
    Credential.[ Bearer (fun () -> "SECRET") ] (Fetch_mock.client echo);;
Exception:
Invalid_argument
 "Fetch.with_credentials: scope \"https://api.example.com/v1?tenant=acme\" has a query, which names more than an origin and a path".
```

## Read-only clients

`read_only` allows the safe methods of RFC 9110 §9.2.1, namely GET,
HEAD and OPTIONS. Every other method is denied at runtime, whether it
arrives through `post` and its siblings or through a dynamic entry
point that only learns the method at run time. Each redirect hop is
checked in turn:

```ocaml
# let ro = Fetch.read_only (Fetch_mock.client echo);;
val ro : plain = Eio.Resource.T (<poly>, <abstr>)
# run @@ fun sw -> status (Fetch.get ~sw ro "https://example.com/hi");;
> GET https://example.com/hi
- : int = 200
# run @@ fun sw ->
  status (Fetch.post ~sw ro ~body:(String "x") "https://example.com/hi");;
Exception:
Eio.Io Http Denied "method POST not permitted by a read-only client",
  POST https://example.com/hi
# run @@ fun sw ->
  status (Fetch.fetch ~sw ro `POST "https://example.com/hi");;
Exception:
Eio.Io Http Denied "method POST not permitted by a read-only client",
  POST https://example.com/hi
```

`restrict ~methods` names any other set. The idempotent methods of RFC
9110 §9.2.2 are one worth naming, since everything leaving such a
client may be blindly retried:

```ocaml
# run @@ fun sw ->
  let t = Fetch.restrict ~methods:[ `GET; `HEAD; `OPTIONS; `PUT; `DELETE ]
      (Fetch_mock.client echo) in
  status (Fetch.put ~sw t ~body:(String "x") "https://example.com/doc");;
> PUT https://example.com/doc
- : int = 200
```

## Appending

`with_credentials` attaches credentials, scoped to the URL prefixes
that may see them. `Bearer` is the `Authorization` form.

A prefix names one origin, so a rule covering a family of them, any
subdomain of a host say, goes in `restrict ~filter`. That is the escape
hatch for wildcard matching, and it sees the parsed URL:

```ocaml
let subdomain_of suffix (req : Middleware.request) =
  let host = host req in
  let n = String.length host and m = String.length suffix in
  if host = suffix
     || (n > m + 1 && String.sub host (n - m - 1) (m + 1) = "." ^ suffix)
  then `Allow
  else `Reject (host ^ " is not under " ^ suffix)
```

```ocaml
# let scope = [ "https://api.example.com" ] in
  let t = Fetch_mock.client echo
    |> Fetch.restrict ~filter:(subdomain_of "example.com")
    |> Fetch.with_credentials ~scope
         Credential.[ Bearer (fun () -> "SECRET") ] in
  Eio_mock.Backend.run @@ fun () ->
  Fetch.read t "https://api.example.com/v1/me";;
> GET https://api.example.com/v1/me
>   authorization: Bearer SECRET
- : string = "ok"
```

Out-of-scope requests pass through untouched:

```ocaml
# let scope = [ "https://api.example.com" ] in
  let t = Fetch_mock.client echo
    |> Fetch.restrict ~filter:(subdomain_of "example.com")
    |> Fetch.with_credentials ~scope
         Credential.[ Bearer (fun () -> "SECRET") ] in
  Eio_mock.Backend.run @@ fun () ->
  Fetch.read t "https://www.example.com/page";;
> GET https://www.example.com/page
- : string = "ok"
```

A scope carries a path as well as an origin, so a credential can be
confined to one part of a host. This one reaches `/v3/` and leaves the
request beside it untouched:

```ocaml
# run @@ fun sw ->
  let t = Fetch.with_credentials ~scope:[ "https://api.example.com/v3/" ]
      Credential.[ Bearer (fun () -> "SECRET") ] (Fetch_mock.client echo) in
  let inside = status (Fetch.get ~sw t "https://api.example.com/v3/users") in
  let beside = status (Fetch.get ~sw t "https://api.example.com/v2/users") in
  (inside, beside);;
> GET https://api.example.com/v3/users
>   authorization: Bearer SECRET
> GET https://api.example.com/v2/users
- : int * int = (200, 200)
```

A caller's own value is replaced rather than obeyed:

```ocaml
# let scope = [ "https://api.example.com" ] in
  let t = Fetch.with_credentials ~scope
      Credential.[ Bearer (fun () -> "SECRET") ] (Fetch_mock.client echo) in
  run @@ fun sw ->
  let headers = Header.[ authorization, `Bearer "spoofed" ] in
  status (Fetch.get ~sw ~headers t "https://api.example.com/v1/me");;
> GET https://api.example.com/v1/me
>   authorization: Bearer SECRET
- : int = 200
```

The holder of a derived client cannot override an appended header
(`` `Set`` mode replaces it):

```ocaml
# let t = Fetch.with_headers Header.[ user_agent, "trusted/1.0" ]
    (Fetch_mock.client echo) in
  run @@ fun sw ->
  status (Fetch.get ~sw t "https://example.com/"
            ~headers:Header.[ user_agent, "spoofed/9.9" ]);;
> GET https://example.com/
- : int = 200
# run @@ fun sw ->
  let show (req : Middleware.request) =
    Fmt.pr "user-agent: %a@." Fmt.(list ~sep:comma string)
      (Http.Header.get_multi req.headers "user-agent");
    Fetch_mock.respond "" req
  in
  let t = Fetch.with_headers Header.[ user_agent, "trusted/1.0" ]
      (Fetch_mock.client show) in
  status (Fetch.get ~sw t "https://example.com/"
            ~headers:Header.[ user_agent, "spoofed/9.9" ]);;
user-agent: trusted/1.0
- : int = 200
```

`with_headers` refuses credential headers outright — it has no
mandatory scope and re-fires per redirect hop, so it would re-attach
what the redirect loop strips. Credentials go through
`with_credentials`, whose scope is mandatory, or through a cookie jar:

```ocaml
# Fetch.with_headers Header.[ raw "Authorization" "Bearer leaky" ]
    (Fetch_mock.client echo);;
Exception:
Invalid_argument
 "Fetch.with_headers: \"Authorization\" is a credential header; use with_credentials (scoped) instead".
```

## Redirects

A mock site: `start.example` redirects to `final.example`, which answers.

```ocaml
let site (req : Middleware.request) =
  Fmt.pr "> %s %s@." (Http.Method.to_string req.meth) (Middleware.Url.to_string req.url);
  (match Http.Header.get req.headers "authorization" with
   | Some v -> Fmt.pr ">   authorization: %s@." v
   | None -> ());
  match host req with
  | "start.example" ->
    Fetch_mock.respond ~status:302
      ~headers:(Http.Header.of_list [ "Location", "https://final.example/landing" ]) "" req
  | _ -> Fetch_mock.respond "landed" req
```

`get` follows the hop; the response reports the final URL:

```ocaml
# run @@ fun sw ->
  let resp = Fetch.get ~sw (Fetch_mock.client site) "https://start.example/" in
  (status resp, url resp);;
> GET https://start.example/
> GET https://final.example/landing
- : int * string = (200, "https://final.example/landing")
```

With `~redirects:0` the 3xx comes back as data; a negative count means
the same (it must not loop forever):

```ocaml
# run @@ fun sw ->
  status (Fetch.get ~sw ~redirects:0 (Fetch_mock.client site) "https://start.example/");;
> GET https://start.example/
- : int = 302
# run @@ fun sw ->
  status (Fetch.get ~sw ~redirects:(-1) (Fetch_mock.client site) "https://start.example/");;
> GET https://start.example/
- : int = 302
```

Every hop re-enters the wrapper stack, so a narrowed client cannot be
escaped by a redirect:

```ocaml
# let t = Fetch.restrict (Fetch_mock.client site) ~under:[ "https://start.example" ] in
  run @@ fun sw ->
  status (Fetch.get ~sw t "https://start.example/");;
> GET https://start.example/
Exception:
Eio.Io Http Denied "url https://final.example/landing not permitted",
  GET https://final.example/landing,
  fetching https://start.example/ (1 redirect followed)
```

And a scoped credential is stripped on the cross-origin hop: the first
request carries it, the redirect target never sees it.

```ocaml
# let t = Fetch.with_credentials ~scope:[ "https://start.example" ]
    Credential.[ Bearer (fun () -> "SECRET") ] (Fetch_mock.client site) in
  run @@ fun sw ->
  status (Fetch.get ~sw t "https://start.example/");;
> GET https://start.example/
>   authorization: Bearer SECRET
> GET https://final.example/landing
- : int = 200
```

A credential the caller attached by hand is dropped on a hop that
changes origin — `with_credentials` is not the only thing that has to
be contained:

```ocaml
# run @@ fun sw ->
  let headers = Header.[ authorization, `Bearer "CALLER-SECRET" ] in
  status (Fetch.get ~sw ~headers (Fetch_mock.client site) "https://start.example/");;
> GET https://start.example/
>   authorization: Bearer CALLER-SECRET
> GET https://final.example/landing
- : int = 200
```

An http to https upgrade of the same host keeps it, since the same party
receives it over a stronger channel:

```ocaml
let upgrader (req : Middleware.request) =
  Fmt.pr "> %s %s@." (Http.Method.to_string req.meth) (Middleware.Url.to_string req.url);
  (match Http.Header.get req.headers "authorization" with
   | Some v -> Fmt.pr ">   authorization: %s@." v
   | None -> ());
  if is_https req then Fetch_mock.respond "secure" req
  else
    Fetch_mock.respond ~status:301
      ~headers:(Http.Header.of_list [ "Location", "https://up.example/" ]) "" req
```

```ocaml
# run @@ fun sw ->
  let headers = Header.[ authorization, `Bearer "KEEP-ME" ] in
  status (Fetch.get ~sw ~headers (Fetch_mock.client upgrader) "http://up.example/");;
> GET http://up.example/
>   authorization: Bearer KEEP-ME
> GET https://up.example/
>   authorization: Bearer KEEP-ME
- : int = 200
```

## Credentials refuse plaintext

Every entry spells out its scheme, and an `http://` one is easy to
write by accident, so attaching a credential over plaintext is refused
unless asked for twice. The check is on the request, not on the
credential, so it holds for every form a credential can take:

```ocaml
# let scope = [ "http://internal.example" ] in
  let t = Fetch.with_credentials ~scope
      Credential.[ Bearer (fun () -> "SECRET") ] (Fetch_mock.client echo) in
  run @@ fun sw ->
  status (Fetch.get ~sw t "http://internal.example/");;
Exception:
Eio.Io Http Denied "refusing to send credentials over plaintext http (http://internal.example)",
  GET http://internal.example/
```

`~allow_insecure:true` is the second asking:

```ocaml
# let scope = [ "http://internal.example" ] in
  let t = Fetch.with_credentials ~scope ~allow_insecure:true
      Credential.[ Bearer (fun () -> "SECRET") ] (Fetch_mock.client echo) in
  run @@ fun sw ->
  status (Fetch.get ~sw t "http://internal.example/");;
> GET http://internal.example/
>   authorization: Bearer SECRET
- : int = 200
```

## Credential headers of the service's own naming

`Credential.Header` carries an API key under whatever name the service
asks for, with the containment `Authorization` gets. The scope is
mandatory, plaintext is refused, and the value is redacted from request
printing. No hop outside the scope ever sees it, because each redirect
hop re-enters the wrapper, which only attaches in scope:

```ocaml
let key_site (req : Middleware.request) =
  Fmt.pr "> %s %s@." (Http.Method.to_string req.meth) (Middleware.Url.to_string req.url);
  (match Http.Header.get req.headers "x-api-key" with
   | Some v -> Fmt.pr ">   x-api-key: %s@." v
   | None -> ());
  match host req with
  | "api.example" ->
    Fetch_mock.respond ~status:302
      ~headers:(Http.Header.of_list [ "Location", "https://cdn.example/big" ]) "" req
  | _ -> Fetch_mock.respond "ok" req
```

```ocaml
# let t = Fetch.with_credentials ~scope:[ "https://api.example" ]
    Credential.[ Header ("X-Api-Key", fun _ -> "SECRET-KEY") ]
    (Fetch_mock.client key_site) in
  run @@ fun sw ->
  status (Fetch.get ~sw t "https://api.example/data");;
> GET https://api.example/data
>   x-api-key: SECRET-KEY
> GET https://cdn.example/big
- : int = 200
```

A key the caller attaches per request is contained by naming it in
`~sensitive`: the cross-origin hop drops it:

```ocaml
# run @@ fun sw ->
  status (Fetch.fetch ~sw (Fetch_mock.client key_site) `GET
            "https://api.example/data"
            ~headers:Header.[ raw "X-Api-Key" "CALLER-KEY" ]
            ~sensitive:[ "X-Api-Key" ]);;
> GET https://api.example/data
>   x-api-key: CALLER-KEY
> GET https://cdn.example/big
- : int = 200
```

and error context redacts it, while printing ordinary header values in
full:

```ocaml
# run @@ fun sw ->
  let t = Fetch.restrict (Fetch_mock.client echo)
      ~under:[ "https://api.example" ] in
  status (Fetch.fetch ~sw t `GET "https://evil.example/steal"
            ~headers:Header.[ raw "X-Api-Key" "SECRET"; raw "Accept" "text/plain" ]
            ~sensitive:[ "X-Api-Key" ]);;
Exception:
Eio.Io Http Denied "url https://evil.example/steal not permitted",
  GET https://evil.example/steal (X-Api-Key: <redacted>,
Accept: text/plain)
```

Names other machinery owns are not claimable:

```ocaml
# Fetch.with_credentials ~scope:[]
    Credential.[ Header ("Cookie", fun _ -> "sid=1") ] (Fetch_mock.client echo);;
Exception:
Invalid_argument
 "Fetch.with_credentials: cookies belong to a jar; see Fetch_cookies".
```

## Query-string credentials

`Credential.Query` is the same discipline for a credential that travels
in the query string, such as an `api_key` parameter. The caller's URL
never carries it, so neither traces nor error context do. A caller's
own binding of the name is replaced, not obeyed:

```ocaml
# run @@ fun sw ->
  let t = Fetch.with_credentials ~scope:[ "https://api.example.com" ]
      Credential.[ Query [ "api_key", "SECRET" ] ] (Fetch_mock.client echo) in
  let plain = status (Fetch.get ~sw t "https://api.example.com/jats?q=doi") in
  let spoofed = status (Fetch.get ~sw t "https://api.example.com/jats?api_key=fake") in
  let out_of_scope = status (Fetch.get ~sw t "https://www.example.com/jats") in
  (plain, spoofed, out_of_scope);;
> GET https://api.example.com/jats?api_key=SECRET&q=doi
> GET https://api.example.com/jats?api_key=SECRET
> GET https://www.example.com/jats
- : int * int * int = (200, 200, 200)
```

A parameter with no name is refused when the wrapper is built, before
any request reaches it:

```ocaml
# Fetch.with_credentials ~scope:[] Credential.[ Query [ "", "SECRET" ] ]
    (Fetch_mock.client echo);;
Exception:
Invalid_argument "Fetch.with_credentials: a parameter name cannot be empty".
```

## Request validation

A method or header field carrying control characters would let its
author write a request line of its own, which no URL-based policy would
ever see. Both are rejected before the network:

```ocaml
# run @@ fun sw ->
  status (Fetch.fetch ~sw (Fetch_mock.client echo)
            (`Other "GET /admin HTTP/1.1\r\nX: y") "https://example.com/public");;
Exception:
Eio.Io Http Invalid_request "method \"GET /admin HTTP/1.1\\r\\nX: y\" is not a token",
  "GET /admin HTTP/1.1\r\nX: y" https://example.com/public
# run @@ fun sw ->
  let headers = Header.[ raw "X-Evil" "a\r\nX-Injected: yes" ] in
  status (Fetch.get ~sw ~headers (Fetch_mock.client echo) "https://example.com/");;
Exception:
Eio.Io Http Invalid_request "value of header \"X-Evil\" contains a control character",
  GET https://example.com/ (X-Evil: "a\r\nX-Injected: yes")
```

The check sits at the capability boundary, not at the entry point, so
reaching for the raw handler does not evade it:

```ocaml
# run @@ fun sw ->
  let h = Fetch.Middleware.handler (Fetch_mock.client echo) in
  let url = Result.get_ok (Middleware.Url.of_string "https://example.com/") in
  status (h ~sw { meth = `Other "GET /admin HTTP/1.1\r\nX: ";
                  url; headers = Http.Header.init (); body = Empty;
                  sensitive = [] });;
Exception:
Eio.Io Http Invalid_request "method \"GET /admin HTTP/1.1\\r\\nX: \" is not a token"
```

`Host` describes which site the request is for, and the framing headers
describe a body the backend frames itself — a request that set them
would tell the policy wrappers one thing and the wire another, so they
are refused (the backend derives all three):

```ocaml
# run @@ fun sw ->
  let t = Fetch.restrict (Fetch_mock.client echo)
      ~under:[ "https://allowed.example" ] in
  status (Fetch.get ~sw t "https://allowed.example/"
            ~headers:Header.[ raw "Host" "other-vhost.example" ]);;
Exception:
Eio.Io Http Invalid_request "header \"Host\" is the backend's to set, not a request's",
  GET https://allowed.example/ (Host: other-vhost.example)
# run @@ fun sw ->
  status (Fetch.post ~sw (Fetch_mock.client echo) ~body:(String "a")
            "https://example.com/" ~headers:Header.[ content_length, 99L ]);;
Exception:
Eio.Io Http Invalid_request "header \"Content-Length\" is the backend's to set, not a request's",
  POST https://example.com/ (Content-Length: 99)
```

An https to http downgrade is refused unless opted in:

```ocaml
let downgrader (req : Middleware.request) =
  Fmt.pr "> %s %s@." (Http.Method.to_string req.meth) (Middleware.Url.to_string req.url);
  if is_https req then
    Fetch_mock.respond ~status:301
      ~headers:(Http.Header.of_list [ "Location", "http://start.example/" ]) "" req
  else Fetch_mock.respond "plaintext" req
```

```ocaml
# run @@ fun sw ->
  status (Fetch.get ~sw (Fetch_mock.client downgrader) "https://start.example/");;
> GET https://start.example/
Exception:
Eio.Io Http Denied "redirect would downgrade https to http (http://start.example/)"
# Eio_mock.Backend.run @@ fun () ->
  Fetch.with_response (Fetch_mock.client downgrader) ~allow_downgrade:true
    `GET "https://start.example/" status;;
> GET https://start.example/
> GET http://start.example/
- : int = 200
```

A 303 (and 301/302 on POST) converts the method to GET and drops the
body; 307 preserves it:

```ocaml
let see_other (req : Middleware.request) =
  Fmt.pr "> %s %s (body length %a)@." (Http.Method.to_string req.meth) (Middleware.Url.to_string req.url)
    Fmt.(option ~none:(any "?") int64) (Middleware.body_length req.body);
  match Middleware.Url.path_segments req.url with
  | [ "submit" ] ->
    Fetch_mock.respond ~status:303
      ~headers:(Http.Header.of_list [ "Location", "/result" ]) "" req
  | [ "submit307" ] ->
    Fetch_mock.respond ~status:307
      ~headers:(Http.Header.of_list [ "Location", "/final" ]) "" req
  | _ -> Fetch_mock.respond "done" req
```

```ocaml
# run @@ fun sw ->
  status
    (Fetch.post ~sw (Fetch_mock.client see_other) ~body:(String "a=1")
       "https://forms.example/submit");;
> POST https://forms.example/submit (body length 3)
> GET https://forms.example/result (body length 0)
- : int = 200
# run @@ fun sw ->
  status
    (Fetch.post ~sw (Fetch_mock.client see_other) ~body:(String "a=1")
       "https://forms.example/submit307");;
> POST https://forms.example/submit307 (body length 3)
> POST https://forms.example/final (body length 3)
- : int = 200
```

A streaming body cannot be replayed through a 307:

```ocaml
# run @@ fun sw ->
  let body = stream (Eio.Flow.string_source "streamed") in
  status
    (Fetch.post ~sw (Fetch_mock.client see_other) ~body
       "https://forms.example/submit307");;
> POST https://forms.example/submit307 (body length ?)
Exception:
Eio.Io Http Body_not_replayable,
  fetching https://forms.example/submit307 (1 redirect followed)
```

A `Location` that resolves to the same request — an empty one does —
can only repeat the exchange, and fails at once rather than after ten
wasted hops:

```ocaml
let self_loop (req : Middleware.request) =
  Fetch_mock.respond ~status:302
    ~headers:(Http.Header.of_list [ "Location", "" ]) "" req
```

```ocaml
# run @@ fun sw ->
  status (Fetch.get ~sw (Fetch_mock.client self_loop) "https://loop.example/");;
Exception: Eio.Io Http Too_many_redirects
```

Unless the redirect changes the method: POST, 303 back to the same URL,
GET — the post/redirect/get idiom — is not a loop:

```ocaml
let prg (req : Middleware.request) =
  Fmt.pr "> %s %s@." (Http.Method.to_string req.meth) (Middleware.Url.to_string req.url);
  match req.meth with
  | `POST ->
    Fetch_mock.respond ~status:303
      ~headers:(Http.Header.of_list [ "Location", "" ]) "" req
  | _ -> Fetch_mock.respond "saved" req
```

```ocaml
# run @@ fun sw ->
  status (Fetch.post ~sw (Fetch_mock.client prg) ~body:(String "a=1")
            "https://forms.example/entry");;
> POST https://forms.example/entry
> GET https://forms.example/entry
- : int = 200
```

A GET that is 303'd back to itself gains nothing from the conversion —
the effective method is unchanged — so it fails after one exchange
rather than burning the hop limit:

```ocaml
# run @@ fun sw ->
  let always_303 (req : Middleware.request) =
    Fmt.pr "> %s %s@." (Http.Method.to_string req.meth) (Middleware.Url.to_string req.url);
    Fetch_mock.respond ~status:303
      ~headers:(Http.Header.of_list [ "Location", "" ]) "" req
  in
  status (Fetch.get ~sw (Fetch_mock.client always_303) "https://loop.example/");;
> GET https://loop.example/
Exception: Eio.Io Http Too_many_redirects
```

A longer loop still hits the hop limit:

```ocaml
let ping_pong (req : Middleware.request) =
  let other =
    match host req with
    | "ping.example" -> "https://pong.example/"
    | _ -> "https://ping.example/"
  in
  Fetch_mock.respond ~status:302
    ~headers:(Http.Header.of_list [ "Location", other ]) "" req
```

```ocaml
# run @@ fun sw ->
  status (Fetch.get ~sw (Fetch_mock.client ping_pong) "https://ping.example/");;
Exception:
Eio.Io Http Too_many_redirects,
  fetching https://ping.example/ (10 redirects followed)
```

A 301 or 302 answering a POST also converts to GET (historical practice,
RFC 9110 §15.4.2), dropping the body and the headers that described it:

```ocaml
# run @@ fun sw ->
  let server (req : Middleware.request) =
    Fmt.pr "> %s %s (type: %s)@." (Http.Method.to_string req.meth)
      (Middleware.Url.to_string req.url)
      (Option.value (Http.Header.get req.headers "content-type") ~default:"-");
    match Middleware.Url.path_segments req.url with
    | [ "form" ] ->
      Fetch_mock.respond ~status:301
        ~headers:(Http.Header.of_list [ "Location", "/done" ]) "" req
    | _ -> Fetch_mock.respond "ok" req
  in
  status (Fetch.post ~sw (Fetch_mock.client server) ~body:(String "a=1")
            "https://forms.example/form"
            ~headers:Header.[ content_type, media "text/plain" ]);;
> POST https://forms.example/form (type: text/plain)
> GET https://forms.example/done (type: -)
- : int = 200
```

When a server sends several `Location` lines, the first is followed —
browser behaviour, and not what a last-wins header lookup would pick:

```ocaml
# run @@ fun sw ->
  let server (req : Middleware.request) =
    Fmt.pr "> %s@." (Middleware.Url.to_string req.url);
    match Middleware.Url.path_segments req.url with
    | [ "start" ] ->
      Fetch_mock.respond ~status:302
        ~headers:(Http.Header.of_list
                    [ "Location", "/first"; "Location", "/second" ]) "" req
    | _ -> Fetch_mock.respond "ok" req
  in
  status (Fetch.get ~sw (Fetch_mock.client server) "https://dup.example/start");;
> https://dup.example/start
> https://dup.example/first
- : int = 200
```

`~mode:`Add` appends rather than replacing, so a caller's value survives
alongside the wrapper's:

```ocaml
# run @@ fun sw ->
  let show (req : Middleware.request) =
    Fmt.pr "accept-encoding: %s@."
      (String.concat " | " (Http.Header.get_multi req.headers "accept-encoding"));
    Fetch_mock.respond "" req
  in
  let t = Fetch.with_headers ~mode:`Add
      Header.[ raw "Accept-Encoding" "identity" ] (Fetch_mock.client show) in
  status (Fetch.get ~sw t "https://example.com/"
            ~headers:Header.[ raw "Accept-Encoding" "gzip" ]);;
accept-encoding: gzip | identity
- : int = 200
```

Trailers are exposed separately from headers, and only once the body has
been read:

```ocaml
# run @@ fun sw ->
  let server (req : Middleware.request) =
    let resp = Fetch_mock.respond "hello" req in
    Fetch.Middleware.Pi.response ~status:(status resp) ~headers:(headers resp)
      ~version:(version resp) ~body:(body resp)
      ~trailers:(fun () ->
          Some (Http.Header.of_list [ "X-Checksum", "abc123" ]))
      ~url:req.url ()
  in
  Fetch.with_response (Fetch_mock.client server) `GET "https://t.example/" @@
  fun resp ->
  let s = Eio.Buf_read.(parse_exn ~max_size:100 take_all) (body resp) in
  (s, Option.bind (Fetch.trailers resp) (fun tr ->
         Http.Header.get tr "x-checksum"));;
- : string * string option = ("hello", Some "abc123")
```
