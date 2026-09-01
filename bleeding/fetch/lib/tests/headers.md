# Typed header values

```ocaml
# #require "fetch";;
# #require "fetch.mock";;
# #require "eio.mock";;
```

```ocaml
let () = Printexc.record_backtrace false
open Fetch

let run fn = Eio_mock.Backend.run @@ fun () -> Eio.Switch.run @@ fun sw -> fn sw
```

## Range requests, typed both ways

A mock file server that honours a single byte range — it reads the
request's `Range` with the same codec the client used to write it:

```ocaml
let file = String.init 1000 (fun i -> Char.chr (i mod 26 + Char.code 'a'))

let file_server (req : Middleware.request) =
  match Header.get Header.range req.headers with
  | Some { unit = "bytes"; ranges = [ `Range (first, Some last) ] } ->
    let first = Int64.to_int first and last = Int64.to_int last in
    Fetch_mock.respond ~status:206
      ~headers:(Http.Header.of_list
                  [ Header.(pair content_range
                              (complete_range ~first:(Int64.of_int first)
                                 ~last:(Int64.of_int last)
                                 ~complete_length:1000L));
                    Header.(pair accept_ranges `Bytes) ])
      (String.sub file first (last - first + 1)) req
  | _ -> Fetch_mock.respond file req
```

Requests take a heterogeneous list of `codec, value` cells (`Header.[]`
rebinds the list syntax; `raw` is the freeform escape hatch) and
`Fetch.header` parses the response side back:

```ocaml
# run @@ fun sw ->
  let resp =
    Fetch.get ~sw (Fetch_mock.client file_server) "https://files.example/f"
      ~headers:Header.[ raw "User-Agent" "demo/1.0";
                        range, bytes [ `Range (100L, Some 103L) ] ]
  in
  let body = Eio.Buf_read.parse_exn ~max_size:1000 Eio.Buf_read.take_all (body resp) in
  (status resp, body,
   Fetch.header Header.content_range resp
   = Some (Header.complete_range ~first:100L ~last:103L
             ~complete_length:1000L));;
- : int * string * bool = (206, "wxyz", true)
```

## Link pagination

The following example follows `rel="next"` links to the last page:

```ocaml
let paged (req : Middleware.request) =
  let pq = Middleware.Url.path_and_query req.url in
  let n =
    match String.index_opt pq '=' with
    | Some i -> int_of_string (String.sub pq (i + 1) (String.length pq - i - 1))
    | None -> 1
  in
  let headers =
    if n >= 3 then Http.Header.init ()
    else
      Http.Header.of_list
        [ Header.(pair links
                    [ link ~rel:"next"
                        (Fmt.str "https://api.example/items?page=%d" (n + 1)) ]) ]
  in
  Fetch_mock.respond ~headers (Fmt.str "page %d" n) req
```

```ocaml
# run @@ fun _sw ->
  let t = Fetch_mock.client paged in
  let rec walk url acc =
    Fetch.with_response t `GET url @@ fun resp ->
    let page = Eio.Buf_read.parse_exn ~max_size:100 Eio.Buf_read.take_all (body resp) in
    match Option.bind (Fetch.header Header.links resp) (Header.link_rel "next") with
    | Some next -> walk next.Header.target (page :: acc)
    | None -> List.rev (page :: acc)
  in
  walk "https://api.example/items?page=1" [];;
- : string list = ["page 1"; "page 2"; "page 3"]
```

An RFC 8187 UTF-8 `title*` is decoded into the link title and takes precedence
over the plain fallback:

```ocaml
# Option.map (Header.encode Header.links)
    (Header.decode Header.links
       "</next>; rel=next; title=plain; title*=UTF-8''caf%C3%A9");;
- : string option = Some "</next>; rel=\"next\"; title=\"café\""
```

A comma inside the `<...>` target is part of the URI, not a separator
between link values:

```ocaml
# Header.decode Header.links {|</a?x=1,2>; rel="next", </b>; rel="prev"|}
  |> Option.map (List.map (fun (l : Header.link) -> l.Header.target, l.rel));;
- : (string * string option) list option =
Some [("/a?x=1,2", Some "next"); ("/b", Some "prev")]
```

The delimiter grammar is fail-closed in both directions: one malformed member
rejects the complete field, and neither a target nor a quoted parameter can
inject Link syntax or control bytes.

```ocaml
# Header.decode Header.links
    {|</ok>; rel=next, </broken>; rel="next" trailing|};;
- : Header.link list option = None
# let raises_invalid f =
    match f () with _ -> false | exception Invalid_argument _ -> true;;
val raises_invalid : (unit -> 'a) -> bool = <fun>
# raises_invalid (fun () -> Header.link "/next>;<evil" |> ignore);;
- : bool = true
# raises_invalid (fun () ->
    Header.(encode links [link ~title:"bad\001title" "/next"]) |> ignore);;
- : bool = true
# let forged : Header.link =
    { target = "/next>;<evil"; rel = None; media_type = None; title = None;
      hreflang = None; params = [] };;
val forged : Header.link =
  {Fetch.Header.target = "/next>;<evil"; rel = None; media_type = None;
   title = None; hreflang = None; params = []}
# raises_invalid (fun () -> Header.encode Header.links [forged] |> ignore);;
- : bool = true
```

## Decoding is total; encoding round-trips

`Retry-After` distinguishes delta-seconds from HTTP-dates (the
`with_retry` wrapper honours the former):

```ocaml
# Header.decode Header.retry_after "120";;
- : Header.retry_after option = Some (`Seconds 120)
# Header.decode Header.retry_after "Fri, 31 Dec 1999 23:59:59 GMT";;
- : Header.retry_after option = Some (`Date "Fri, 31 Dec 1999 23:59:59 GMT")
```

Conditional dates accept both obsolete wire forms and normalize them to the
IMF-fixdate form that senders are required to generate:

```ocaml
# Option.map (Header.encode Header.if_modified_since)
    (Header.decode Header.if_modified_since
       "Sunday, 06-Nov-94 08:49:37 GMT");;
- : string option = Some "Sun, 06 Nov 1994 08:49:37 GMT"
# Option.map (Header.encode Header.if_unmodified_since)
    (Header.decode Header.if_unmodified_since
       "Sun Nov  6 08:49:37 1994");;
- : string option = Some "Sun, 06 Nov 1994 08:49:37 GMT"
# Header.decode Header.if_modified_since "yesterday";;
- : string option = None
```

`Cache-Status` lists every cache the response traversed; a malformed
value decodes to `None` rather than raising:

```ocaml
# (fun cs -> (Header.cache_hit cs, Header.encode Header.cache_status cs))
    (Option.get
       (Header.decode Header.cache_status
          "Cloudflare; hit, ExampleCDN; fwd=uri-miss; stored"));;
- : bool * string =
(true, "Cloudflare; hit, ExampleCDN; fwd=uri-miss; stored")
# Header.decode Header.cache_status "";;
- : Header.cache_status list option = None
# List.map (Header.decode Header.cache_status)
    [ "Cache; ttl=wat"; "Cache; ttl=1234567890123456" ];;
- : Header.cache_status list option list = [None; None]
```

Digests parse to their algorithm and base64 value (hashing to verify
them is the caller's, or a session layer's, job). `strongest_digest`
picks the best of a parsed list:

```ocaml
# Header.decode Header.content_digest
    "sha-256=:X48E9qOokqqrvdts8nOJRJN3OWDUoyWxBf7kbu9DBPE=:"
  |> Option.map (List.map (fun d -> d.Header.algorithm, d.digest));;
- : ([ `Other of string | `Sha256 | `Sha512 ] * string) list option =
Some [(`Sha256, "X48E9qOokqqrvdts8nOJRJN3OWDUoyWxBf7kbu9DBPE=")]
# (Option.map (fun d -> d.Header.algorithm, d.digest)
     (Header.strongest_digest
        (Option.get
           (Header.decode Header.content_digest
              "sha-256=:c2hh:, sha-512=:c2hi:"))))
  = Some (`Sha512, "c2hi");;
- : bool = true
```

The following values exercise `Strict-Transport-Security` and `Allow`:

```ocaml
# Header.decode Header.strict_transport_security
    "max-age=31536000; includeSubDomains"
  |> Option.map (fun h -> h.Header.max_age, h.include_subdomains, h.preload);;
- : (int64 * bool * bool) option = Some (31536000L, true, false)
# List.map (Header.decode Header.strict_transport_security)
    [ "max-age"; "max-age=1; max-age=2";
      "max-age=1; includeSubDomains; includeSubDomains" ];;
- : Header.hsts option list = [None; None; None]
# Header.decode Header.allow "GET, HEAD, PUT";;
- : Http.Method.t list option = Some [`GET; `HEAD; `PUT]
```

## Content negotiation and conditionals

The following request binds an `Accept` preference list and a cache
revalidation as typed values. The echo server shows the wire form produced by
each codec:

```ocaml
let show_headers (req : Middleware.request) =
  List.iter (fun (n, v) -> Fmt.pr "> %s: %s@." n v)
    (Http.Header.to_list req.headers);
  Fetch_mock.respond ~status:304 "" req
```

```ocaml
# run @@ fun sw ->
  status
    (Fetch.get ~sw (Fetch_mock.client show_headers) "https://api.example/doc"
       ~headers:Header.[
         accept, [ pref "application/json"; pref ~q:0.5 "text/*" ];
         accept_encoding, [ pref "gzip"; pref "br" ];
         if_none_match, `Etags [ { weak = false; tag = "abc123" } ];
         cache_control, cache_directives ~max_age:0 ~no_store:true ();
       ]);;
> Accept: application/json, text/*;q=0.5
> Accept-Encoding: gzip, br
> If-None-Match: "abc123"
> Cache-Control: max-age=0, no-store
- : int = 304
```

An `ETag` read from one response feeds the next request's
`If-None-Match` without touching its string form:

```ocaml
# Header.decode Header.etag "W/\"v2.1\""
  |> Option.map (fun e -> e.Header.weak, e.tag);;
- : (bool * string) option = Some (true, "v2.1")
# Header.(encode if_none_match)
    (`Etags [ Option.get (Header.decode Header.etag "W/\"v2.1\"") ]);;
- : string = "W/\"v2.1\""
```

## Caching, content type, authorization

```ocaml
# Header.decode Header.cache_control
    "public, max-age=604800, stale-while-revalidate=86400, immutable"
  = Some (Header.cache_directives ~public:true ~max_age:604800
            ~stale_while_revalidate:86400 ~immutable:true ());;
- : bool = true
# Header.decode Header.content_type "Text/HTML; charset=UTF-8"
  = Some (Header.media ~params:[ "charset", "UTF-8" ] "text/html");;
- : bool = true
# Header.decode Header.vary "Accept-Encoding, Origin";;
- : Header.vary option = Some (`Fields ["accept-encoding"; "origin"])
```

A `*` anywhere in a `Vary` list makes the response unreusable, whatever
else the list names:

```ocaml
# Header.decode Header.vary "*, Accept";;
- : Header.vary option = Some `Any
```

`Content-Language` members keep their case but must have the shape of a
language tag:

```ocaml
# Header.decode Header.content_language "en, fr-CA";;
- : string list option = Some ["en"; "fr-CA"]
# List.map (Header.decode Header.content_language)
    [ "en_US"; "1abc"; "toolonglanguage" ];;
- : string list option list = [None; None; None]
```

Malformed members reject the complete field instead of leaving a partial
typed value:

```ocaml
# let rejects codec value = Option.is_none (Header.decode codec value);;
val rejects : 'a Header.t -> string -> bool = <fun>
# rejects Header.content_type "text/plain; charset"
  && rejects Header.accept "text/plain;q=1.001"
  && rejects Header.if_none_match {|"good", broken|}
  && rejects Header.range "bytes=9-1"
  && rejects Header.cache_status "Cache; hit=true"
  && rejects Header.content_digest "sha-256=c2hh"
  && rejects Header.location "http://[::1";;
- : bool = true
# Header.decode Header.age
    "99999999999999999999999999999999999999999999999999";;
- : int64 option = Some 2147483648L
```

`Basic` credentials are Base64-encoded by the codec (using the example from
[RFC 7617](https://www.rfc-editor.org/rfc/rfc7617)), and a 401's challenges
parse to their schemes and parameters:

```ocaml
# Header.(encode authorization) (`Basic ("Aladdin", "open sesame"));;
- : string = "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="
# Header.(decode authorization) "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==";;
- : Header.credentials option = Some (`Basic ("Aladdin", "open sesame"))
# Header.decode Header.www_authenticate
    {|Bearer realm="api", error="invalid_token", Basic realm="fallback"|}
  |> Option.map (List.map (fun c -> c.Header.scheme, c.params));;
- : (string * (string * string) list) list option =
Some
 [("Bearer", [("realm", "api"); ("error", "invalid_token")]);
  ("Basic", [("realm", "fallback")])]
```

A scheme may carry a token68 blob instead of parameters, under the empty
key. Anything else after a scheme, and any parameter that precedes the
first scheme, rejects the field rather than dropping the member:

```ocaml
# Header.decode Header.www_authenticate "Negotiate SGVsbG8="
  |> Option.map (List.map (fun c -> c.Header.scheme, c.params));;
- : (string * (string * string) list) list option =
Some [("Negotiate", [("", "SGVsbG8=")])]
# Header.decode Header.www_authenticate {|Basic abc"def|};;
- : Header.challenge list option = None
# Header.decode Header.www_authenticate {|realm="x", Basic|};;
- : Header.challenge list option = None
```

A `Basic` credential whose blob is not canonical Base64 is rejected, not
handed back as an opaque `` `Other ``:

```ocaml
# Header.decode Header.authorization "Basic YW=xh";;
- : Header.credentials option = None
# Header.decode Header.authorization "Basic YWxhZGRpbjpvcGVuc2VzYW1l";;
- : Header.credentials option = Some (`Basic ("aladdin", "opensesame"))
```

The colon separates the pair, so a user-id carrying one is refused rather
than encoded into a credential naming a different user and password
([RFC 7617 §2](https://www.rfc-editor.org/rfc/rfc7617#section-2)):

```ocaml
# Header.(encode authorization) (`Basic ("root:x", "pw"));;
Exception:
Invalid_argument
 "Header.authorization: a Basic user-id cannot contain a colon".
# Header.(encode proxy_authorization) (`Basic ("a:b", ""));;
Exception:
Invalid_argument
 "Header.proxy-authorization: a Basic user-id cannot contain a colon".
# Header.(encode authorization) (`Basic ("root", "p:w"));;
- : string = "Basic cm9vdDpwOnc="
# Header.decode Header.authorization "Bearer two words";;
- : Header.credentials option = None
# Header.(encode authorization) (`Bearer "two words");;
Exception:
Invalid_argument "Header.authorization: Bearer value is not a valid b64token".
```

Link parameters use their bare token form when possible, while values that
need quoting remain quoted:

```ocaml
# Header.(encode links)
    [ Header.link ~rel:"next page" ~hreflang:"en"
        ~params:[ "anchor", "next" ] "/items" ];;
- : string = "</items>; rel=\"next page\"; hreflang=en; anchor=next"
```

A repeated single-valued field is read from its first occurrence, so a
`max-age=0` line appended after the origin's cannot strip HSTS:

```ocaml
# Header.get Header.strict_transport_security
    (Http.Header.of_list
       [ ("Strict-Transport-Security", "max-age=31536000; includeSubDomains");
         ("Strict-Transport-Security", "max-age=0") ])
  |> Option.map (fun h -> h.Header.max_age);;
- : int64 option = Some 31536000L
```

## Defining further headers

`Header.v` gives an application's own headers the same treatment:

```ocaml
# let x_page = Header.v "X-Page" ~encode:string_of_int ~decode:int_of_string_opt;;
val x_page : int Header.t = <abstr>
# Header.pair x_page 5;;
- : string * string = ("X-Page", "5")
# Header.decode x_page "not-a-number";;
- : int option = None
```

A block that arrived elsewhere joins them with `of_http`, which reads
each field as text:

```ocaml
# Header.to_list
    (Header.append
       (Header.of_http (Http.Header.of_list [ ("X-Trace", "abc") ]))
       Header.[ (x_page, 5) ]);;
- : (string * string) list = [("X-Trace", "abc"); ("X-Page", "5")]
```
