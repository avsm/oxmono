# Typed header values

```ocaml
# #require "fetch";;
# #require "fetch.mock";;
# #require "eio.mock";;
```

```ocaml
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
  (status resp, body, Fetch.header Header.content_range resp);;
- : int * string * Header.content_range option =
(206, "wxyz",
 Some
  {Fetch.Header.unit = "bytes"; range = Some (100L, 103L);
   complete_length = Some 1000L})
```

## Link pagination

`rel="next"` links walked to the last page:

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

## Decoding is total; encoding round-trips

`Retry-After` distinguishes delta-seconds from HTTP-dates (the
`with_retry` wrapper honours the former):

```ocaml
# Header.decode Header.retry_after "120";;
- : Header.retry_after option = Some (`Seconds 120)
# Header.decode Header.retry_after "Fri, 31 Dec 1999 23:59:59 GMT";;
- : Header.retry_after option = Some (`Date "Fri, 31 Dec 1999 23:59:59 GMT")
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
```

Digests parse to their algorithm and base64 value (hashing to verify
them is the caller's, or a session layer's, job):

```ocaml
# Header.decode Header.content_digest
    "sha-256=:X48E9qOokqqrvdts8nOJRJN3OWDUoyWxBf7kbu9DBPE=:";;
- : Header.digest list option =
Some
 [{Fetch.Header.algorithm = `Sha256;
   digest = "X48E9qOokqqrvdts8nOJRJN3OWDUoyWxBf7kbu9DBPE="}]
```

`Strict-Transport-Security` and `Allow`:

```ocaml
# Header.decode Header.strict_transport_security "max-age=31536000; includeSubDomains";;
- : Header.hsts option =
Some
 {Fetch.Header.max_age = 31536000L; include_subdomains = true;
  preload = false}
# Header.decode Header.allow "GET, HEAD, PUT";;
- : Http.Method.t list option = Some [`GET; `HEAD; `PUT]
```

## Content negotiation and conditionals

An `Accept` preference list and a cache revalidation, bound typed; the
echo server shows the wire form each codec produced:

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
# Header.decode Header.etag "W/\"v2.1\"";;
- : Header.etag option = Some {Fetch.Header.weak = true; tag = "v2.1"}
# Header.(encode if_none_match)
    (`Etags [ Option.get (Header.decode Header.etag "W/\"v2.1\"") ]);;
- : string = "W/\"v2.1\""
```

## Caching, content type, authorization

```ocaml
# Header.decode Header.cache_control
    "public, max-age=604800, stale-while-revalidate=86400, immutable";;
- : Header.cache_control option =
Some
 {Fetch.Header.max_age = Some 604800; s_maxage = None; no_cache = false;
  no_store = false; no_transform = false; only_if_cached = false;
  must_revalidate = false; proxy_revalidate = false; public = true;
  private_ = false; immutable = true; min_fresh = None; max_stale = None;
  stale_while_revalidate = Some 86400; extension = []}
# Header.decode Header.content_type "Text/HTML; charset=UTF-8";;
- : Header.media_type option =
Some {Fetch.Header.media = "text/html"; params = [("charset", "UTF-8")]}
# Header.decode Header.vary "Accept-Encoding, Origin";;
- : Header.vary option = Some (`Fields ["accept-encoding"; "origin"])
```

`Basic` credentials are base64d by the codec (RFC 7617's example), and
a 401's challenges parse to their schemes and parameters:

```ocaml
# Header.(encode authorization) (`Basic ("Aladdin", "open sesame"));;
- : string = "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="
# Header.(decode authorization) "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==";;
- : Header.credentials option = Some (`Basic ("Aladdin", "open sesame"))
# Header.decode Header.www_authenticate
    {|Bearer realm="api", error="invalid_token", Basic realm="fallback"|};;
- : Header.challenge list option =
Some
 [{Fetch.Header.scheme = "Bearer";
   params = [("realm", "api"); ("error", "invalid_token")]};
  {Fetch.Header.scheme = "Basic"; params = [("realm", "fallback")]}]
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
