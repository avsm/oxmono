# The curl backend, against a local Eio server

```ocaml
# #require "fetch-curl";;
# #require "eio_main";;
```

A minimal HTTP/1.1 server: prints each request line it receives and
serves a few canned paths.

```ocaml
open Eio.Std
open Fetch

let handle_client flow _addr =
  let buf = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
  let request_line = Eio.Buf_read.line buf in
  let rec headers acc =
    match Eio.Buf_read.line buf with
    | "" -> List.rev acc
    | l -> headers (l :: acc)
  in
  let req_headers = headers [] in
  let header name =
    List.find_map (fun l ->
        match String.index_opt l ':' with
        | Some i when String.lowercase_ascii (String.sub l 0 i) = name ->
          Some (String.trim (String.sub l (i + 1) (String.length l - i - 1)))
        | _ -> None)
      req_headers
  in
  (* Read a request body if one is framed, and report which framing was
     used: a known length goes out with Content-Length, an unknown one
     chunked. *)
  let chunked () =
    let b = Buffer.create 1024 in
    let rec go () =
      match int_of_string ("0x" ^ Eio.Buf_read.line buf) with
      | 0 -> Buffer.contents b
      | n ->
        Buffer.add_string b (Eio.Buf_read.take n buf);
        ignore (Eio.Buf_read.line buf : string);
        go ()
    in
    go ()
  in
  let framing, body =
    match header "transfer-encoding", header "content-length" with
    | Some "chunked", _ -> ("chunked", chunked ())
    | _, Some n -> ("content-length", Eio.Buf_read.take (int_of_string n) buf)
    | _ -> ("none", "")
  in
  Fmt.pr "> %s%s@." request_line
    (match String.length body with
     | 0 -> ""
     | n when n <= 64 -> Fmt.str " [body %S]" body
     | n -> Fmt.str " [body %d bytes]" n);
  let respond ?(extra = "") status body =
    Eio.Flow.copy_string
      (Fmt.str "HTTP/1.1 %s\r\n%sContent-Length: %d\r\nConnection: close\r\n\r\n%s"
         status extra (String.length body) body)
      flow
  in
  (* "hello gzip from eio", gzip-compressed (mtime 0, so the bytes are
     stable). *)
  let gzip_body =
    "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\xff\xcb\x48\xcd\xc9\xc9\x57\x48\
     \xaf\xca\x2c\x50\x48\x2b\xca\xcf\x55\x48\xcd\xcc\x07\x00\x5d\x0e\xeb\
     \x88\x13\x00\x00\x00"
  in
  match String.split_on_char ' ' request_line with
  | [ _; "/hello"; _ ] -> respond "200 OK" "hello from eio"
  | [ _; "/big"; _ ] -> respond "200 OK" (String.make (1024 * 1024) 'x')
  | [ _; "/redirect"; _ ] ->
    respond ~extra:"Location: /hello\r\n" "302 Found" ""
  | [ _; "/whoami"; _ ] ->
    respond "200 OK" (Option.value (header "authorization") ~default:"anonymous")
  | [ _; "/echo-header"; _ ] ->
    (* Reports whether X-Flag arrived at all, and with what value. *)
    respond "200 OK"
      (match header "x-flag" with
       | None -> "absent"
       | Some v -> Fmt.str "present:%S" v)
  | [ "POST"; "/echo"; _ ] -> respond "200 OK" body
  | [ _; "/upload"; _ ] ->
    respond "200 OK" (Fmt.str "%s:%d" framing (String.length body))
  | [ _; "/gzip"; _ ] ->
    respond ~extra:"Content-Encoding: gzip\r\n" "200 OK" gzip_body
  | [ _; "/setcookie"; _ ] ->
    respond ~extra:"Set-Cookie: sid=s3; Path=/\r\n" "200 OK" "set"
  | [ _; "/cookie-echo"; _ ] ->
    respond "200 OK" (Option.value (header "cookie") ~default:"no cookies")
  | [ _; "/trailers"; _ ] ->
    (* A chunked body followed by a trailer field. *)
    Eio.Flow.copy_string
      ("HTTP/1.1 200 OK\r\n\
        Transfer-Encoding: chunked\r\n\
        Trailer: X-Checksum\r\n\
        Connection: close\r\n\r\n\
        5\r\nhello\r\n0\r\nX-Checksum: abc123\r\n\r\n")
      flow
  | _ -> respond "404 Not Found" "nope"

let with_server_env fn =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let sock = Eio.Net.listen ~sw ~backlog:5 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let port =
    match Eio.Net.listening_addr sock with
    | `Tcp (_, port) -> port
    | _ -> assert false
  in
  Fiber.fork_daemon ~sw (fun () ->
      (* Errors are ignored: a client abandoning a body mid-transfer
         breaks the handler's write, which is expected in these tests. *)
      Eio.Net.run_server sock handle_client ~on_error:(fun _ -> ()));
  fn env sw (fun path -> Fmt.str "http://127.0.0.1:%d%s" port path)

let with_server fn = with_server_env (fun _env sw url -> fn sw url)

(* A body flow that hands over one piece per call to [wait], so a client
   sending it runs out of body between pieces. *)
module Trickle = struct
  type t = { wait : unit -> unit; mutable pieces : string list }

  let read_methods = []

  let single_read t buf =
    t.wait ();
    match t.pieces with
    | [] -> raise End_of_file
    | p :: rest ->
      t.pieces <- rest;
      Cstruct.blit_from_string p 0 buf 0 (String.length p);
      String.length p
end

let trickle wait pieces =
  Eio.Resource.T ({ Trickle.wait; pieces }, Eio.Flow.Pi.source (module Trickle))
```

## Basic fetch

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Fetch.read t (url "/hello");;
> GET /hello HTTP/1.1
- : string = "hello from eio"
```

## Redirects stay in the portable layer

libcurl never follows redirects itself — the portable `fetch` loop does,
so the server sees one request per hop and policy applies to each:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Fetch.read t (url "/redirect");;
> GET /redirect HTTP/1.1
> GET /hello HTTP/1.1
- : string = "hello from eio"
```

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun sw ->
  status (Fetch.get ~sw ~redirects:0 t (url "/redirect"));;
> GET /redirect HTTP/1.1
- : int = 302
```

## Narrowing and appending work over curl

The same wrappers tested against the mock apply unchanged. The test
server speaks plaintext http, so attaching a credential needs the
explicit `~allow_insecure:true` opt-in:

```ocaml
# with_server @@ fun sw url ->
  let scope = [ url "" ] in
  let t = Fetch_curl.v ~sw ()
    |> Fetch.restrict ~under:scope
    |> Fetch.with_credentials ~scope ~allow_insecure:true
         Fetch.Credential.[ Bearer (fun () -> "S3CRET") ] in
  Fetch.read t (url "/whoami");;
> GET /whoami HTTP/1.1
- : string = "Bearer S3CRET"
```

A denial happens before the network is touched (the server prints
nothing). The denial message names the rejected URL; we match on the
error code here because the server's port differs per run:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch.restrict (Fetch_curl.v ~sw ())
      ~under:[ "https://allowed.example" ] in
  (try ignore (Fetch.read t (url "/hello")); "reached the network!"
   with Eio.Io (E (Denied _), _) -> "denied before the network");;
- : string = "denied before the network"
```

## Request bodies

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.post ~sw t ~body:(String "ping") (url "/echo") in
  Eio.Buf_read.parse_exn ~max_size:1000 Eio.Buf_read.take_all (body resp);;
> POST /echo HTTP/1.1 [body "ping"]
- : string = "ping"
```

## Request bodies stream

A `Stream` body is read as libcurl asks for it, not buffered up front.
`/upload` reports the framing the server saw and the number of body
bytes it received. A declared length goes out as `Content-Length`:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun sw ->
  let payload = Fetch.stream ~length:11L (Eio.Flow.string_source "hello world") in
  let resp = Fetch.post ~sw t ~body:payload (url "/upload") in
  Eio.Buf_read.(parse_exn ~max_size:100 take_all) (body resp);;
> POST /upload HTTP/1.1 [body "hello world"]
- : string = "content-length:11"
```

An undeclared length leaves the framing to libcurl, which chunks it on
HTTP/1.1:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun sw ->
  let payload = Fetch.stream (Eio.Flow.string_source "hello world") in
  let resp = Fetch.post ~sw t ~body:payload (url "/upload") in
  Eio.Buf_read.(parse_exn ~max_size:100 take_all) (body resp);;
> POST /upload HTTP/1.1 [body "hello world"]
- : string = "chunked:11"
```

The queue between the flow and the connection is bounded, so a body
costs the queue's high-water mark in memory rather than its own size:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun sw ->
  let mib = String.make (1024 * 1024) 'x' in
  let payload = Fetch.stream (Eio.Flow.string_source mib) in
  let resp = Fetch.post ~sw t ~body:payload (url "/upload") in
  Eio.Buf_read.(parse_exn ~max_size:100 take_all) (body resp);;
> POST /upload HTTP/1.1 [body 1048576 bytes]
- : string = "chunked:1048576"
```

A flow slower than the connection pauses the transfer instead of
spinning on it, and each piece delivered resumes it:

```ocaml
# with_server_env @@ fun env sw url ->
  let slow = trickle (fun () -> Eio.Time.sleep (Eio.Stdenv.clock env) 0.05) in
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun sw ->
  let payload = Fetch.stream (slow [ "one"; "two"; "three" ]) in
  let resp = Fetch.post ~sw t ~body:payload (url "/upload") in
  Eio.Buf_read.(parse_exn ~max_size:100 take_all) (body resp);;
> POST /upload HTTP/1.1 [body "onetwothree"]
- : string = "chunked:11"
```

Cancelling a request mid-send stops the pump and frees the transfer,
leaving the client fit for the next request:

```ocaml
# with_server_env @@ fun env sw url ->
  let clock = Eio.Stdenv.clock env in
  let slow = trickle (fun () -> Eio.Time.sleep clock 0.05) in
  let t = Fetch_curl.v ~sw () in
  let first =
    Fiber.first
      (fun () ->
         Eio.Switch.run @@ fun sw ->
         let payload = Fetch.stream (slow [ "one"; "two"; "three" ]) in
         ignore (Fetch.post ~sw t ~body:payload (url "/upload") : response);
         "sent")
      (fun () -> Eio.Time.sleep clock 0.07; "cancelled")
  in
  (first, Fetch.read t (url "/hello"));;
> GET /hello HTTP/1.1
- : string * string = ("cancelled", "hello from eio")
```

`max_request` bounds an undeclared body as it is read, failing the
request once the count passes it:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw ~max_request:1024 () in
  Eio.Switch.run @@ fun sw ->
  let payload = Fetch.stream (Eio.Flow.string_source (String.make 4096 'x')) in
  (try ignore (Fetch.post ~sw t ~body:payload (url "/upload") : response);
       "sent it all!"
   with Eio.Io (E (Invalid_request msg), _) -> msg);;
- : string = "request body exceeds 1024 bytes"
```

A declared length over the cap is refused before the network is
touched, so the server sees nothing:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw ~max_request:1024 () in
  Eio.Switch.run @@ fun sw ->
  let flow = Eio.Flow.string_source (String.make 4096 'x') in
  let payload = Fetch.stream ~length:4096L flow in
  (try ignore (Fetch.post ~sw t ~body:payload (url "/upload") : response);
       "sent it all!"
   with Eio.Io (E (Invalid_request msg), _) -> msg);;
- : string = "request body exceeds 1024 bytes"
```

## Response bodies stream

`/big` serves 1 MiB. The body flows through a bounded queue — libcurl
is paused whenever the reader falls behind and resumed as it drains —
so a response costs the queue's high-water mark in memory, not its own
size:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  String.length (Fetch.read t (url "/big"));;
> GET /big HTTP/1.1
- : int = 1048576
```

An abandoned body does not wedge the engine: the paused transfer is
removed when its request's switch finishes, and the client carries on:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  let first =
    Fetch.with_response t `GET (url "/big") @@ fun resp ->
    let b = Cstruct.create 5 in
    ignore (Eio.Flow.single_read (body resp) b : int);
    Cstruct.to_string b
  in
  (first, Fetch.read t (url "/hello"));;
> GET /big HTTP/1.1
> GET /hello HTTP/1.1
- : string * string = ("xxxxx", "hello from eio")
```

`max_response` caps a body's total size; past it the transfer aborts
with a `Protocol_error` rather than buffering on:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw ~max_response:1024 () in
  (try ignore (Fetch.read t (url "/big") : string); "read it all!"
   with Eio.Io (E (Protocol_error msg), _) -> msg);;
> GET /big HTTP/1.1
- : string = "response body exceeds 1024 bytes"
```

## Transparent content-coding

The backend negotiates compression by default and hands back decoded
bytes, with the headers describing the decoded view (no
`Content-Encoding` left behind):

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.get ~sw t (url "/gzip") in
  let s = Eio.Buf_read.(parse_exn ~max_size:1000 take_all) (body resp) in
  (s, Http.Header.mem (headers resp) "content-encoding");;
> GET /gzip HTTP/1.1
- : string * bool = ("hello gzip from eio", false)
```

## Trailer fields stay out of the headers

A trailer arrives after the body; folding it into the header block
would let a server smuggle e.g. a late `Set-Cookie` past RFC 9110
§6.5.1. It is exposed separately, once the body has been drained:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.get ~sw t (url "/trailers") in
  let s = Eio.Buf_read.(parse_exn ~max_size:1000 take_all) (body resp) in
  let trailer =
    match Fetch.trailers resp with
    | Some tr -> Http.Header.get tr "x-checksum"
    | None -> None
  in
  (s, Http.Header.mem (headers resp) "x-checksum", trailer);;
> GET /trailers HTTP/1.1
- : string * bool * string option = ("hello", false, Some "abc123")
```

## An empty header value is still sent

libcurl reads `"Name:"` as *delete this header*, so an empty value needs
its `"Name;"` form — otherwise a header the mock backend would deliver
silently vanishes over curl:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun sw ->
  let get_flag headers =
    let resp = Fetch.get ~sw t (url "/echo-header") ~headers in
    Eio.Buf_read.(parse_exn ~max_size:100 take_all) (body resp)
  in
  (get_flag Header.[ raw "X-Flag" "" ], get_flag Header.[ raw "X-Flag" "set" ]);;
> GET /echo-header HTTP/1.1
> GET /echo-header HTTP/1.1
- : string * string = ("present:\"\"", "present:\"set\"")
```

## The std client

`Fetch_curl.std ~sw env` is the recommended real-world stack in one
call: the curl backend plus a cookie jar, per-origin flow control and
retries, minted from stdenv capabilities. Cookies flow without further
setup:

```ocaml
# with_server_env @@ fun env sw url ->
  let t = Fetch_curl.std ~sw env in
  ignore (Fetch.read t (url "/setcookie") : string);
  Fetch.read t (url "/cookie-echo");;
> GET /setcookie HTTP/1.1
> GET /cookie-echo HTTP/1.1
- : string = "sid=s3"
```

Policy narrows it like any other client:

```ocaml
# with_server_env @@ fun env sw url ->
  let t = Fetch.restrict (Fetch_curl.std ~sw env)
      ~under:[ "https://allowed.example" ] in
  (try ignore (Fetch.read t (url "/hello") : string); "reached the network!"
   with Eio.Io (E (Denied _), _) -> "denied before the network");;
- : string = "denied before the network"
```

## Connection failures

Errors carry the request context and the curl detail:

The error code and the request context are what a caller matches on;
libcurl's own message text varies by version, so it is not pinned here.
The port is one nothing listens on, and the connection is refused before
any exchange:

```ocaml
# Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let t = Fetch_curl.v ~sw ~connect_timeout:5. () in
  (try ignore (Fetch.read t "http://127.0.0.1:9/" : string); "connected!"
   with Eio.Io (E (Connection_failure (Refused _)), _) -> "refused");;
- : string = "refused"
```
