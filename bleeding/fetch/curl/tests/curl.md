# The curl backend, against a local Eio server

```ocaml
# #require "fetch-curl";;
# #require "eio_main";;
```

The following minimal HTTP/1.1 server prints each request line it receives and
serves several fixed paths.

```ocaml
let () = Printexc.record_backtrace false
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
  let request = String.split_on_char ' ' request_line in
  let quiet =
    match request with
    | [ _;
        ( "/quiet" | "/quiet-big" | "/quiet-trickle" | "/bad-status"
        | "/bad-header" | "/unterminated" | "/switch" | "/http10-te"
        | "/reset-chunked" | "/reset-content" | "/wire-amplification"
        | "/late-trailing" );
        _ ] -> true
    | _ -> false
  in
  if not quiet then
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
  (* "hello brotli from eio", brotli-compressed. *)
  let brotli_body =
    "\x0b\x0a\x80\x68\x65\x6c\x6c\x6f\x20\x62\x72\x6f\x74\x6c\x69\
     \x20\x66\x72\x6f\x6d\x20\x65\x69\x6f\x03"
  in
  match request with
  | [ _; "/hello"; _ ] -> respond "200 OK" "hello from eio"
  | [ _; "/quiet"; _ ] -> respond "200 OK" "ok"
  | [ _; "/quiet-big"; _ ] -> respond "200 OK" (String.make (1024 * 1024) 'q')
  | [ _; "/quiet-trickle"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\na"
      flow;
    Eio_unix.sleep 0.05;
    Eio.Flow.copy_string "b" flow
  | [ _; "/big"; _ ] -> respond "200 OK" (String.make (1024 * 1024) 'x')
  | [ _; "/redirect"; _ ] ->
    respond ~extra:"Location: /hello\r\n" "302 Found" ""
  | [ _; "/whoami"; _ ] ->
    respond "200 OK" (Option.value (header "authorization") ~default:"anonymous")
  | [ _; "/echo-header"; _ ] ->
    respond "200 OK"
      (match header "x-flag" with
       | None -> "absent"
       | Some v -> Fmt.str "present:%S" v)
  | [ "POST"; "/echo"; _ ] -> respond "200 OK" body
  | [ _; "/upload"; _ ] ->
    respond "200 OK" (Fmt.str "%s:%d" framing (String.length body))
  | [ _; "/gzip"; _ ] ->
    respond ~extra:"Content-Encoding: gzip\r\n" "200 OK" gzip_body
  | [ _; "/brotli"; _ ] ->
    respond ~extra:"Content-Encoding: br\r\n" "200 OK" brotli_body
  | [ _; "/bad-status"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 2000 OK\r\nContent-Length: 5\r\n\r\nhello" flow
  | [ _; "/bad-header"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 200 OK\r\nBad Name: value\r\nContent-Length: 5\r\n\r\nhello"
      flow
  | [ _; "/unterminated"; _ ] ->
    Eio.Flow.copy_string "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nhello" flow
  | [ _; "/switch"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 101 Switching Protocols\r\n\
       Connection: Upgrade\r\n\
       Upgrade: example\r\n\r\nUPGRADED"
      flow
  | [ _; "/http10-te"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.0 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n" flow
  | [ "HEAD"; "/head-framing"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\
       Transfer-Encoding: chunked\r\nConnection: close\r\n\r\n"
      flow
  | [ _; "/large-header"; _ ] ->
    respond
      ~extra:(Fmt.str "X-Large: %s\r\n" (String.make (128 * 1024) 'x'))
      "200 OK" "hello"
  | [ _; "/setcookie"; _ ] ->
    respond ~extra:"Set-Cookie: sid=s3; Path=/\r\n" "200 OK" "set"
  | [ _; "/cookie-echo"; _ ] ->
    respond "200 OK" (Option.value (header "cookie") ~default:"no cookies")
  | [ _; "/trailers"; _ ] ->
    Eio.Flow.copy_string
      ("HTTP/1.1 200 OK\r\n\
        Transfer-Encoding: chunked\r\n\
        Trailer: X-Checksum\r\n\
        Connection: close\r\n\r\n\
        5\r\nhello\r\n0\r\nX-Checksum: abc123\r\n\r\n")
      flow
  | [ _; "/empty-trailers"; _ ] ->
    Eio.Flow.copy_string
      ("HTTP/1.1 200 OK\r\n\
        Transfer-Encoding: chunked\r\n\
        Trailer: Set-Cookie\r\n\
        Connection: close\r\n\r\n\
        0\r\nSet-Cookie: late=yes\r\n\r\n")
      flow
  | [ _; "/reset-chunked"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 205 Reset Content\r\nTransfer-Encoding: chunked\r\n\
       Connection: close\r\n\r\n0\r\n\r\n" flow
  | [ _; "/reset-content"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 205 Reset Content\r\nContent-Length: 5\r\n\
       Connection: close\r\n\r\nhello" flow
  | [ _; "/wire-amplification"; _ ] ->
    Eio.Flow.copy_string
      ("HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\
        Connection: close\r\n\r\n1;" ^ String.make 256 'x'
       ^ "\r\na\r\n0\r\n\r\n") flow
  | [ _; "/late-trailing"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\
       Connection: close\r\n\r\n1\r\na\r\n0\r\n\r\n" flow;
    Eio_unix.sleep 0.02;
    Eio.Flow.copy_string "garbage" flow
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

(* A server that can tell whether libcurl reused the first connection after a
   close token hidden in a list. The first response has no body callback, so
   this covers the narrow completion-ordering case. *)
let with_reuse_probe fn =
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
  let accepted = ref 0 in
  let read_request reader =
    ignore (Eio.Buf_read.line reader : string);
    let rec fields () =
      if Eio.Buf_read.line reader <> "" then fields ()
    in
    fields ()
  in
  let reply flow body =
    Eio.Flow.copy_string
      (Fmt.str "HTTP/1.1 200 OK\r\nContent-Length: %d\r\n\
                Connection: close\r\n\r\n%s" (String.length body) body)
      flow
  in
  let handler flow _addr =
    incr accepted;
    let reader = Eio.Buf_read.of_flow flow ~max_size:4096 in
    read_request reader;
    if !accepted = 1 then begin
      Eio.Flow.copy_string
        "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\
         Connection: keep-alive, close\r\n\r\n" flow;
      match read_request reader with
      | () -> reply flow "reused"
      | exception End_of_file -> ()
    end
    else reply flow "fresh"
  in
  Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server sock handler ~on_error:(fun _ -> ()));
  fn sw (fun path -> Fmt.str "http://127.0.0.1:%d%s" port path)

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

## Configuration is checked before native allocation

Invalid sizes, timeouts and C strings fail deterministically instead of being
truncated or passed through to a platform-dependent C `long` conversion:

```ocaml
# let invalid f =
    match f () with
    | _ -> "accepted"
    | exception Invalid_argument msg -> msg;;
val invalid : (unit -> 'a) -> string = <fun>
# Eio_main.run @@ fun _env ->
  Switch.run @@ fun sw ->
  [ invalid (fun () -> Fetch_curl.v ~sw ~timeout:nan ());
    invalid (fun () -> Fetch_curl.v ~sw ~connect_timeout:(-1.) ());
    invalid (fun () -> Fetch_curl.v ~sw ~max_response:(-1) ());
    invalid (fun () -> Fetch_curl.v ~sw ~max_request:(-1) ());
    invalid (fun () -> Fetch_curl.v ~sw ~max_total_connections:(-1) ());
    invalid (fun () -> Fetch_curl.v ~sw ~proxy:"http://proxy\x00.invalid" ());
    invalid (fun () -> Fetch_curl.v ~sw ~user_agent:"bad\ragent" ());
    invalid (fun () ->
      Fetch_curl.v ~sw ~resolve:[ "bad,host", 80, "127.0.0.1" ] ());
    invalid (fun () ->
      Fetch_curl.v ~sw ~resolve:[ "example.com", 0, "127.0.0.1" ] ());
    invalid (fun () ->
      Fetch_curl.v ~sw ~resolve:[ "example.com", 80, "127.0.0.1,evil" ] ())
  ];;
- : string list =
["Fetch_curl.v: timeout must be finite";
 "Fetch_curl.v: connect_timeout must be non-negative";
 "Fetch_curl.v: max_response must be non-negative";
 "Fetch_curl.v: max_request must be non-negative";
 "Fetch_curl.v: max_total_connections must be non-negative";
 "Fetch_curl.v: proxy contains NUL";
 "Fetch_curl.v: user_agent contains a forbidden control byte";
 "Fetch_curl.v: resolve host invalid character ',' in host";
 "Fetch_curl.v: resolve port must be between 1 and 65535";
 "Fetch_curl.v: resolve address must be a numeric IPv4 or IPv6 address"]
```

Repeatedly creating and releasing an unused client exercises the multi-handle
initialization and shutdown path, including callback-root cleanup:

```ocaml
# Eio_main.run @@ fun _env ->
  for _ = 1 to 200 do
    Switch.run @@ fun sw -> ignore (Fetch_curl.v ~sw () : Fetch_curl.t)
  done;
  Gc.full_major ();
  "released";;
- : string = "released"
```

## Basic fetch

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Fetch.read t (url "/hello");;
> GET /hello HTTP/1.1
- : string = "hello from eio"
```

## Concurrent handle churn

Many fibers can add, drive, finish and clean up easy handles through one multi
handle. This test forces connection churn while sharing the callback roots and
socket/timer machinery:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw ~max_connections_per_host:8
      ~max_total_connections:8 () in
  let results =
    Fiber.List.map ~max_fibers:16
      (fun _ -> Fetch.read t (url "/quiet"))
      (List.init 64 Fun.id)
  in
  Gc.full_major ();
  (List.length results, List.for_all (String.equal "ok") results);;
- : int * bool = (64, true)
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

A caller cannot attach a body to HEAD. This is rejected at the portable Fetch
boundary, before libcurl can turn its POSTFIELDS or upload mode into a POST or
PUT request:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun request_sw ->
  try
    ignore
      (Fetch.fetch ~sw:request_sw ~body:(String "surprise") t `HEAD
         (url "/echo") : response);
    "sent"
  with Eio.Io (E (Invalid_request msg), _) -> msg;;
- : string = "a HEAD request cannot carry a body"
```

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

An early end aborts an upload with a declared length rather than leaving the
peer waiting forever for bytes that cannot arrive, and the client remains
usable afterwards:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  let failed =
    Eio.Switch.run @@ fun request_sw ->
    let payload = Fetch.stream ~length:64L (Eio.Flow.string_source "short") in
    try ignore (Fetch.post ~sw:request_sw t ~body:payload (url "/upload") : response);
        "sent"
    with Eio.Io (E (Invalid_request msg), _) -> msg
  in
  (failed, Fetch.read t (url "/hello"));;
> GET /hello HTTP/1.1
- : string * string =
("request body ended 59 bytes short of the declared length of 64",
 "hello from eio")
```

The declared length is also the exact number of bytes sent. Bytes after that
count remain unread, matching the portable backend:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun request_sw ->
  let payload = Fetch.stream ~length:3L (Eio.Flow.string_source "longer") in
  let resp = Fetch.post ~sw:request_sw t ~body:payload (url "/upload") in
  Eio.Buf_read.(parse_exn ~max_size:100 take_all) (body resp);;
> POST /upload HTTP/1.1 [body "lon"]
- : string = "content-length:3"
```

An exact empty stream is valid:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun request_sw ->
  let payload = Fetch.stream ~length:0L (Eio.Flow.string_source "") in
  let resp = Fetch.post ~sw:request_sw t ~body:payload (url "/upload") in
  Eio.Buf_read.(parse_exn ~max_size:100 take_all) (body resp);;
> POST /upload HTTP/1.1
- : string = "content-length:0"
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

A negative declared length is rejected when the body is constructed, before
native handle allocation or network setup:

```ocaml
# try
    ignore (Fetch.stream ~length:(-1L) (Eio.Flow.string_source "") : body);
    "accepted"
  with Invalid_argument message -> message;;
- : string = "Fetch.stream: length -1 is negative"
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

A response flow has one reader. Rejecting a second reader avoids overwriting
the first fiber's native-callback wakeup and turning a misuse into a hang:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun request_sw ->
  let resp = Fetch.get ~sw:request_sw t (url "/quiet-trickle") in
  let first = Cstruct.create 1 in
  ignore (Eio.Flow.single_read (body resp) first : int);
  let read_second_byte () =
    let buf = Cstruct.create 1 in
    ignore (Eio.Flow.single_read (body resp) buf : int);
    Cstruct.to_string buf
  in
  Fiber.pair read_second_byte (fun () ->
      try read_second_byte ()
      with Invalid_argument msg -> msg);;
- : string * string =
("b", "Fetch_curl: concurrent reads from one response body")
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

The same limit applies before transfer decoding, so chunk extensions and
trailer framing cannot amplify a tiny representation into an unbounded wire
stream:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw ~max_response:64 () in
  try ignore (Fetch.read t (url "/wire-amplification") : string); "accepted"
  with Eio.Io (E (Protocol_error msg), _) -> msg;;
- : string = "response body exceeds 64 bytes"
```

libcurl also has a per-header-line limit. Its newest error code must remain a
normal protocol error even when the OCaml curl binding predates that code, and
the client must still be usable after rejecting the response:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  let large =
    match Fetch.read t (url "/large-header") with
    | _ -> "accepted"
    | exception Eio.Io (E (Protocol_error _), _) -> "rejected"
  in
  (large, Fetch.read t (url "/hello"));;
> GET /large-header HTTP/1.1
> GET /hello HTTP/1.1
- : string * string = ("rejected", "hello from eio")
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

Content and transfer decoding are disabled in libcurl. The backend validates
HTTP/1 framing itself and uses the same strict streaming gzip decoder as
Fetch/httpz, including concatenated members and complete RFC 1952 header and
trailer checks. The only known corpus differences left are libcurl's
conservative rejection of status 099 and HTTP/1 minor versions above 1.1;
those requirements are interoperability `SHOULD`s.

Only gzip is negotiated and only gzip is decoded. An unsolicited `br`
response therefore reaches the caller exactly as coded, with its metadata
intact, independently of the optional decoders in the system libcurl:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun sw ->
  match Fetch.get ~sw t (url "/brotli") with
  | resp ->
    let s = Eio.Buf_read.(parse_exn ~max_size:1000 take_all) (body resp) in
    (s = "hello brotli from eio",
     Http.Header.mem (headers resp) "content-encoding")
  | exception Eio.Io (E (Protocol_error _), _) -> (true, false);;
> GET /brotli HTTP/1.1
- : bool * bool = (false, true)
```

Setting `Accept-Encoding` explicitly opts out of automatic decoding and returns
the coded representation with its metadata intact:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun sw ->
  let resp =
    Fetch.get ~sw t (url "/gzip")
      ~headers:Header.[ raw "Accept-Encoding" "gzip" ]
  in
  let s = Eio.Buf_read.(parse_exn ~max_size:1000 take_all) (body resp) in
  (String.length s, Http.Header.get (headers resp) "content-encoding");;
> GET /gzip HTTP/1.1
- : int * string option = (39, Some "gzip")
```

## Malformed response heads and protocol switches are rejected

The backend performs a small syntactic check before exposing status and header
lines normalized by libcurl. It also rejects an unsolicited protocol switch,
since the Fetch interface cannot expose the upgraded connection:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  let rejected path =
    try ignore (Fetch.read t (url path) : string); false with
    | Eio.Io (E (Protocol_error _), _) -> true
  in
  List.map rejected
    [ "/bad-status"; "/bad-header"; "/unterminated"; "/switch"; "/http10-te" ];;
- : bool list = [true; true; true; true; true]
```

Framing fields describe the corresponding GET representation on HEAD. Since
the response itself is bodyless, their coexistence is not ambiguous:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  let resp = Fetch.head ~sw t (url "/head-framing") in
  let content_length = Http.Header.get (headers resp) "content-length" in
  let transfer_encoding = Http.Header.get (headers resp) "transfer-encoding" in
  let body = Eio.Buf_read.(parse_exn ~max_size:10 take_all) (body resp) in
  (status resp, content_length, transfer_encoding, body);;
> HEAD /head-framing HTTP/1.1
- : int * string option * string option * string =
(200, Some "5", Some "chunked", "")
```

A 205 can legally carry zero-length chunked framing. It is consumed and the
decoded response no longer advertises `Transfer-Encoding`:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun request_sw ->
  let resp = Fetch.get ~sw:request_sw t (url "/reset-chunked") in
  let body = Eio.Buf_read.(parse_exn ~max_size:10 take_all) (body resp) in
  (status resp, Http.Header.mem (headers resp) "transfer-encoding", body);;
- : int * bool * string = (205, false, "")
```

Content is semantically absent from a 205 even when a broken peer sends it;
the backend closes that transfer rather than surfacing the bytes:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Fetch.read t (url "/reset-content");;
- : string = ""
```

A close token hidden in a list is applied before a zero-body transfer can
enter libcurl's pool. The following request therefore uses a fresh connection:

```ocaml
# with_reuse_probe @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  let first = Fetch.read t (url "/first") in
  let second = Fetch.read t (url "/second") in
  (first, second);;
- : string * string = ("", "fresh")
```

## Trailer fields stay out of the headers

A trailer arrives after the body; folding it into the header block
would let a server smuggle, for example, a late `Set-Cookie` past
[RFC 9110 §6.5.1](https://www.rfc-editor.org/rfc/rfc9110#section-6.5.1).
It is exposed separately once the body has been drained:

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
  (s, Http.Header.mem (headers resp) "transfer-encoding",
   Http.Header.mem (headers resp) "x-checksum", trailer);;
> GET /trailers HTTP/1.1
- : string * bool * bool * string option =
("hello", false, false, Some "abc123")
```

Garbage discovered in a later delivery after the terminal chunk is quarantined
and resumed from fiber context; it cannot leave the reader paused forever:

```ocaml
# with_server_env @@ fun env sw url ->
  let t = Fetch_curl.v ~sw () in
  match Eio.Time.with_timeout (Eio.Stdenv.clock env) 1. (fun () ->
      Ok (Fetch.read t (url "/late-trailing"))) with
  | Ok body -> body
  | Error `Timeout -> "timed out";;
- : string = "a"
```

The boundary is the terminal chunk, not the first body callback. A recognized
field that is forbidden in trailers is consumed and discarded, even when the
body is empty:

```ocaml
# with_server @@ fun sw url ->
  let t = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.get ~sw t (url "/empty-trailers") in
  let s = Eio.Buf_read.(parse_exn ~max_size:100 take_all) (body resp) in
  let trailer = Option.bind (Fetch.trailers resp)
      (fun tr -> Http.Header.get tr "set-cookie") in
  (s, Http.Header.mem (headers resp) "set-cookie", trailer);;
> GET /empty-trailers HTTP/1.1
- : string * bool * string option = ("", false, None)
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
