# The httpz backend, against a local Eio server

```ocaml
# #require "checkseum.c";;
# #require "fetch-httpz";;
# #require "eio_main";;
```

(`checkseum` is a virtual library, so a toplevel has to be told which
implementation to link. Dune picks the default on its own.)

The following minimal HTTP/1.1 server prints each request line it receives and
serves a few canned paths.

```ocaml
open Eio.Std
open Fetch

let handle_client flow _addr =
  let buf = Eio.Buf_read.of_flow flow ~max_size:65536 in
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
  let body =
    match header "content-length", header "transfer-encoding" with
    | Some n, _ -> Eio.Buf_read.take (int_of_string n) buf
    | None, Some "chunked" ->
      let rec chunks acc =
        let n = int_of_string ("0x" ^ String.trim (Eio.Buf_read.line buf)) in
        if n = 0 then String.concat "" (List.rev acc)
        else
          let c = Eio.Buf_read.take n buf in
          ignore (Eio.Buf_read.line buf : string);
          chunks (c :: acc)
      in
      "chunked:" ^ chunks []
    | None, _ -> ""
  in
  Fmt.pr "> %s%s@." request_line
    (if body = "" then "" else Fmt.str " [body %S]" body);
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
    respond "200 OK"
      (match header "x-flag" with
       | None -> "absent"
       | Some v -> Fmt.str "present:%S" v)
  | [ _; "/framing"; _ ] ->
    respond "200 OK"
      (Fmt.str "content-length=%s transfer-encoding=%s"
         (Option.value (header "content-length") ~default:"-")
         (Option.value (header "transfer-encoding") ~default:"-"))
  | [ _; "/dump"; _ ] ->
    respond "200 OK" (String.concat "\n" (List.sort compare req_headers))
  | [ _; "/agent"; _ ] ->
    respond "200 OK" (Option.value (header "user-agent") ~default:"none")
  | [ _; "/echo"; _ ] -> respond "200 OK" body
  | [ _; "/gzip"; _ ] ->
    respond ~extra:"Content-Encoding: gzip\r\n" "200 OK" gzip_body
  | [ _; "/setcookie"; _ ] ->
    respond ~extra:"Set-Cookie: sid=s3; Path=/\r\n" "200 OK" "set"
  | [ _; "/cookie-echo"; _ ] ->
    respond "200 OK" (Option.value (header "cookie") ~default:"no cookies")
  | [ _; "/chunked"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 200 OK\r\n\
       Transfer-Encoding: chunked\r\n\
       Trailer: X-Checksum\r\n\
       Connection: close\r\n\r\n\
       5\r\nhello\r\n6\r\n world\r\n0\r\nX-Checksum: abc123\r\n\r\n"
      flow
  | [ _; "/eof"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\nno length here" flow
  | [ _; "/early"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 103 Early Hints\r\nLink: </s.css>; rel=preload\r\n\r\n\
       HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nhi"
      flow
  | [ _; "/manyheaders"; _ ] ->
    Eio.Flow.copy_string "HTTP/1.1 200 OK\r\n" flow;
    let line = Fmt.str "X-Pad: %s\r\n" (String.make 1000 'p') in
    for _ = 1 to 1000 do Eio.Flow.copy_string line flow done
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
  fn env (fun path -> Fmt.str "http://127.0.0.1:%d%s" port path)

let with_server fn =
  with_server_env (fun env url -> fn (Fetch_httpz.v (Eio.Stdenv.net env) ()) url)

(* The server speaks plaintext, so an https URL naming it is only useful
   alongside a TLS wrapper that does nothing. *)
let as_https s = "https" ^ String.sub s 4 (String.length s - 4)

(* Prefers IPv6, so that the bracketed-literal path gets exercised, and
   falls back where there is no [::1]. What the test below asserts is
   about the authority rather than the address family, so it reads the
   same either way. *)
let with_loopback_server fn =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let listen ip = Eio.Net.listen ~sw ~backlog:5 ~reuse_addr:true net (`Tcp (ip, 0)) in
  let host, sock =
    match listen Eio.Net.Ipaddr.V6.loopback with
    | sock -> ("[::1]", sock)
    | exception _ -> ("127.0.0.1", listen Eio.Net.Ipaddr.V4.loopback)
  in
  let port =
    match Eio.Net.listening_addr sock with
    | `Tcp (_, port) -> port
    | _ -> assert false
  in
  Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server sock handle_client ~on_error:(fun _ -> ()));
  fn (Fetch_httpz.v net ()) (fun path -> Fmt.str "http://%s:%d%s" host port path)
```

## Basic fetch

A client is just configuration: it needs the `net` capability and no
switch, since nothing outlives a request.

```ocaml
# with_server @@ fun t url ->
  Fetch.read t (url "/hello");;
> GET /hello HTTP/1.1
- : string = "hello from eio"
```

## Redirects stay in the portable layer

The backend performs one exchange and never follows a `Location`, so the
server sees a request per hop and policy applies to each:

```ocaml
# with_server @@ fun t url ->
  Fetch.read t (url "/redirect");;
> GET /redirect HTTP/1.1
> GET /hello HTTP/1.1
- : string = "hello from eio"
```

```ocaml
# with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  status (Fetch.get ~sw ~redirects:0 t (url "/redirect"));;
> GET /redirect HTTP/1.1
- : int = 302
```

## Narrowing and appending work over httpz

The same wrappers tested against the mock apply unchanged. The test
server speaks plaintext http, so attaching a credential needs the
explicit `~allow_insecure:true` opt-in:

```ocaml
# with_server @@ fun t url ->
  let scope = [ url "" ] in
  let t = t
    |> Fetch.restrict ~under:scope
    |> Fetch.with_credentials ~scope ~allow_insecure:true
         Fetch.Credential.[ Bearer (fun () -> "S3CRET") ] in
  Fetch.read t (url "/whoami");;
> GET /whoami HTTP/1.1
- : string = "Bearer S3CRET"
```

A denial happens before the network is touched, so the server prints
nothing:

```ocaml
# with_server @@ fun t url ->
  let t = Fetch.restrict t ~under:[ "https://allowed.example" ] in
  (try ignore (Fetch.read t (url "/hello")); "reached the network!"
   with Eio.Io (E (Denied _), _) -> "denied before the network");;
- : string = "denied before the network"
```

## The request target is sent as fetch canonicalized it

`fetch` normalizes a URL once, and the backend serializes that rather
than re-parsing what the user typed, so an encoded `/` survives to the
server intact:

```ocaml
# with_server @@ fun t url ->
  Fetch.read t (url "/a%2Fb?q=a+b");;
> GET /a%2Fb?q=a%20b HTTP/1.1
- : string = "nope"
```

## What a bare GET puts on the wire

A request with no content carries no framing header. Under
[RFC 9112 §6.3](https://www.rfc-editor.org/rfc/rfc9112#section-6.3),
the request therefore has no message body:

```ocaml
# with_server @@ fun t url ->
  (* [Host] must name the origin, port included. The port changes per
     run, so it is compared rather than printed. *)
  let origin = url "" in
  let expected = "host: " ^ String.sub origin 7 (String.length origin - 7) in
  Fetch.read t (url "/dump")
  |> String.split_on_char '\n'
  |> List.map (fun l -> if l = expected then "host: <the origin>" else l);;
> GET /dump HTTP/1.1
- : string list =
["accept-encoding: gzip"; "connection: close"; "host: <the origin>";
 "user-agent: fetch-httpz"]
```

`Host` is derived from the URL the policy layer approved. It matches the
URL's authority whichever address family the server is on, brackets
included for an IPv6 literal:

```ocaml
# with_loopback_server @@ fun t url ->
  let origin = url "" in
  let authority = String.sub origin 7 (String.length origin - 7) in
  let dump = String.split_on_char '\n' (Fetch.read t (url "/dump")) in
  List.mem ("host: " ^ authority) dump;;
> GET /dump HTTP/1.1
- : bool = true
```

## Request bodies

A string body is sent with `Content-Length`:

```ocaml
# with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.post ~sw t ~body:(String "ping") (url "/echo") in
  Eio.Buf_read.parse_exn ~max_size:1000 Eio.Buf_read.take_all (body resp);;
> POST /echo HTTP/1.1 [body "ping"]
- : string = "ping"
```

A `Stream` body is sent as it is read, never buffered whole. Declaring
its length frames it with `Content-Length`; leaving the length out frames
it chunked:

```ocaml
# with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let framing ?length () =
    let flow = Eio.Flow.string_source "0123456789" in
    let resp = Fetch.post ~sw t ~body:(Fetch.stream ?length flow) (url "/framing") in
    Eio.Buf_read.(parse_exn ~max_size:1000 take_all) (body resp)
  in
  let declared = framing ~length:10L () in
  let undeclared = framing () in
  (declared, undeclared);;
> POST /framing HTTP/1.1 [body "0123456789"]
> POST /framing HTTP/1.1 [body "chunked:0123456789"]
- : string * string =
("content-length=10 transfer-encoding=-",
 "content-length=- transfer-encoding=chunked")
```

A declared length is a promise the flow is held to. Overrunning it would
leave bytes behind for the server to read as the start of another
request, so the body is cut at the declared length; falling short of it
fails the request:

```ocaml
# with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let post ~length content =
    let flow = Eio.Flow.string_source content in
    let resp = Fetch.post ~sw t ~body:(Fetch.stream ~length flow) (url "/echo") in
    Eio.Buf_read.(parse_exn ~max_size:100 take_all) (body resp)
  in
  post ~length:4L "0123456789";;
> POST /echo HTTP/1.1 [body "0123"]
- : string = "0123"
```

```ocaml
# with_server @@ fun t url ->
  let short () =
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Flow.string_source "abc" in
    ignore (Fetch.post ~sw t ~body:(Fetch.stream ~length:10L flow) (url "/echo"))
  in
  (try short (); "sent a short body" with
   | Eio.Io (E (Invalid_request msg), _) -> msg);;
- : string = "request body ended 7 bytes short of the declared length of 10"
```

## Response bodies stream

`/big` serves 1 MiB, which arrives without being held in memory at once:

```ocaml
# with_server @@ fun t url ->
  String.length (Fetch.read t (url "/big"));;
> GET /big HTTP/1.1
- : int = 1048576
```

Every response framing an HTTP/1.1 server may use is read: a length, a
chunked body, or a body that simply ends with the connection. The
trailer fields of the chunked one are kept, and `Fetch.trailers`
answers with them once the body has been read to its end:

```ocaml
# with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let get path =
    let resp = Fetch.get ~sw t (url path) in
    let s = Eio.Buf_read.(parse_exn ~max_size:1000 take_all) (body resp) in
    (s, Fetch.trailers resp <> None)
  in
  let chunked = get "/chunked" in
  let closed = get "/eof" in
  (chunked, closed);;
> GET /chunked HTTP/1.1
> GET /eof HTTP/1.1
- : (string * bool) * (string * bool) =
(("hello world", true), ("no length here", false))
```

```ocaml
# with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.get ~sw t (url "/chunked") in
  ignore (Eio.Buf_read.(parse_exn ~max_size:1000 take_all) (body resp) : string);
  Option.map (fun h -> Http.Header.get h "x-checksum") (Fetch.trailers resp);;
> GET /chunked HTTP/1.1
- : string option option = Some (Some "abc123")
```

An abandoned body does not wedge the client: its connection is dropped
when the request's switch finishes, and the next request gets a fresh
one:

```ocaml
# with_server @@ fun t url ->
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

`max_response` caps a body's total size; past it the connection is
dropped rather than read on:

```ocaml
# with_server_env @@ fun env url ->
  let t = Fetch_httpz.v ~max_response:1024 (Eio.Stdenv.net env) () in
  (try ignore (Fetch.read t (url "/big") : string); "read it all!"
   with Eio.Io (E (Protocol_error msg), _) -> msg);;
> GET /big HTTP/1.1
- : string = "response body exceeds 1024 bytes"
```

## Transparent content-coding

The backend asks for gzip and hands back decoded bytes, with the headers
describing the decoded view (no `Content-Encoding` left behind):

```ocaml
# with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.get ~sw t (url "/gzip") in
  let s = Eio.Buf_read.(parse_exn ~max_size:1000 take_all) (body resp) in
  (s, Http.Header.mem (headers resp) "content-encoding");;
> GET /gzip HTTP/1.1
- : string * bool = ("hello gzip from eio", false)
```

A caller who negotiates its own coding gets the response raw, coding
header and all:

```ocaml
# with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let resp =
    Fetch.get ~sw t (url "/gzip") ~headers:Header.[ raw "Accept-Encoding" "gzip" ]
  in
  let s = Eio.Buf_read.(parse_exn ~max_size:1000 take_all) (body resp) in
  (String.length s, Http.Header.get (headers resp) "content-encoding");;
> GET /gzip HTTP/1.1
- : int * string option = (39, Some "gzip")
```

## HEAD

A HEAD response describes a body that is not there; reading it would
block until the peer gave up:

```ocaml
# with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.head ~sw t (url "/hello") in
  (status resp,
   Http.Header.get (headers resp) "content-length",
   Eio.Buf_read.(parse_exn ~max_size:100 take_all) (body resp));;
> HEAD /hello HTTP/1.1
- : int * string option * string = (200, Some "14", "")
```

## An empty header value is still sent

```ocaml
# with_server @@ fun t url ->
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

## A user agent is sent, and a caller's own wins

```ocaml
# with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let agent ?headers () =
    let resp = Fetch.get ~sw ?headers t (url "/agent") in
    Eio.Buf_read.(parse_exn ~max_size:100 take_all) (body resp)
  in
  (agent (), agent ~headers:Header.[ user_agent, "mine/1.0" ] ());;
> GET /agent HTTP/1.1
> GET /agent HTTP/1.1
- : string * string = ("fetch-httpz", "mine/1.0")
```

## Oversized header blocks are refused

The response head must fit httpz's parse window, so a server that never
finishes its header block cannot make the client allocate without
bound:

```ocaml
# with_server @@ fun t url ->
  (try ignore (Fetch.read t (url "/manyheaders") : string); "read it all!"
   with Eio.Io (E (Protocol_error msg), _) -> msg);;
> GET /manyheaders HTTP/1.1
- : string = "response headers exceed 30000 bytes"
```

## Interim responses are skipped

An unsolicited `1xx` is a bare head that precedes the response proper
([RFC 9110 §15.2](https://www.rfc-editor.org/rfc/rfc9110#section-15.2)).
The backend reads past it to the answer:

```ocaml
# with_server @@ fun t url ->
  Fetch.read t (url "/early");;
> GET /early HTTP/1.1
- : string = "hi"
```

## Bare clients need a TLS provider

`Fetch_httpz.std` supplies system-trust TLS. The deliberately bare
`Fetch_httpz.v` constructor accepts a caller-supplied wrapper and, without
one, refuses an https URL before any connection is made:

```ocaml
# Eio_main.run @@ fun env ->
  let t = Fetch_httpz.v (Eio.Stdenv.net env) () in
  (try ignore (Fetch.read t "https://example.com/" : string); "connected!"
   with Eio.Io (E (Tls_failure msg), _) -> msg);;
- : string = "no TLS provider: pass ~https to fetch https URLs"
```

`~https` is handed the URL and the freshly connected socket, and returns
the connection to speak HTTP over. The wrapper below is a sham that hands
the socket straight back, so the plaintext test server answers an `https`
URL:

```ocaml
# with_server_env @@ fun env url ->
  let https uri conn =
    let host =
      match Httpz.Uriz.decoded_host uri with
      | This host -> host
      | Null -> assert false
    in
    Fmt.pr "wrapping %s@." host; conn in
  let t = Fetch_httpz.v ~https (Eio.Stdenv.net env) () in
  Fetch.read t (as_https (url "/hello"));;
wrapping 127.0.0.1
> GET /hello HTTP/1.1
- : string = "hello from eio"
```

Whatever a wrapper raises when it rejects a certificate is reported as a
`Tls_failure`, which is not retried:

```ocaml
# with_server_env @@ fun env url ->
  let https _uri _conn = failwith "certificate rejected" in
  let t = Fetch_httpz.v ~https (Eio.Stdenv.net env) () in
  (try ignore (Fetch.read t (as_https (url "/hello")) : string); "connected!"
   with Eio.Io (E (Tls_failure msg), _) -> msg);;
- : string = "Failure(\"certificate rejected\")"
```

## Connection failures

The error code and the request context are what a caller matches on. The
port is one nothing listens on:

```ocaml
# Eio_main.run @@ fun env ->
  let t = Fetch_httpz.v (Eio.Stdenv.net env) () in
  (try ignore (Fetch.read t "http://127.0.0.1:9/" : string); "connected!"
   with Eio.Io (E (Connection_failure (Refused _)), _) -> "refused");;
- : string = "refused"
```

An IPv6 literal reaches the resolver without its brackets, so it is a
connection that is refused rather than a name that cannot be found:

```ocaml
# Eio_main.run @@ fun env ->
  let t = Fetch_httpz.v (Eio.Stdenv.net env) () in
  (try ignore (Fetch.read t "http://[::1]:9/" : string); "connected!"
   with Eio.Io (E (Connection_failure (Refused _)), _) -> "refused");;
- : string = "refused"
```

## The std client

`Fetch_httpz.std env` is the recommended real-world stack in one call:
the httpz backend plus a cookie jar, per-origin flow control and
retries, minted from stdenv capabilities. Cookies flow without further
setup:

```ocaml
# with_server_env @@ fun env url ->
  let t = Fetch_httpz.std env in
  ignore (Fetch.read t (url "/setcookie") : string);
  Fetch.read t (url "/cookie-echo");;
> GET /setcookie HTTP/1.1
> GET /cookie-echo HTTP/1.1
- : string = "sid=s3"
```

Policy narrows it like any other client:

```ocaml
# with_server_env @@ fun env url ->
  let t = Fetch.restrict (Fetch_httpz.std env)
      ~under:[ "https://allowed.example" ] in
  (try ignore (Fetch.read t (url "/hello") : string); "reached the network!"
   with Eio.Io (E (Denied _), _) -> "denied before the network");;
- : string = "denied before the network"
```
