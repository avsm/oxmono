# The NSURLSession backend, against a local Eio server

```ocaml
# #require "fetch-macos";;
# #require "eio_main";;
```

A minimal HTTP/1.1 server: prints each request line it receives and
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
  (* Read a request body if one is declared. *)
  let body =
    match header "content-length" with
    | Some n -> Eio.Buf_read.take (int_of_string n) buf
    | None -> ""
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
  | [ "POST"; "/echo"; _ ] -> respond "200 OK" body
  | [ _; "/gzip"; _ ] ->
    respond ~extra:"Content-Encoding: gzip\r\n" "200 OK" gzip_body
  | [ _; "/setcookie"; _ ] ->
    respond ~extra:"Set-Cookie: sid=s3; Path=/\r\n" "200 OK" "set"
  | [ _; "/cookie-echo"; _ ] ->
    respond "200 OK" (Option.value (header "cookie") ~default:"no cookies")
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
```

## Basic fetch

```ocaml
# with_server @@ fun _sw url ->
  let t = Fetch_macos.v () in
  Fetch.read t (url "/hello");;
> GET /hello HTTP/1.1
- : string = "hello from eio"
```

## Redirects stay in the portable layer

NSURLSession never follows redirects itself for this backend — the
portable `fetch` loop does, so the server sees one request per hop and
policy applies to each:

```ocaml
# with_server @@ fun _sw url ->
  let t = Fetch_macos.v () in
  Fetch.read t (url "/redirect");;
> GET /redirect HTTP/1.1
> GET /hello HTTP/1.1
- : string = "hello from eio"
```

With the redirect budget spent, the 302 itself is the response:

```ocaml
# with_server @@ fun _sw url ->
  let t = Fetch_macos.v () in
  Eio.Switch.run @@ fun sw ->
  status (Fetch.get ~sw ~redirects:0 t (url "/redirect"));;
> GET /redirect HTTP/1.1
- : int = 302
```

## Narrowing and appending work over NSURLSession

The same wrappers tested against the mock apply unchanged. The test
server speaks plaintext http, so attaching a credential needs the
explicit `~allow_insecure:true` opt-in:

```ocaml
# with_server @@ fun _sw url ->
  let scope = [ url "" ] in
  let t = Fetch_macos.v ()
    |> Fetch.restrict ~under:scope
    |> Fetch.with_credentials ~scope ~allow_insecure:true
         Fetch.Credential.[ Bearer (fun () -> "S3CRET") ] in
  Fetch.read t (url "/whoami");;
> GET /whoami HTTP/1.1
- : string = "Bearer S3CRET"
```

A denial happens before the network is touched (the server prints
nothing):

```ocaml
# with_server @@ fun _sw url ->
  let t = Fetch.restrict (Fetch_macos.v ())
      ~under:[ "https://allowed.example" ] in
  (try ignore (Fetch.read t (url "/hello")); "reached the network!"
   with Eio.Io (E (Denied _), _) -> "denied before the network");;
- : string = "denied before the network"
```

## Request bodies

```ocaml
# with_server @@ fun _sw url ->
  let t = Fetch_macos.v () in
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.post ~sw t ~body:(String "ping") (url "/echo") in
  Eio.Buf_read.parse_exn ~max_size:1000 Eio.Buf_read.take_all (body resp);;
> POST /echo HTTP/1.1 [body "ping"]
- : string = "ping"
```

A streaming body with a declared length goes out with `Content-Length`
framing, derived by the backend (the test server does not speak
chunked):

```ocaml
# with_server @@ fun _sw url ->
  let t = Fetch_macos.v () in
  Eio.Switch.run @@ fun sw ->
  let body = Fetch.stream ~length:9L (Eio.Flow.string_source "streamed!") in
  let resp = Fetch.post ~sw t ~body (url "/echo") in
  Eio.Buf_read.parse_exn ~max_size:1000 Eio.Buf_read.take_all (Fetch.body resp);;
> POST /echo HTTP/1.1 [body "streamed!"]
- : string = "streamed!"
```

## Response bodies stream

`/big` serves 1 MiB. The body flows through a bounded queue — the task
is suspended whenever the reader falls behind and resumed as it drains
— so a response costs the queue's high-water mark in memory, not its
own size:

```ocaml
# with_server @@ fun _sw url ->
  let t = Fetch_macos.v () in
  String.length (Fetch.read t (url "/big"));;
> GET /big HTTP/1.1
- : int = 1048576
```

`max_response` caps a body's total size; past it the read fails with a
`Protocol_error` rather than buffering on:

```ocaml
# with_server @@ fun _sw url ->
  let t = Fetch_macos.v ~max_response:1024 () in
  (try ignore (Fetch.read t (url "/big") : string); "read it all!"
   with Eio.Io (E (Protocol_error msg), _) -> msg);;
> GET /big HTTP/1.1
- : string = "response body exceeds 1024 bytes"
```

## Transparent content-coding

The backend lets NSURLSession negotiate compression by default and
hands back decoded bytes, with the headers describing the decoded view
(no `Content-Encoding` left behind):

```ocaml
# with_server @@ fun _sw url ->
  let t = Fetch_macos.v () in
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.get ~sw t (url "/gzip") in
  let s = Eio.Buf_read.(parse_exn ~max_size:1000 take_all) (body resp) in
  (s, Http.Header.mem (headers resp) "content-encoding");;
> GET /gzip HTTP/1.1
- : string * bool = ("hello gzip from eio", false)
```

## The std client

`Fetch_macos.std env` is the recommended real-world stack in one call:
the NSURLSession backend plus a cookie jar, per-origin flow control and
retries, minted from stdenv capabilities. The session itself stores no
cookies — the jar does, where policy can see it:

```ocaml
# with_server_env @@ fun env _sw url ->
  let t = Fetch_macos.std env in
  ignore (Fetch.read t (url "/setcookie") : string);
  Fetch.read t (url "/cookie-echo");;
> GET /setcookie HTTP/1.1
> GET /cookie-echo HTTP/1.1
- : string = "sid=s3"
```

## Connection failures

The NSError domain and code decide the error constructor; the message
text varies by OS version, so it is not pinned here. The port is one
nothing listens on, and the connection is refused before any exchange:

```ocaml
# Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let t = Fetch_macos.v () in
  (try ignore (Fetch.read t "http://127.0.0.1:9/" : string); "connected!"
   with Eio.Io (E (Connection_failure (Refused _)), _) -> "refused");;
- : string = "refused"
```
