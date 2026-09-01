open Eio.Std
open Fetch

let credential_redirect_target = ref None

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
    List.find_map
      (fun l ->
        match String.index_opt l ':' with
        | Some i when String.lowercase_ascii (String.sub l 0 i) = name ->
          Some (String.trim (String.sub l (i + 1) (String.length l - i - 1)))
        | _ -> None)
      req_headers
  in
  let body =
    match (header "content-length", header "transfer-encoding") with
    | Some n, _ -> Eio.Buf_read.take (int_of_string n) buf
    | None, Some "chunked" ->
      let rec chunks acc =
        let n = int_of_string ("0x" ^ String.trim (Eio.Buf_read.line buf)) in
        if n = 0 then String.concat "" (List.rev acc)
        else begin
          let c = Eio.Buf_read.take n buf in
          ignore (Eio.Buf_read.line buf : string);
          chunks (c :: acc)
        end
      in
      "chunked:" ^ chunks []
    | None, _ -> ""
  in
  let respond ?(extra = "") status body =
    Eio.Flow.copy_string
      (Fmt.str
         "HTTP/1.1 %s\r\n%sContent-Length: %d\r\nConnection: close\r\n\r\n%s"
         status extra (String.length body) body)
      flow
  in
  (* "hello gzip from eio", gzip-compressed. *)
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
  | [ _; "/credential-redirect"; _ ] ->
    respond ~extra:"Location: /who\r\n" "302 Found" ""
  | [ _; "/credential-cross"; _ ] ->
    respond
      ~extra:("Location: " ^ Option.get !credential_redirect_target ^ "\r\n")
      "302 Found" ""
  | [ _; "/who"; _ ] ->
    respond "200 OK"
      (Fmt.str "%s|%s"
         (Option.value (header "x-api-key") ~default:"none")
         (Option.value (header "x-second") ~default:"none"))
  | [ _; "/echo-header"; _ ] ->
    respond "200 OK"
      (match header "x-flag" with
      | None -> "absent"
      | Some v -> Fmt.str "present:%S" v)
  | [ _; "/framing"; _ ] ->
    respond "200 OK"
      (Fmt.str "content-length=%s transfer-encoding=%s body=%s"
         (Option.value (header "content-length") ~default:"-")
         (Option.value (header "transfer-encoding") ~default:"-")
         body)
  | [ _; "/dump"; _ ] ->
    respond "200 OK" (String.concat "\n" (List.sort compare req_headers))
  | [ _; "/agent"; _ ] ->
    respond "200 OK" (Option.value (header "user-agent") ~default:"none")
  | [ _; "/echo"; _ ] -> respond "200 OK" body
  | [ _; "/gzip"; _ ] ->
    respond ~extra:"Content-Encoding: gzip\r\n" "200 OK" gzip_body
  | [ _; "/gzip-concat"; _ ] ->
    respond ~extra:"Content-Encoding: gzip\r\n" "200 OK" (gzip_body ^ gzip_body)
  | [ _; "/gzip-junk"; _ ] ->
    respond ~extra:"Content-Encoding: gzip\r\n" "200 OK" (gzip_body ^ "junk")
  | [ _; "/setcookie"; _ ] ->
    respond ~extra:"Set-Cookie: sid=s3; Path=/\r\n" "200 OK" "set"
  | [ _; "/cookie-echo"; _ ] ->
    respond "200 OK" (Option.value (header "cookie") ~default:"no cookies")
  | [ _; "/chunked"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 200 OK\r\n\
       Transfer-Encoding: chunked\r\n\
       Trailer: X-Checksum\r\n\
       Connection: close\r\n\
       \r\n\
       5\r\nhello\r\n6\r\n world\r\n0\r\nX-Checksum: abc123\r\n\r\n"
      flow
  | [ _; "/bigchunk"; _ ] ->
    (* One chunk larger than the 32KB parse window. *)
    let data = String.make 100_000 'y' in
    Eio.Flow.copy_string
      (Fmt.str
         "HTTP/1.1 200 OK\r\n\
          Transfer-Encoding: chunked\r\n\
          Connection: close\r\n\
          \r\n\
          %x\r\n%s\r\n0\r\n\r\n"
         (String.length data) data)
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
    for _ = 1 to 1000 do
      Eio.Flow.copy_string line flow
    done
  | [ _; "/noreason"; _ ] ->
    Eio.Flow.copy_string
      "HTTP/1.1 204\r\nConnection: close\r\n\r\n" flow
  | _ -> respond "404 Not Found" "nope"

let with_server_env fn =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let sock =
    Eio.Net.listen ~sw ~backlog:5 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr sock with
    | `Tcp (_, port) -> port
    | _ -> assert false
  in
  Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server sock handle_client ~on_error:(fun _ -> ()));
  fn env (fun path -> Fmt.str "http://127.0.0.1:%d%s" port path)

let with_server fn =
  with_server_env (fun env url ->
      fn (Fetch_httpz.v (Eio.Stdenv.net env) ()) url)

let read_all resp =
  Eio.Buf_read.(parse_exn ~max_size:(2 * 1024 * 1024) take_all) (body resp)

let check = Alcotest.(check string)

let test_basic () =
  with_server @@ fun t url ->
  check "body" "hello from eio" (Fetch.read t (url "/hello"))

let test_redirect () =
  with_server @@ fun t url ->
  check "followed" "hello from eio" (Fetch.read t (url "/redirect"));
  Eio.Switch.run @@ fun sw ->
  Alcotest.(check int) "hop status" 302
    (status (Fetch.get ~sw ~redirects:0 t (url "/redirect")))

let test_credential_redirect_wire () =
  with_server_env @@ fun env url ->
  let scope = [ url "" ] in
  let client =
    Fetch_httpz.v (Eio.Stdenv.net env) ()
    |> Fetch.with_credentials ~scope ~allow_insecure:true
         Fetch.Credential.[ Header ("X-aPI-kEY", fun _ -> "httpz-secret") ]
    |> Fetch.with_credentials ~scope ~allow_insecure:true
         Fetch.Credential.[ Header ("X-SECOND", fun _ -> "second-secret") ]
  in
  let caller =
    Fetch.Header.[ raw "x-api-key" "CALLER"; raw "X-API-KEY" "COPY";
                   raw "x-second" "CALLER2" ]
  in
  let read_with_caller target =
    Fetch.with_response ~headers:caller client `GET target (fun response ->
      Eio.Buf_read.(parse_exn ~max_size:1024 take_all) (Fetch.body response))
  in
  check "credential reaches redirected transport request"
    "httpz-secret|second-secret"
    (read_with_caller (url "/credential-redirect"));
  Eio.Switch.run @@ fun other_sw ->
  let net = Eio.Stdenv.net env in
  let socket =
    Eio.Net.listen ~sw:other_sw ~backlog:5 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | _ -> assert false
  in
  Fiber.fork_daemon ~sw:other_sw (fun () ->
      Eio.Net.run_server socket handle_client ~on_error:(fun _ -> ()));
  credential_redirect_target :=
    Some (Fmt.str "http://127.0.0.1:%d/who" port);
  check "caller credential is stripped cross-origin" "none|none"
    (read_with_caller (url "/credential-cross"))

let test_wire_headers () =
  with_server @@ fun t url ->
  let origin = url "" in
  let host = "host: " ^ String.sub origin 7 (String.length origin - 7) in
  let dump =
    Fetch.read t (url "/dump")
    |> String.split_on_char '\n'
    |> List.map (fun l -> if l = host then "host: <origin>" else l)
  in
  Alcotest.(check (list string)) "bare GET wire headers"
    [ "accept-encoding: gzip"; "connection: close"; "host: <origin>";
      "user-agent: fetch-httpz" ]
    dump

let test_string_body () =
  with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.post ~sw t ~body:(String "ping") (url "/echo") in
  check "echo" "ping" (read_all resp)

let test_stream_framing () =
  with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let framing ?length () =
    let flow = Eio.Flow.string_source "0123456789" in
    let resp =
      Fetch.post ~sw t ~body:(Fetch.stream ?length flow) (url "/framing")
    in
    read_all resp
  in
  check "declared length"
    "content-length=10 transfer-encoding=- body=0123456789"
    (framing ~length:10L ());
  check "undeclared is chunked"
    "content-length=- transfer-encoding=chunked body=chunked:0123456789"
    (framing ())

let test_empty_body_framing () =
  with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let framing meth =
    let resp = Fetch.fetch ~sw t meth (url "/framing") in
    read_all resp
  in
  List.iter
    (fun meth ->
       check "defined content method"
         "content-length=0 transfer-encoding=- body=" (framing meth))
    [ `POST; `PUT; `PATCH ];
  check "method without content semantics"
    "content-length=- transfer-encoding=- body=" (framing `DELETE)

let test_stream_length_held () =
  with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let post ~length content =
    let flow = Eio.Flow.string_source content in
    let resp =
      Fetch.post ~sw t ~body:(Fetch.stream ~length flow) (url "/echo")
    in
    read_all resp
  in
  check "cut at declared length" "0123" (post ~length:4L "0123456789");
  let short () =
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Flow.string_source "abc" in
    ignore
      (Fetch.post ~sw t ~body:(Fetch.stream ~length:10L flow) (url "/echo"))
  in
  check "short body fails"
    "request body ended 7 bytes short of the declared length of 10"
    (try short (); "sent a short body" with
     | Eio.Io (E (Invalid_request msg), _) -> msg)

let test_big_body () =
  with_server @@ fun t url ->
  Alcotest.(check int) "1 MiB arrives" (1024 * 1024)
    (String.length (Fetch.read t (url "/big")))

let test_response_framings () =
  with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let get path =
    let resp = Fetch.get ~sw t (url path) in
    let s = read_all resp in
    (s, headers resp, Fetch.trailers resp)
  in
  let chunked, headers, trailers = get "/chunked" in
  check "chunked body" "hello world" chunked;
  Alcotest.(check bool) "decoded framing removed" false
    (Http.Header.mem headers "transfer-encoding");
  Alcotest.(check (option string)) "trailer kept" (Some "abc123")
    (match trailers with
     | None -> None
     | Some h -> Http.Header.get h "x-checksum");
  let closed, _headers, trailers = get "/eof" in
  check "eof-framed body" "no length here" closed;
  Alcotest.(check bool) "no trailers" true (trailers = None)

let test_big_chunk () =
  with_server @@ fun t url ->
  check "chunk larger than the parse window"
    (String.make 100_000 'y') (Fetch.read t (url "/bigchunk"))

let test_abandoned_body () =
  with_server @@ fun t url ->
  let first =
    Fetch.with_response t `GET (url "/big") @@ fun resp ->
    let b = Cstruct.create 5 in
    ignore (Eio.Flow.single_read (body resp) b : int);
    Cstruct.to_string b
  in
  check "partial read" "xxxxx" first;
  check "next request is fresh" "hello from eio" (Fetch.read t (url "/hello"))

let test_max_response () =
  with_server_env @@ fun env url ->
  let t = Fetch_httpz.v ~max_response:1024 (Eio.Stdenv.net env) () in
  check "capped" "response body exceeds 1024 bytes"
    (try ignore (Fetch.read t (url "/big") : string); "read it all!" with
     | Eio.Io (E (Protocol_error msg), _) -> msg)

let test_config_validation () =
  Eio_mock.Backend.run_full @@ fun _env ->
  let net = Eio_mock.Net.make "net" in
  let invalid f =
    match f () with
    | _ -> "accepted"
    | exception Invalid_argument msg -> msg
  in
  check "negative response limit"
    "Fetch_httpz.v: max_response must be non-negative"
    (invalid (fun () -> Fetch_httpz.v ~max_response:(-1) net ()));
  check "invalid user agent"
    "Fetch_httpz.v: user_agent contains a forbidden control byte"
    (invalid (fun () -> Fetch_httpz.v ~user_agent:"bad\ragent" net ()));
  check "non-finite timeout"
    "Fetch_httpz.v: connect_timeout must be finite"
    (invalid (fun () -> Fetch_httpz.v ~connect_timeout:nan net ()))

let test_oversized_request_head () =
  with_server @@ fun t url ->
  let path = "/" ^ String.make 30_000 'x' in
  check "refused before an unchecked write" "request head exceeds 30000 bytes"
    (try ignore (Fetch.read t (url path) : string); "sent it"
     with Eio.Io (E (Invalid_request msg), _) -> msg)

let test_gzip () =
  with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.get ~sw t (url "/gzip") in
  check "decoded" "hello gzip from eio" (read_all resp);
  Alcotest.(check bool) "no content-encoding left" false
    (Http.Header.mem (headers resp) "content-encoding");
  let resp =
    Fetch.get ~sw t (url "/gzip")
      ~headers:Header.[ raw "Accept-Encoding" "gzip" ]
  in
  Alcotest.(check int) "raw when caller negotiates" 39
    (String.length (read_all resp));
  Alcotest.(check (option string)) "coding header intact" (Some "gzip")
    (Http.Header.get (headers resp) "content-encoding")

let test_gzip_members () =
  with_server @@ fun t url ->
  check
    "concatenated members"
    "hello gzip from eiohello gzip from eio"
    (Fetch.read t (url "/gzip-concat"));
  Alcotest.(check bool)
    "trailing junk rejected"
    true
    (try
       ignore (Fetch.read t (url "/gzip-junk") : string);
       false
     with
     | Eio.Io (E (Protocol_error msg), _) ->
       String.starts_with ~prefix:"malformed gzip response:" msg)

let test_head () =
  with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.head ~sw t (url "/hello") in
  Alcotest.(check int) "status" 200 (status resp);
  Alcotest.(check (option string)) "content-length header" (Some "14")
    (Http.Header.get (headers resp) "content-length");
  check "no body" "" (read_all resp)

let test_empty_header_value () =
  with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let get_flag headers =
    read_all (Fetch.get ~sw t (url "/echo-header") ~headers)
  in
  check "empty value sent" "present:\"\"" (get_flag Header.[ raw "X-Flag" "" ]);
  check "value sent" "present:\"set\"" (get_flag Header.[ raw "X-Flag" "set" ])

let test_user_agent () =
  with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  let agent ?headers () = read_all (Fetch.get ~sw ?headers t (url "/agent")) in
  check "default" "fetch-httpz" (agent ());
  check "caller's wins" "mine/1.0"
    (agent ~headers:Header.[ (user_agent, "mine/1.0") ] ())

let test_oversized_head () =
  with_server @@ fun t url ->
  check "refused" "response headers exceed 30000 bytes"
    (try ignore (Fetch.read t (url "/manyheaders") : string); "read it all!"
     with Eio.Io (E (Protocol_error msg), _) -> msg)

let test_interim_skipped () =
  with_server @@ fun t url ->
  check "1xx skipped" "hi" (Fetch.read t (url "/early"))

let test_no_reason_phrase () =
  with_server @@ fun t url ->
  Eio.Switch.run @@ fun sw ->
  Alcotest.(check int) "status" 204
    (status (Fetch.get ~sw t (url "/noreason")))

let test_https () =
  let as_https s = "https" ^ String.sub s 4 (String.length s - 4) in
  (Eio_main.run @@ fun env ->
   let t = Fetch_httpz.v (Eio.Stdenv.net env) () in
   check "no provider" "no TLS provider: pass ~https to fetch https URLs"
     (try ignore (Fetch.read t "https://example.com/" : string); "connected!"
      with Eio.Io (E (Tls_failure msg), _) -> msg));
  (with_server_env @@ fun env url ->
   let https _uri conn = conn in
   let t = Fetch_httpz.v ~https (Eio.Stdenv.net env) () in
   check "sham wrapper" "hello from eio"
     (Fetch.read t (as_https (url "/hello"))));
  with_server_env @@ fun env url ->
  let https _uri _conn = failwith "certificate rejected" in
  let t = Fetch_httpz.v ~https (Eio.Stdenv.net env) () in
  check "wrapper failure is Tls_failure" "Failure(\"certificate rejected\")"
    (try ignore (Fetch.read t (as_https (url "/hello")) : string); "connected!"
     with Eio.Io (E (Tls_failure msg), _) -> msg);
  with_server_env @@ fun env url ->
  let https _uri _conn = raise (err (Tls_failure "already typed")) in
  let t = Fetch_httpz.v ~https (Eio.Stdenv.net env) () in
  check "typed failure is preserved" "already typed"
    (try ignore (Fetch.read t (as_https (url "/hello")) : string); "connected!"
     with Eio.Io (E (Tls_failure msg), _) -> msg)

let test_connection_refused () =
  Eio_main.run @@ fun env ->
  let t = Fetch_httpz.v (Eio.Stdenv.net env) () in
  check "refused" "refused"
    (try ignore (Fetch.read t "http://127.0.0.1:9/" : string); "connected!"
     with Eio.Io (E (Connection_failure (Refused _)), _) -> "refused")

let test_std_cookies () =
  with_server_env @@ fun env url ->
  let t = Fetch_httpz.std env in
  ignore (Fetch.read t (url "/setcookie") : string);
  check "cookie flows" "sid=s3" (Fetch.read t (url "/cookie-echo"))

let test_policy () =
  with_server @@ fun t url ->
  let t = Fetch.restrict t ~under:[ "https://allowed.example" ] in
  check "denied before the network" "denied before the network"
    (try ignore (Fetch.read t (url "/hello")); "reached the network!"
     with Eio.Io (E (Denied _), _) -> "denied before the network")

(* A peer that takes the request and then says nothing. *)
module Stalled = struct
  type t = unit

  let read_methods = []
  let single_read () _ = Eio.Fiber.await_cancel ()
  let single_write () bufs = Cstruct.lenv bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown () _ = ()
  let close () = ()
end

let stalled_handler :
    (unit, [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ])
    Eio.Resource.handler =
  Eio.Resource.handler
    (Eio.Resource.H (Eio.Resource.Close, Stalled.close)
     :: Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Stalled)))

let stalled () : Fetch_httpz.conn = Eio.Resource.T ((), stalled_handler)

module Closable = struct
  type t = { mutable closed : bool }

  let read_methods = []
  let single_read _ _ = raise End_of_file
  let single_write _ bufs = Cstruct.lenv bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown _ _ = ()
  let close t = t.closed <- true
end

let closable_handler :
    (Closable.t, [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ])
    Eio.Resource.handler =
  Eio.Resource.handler
    (Eio.Resource.H (Eio.Resource.Close, Closable.close)
     :: Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Closable)))

let closable state : Fetch_httpz.conn = Eio.Resource.T (state, closable_handler)

(* A peer that replays one canned response and closes. *)
module Canned = struct
  type t = { mutable left : string }

  let read_methods = []

  let single_read t (buf @ local) =
    if t.left = "" then raise End_of_file
    else begin
      let n = min (Cstruct.length buf) (String.length t.left) in
      Cstruct.blit_from_string t.left 0 buf 0 n;
      t.left <- String.sub t.left n (String.length t.left - n);
      n
    end

  let single_write _ (bufs @ local) = Cstruct.lenv bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown _ _ = ()
  let close _ = ()
end

let canned_handler :
    (Canned.t, [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ])
    Eio.Resource.handler =
  Eio.Resource.handler
    (Eio.Resource.H (Eio.Resource.Close, Canned.close)
     :: Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Canned)))

let canned response : Fetch_httpz.conn =
  Eio.Resource.T ({ Canned.left = response }, canned_handler)

module Retry_connection = struct
  type shared = {
    mutable attempts : int;
    mutable active : int;
    mutable closes : int;
    mutable overlap : bool;
  }

  type t = { shared : shared; mutable left : string; mutable closed : bool }
  let read_methods = []
  let single_read t (buf @ local) =
    if t.closed || t.left = "" then raise End_of_file;
    let n = min (Cstruct.length buf) (String.length t.left) in
    Cstruct.blit_from_string t.left 0 buf 0 n;
    t.left <- String.sub t.left n (String.length t.left - n);
    n
  let single_write _ (bufs @ local) = Cstruct.lenv bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown _ _ = ()
  let close t =
    if not t.closed then begin
      t.closed <- true;
      t.shared.active <- t.shared.active - 1;
      t.shared.closes <- t.shared.closes + 1
    end
end

let retry_connection_handler =
  Eio.Resource.handler
    (Eio.Resource.H (Eio.Resource.Close, Retry_connection.close)
     :: Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Retry_connection)))

let test_retry_releases_each_response () =
  Eio_mock.Backend.run_full @@ fun env ->
  let shared =
    { Retry_connection.attempts = 0; active = 0; closes = 0; overlap = false }
  in
  let connect ~sw:_ ~host:_ ~port:_ =
    if shared.active <> 0 then shared.overlap <- true;
    shared.active <- shared.active + 1;
    shared.attempts <- shared.attempts + 1;
    let left =
      if shared.attempts <= 3 then
        "HTTP/1.1 503 Service Unavailable\r\nContent-Length: 100000\r\n\r\n"
      else "HTTP/1.1 200 OK\r\nContent-Length: 7\r\n\r\nretried"
    in
    Eio.Resource.T ({ Retry_connection.shared; left; closed = false },
                    retry_connection_handler)
  in
  let config =
    Fetch.Retry.v ~max_retries:3 ~backoff_factor:0. ~jitter:false ()
  in
  let client =
    Fetch_httpz.v ~connect (Eio_mock.Net.make "net") ()
    |> Fetch.with_retry ~clock:env#mono_clock
         ~random:(Eio.Flow.string_source "") ~config
  in
  check "eventual response" "retried" (Fetch.read client "http://retry.example/");
  Alcotest.(check int) "four attempts" 4 shared.attempts;
  Alcotest.(check int) "every transport closed" 4 shared.closes;
  Alcotest.(check bool) "no overlapping discarded exchanges" false shared.overlap

let test_explicit_response_close () =
  Eio_mock.Backend.run_full @@ fun env ->
  let state = { Closable.closed = false } in
  let response =
    "HTTP/1.1 200 OK\r\nContent-Length: 100000\r\n\r\n"
  in
  let connection =
    let source = { Canned.left = response } in
    let module Closeable_canned = struct
      type t = Canned.t * Closable.t
      let read_methods = []
      let single_read (source, state) (buf @ local) =
        if state.Closable.closed then raise End_of_file
        else if source.Canned.left = "" then Eio.Fiber.await_cancel ()
        else Canned.single_read source buf
      let single_write _ (bufs @ local) = Cstruct.lenv bufs
      let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
      let shutdown _ _ = ()
      let close (_, state) = Closable.close state
    end in
    let handler =
      Eio.Resource.handler
        (Eio.Resource.H (Eio.Resource.Close, Closeable_canned.close)
         :: Eio.Resource.bindings
              (Eio.Flow.Pi.two_way (module Closeable_canned)))
    in
    Eio.Resource.T ((source, state), handler)
  in
  let t =
    Fetch_httpz.v
      ~connect:(fun ~sw:_ ~host:_ ~port:_ -> connection)
      (Eio_mock.Net.make "net") ()
  in
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.get ~sw t "http://close.example/" in
  let cancelled =
    Eio.Fiber.first
      (fun () ->
         ignore (Eio.Flow.single_read (body resp) (Cstruct.create 1) : int);
         false)
      (fun () -> Eio.Time.Mono.sleep env#mono_clock 0.01; true)
  in
  Alcotest.(check bool) "stalled body read cancelled" true cancelled;
  Fetch.close resp;
  Fetch.close resp;
  Alcotest.(check bool) "transport closed" true state.closed;
  let eof =
    try
      ignore (Eio.Flow.single_read (body resp) (Cstruct.create 1) : int);
      false
    with End_of_file -> true
  in
  Alcotest.(check bool) "closed body is EOF" true eof

(* A peer that answers in exactly the segments it was given, so a test can
   pin where the transport splits a response. An empty segment is where
   the peer closes, and so is the end of the list. *)
module Segmented = struct
  type t = { mutable left : string list }

  let read_methods = []

  let single_read t buf =
    match t.left with
    | [] -> raise End_of_file
    | "" :: rest -> t.left <- rest; raise End_of_file
    | segment :: rest ->
      let n = min (Cstruct.length buf) (String.length segment) in
      Cstruct.blit_from_string segment 0 buf 0 n;
      t.left <-
        (if n = String.length segment then rest
         else String.sub segment n (String.length segment - n) :: rest);
      n

  let single_write _ bufs = Cstruct.lenv bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown _ _ = ()
  let close _ = ()
end

let segmented_handler :
    (Segmented.t, [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ])
    Eio.Resource.handler =
  Eio.Resource.handler
    (Eio.Resource.H (Eio.Resource.Close, Segmented.close)
     :: Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Segmented)))

let segmented segments : Fetch_httpz.conn =
  Eio.Resource.T ({ Segmented.left = segments }, segmented_handler)

(* "hello gzip from eio", gzip-compressed: a ten byte fixed header, the
   deflate stream, then the eight byte CRC and ISIZE trailer. *)
let gzip_member =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\xff"
  ^ "\xcb\x48\xcd\xc9\xc9\x57\x48\xaf\xca\x2c\x50\x48\x2b\xca\xcf\x55"
  ^ "\x48\xcd\xcc\x07\x00"
  ^ "\x5d\x0e\xeb\x88\x13\x00\x00\x00"

let gzip_hello_bad_cm =
  "\031\139\000\000\000\000\000\000\000\003\203H\205\201\201\007\000\134\166\016\054\005\000\000\000"

let gzip_hello_fextra =
  "\031\139\008\004\000\000\000\000\000\003\002\000xy\203H\205\201\201\007\000\134\166\016\054\005\000\000\000"

let gzip_hello_fhcrc =
  "\031\139\008\002\000\000\000\000\000\003\167\119\203H\205\201\201\007\000\134\166\016\054\005\000\000\000"

let gzip_hello_reserved_flag =
  "\031\139\008\032\000\000\000\000\000\003\203H\205\201\201\007\000\134\166\016\054\005\000\000\000"

let gzip_head ?length () =
  Fmt.str "HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\n%sConnection: close\r\n\r\n"
    (match length with
     | None -> ""
     | Some n -> Fmt.str "Content-Length: %d\r\n" n)

let read_segmented_gzip body =
  Eio_mock.Backend.run_full @@ fun _env ->
  let t =
    Fetch_httpz.v
      ~connect:(fun ~sw:_ ~host:_ ~port:_ ->
        segmented [ gzip_head ~length:(String.length body) (); body ])
      (Eio_mock.Net.make "net") ()
  in
  Fetch.read t "http://gzip-header.example/"

let rejects_segmented_gzip body =
  try
    ignore (read_segmented_gzip body : string);
    false
  with
  | Eio.Io (E (Protocol_error msg), _) ->
    String.starts_with ~prefix:"malformed gzip response:" msg

let test_gzip_member_limit () =
  Eio_mock.Backend.run_full @@ fun _env ->
  let body = String.concat "" (List.init 1025 (fun _ -> gzip_member)) in
  let t =
    Fetch_httpz.v
      ~connect:(fun ~sw:_ ~host:_ ~port:_ ->
        segmented [ gzip_head ~length:(String.length body) (); body ])
      (Eio_mock.Net.make "net") ()
  in
  Alcotest.(check bool) "member sequence is bounded" true
    (try
       ignore (Fetch.read t "http://many-members.example/" : string);
       false
     with
     | Eio.Io (E (Protocol_error msg), _) ->
       String.equal msg
         "malformed gzip response: representation has more than 1024 members")

let test_gzip_header_validation () =
  check "FEXTRA uses a little-endian XLEN" "hello"
    (read_segmented_gzip gzip_hello_fextra);
  check "FHCRC covers the complete gzip header" "hello"
    (read_segmented_gzip gzip_hello_fhcrc);
  Alcotest.(check bool) "compression method must be DEFLATE" true
    (rejects_segmented_gzip gzip_hello_bad_cm);
  Alcotest.(check bool) "reserved flags are rejected" true
    (rejects_segmented_gzip gzip_hello_reserved_flag)

(* decompress 1.6.0 asks for the fixed header, and other structures, in one
   input window: the backend has to retain what a read left unconsumed, or
   the split the peer happened to choose would decide whether the member
   decodes. Every split of the whole member is exercised. *)
let test_gzip_split_reads () =
  let body = gzip_member in
  let head = gzip_head ~length:(String.length body) () in
  for cut = 1 to String.length body - 1 do
    Eio_mock.Backend.run_full @@ fun _env ->
    let segments =
      [ head; String.sub body 0 cut;
        String.sub body cut (String.length body - cut) ]
    in
    let t =
      Fetch_httpz.v
        ~connect:(fun ~sw:_ ~host:_ ~port:_ -> segmented segments)
        (Eio_mock.Net.make "net") ()
    in
    check (Fmt.str "split after %d body bytes" cut) "hello gzip from eio"
      (try Fetch.read t "http://split.example/" with
       | Eio.Io (E (Protocol_error m), _) -> m)
  done

(* Byte-at-a-time delivery of a two member representation: every structure
   in the format, header, deflate stream and trailer alike, then arrives
   split. *)
let test_gzip_dribbled () =
  Eio_mock.Backend.run_full @@ fun _env ->
  let body = gzip_member ^ gzip_member in
  let segments =
    gzip_head ~length:(String.length body) ()
    :: List.init (String.length body) (fun i -> String.sub body i 1)
  in
  let t =
    Fetch_httpz.v
      ~connect:(fun ~sw:_ ~host:_ ~port:_ -> segmented segments)
      (Eio_mock.Net.make "net") ()
  in
  check "one byte at a time" "hello gzip from eiohello gzip from eio"
    (Fetch.read t "http://dribble.example/")

(* A member cut short is still rejected: the retained prefix can never be
   completed, so the end of the transport ends the input. *)
let test_gzip_truncated () =
  let truncated n =
    Eio_mock.Backend.run_full @@ fun _env ->
    let body = String.sub gzip_member 0 n in
    let t =
      Fetch_httpz.v
        ~connect:(fun ~sw:_ ~host:_ ~port:_ ->
          segmented [ gzip_head () ^ body ])
        (Eio_mock.Net.make "net") ()
    in
    try ignore (Fetch.read t "http://short.example/" : string); "decoded it!"
    with Eio.Io (E (Protocol_error _), _) -> "rejected"
  in
  check "header cut short" "rejected" (truncated 5);
  check "deflate stream cut short" "rejected" (truncated 20);
  check "trailer cut short" "rejected" (truncated 35)

let test_idle_timeout () =
  Eio_mock.Backend.run_full @@ fun env ->
  let t =
    Fetch_httpz.v ~clock:env#mono_clock ~idle_timeout:5.
      ~connect:(fun ~sw:_ ~host:_ ~port:_ -> stalled ())
      (Eio_mock.Net.make "net") ()
  in
  check "silent peer times out"
    "idle timeout of 5s elapsed while reading from the connection"
    (try ignore (Fetch.read t "http://stalled.example/" : string); "answered!"
     with Eio.Io (E (Protocol_error msg), _) -> msg)

let test_connect_timeout () =
  Eio_mock.Backend.run_full @@ fun env ->
  let t =
    Fetch_httpz.v ~clock:env#mono_clock ~connect_timeout:3.
      ~connect:(fun ~sw:_ ~host:_ ~port:_ -> Eio.Fiber.await_cancel ())
      (Eio_mock.Net.make "net") ()
  in
  check "dialling times out" "connect timed out"
    (try ignore (Fetch.read t "http://slow.example/" : string); "connected!"
     with Eio.Io (E (Connection_failure Timeout), _) -> "connect timed out")

(* [idle_timeout] without a clock bounds nothing: the request waits on the
   silent peer until the whole scheduler has nothing left to run. *)
let test_unbounded_without_a_clock () =
  check "no clock, no bound" "waited forever"
    (try
       Eio_mock.Backend.run_full @@ fun _env ->
       let t =
         Fetch_httpz.v ~idle_timeout:5.
           ~connect:(fun ~sw:_ ~host:_ ~port:_ -> stalled ())
           (Eio_mock.Net.make "net") ()
       in
       ignore (Fetch.read t "http://stalled.example/" : string);
       "answered!"
     with Eio_mock.Backend.Deadlock_detected -> "waited forever")

let test_timeout_releases_slot () =
  Eio_mock.Backend.run_full @@ fun env ->
  let calls = ref 0 in
  let connect ~sw:_ ~host:_ ~port:_ =
    incr calls;
    if !calls = 1 then stalled ()
    else canned "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhi"
  in
  let t =
    Fetch_httpz.v ~clock:env#mono_clock ~idle_timeout:5. ~connect
      (Eio_mock.Net.make "net") ()
    |> Fetch.with_limits ~clock:env#mono_clock ~max_concurrent:1
  in
  (try ignore (Fetch.read t "http://stalled.example/" : string) with
   | Eio.Io (E (Protocol_error _), _) -> ());
  check "the flow-control slot is released" "hi"
    (Fetch.read t "http://stalled.example/")

let test_https_checked_before_connect () =
  Eio_mock.Backend.run_full @@ fun _env ->
  let t =
    Fetch_httpz.v
      ~connect:(fun ~sw:_ ~host:_ ~port:_ ->
        Alcotest.fail "connect ran for an https URL with no TLS provider")
      (Eio_mock.Net.make "net") ()
  in
  check "refused before dialling"
    "no TLS provider: pass ~https to fetch https URLs"
    (try ignore (Fetch.read t "https://example.com/" : string); "connected!"
     with Eio.Io (E (Tls_failure msg), _) -> msg)

let test_tls_failure_closes_raw_connection () =
  Eio_mock.Backend.run_full @@ fun _env ->
  let state = { Closable.closed = false } in
  let t =
    Fetch_httpz.v
      ~connect:(fun ~sw:_ ~host:_ ~port:_ -> closable state)
      ~https:(fun _ _ -> raise (err (Tls_failure "rejected")))
      (Eio_mock.Net.make "net") ()
  in
  (try ignore (Fetch.read t "https://example.com/" : string)
   with Eio.Io (E (Tls_failure _), _) -> ());
  Alcotest.(check bool) "raw connection closed" true state.closed

let test_std_installs_https () =
  Eio_mock.Backend.run_full @@ fun env ->
  let net = Eio_mock.Net.make "net" in
  let std_env =
    object
      method net = net
      method clock = env#clock
      method mono_clock = env#mono_clock
      method secure_random = Eio.Flow.string_source (String.make 64 '\x00')
    end
  in
  let t =
    Fetch_httpz.std ~cookies:`Off
      ~connect:(fun ~sw:_ ~host:_ ~port:_ -> raise Exit)
      std_env
  in
  match Fetch.read t "https://example.com/" with
  | (_ : string) -> Alcotest.fail "unexpected HTTPS response"
  | exception Exit -> ()

let () =
  Alcotest.run "fetch-httpz"
    [ ( "backend",
        [ Alcotest.test_case "basic fetch" `Quick test_basic;
          Alcotest.test_case "redirects stay portable" `Quick test_redirect;
          Alcotest.test_case "credential redirect reaches wire" `Quick
            test_credential_redirect_wire;
          Alcotest.test_case "bare GET wire headers" `Quick test_wire_headers;
          Alcotest.test_case "string body" `Quick test_string_body;
          Alcotest.test_case "stream framing" `Quick test_stream_framing;
          Alcotest.test_case "empty body framing" `Quick
            test_empty_body_framing;
          Alcotest.test_case "declared length held" `Quick
            test_stream_length_held;
          Alcotest.test_case "big body streams" `Quick test_big_body;
          Alcotest.test_case "response framings" `Quick test_response_framings;
          Alcotest.test_case "chunk larger than window" `Quick test_big_chunk;
          Alcotest.test_case "abandoned body" `Quick test_abandoned_body;
          Alcotest.test_case "explicit response close" `Quick
            test_explicit_response_close;
          Alcotest.test_case "retry releases every response" `Quick
            test_retry_releases_each_response;
          Alcotest.test_case "max_response cap" `Quick test_max_response;
          Alcotest.test_case "configuration validation" `Quick
            test_config_validation;
          Alcotest.test_case "oversized request head" `Quick
            test_oversized_request_head;
          Alcotest.test_case "transparent gzip" `Quick test_gzip;
          Alcotest.test_case "gzip member sequence" `Quick test_gzip_members;
          Alcotest.test_case "gzip member limit" `Quick test_gzip_member_limit;
          Alcotest.test_case "gzip split across reads" `Quick
            test_gzip_split_reads;
          Alcotest.test_case "gzip dribbled a byte at a time" `Quick
            test_gzip_dribbled;
          Alcotest.test_case "gzip truncation rejected" `Quick
            test_gzip_truncated;
          Alcotest.test_case "gzip header validation" `Quick
            test_gzip_header_validation;
          Alcotest.test_case "HEAD" `Quick test_head;
          Alcotest.test_case "empty header value" `Quick
            test_empty_header_value;
          Alcotest.test_case "user agent" `Quick test_user_agent;
          Alcotest.test_case "oversized head" `Quick test_oversized_head;
          Alcotest.test_case "interim responses skipped" `Quick
            test_interim_skipped;
          Alcotest.test_case "reason phrase optional" `Quick
            test_no_reason_phrase;
          Alcotest.test_case "https wrapper" `Quick test_https;
          Alcotest.test_case "connection refused" `Quick
            test_connection_refused;
          Alcotest.test_case "std cookies" `Quick test_std_cookies;
          Alcotest.test_case "policy denial" `Quick test_policy;
          Alcotest.test_case "idle timeout" `Quick test_idle_timeout;
          Alcotest.test_case "connect timeout" `Quick test_connect_timeout;
          Alcotest.test_case "no clock, no bound" `Quick
            test_unbounded_without_a_clock;
          Alcotest.test_case "timeout releases the slot" `Quick
            test_timeout_releases_slot;
          Alcotest.test_case "TLS provider checked before connecting" `Quick
            test_https_checked_before_connect;
          Alcotest.test_case "TLS failure closes raw connection" `Quick
            test_tls_failure_closes_raw_connection;
          Alcotest.test_case "std installs HTTPS" `Quick test_std_installs_https
        ] )
    ]
