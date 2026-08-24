(* The httpz backend against a local Eio server. This mirrors
   tests/httpz.md, which needs mdx and so cannot run in a workspace
   without it. *)

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
    (s, Fetch.trailers resp)
  in
  let chunked, trailers = get "/chunked" in
  check "chunked body" "hello world" chunked;
  Alcotest.(check (option string)) "trailer kept" (Some "abc123")
    (match trailers with
     | None -> None
     | Some h -> Http.Header.get h "x-checksum");
  let closed, trailers = get "/eof" in
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

let () =
  Alcotest.run "fetch-httpz"
    [ ( "backend",
        [ Alcotest.test_case "basic fetch" `Quick test_basic;
          Alcotest.test_case "redirects stay portable" `Quick test_redirect;
          Alcotest.test_case "bare GET wire headers" `Quick test_wire_headers;
          Alcotest.test_case "string body" `Quick test_string_body;
          Alcotest.test_case "stream framing" `Quick test_stream_framing;
          Alcotest.test_case "declared length held" `Quick
            test_stream_length_held;
          Alcotest.test_case "big body streams" `Quick test_big_body;
          Alcotest.test_case "response framings" `Quick test_response_framings;
          Alcotest.test_case "chunk larger than window" `Quick test_big_chunk;
          Alcotest.test_case "abandoned body" `Quick test_abandoned_body;
          Alcotest.test_case "max_response cap" `Quick test_max_response;
          Alcotest.test_case "transparent gzip" `Quick test_gzip;
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
          Alcotest.test_case "policy denial" `Quick test_policy
        ] )
    ]
