open Eio.Std
open Fetch

let get = function Ok x -> x | Error (`Msg m) -> failwith m

let read_headers reader =
  let rec loop acc =
    match Eio.Buf_read.line reader with
    | "" -> List.rev acc
    | line -> loop (line :: acc)
  in
  loop []

let reply flow ?(status = "200 OK") ?(extra = "") body =
  Eio.Flow.copy_string
    (Fmt.str "HTTP/1.1 %s\r\n%sContent-Length: %d\r\nConnection: close\r\n\r\n%s"
       status extra (String.length body) body)
    flow

let retry_attempts = ref 0
let credential_redirect_target = ref None

let trickle flow ~status ~extra =
  Eio.Flow.copy_string
    (Fmt.str "HTTP/1.1 %s\r\n%sContent-Length: 1000000\r\nConnection: close\r\n\r\n"
       status extra)
    flow;
  while true do
    Eio.Flow.copy_string "x" flow;
    Eio_unix.sleep 0.001
  done

let serve_origin ~active flow _ =
  incr active;
  Fun.protect ~finally:(fun () -> decr active) @@ fun () ->
  let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
  let request = Eio.Buf_read.line reader in
  let headers = read_headers reader in
  match String.split_on_char ' ' request with
  | [ _; "/redirect"; _ ] ->
      trickle flow ~status:"302 Found" ~extra:"Location: /who\r\n"
  | [ _; "/credential-cross"; _ ] ->
      let target = Option.get !credential_redirect_target in
      trickle flow ~status:"302 Found" ~extra:("Location: " ^ target ^ "\r\n")
  | [ _; "/who"; _ ] ->
      let value name =
        List.find_map
          (fun line ->
            match String.index_opt line ':' with
            | Some i when String.equal name
                (String.lowercase_ascii (String.sub line 0 i)) ->
                Some (String.trim
                  (String.sub line (i + 1) (String.length line - i - 1)))
            | _ -> None)
          headers
      in
      reply flow
        (Fmt.str "%s|%s"
           (Option.value (value "x-api-key") ~default:"none")
           (Option.value (value "x-second") ~default:"none"))
  | [ _; "/stalled"; _ ] ->
      trickle flow ~status:"503 Service Unavailable" ~extra:""
  | [ _; "/retry"; _ ] ->
      incr retry_attempts;
      if !retry_attempts <= 3 then
        trickle flow ~status:"503 Service Unavailable" ~extra:""
      else reply flow "retried"
  | [ _; "/head-delay"; _ ] ->
      Eio.Flow.copy_string
        "HTTP/1.1 200 OK\r\nContent-Length: 1\r\nConnection: close\r\n\r\n"
        flow;
      Eio_unix.sleep 0.1;
      Eio.Flow.copy_string "x" flow
  | [ _; "/interim-delay"; _ ] ->
      Eio.Flow.copy_string
        "HTTP/1.1 103 Early Hints\r\nLink: </a>\r\n\r\n" flow;
      Eio_unix.sleep 0.02;
      reply flow "final"
  | _ -> reply flow "origin"

let listen env sw handler =
  let net = Eio.Stdenv.net env in
  let socket =
    Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> assert false
  in
  Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server socket handler ~on_error:(fun _ -> ()));
  port

let certificate () =
  let key = X509.Private_key.generate ~seed:"fetch curl transport test" ~bits:2048 `RSA in
  let open X509.Distinguished_name in
  let subject = [ Relative_distinguished_name.singleton (CN "localhost") ] in
  let csr = X509.Signing_request.create subject key |> get in
  let public_key = (X509.Signing_request.info csr).public_key in
  let names = X509.General_name.singleton DNS [ "localhost" ] in
  let extensions =
    let open X509.Extension in
    empty
    |> add Subject_alt_name (false, names)
    |> add Subject_key_id (false, X509.Public_key.id public_key)
    |> add Basic_constraints (true, (false, None))
    |> add Key_usage (true, [ `Digital_signature; `Key_encipherment ])
    |> add Ext_key_usage (true, [ `Server_auth ])
  in
  let valid_from = Option.get (Ptime.of_float_s 0.) in
  let valid_until = Option.get (Ptime.of_float_s 4_102_444_800.) in
  let cert =
    X509.Signing_request.sign csr ~valid_from ~valid_until ~digest:`SHA256
      ~serial:"\x01" ~extensions key subject
    |> Result.get_ok
  in
  cert, key

let tls_handler env active =
  let cert, key = certificate () in
  let config =
    Tls.Config.server ~certificates:(`Single ([ cert ], key))
      ~alpn_protocols:[ "http/1.1" ] () |> get
  in
  fun raw addr ->
    let flow = Httpz_tls.server config (raw :> Httpz_tls.flow) in
    Fun.protect
      ~finally:(fun () -> Httpz_tls.close ~clock:(Eio.Stdenv.mono_clock env) flow)
      (fun () -> serve_origin ~active flow addr)

let proxy_handler env ~hits flow _ =
  let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
  let request = Eio.Buf_read.line reader in
  ignore (read_headers reader : string list);
  incr hits;
  match String.split_on_char ' ' request with
  | [ "CONNECT"; authority; _ ] ->
      let port =
        match String.rindex_opt authority ':' with
        | Some i -> int_of_string (String.sub authority (i + 1) (String.length authority - i - 1))
        | None -> failwith "CONNECT without port"
      in
      Switch.run @@ fun sw ->
      let upstream =
        Eio.Net.connect ~sw (Eio.Stdenv.net env)
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      in
      Eio.Flow.copy_string "HTTP/1.1 200 Connection established\r\n\r\n" flow;
      ignore
        (Fiber.first
           (fun () -> Eio.Flow.copy flow upstream)
           (fun () -> Eio.Flow.copy upstream flow))
  | _ -> reply flow "proxy"

let with_fixture fn =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let active = ref 0 and proxy_hits = ref 0 in
  let http_port = listen env sw (serve_origin ~active) in
  let https_port = listen env sw (tls_handler env active) in
  let proxy_port = listen env sw (proxy_handler env ~hits:proxy_hits) in
  fn env sw ~active ~proxy_hits
    ~http:(Fmt.str "http://localhost:%d" http_port)
    ~https:(Fmt.str "https://localhost:%d" https_port)
    ~proxy:(Fmt.str "http://127.0.0.1:%d" proxy_port)

let test_proxy_ignores_environment () =
  with_fixture @@ fun _env sw ~active:_ ~proxy_hits ~http ~https ~proxy ->
  List.iter
    (fun (name, value) ->
      Unix.putenv "NO_PROXY" "";
      Unix.putenv "no_proxy" "";
      Unix.putenv name value;
      let before = !proxy_hits in
      let client = Fetch_curl.v ~sw ~proxy ~tls_verify:false () in
      Alcotest.(check string) (name ^ " http") "proxy" (Fetch.read client (http ^ "/"));
      Alcotest.(check string) (name ^ " https") "origin" (Fetch.read client (https ^ "/"));
      Alcotest.(check int) (name ^ " used proxy twice") (before + 2) !proxy_hits)
    [ "NO_PROXY", "*"; "no_proxy", "localhost" ];
  Unix.putenv "NO_PROXY" "";
  Unix.putenv "no_proxy" "";
  Unix.putenv "HTTP_PROXY" proxy;
  Unix.putenv "http_proxy" proxy;
  Unix.putenv "HTTPS_PROXY" proxy;
  Unix.putenv "https_proxy" proxy;
  Unix.putenv "ALL_PROXY" proxy;
  Unix.putenv "all_proxy" proxy;
  let before = !proxy_hits in
  let direct = Fetch_curl.v ~sw ~tls_verify:false () in
  Alcotest.(check string) "default client is direct" "origin" (Fetch.read direct (http ^ "/"));
  Alcotest.(check string) "default HTTPS client is direct" "origin"
    (Fetch.read direct (https ^ "/"));
  Alcotest.(check int) "ambient proxy unused" before !proxy_hits

let test_close_redirect_and_credentials () =
  with_fixture @@ fun env sw ~active ~proxy_hits:_ ~http ~https:_ ~proxy:_ ->
  let scope = [ http ^ "/" ] in
  let client =
    Fetch_curl.v ~sw ~max_total_connections:1 ()
    |> Fetch.with_credentials ~scope ~allow_insecure:true
         Fetch.Credential.[ Header ("X-aPI-kEY", fun _ -> "transport-secret") ]
    |> Fetch.with_credentials ~scope ~allow_insecure:true
         Fetch.Credential.[ Header ("X-SECOND", fun _ -> "second-secret") ]
  in
  let caller =
    Fetch.Header.[ raw "x-api-key" "CALLER"; raw "X-API-KEY" "COPY";
                   raw "x-second" "CALLER2" ]
  in
  let read_with_caller url =
    Fetch.with_response ~headers:caller client `GET url (fun response ->
      Eio.Buf_read.(parse_exn ~max_size:1024 take_all) (Fetch.body response))
  in
  Alcotest.(check string) "credential survives same-origin redirect"
    "transport-secret|second-secret"
    (read_with_caller (http ^ "/redirect"));
  let other_port = listen env sw (serve_origin ~active) in
  credential_redirect_target :=
    Some (Fmt.str "http://localhost:%d/who" other_port);
  Alcotest.(check string) "caller credential is stripped cross-origin"
    "none|none"
    (read_with_caller (http ^ "/credential-cross"));
  Eio.Switch.run @@ fun request_sw ->
  let response = Fetch.get ~sw:request_sw client (http ^ "/stalled") in
  Fetch.close response;
  Fetch.close response;
  let eof =
    try
      ignore (Eio.Flow.single_read (body response) (Cstruct.create 1) : int);
      false
    with End_of_file -> true
  in
  Alcotest.(check bool) "closed body is EOF" true eof;
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
  Alcotest.(check bool) "stalled exchange released" true (!active <= 1)

let test_retry_releases_each_exchange () =
  with_fixture @@ fun env sw ~active ~proxy_hits:_ ~http ~https:_ ~proxy:_ ->
  retry_attempts := 0;
  let config =
    Fetch.Retry.v ~max_retries:3 ~backoff_factor:0. ~jitter:false ()
  in
  let client =
    Fetch_curl.v ~sw ~max_total_connections:1 ()
    |> Fetch.with_retry ~clock:(Eio.Stdenv.mono_clock env)
         ~random:(Eio.Flow.string_source "") ~config
  in
  Alcotest.(check string) "eventual response" "retried"
    (Fetch.read client (http ^ "/retry"));
  Alcotest.(check int) "all attempts" 4 !retry_attempts;
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
  Alcotest.(check bool) "discarded exchanges released" true (!active <= 1)

let test_final_head_boundary () =
  with_fixture @@ fun env sw ~active:_ ~proxy_hits:_ ~http ~https:_ ~proxy:_ ->
  let client = Fetch_curl.v ~sw () in
  Eio.Switch.run @@ fun request_sw ->
  let result =
    Fiber.first
      (fun () -> Some (Fetch.get ~sw:request_sw client (http ^ "/head-delay")))
      (fun () -> Eio.Time.sleep (Eio.Stdenv.clock env) 0.03; None)
  in
  let response =
    match result with
    | Some response -> response
    | None -> Alcotest.fail "response metadata waited for the first body byte"
  in
  Alcotest.(check int) "final status" 200 (Fetch.status response);
  Fetch.close response;
  let interim = Fetch.read client (http ^ "/interim-delay") in
  Alcotest.(check string) "interim block skipped" "final" interim

let unused_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> assert false
  in
  Unix.close socket;
  port

let with_h2 ?max_response mode fn =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let port = unused_port () in
  let script =
    let source = Filename.concat (Sys.getcwd ()) "fetch/curl/tests/h2_fixture.py" in
    if Sys.file_exists source then source
    else Filename.concat (Filename.dirname Sys.executable_name) "h2_fixture.py"
  in
  ignore
    (Eio.Process.spawn ~sw (Eio.Stdenv.process_mgr env)
       [ "python3"; script; string_of_int port; mode ] : _ Eio.Process.t);
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.3;
  let client =
    Fetch_curl.v ~sw ~tls_verify:false ~http_version:`Auto
      ?max_response
      ~resolve:[ "localhost", port, "127.0.0.1" ] ()
  in
  fn client (Fmt.str "https://localhost:%d/" port)

let test_h2_trailers () =
  let check mode expected_body field expected =
    with_h2 mode @@ fun client url ->
    Eio.Switch.run @@ fun sw ->
    let response = Fetch.get ~sw client url in
    Alcotest.(check bool) (mode ^ " negotiated HTTP/2") true
      (Fetch.version response = `HTTP_2);
    Alcotest.(check string) (mode ^ " body") expected_body
      (Eio.Buf_read.(parse_exn ~max_size:8 take_all) (body response));
    Alcotest.(check (option string)) (mode ^ " trailer") expected
      (Option.bind (Fetch.trailers response) (fun h -> Http.Header.get h field))
  in
  check "allowed" "a" "x-checksum" (Some "ok");
  check "forbidden" "a" "set-cookie" None;
  check "empty" "" "x-checksum" (Some "ok")

let test_h2_trailer_budget () =
  with_h2 ~max_response:17 "allowed" @@ fun client url ->
  Alcotest.(check string) "exact body plus trailer boundary accepted" "a"
    (Fetch.read client url);
  with_h2 ~max_response:16 "allowed" @@ fun client url ->
  (* One byte less than the fixture's body-plus-trailer wire budget. *)
  let rejected =
    try
      ignore (Fetch.read client url : string);
      false
    with Eio.Io (E (Protocol_error _), _) -> true
  in
  Alcotest.(check bool) "one byte over HTTP/2 trailer budget rejected" true
    rejected

let test_h2_multiplex_cancellation () =
  with_h2 "multiplex" @@ fun client url ->
  let first_cancelled = ref None and second = ref None in
  Fiber.both
    (fun () ->
       first_cancelled := Some (
         Fetch.with_response client `GET url @@ fun response ->
         Alcotest.(check bool) "first stream is HTTP/2" true
           (Fetch.version response = `HTTP_2);
         let result =
           Fiber.first
             (fun () ->
                Eio.Buf_read.(parse_exn ~max_size:8 take_all)
                  (Fetch.body response))
             (fun () -> Eio_unix.sleep 0.05; "cancelled")
         in
         Alcotest.(check (option string)) "cancelled stream has no trailers"
           None (Option.map (fun _ -> "present") (Fetch.trailers response));
         result))
    (fun () ->
       Eio_unix.sleep 0.01;
       second := Some (Fetch.read client url));
  Alcotest.(check (option string)) "first body read cancelled"
    (Some "cancelled") !first_cancelled;
  Alcotest.(check (option string)) "second multiplexed stream survives reset"
    (Some "bc") !second

let () =
  Mirage_crypto_rng_unix.use_default ();
  Alcotest.run "fetch-curl transport"
    [ "transport",
      [ Alcotest.test_case "explicit proxy ignores environment" `Quick
          test_proxy_ignores_environment;
        Alcotest.test_case "close, redirect and credentials" `Quick
          test_close_redirect_and_credentials;
        Alcotest.test_case "retry releases stalled exchanges" `Quick
          test_retry_releases_each_exchange;
        Alcotest.test_case "response completes at final head" `Quick
          test_final_head_boundary;
        Alcotest.test_case "HTTP/2 trailer policy" `Quick test_h2_trailers;
        Alcotest.test_case "HTTP/2 trailer budget" `Quick
          test_h2_trailer_budget;
        Alcotest.test_case "HTTP/2 multiplex cancellation" `Quick
          test_h2_multiplex_cancellation ] ]
