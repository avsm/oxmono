open Eio.Std

let () = Mirage_crypto_rng_unix.use_default ()

let get = function Ok value -> value | Error (`Msg message) -> failwith message

let key =
  lazy
    (X509.Private_key.generate ~seed:"httpz.tls deterministic test key"
       ~bits:2048 `RSA)

let certificate ~dns ~ips =
  let subject =
    let open X509.Distinguished_name in
    [ Relative_distinguished_name.singleton (CN "httpz.tls test") ]
  in
  let key = Lazy.force key in
  let csr = X509.Signing_request.create subject key |> get in
  let public_key = (X509.Signing_request.info csr).public_key in
  let names =
    let names =
      if dns = [] then X509.General_name.empty
      else X509.General_name.singleton DNS dns
    in
    if ips = [] then names else X509.General_name.add IP ips names
  in
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
  let certificate =
    X509.Signing_request.sign csr ~valid_from ~valid_until ~digest:`SHA256
      ~serial:"\x01" ~extensions key subject
    |> Result.get_ok
  in
  (certificate, key)

let server_config ?(chain = []) ?version certificate key =
  Tls.Config.server ~certificates:(`Single (certificate :: chain, key))
    ?version ~alpn_protocols:[ "http/1.1" ] ()
  |> get

let with_server ?chain ?version certificate key fn =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let socket =
    Eio.Net.listen ~sw ~backlog:8 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> assert false
  in
  let tls = Httpz_tls.server (server_config ?chain ?version certificate key) in
  Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server socket ~on_error:(fun _ -> ()) (fun flow _ ->
          let flow = tls (flow :> Httpz_tls.flow) in
          let reader = Eio.Buf_read.of_flow ~max_size:16 flow in
          let request = Eio.Buf_read.take 4 reader in
          if String.equal request "ping" then Eio.Flow.copy_string "pong" flow));
  fn ~clock:(Eio.Stdenv.mono_clock env) ~net ~port

let authenticator certificate expected : X509.Authenticator.t @ portable =
  let fingerprint = X509.Certificate.fingerprint `SHA256 certificate in
  let time : (unit -> Ptime.t option) @ portable =
    fun () -> Some (Ptime.unsafe_of_d_ps (19_675, 80_000_000_000_000_000L))
  in
  let verify =
    X509.Authenticator.cert_fingerprint ~time ~hash:`SHA256 ~fingerprint
  in
  let verifier ?ip ~host certificates =
    match (expected, host, ip) with
    | `Dns, Some _, None | `Ip, None, Some _ ->
        verify ?ip ~host certificates
    | _ -> Error `InvalidChain
  in
  verifier

let connect ~clock ~net ~port ~uri authenticator =
  Switch.run @@ fun sw ->
  let raw =
    Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let flow =
    (Httpz_tls.client ~authenticator) (Httpz.Uriz.of_string_exn uri)
      (raw :> Httpz_tls.flow)
  in
  Fun.protect ~finally:(fun () -> Httpz_tls.close ~clock flow) @@ fun () ->
  Eio.Flow.copy_string "ping" flow;
  Eio.Buf_read.(parse_exn ~max_size:4 (take 4)) flow

let check_peer ~certificate ~key ~uri ~expected =
  let authenticator = authenticator certificate expected in
  with_server certificate key @@ fun ~clock ~net ~port ->
  let uri = Fmt.str uri port in
  Alcotest.(check string) "TLS exchange" "pong"
    (connect ~clock ~net ~port ~uri authenticator)

let test_dns () =
  let certificate, key = certificate ~dns:[ "localhost" ] ~ips:[] in
  check_peer ~certificate ~key ~uri:"https://localhost:%d/"
    ~expected:`Dns

let test_ipv4 () =
  let certificate, key =
    certificate ~dns:[] ~ips:[ "\x7f\x00\x00\x01" ]
  in
  check_peer ~certificate ~key ~uri:"https://127.0.0.1:%d/"
    ~expected:`Ip

let test_ipv6 () =
  let certificate, key =
    certificate ~dns:[]
      ~ips:[ "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01" ]
  in
  check_peer ~certificate ~key ~uri:"https://[::1]:%d/" ~expected:`Ip

let rejected ~certificate ~key ~expected uri =
  let authenticator = authenticator certificate expected in
  with_server certificate key @@ fun ~clock ~net ~port ->
  match connect ~clock ~net ~port ~uri:(Fmt.str uri port) authenticator with
  | _ -> Alcotest.fail "certificate identity mismatch was accepted"
  | exception Httpz_tls.Error _ -> ()

let test_dns_is_not_ip () =
  let certificate, key = certificate ~dns:[ "localhost" ] ~ips:[] in
  rejected ~certificate ~key ~expected:`Ip "https://127.0.0.1:%d/"

let test_ip_is_not_dns () =
  let certificate, key =
    certificate ~dns:[] ~ips:[ "\x7f\x00\x00\x01" ]
  in
  rejected ~certificate ~key ~expected:`Dns "https://localhost:%d/"

let invalid_peer uri expected =
  let certificate, key = certificate ~dns:[ "localhost" ] ~ips:[] in
  let authenticator = authenticator certificate `Dns in
  with_server certificate key @@ fun ~clock:_ ~net ~port ->
  Switch.run @@ fun sw ->
  let raw =
    Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let uri = Httpz.Uriz.of_string_exn (Fmt.str uri port) in
  match (Httpz_tls.client ~authenticator) uri (raw :> Httpz_tls.flow) with
  | _ -> Alcotest.fail "invalid TLS peer was accepted"
  | exception Httpz_tls.Error message ->
      Alcotest.(check bool) "specific error" true (expected message)

let test_invalid_peers () =
  invalid_peer "https:/missing/%d" (String.equal "an HTTPS URL must have a host");
  invalid_peer "https://[v1.fe80]:%d/" (fun message ->
      String.starts_with ~prefix:"TLS does not support the IPvFuture" message);
  invalid_peer "https://bad%%00.example:%d/" (fun message ->
      String.starts_with ~prefix:"invalid TLS host" message)

let distinguished_name cn_value =
  let open X509.Distinguished_name in
  [ Relative_distinguished_name.singleton (CN cn_value) ]

let test_chain_tls12 () =
  let valid_from = Option.get (Ptime.of_float_s 0.) in
  let valid_until = Option.get (Ptime.of_float_s 4_102_444_800.) in
  let ca_subject = distinguished_name "httpz.tls test root" in
  let ca_key =
    X509.Private_key.generate ~seed:"httpz.tls deterministic CA key"
      ~bits:2048 `RSA
  in
  let ca_request = X509.Signing_request.create ca_subject ca_key |> get in
  let ca_public_key = (X509.Signing_request.info ca_request).public_key in
  let ca_extensions =
    let open X509.Extension in
    empty
    |> add Subject_key_id (false, X509.Public_key.id ca_public_key)
    |> add Basic_constraints (true, (true, Some 1))
    |> add Key_usage (true, [ `Key_cert_sign; `CRL_sign ])
  in
  let ca =
    X509.Signing_request.sign ca_request ~valid_from ~valid_until
      ~digest:`SHA256 ~serial:"\x02" ~extensions:ca_extensions ca_key
      ca_subject
    |> Result.get_ok
  in
  let leaf_subject = distinguished_name "httpz.tls chained leaf" in
  let leaf_key =
    X509.Private_key.generate ~seed:"httpz.tls deterministic chained key"
      ~bits:2048 `RSA
  in
  let leaf_request = X509.Signing_request.create leaf_subject leaf_key |> get in
  let leaf_public_key = (X509.Signing_request.info leaf_request).public_key in
  let names = X509.General_name.singleton DNS [ "localhost" ] in
  let leaf_extensions =
    let open X509.Extension in
    empty
    |> add Subject_alt_name (false, names)
    |> add Subject_key_id (false, X509.Public_key.id leaf_public_key)
    |> add Basic_constraints (true, (false, None))
    |> add Key_usage (true, [ `Digital_signature; `Key_encipherment ])
    |> add Ext_key_usage (true, [ `Server_auth ])
  in
  let leaf =
    X509.Signing_request.sign leaf_request ~valid_from ~valid_until
      ~digest:`SHA256 ~serial:"\x03" ~extensions:leaf_extensions ca_key
      ca_subject
    |> Result.get_ok
  in
  let time : (unit -> Ptime.t option) @ portable =
    fun () ->
      Some (Ptime.unsafe_of_d_ps (19_675, 80_000_000_000_000_000L))
  in
  let authenticator = X509.Authenticator.chain_of_trust_no_crl ~time [ ca ] in
  with_server ~chain:[ ca ] ~version:(`TLS_1_2, `TLS_1_2) leaf leaf_key
  @@ fun ~clock ~net ~port ->
  Alcotest.(check string) "chained TLS 1.2 exchange" "pong"
    (connect ~clock ~net ~port ~uri:(Fmt.str "https://localhost:%d/" port)
       authenticator)

let test_system_anchors () =
  match Ca_certs.system_authenticator () with
  | Ok _ -> ()
  | Error (`Msg message) -> Alcotest.fail message

let test_system_client_is_portable () =
  let system = Httpz_tls.system in
  let worker : (unit -> Httpz_tls.client) @ portable = fun () -> system in
  let domain = Domain.Safe.spawn worker in
  let (_ : Httpz_tls.client) = Domain.join domain in
  ()

module Close_spy = struct
  type t = {
    mutable shutdowns : int;
    mutable closes : int;
    shutdown : [ `Return | `Raise | `Stall ];
  }

  let read_methods = []
  let single_read _ (_buf @ local) = raise End_of_file
  let single_write _ (bufs @ local) = Cstruct.lenv bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown t _ =
    t.shutdowns <- t.shutdowns + 1;
    match t.shutdown with
    | `Return -> ()
    | `Raise -> failwith "peer disappeared during shutdown"
    | `Stall -> Eio.Fiber.await_cancel ()
  let close t = t.closes <- t.closes + 1
end

let close_spy shutdown =
  let state = { Close_spy.shutdowns = 0; closes = 0; shutdown } in
  let handler =
    Eio.Resource.handler
      (Eio.Resource.H (Eio.Resource.Close, Close_spy.close)
       :: Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Close_spy)))
  in
  state, Eio.Resource.T (state, handler)

let test_bounded_close () =
  Eio_mock.Backend.run_full @@ fun env ->
  List.iter
    (fun mode ->
      let state, flow = close_spy mode in
      Httpz_tls.close ~timeout:0.01 ~clock:env#mono_clock flow;
      Alcotest.(check int) "shutdown attempted" 1 state.shutdowns;
      Alcotest.(check int) "resource closed" 1 state.closes)
    [ `Return; `Raise; `Stall ]

let test_cancelled_close () =
  Eio_mock.Backend.run_full @@ fun env ->
  let state, flow = close_spy `Stall in
  ignore
    (Eio.Fiber.first
       (fun () ->
          Httpz_tls.close ~timeout:10. ~clock:env#mono_clock flow;
          `Closed)
       (fun () -> Eio.Time.Mono.sleep env#mono_clock 0.01; `Cancelled));
  Alcotest.(check int) "cancelled shutdown attempted" 1 state.shutdowns;
  Alcotest.(check int) "cancelled cleanup closed resource" 1 state.closes

let () =
  Alcotest.run "httpz.tls"
    [ ( "client identity",
        [ Alcotest.test_case "DNS verification" `Quick test_dns;
          Alcotest.test_case "IPv4 verification" `Quick test_ipv4;
          Alcotest.test_case "IPv6 verification" `Quick test_ipv6;
          Alcotest.test_case "DNS certificate is not an IP certificate" `Quick
            test_dns_is_not_ip;
          Alcotest.test_case "IP certificate is not a DNS certificate" `Quick
            test_ip_is_not_dns;
          Alcotest.test_case "invalid peers fail closed" `Quick
            test_invalid_peers;
          Alcotest.test_case "CA chain over TLS 1.2" `Quick test_chain_tls12;
          Alcotest.test_case "system trust anchors load" `Quick
            test_system_anchors;
          Alcotest.test_case "system client crosses domains" `Quick
            test_system_client_is_portable ] );
      ( "cleanup",
        [ Alcotest.test_case "bounded graceful close always releases" `Quick
            test_bounded_close;
          Alcotest.test_case "external cancellation still releases" `Quick
            test_cancelled_close ] ) ]
