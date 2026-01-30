(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP CONNECT Tunneling for HTTPS via Proxy

    Per RFC 9110 Section 9.3.6:
    The CONNECT method requests that the recipient establish a tunnel
    to the destination origin server and, if successful, thereafter restrict
    its behavior to blind forwarding of packets in both directions. *)

let src = Logs.Src.create "requests.proxy_tunnel" ~doc:"HTTPS proxy tunneling"
module Log = (val Logs.src_log src : Logs.LOG)

module Write = Eio.Buf_write
module Read = Eio.Buf_read

(** {1 Low-level Functions} *)

let write_connect_request w ~proxy ~target_host ~target_port =
  let target = Printf.sprintf "%s:%d" target_host target_port in

  (* CONNECT request line per RFC 9110 Section 9.3.6 *)
  Write.string w "CONNECT ";
  Write.string w target;
  Write.string w " HTTP/1.1\r\n";

  (* Host header is required *)
  Write.string w "Host: ";
  Write.string w target;
  Write.string w "\r\n";

  (* Proxy-Authorization if configured *)
  (match proxy.Proxy.auth with
   | Some auth ->
       (* Apply auth to get the Authorization header, then rename to Proxy-Authorization *)
       let headers = Auth.apply auth Headers.empty in
       (match Headers.get `Authorization headers with
        | Some value ->
            Write.string w "Proxy-Authorization: ";
            Write.string w value;
            Write.string w "\r\n"
        | None -> ())
   | None -> ());

  (* User-Agent for debugging *)
  Write.string w "User-Agent: ocaml-requests\r\n";

  (* End of headers *)
  Write.string w "\r\n";

  Log.debug (fun m -> m "Wrote CONNECT request for %s via %s:%d"
    target proxy.Proxy.host proxy.Proxy.port)

let parse_connect_response r ~proxy ~target =
  (* Parse status line - we just need version and status code *)
  let version_str = Read.take_while (function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '/' | '.' -> true
    | _ -> false) r
  in
  Read.char ' ' r;
  let status_str = Read.take_while (function '0'..'9' -> true | _ -> false) r in
  Read.char ' ' r;
  let reason = Read.line r in

  let status =
    try int_of_string status_str
    with _ ->
      raise (Error.proxy_errorf ~host:proxy.Proxy.host
        "Invalid status code in CONNECT response: %s" status_str)
  in

  Log.debug (fun m -> m "CONNECT response: %s %d %s" version_str status reason);

  (* Read headers until empty line *)
  let rec skip_headers () =
    let line = Read.line r in
    if line <> "" then skip_headers ()
  in
  skip_headers ();

  (* Check for success (2xx) *)
  if status < 200 || status >= 300 then
    raise (Error.proxy_errorf ~host:proxy.Proxy.host
      "CONNECT to %s failed: %d %s" target status reason);

  Log.info (fun m -> m "CONNECT tunnel established to %s via proxy %s:%d"
    target proxy.Proxy.host proxy.Proxy.port)

(** {1 Tunnel Establishment} *)

let connect ~sw ~net ~proxy ~target_host ~target_port () =
  let target = Printf.sprintf "%s:%d" target_host target_port in

  Log.debug (fun m -> m "Establishing CONNECT tunnel to %s via %s:%d"
    target proxy.Proxy.host proxy.Proxy.port);

  (* Connect to proxy server *)
  let proxy_addr =
    let addrs = Eio.Net.getaddrinfo_stream net proxy.Proxy.host
      ~service:(string_of_int proxy.Proxy.port)
    in
    match addrs with
    | [] ->
        raise (Error.err (Error.Dns_resolution_failed {
          hostname = proxy.Proxy.host
        }))
    | addr :: _ -> addr
  in

  let flow =
    try
      Eio.Net.connect ~sw net proxy_addr
    with exn ->
      raise (Error.err (Error.Tcp_connect_failed {
        host = proxy.Proxy.host;
        port = proxy.Proxy.port;
        reason = Printexc.to_string exn
      }))
  in

  Log.debug (fun m -> m "Connected to proxy %s:%d" proxy.Proxy.host proxy.Proxy.port);

  (* Send CONNECT request *)
  Http_write.write_and_flush flow (fun w ->
    write_connect_request w ~proxy ~target_host ~target_port
  );

  (* Read and validate response *)
  let buf_read = Read.of_flow ~max_size:65536 flow in
  parse_connect_response buf_read ~proxy ~target;

  (* Return the raw flow - caller is responsible for TLS wrapping *)
  (flow :> [`Close | `Flow | `R | `Shutdown | `W] Eio.Resource.t)

let connect_with_tls ~sw ~net ~clock:_ ~proxy ~target_host ~target_port
    ?tls_config () =
  (* First establish the tunnel *)
  let tunnel_flow = connect ~sw ~net ~proxy ~target_host ~target_port () in

  (* Get or create TLS config *)
  let tls_config = match tls_config with
    | Some cfg -> cfg
    | None ->
        (* Use system CA certificates *)
        let authenticator =
          match Ca_certs.authenticator () with
          | Ok auth -> auth
          | Error (`Msg msg) ->
              Log.warn (fun m -> m "Failed to load CA certificates: %s, using null authenticator" msg);
              fun ?ip:_ ~host:_ _ -> Ok None
        in
        match Tls.Config.client ~authenticator () with
        | Ok cfg -> cfg
        | Error (`Msg msg) ->
            raise (Error.err (Error.Tls_handshake_failed {
              host = target_host;
              reason = "TLS config error: " ^ msg
            }))
  in

  (* Perform TLS handshake through the tunnel *)
  let host =
    match Domain_name.of_string target_host with
    | Ok domain ->
        (match Domain_name.host domain with
         | Ok host -> host
         | Error (`Msg msg) ->
             raise (Error.tls_handshake_failedf ~host:target_host
               "Invalid hostname for SNI: %s" msg))
    | Error (`Msg msg) ->
        raise (Error.tls_handshake_failedf ~host:target_host
          "Invalid domain name: %s" msg)
  in

  Log.debug (fun m -> m "Starting TLS handshake with %s through tunnel" target_host);

  try
    let tls_flow = Tls_eio.client_of_flow tls_config ~host tunnel_flow in
    Log.info (fun m -> m "TLS tunnel established to %s via proxy %s:%d"
      target_host proxy.Proxy.host proxy.Proxy.port);
    (tls_flow :> Eio.Flow.two_way_ty Eio.Resource.t)
  with exn ->
    raise (Error.tls_handshake_failedf ~host:target_host "%s" (Printexc.to_string exn))
