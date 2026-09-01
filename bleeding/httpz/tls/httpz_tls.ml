type flow =
  [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t

type client = Httpz.Uriz.t -> flow -> flow
type server = flow -> flow

exception Error of string

let error fmt = Format.kasprintf (fun message -> raise (Error message)) fmt

let close ?(timeout = 1.) ~clock flow =
  if not (Float.is_finite timeout) || timeout < 0. then
    invalid_arg "Httpz_tls.close: timeout must be finite and non-negative";
  (* Sending close_notify is best effort. Resource close must always happen,
     even if shutdown times out, the peer disappears, or the caller is
     cancelled. *)
  Eio.Cancel.protect (fun () ->
      (try
         Eio.Time.Timeout.run_exn
           (Eio.Time.Timeout.seconds clock timeout)
           (fun () -> Eio.Flow.shutdown flow `All)
       with _ -> ());
      try Eio.Resource.close flow with _ -> ())

let ensure_default_rng () =
  match Mirage_crypto_rng.default_generator () with
  | _ -> ()
  | exception Mirage_crypto_rng.No_default_generator ->
      Mirage_crypto_rng_unix.use_default ()

type peer = Host of [ `host ] Domain_name.t | Ip of Ipaddr.t

let peer : (Httpz.Uriz.t -> peer) @ portable = fun uri ->
  match (Httpz.Uriz.host_kind uri, Httpz.Uriz.decoded_host uri) with
  | This `Reg_name, This name -> (
      match Domain_name.of_string name with
      | Error (`Msg message) ->
          error "invalid TLS host %S: %s" name message
      | Ok domain -> (
          match Domain_name.host domain with
          | Ok host -> Host host
          | Error (`Msg message) ->
              error "invalid TLS host %S: %s" name message))
  | This (`Ipv4 | `Ipv6), This address -> (
      match Ipaddr.of_string address with
      | Ok ip -> Ip ip
      | Error (`Msg message) ->
          error "invalid TLS IP address %S: %s" address message)
  | This `Ipvfuture, This address ->
      error "TLS does not support the IPvFuture address %S" address
  | _, _ -> error "an HTTPS URL must have a host"

let handshake : (string -> (unit -> flow) -> flow) @ portable = fun side f ->
  match f () with
  | flow -> flow
  | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
  | exception (Error _ as ex) -> raise ex
  | exception (Tls_eio.Tls_alert alert) ->
      error "%s TLS handshake: peer sent alert %s" side
        (Tls.Packet.alert_type_to_string alert)
  | exception (Tls_eio.Tls_failure message) ->
      error "%s TLS handshake failed: %s" side message
  | exception End_of_file ->
      error "%s TLS handshake failed: peer closed the connection" side
  | exception ex ->
      error "%s TLS handshake failed: %s" side (Printexc.to_string ex)

let (client_for_peer @ portable) (authenticator @ portable) peer connection =
  let host = match peer with Host host -> Some host | Ip _ -> None in
  let ip = match peer with Ip ip -> Some ip | Host _ -> None in
  let config =
    match
      Tls.Config.client_no_cert ~authenticator ?peer_name:host ?ip
        ~alpn_protocols:[ "http/1.1" ] ()
    with
    | Ok config -> config
    | Error (`Msg message) -> error "cannot configure TLS client: %s" message
  in
  let g = Mirage_crypto_rng_unix.fresh_generator () in
  handshake "client" (fun () ->
      (Tls_eio.client_of_flow_with_rng ~g config connection :> flow))

let client ~(authenticator @ portable) : client @ portable =
 fun uri connection ->
  let peer = peer uri in
  client_for_peer authenticator peer connection

let (system_authenticator @ portable) =
  match Ca_certs.system_authenticator () with
  | Ok authenticator -> Ok authenticator
  | Error (`Msg message) -> Error message
  | exception ex -> Error (Printexc.to_string ex)

let system : client @ portable = fun uri connection ->
  match system_authenticator with
  | Error message -> error "cannot load the system trust anchors: %s" message
  | Ok authenticator ->
      client_for_peer authenticator (peer uri) connection

let server config connection =
  ensure_default_rng ();
  handshake "server" (fun () ->
      (Tls_eio.server_of_flow config connection :> flow))
