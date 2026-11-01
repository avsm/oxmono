type flow =
  [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t

type client = Httpz_uri.t -> flow -> flow
type server = flow -> flow

exception Error of string

let error fmt = Format.kasprintf (fun message -> raise (Error message)) fmt

let close ?(timeout = Duration.of_sec 1) ~clock flow =
  (* Closing the resource must survive shutdown failure and cancellation. *)
  Eio.Cancel.protect (fun () ->
      (try
         Eio.Time.Timeout.run_exn
           (Eio.Time.Timeout.seconds clock (Duration.to_f timeout))
           (fun () -> Eio.Flow.shutdown flow `All)
       with _ -> ());
      try Eio.Resource.close flow with _ -> ())

let ensure_default_rng () =
  match Mirage_crypto_rng.default_generator () with
  | _ -> ()
  | exception Mirage_crypto_rng.No_default_generator ->
      Mirage_crypto_rng_unix.use_default ()

type peer = Host of [ `host ] Domain_name.t | Ip of Ipaddr.t

let peer : (Httpz_uri.t -> peer) @ portable = fun uri ->
  match (Httpz_uri.host_kind uri, Httpz_uri.decoded_host uri) with
  (* An empty authority host is a registered name that [Domain_name] accepts as
     the DNS root, so it would otherwise reach the handshake as the empty SNI
     that RFC 6066, Section 3, forbids. *)
  | This `Reg_name, This "" -> error "an HTTPS URL must have a host"
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

(* A handshake failure describes what the peer sent, so its text is flattened
   to printable ASCII and elided in the middle, which keeps both the failure it
   names and the detail that follows. *)
let summary : (string -> string) @ portable = fun text ->
  let limit = 200 in
  let buffer = Buffer.create (String.length text) in
  let spaced = ref true in
  String.iter
    (fun c ->
      if c >= '\x21' && c <= '\x7e' then begin
        Buffer.add_char buffer c;
        spaced := false
      end
      else if not !spaced then begin
        Buffer.add_char buffer ' ';
        spaced := true
      end)
    text;
  let flat = String.trim (Buffer.contents buffer) in
  let length = String.length flat in
  if length <= limit then flat
  else
    let head = limit * 2 / 3 in
    let tail = limit - head in
    String.sub flat 0 head ^ "..." ^ String.sub flat (length - tail) tail

let handshake : (string -> (unit -> flow) -> flow) @ portable = fun side f ->
  match f () with
  | flow -> flow
  | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
  | exception (Error _ as ex) -> raise ex
  | exception (Tls_eio.Tls_alert alert) ->
      error "%s TLS handshake: peer sent alert %s" side
        (Tls.Packet.alert_type_to_string alert)
  | exception (Tls_eio.Tls_failure message) ->
      error "%s TLS handshake failed: %s" side (summary message)
  | exception End_of_file ->
      error "%s TLS handshake failed: peer closed the connection" side
  | exception ex ->
      error "%s TLS handshake failed: %s" side (summary (Printexc.to_string ex))

(* [Tls.Config.client] does not cross portability, so the configuration cannot
   be hoisted out of the portable client closure and is built per connection.
   The peer name and IP literal go into it rather than into the handshake. *)
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
