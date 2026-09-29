type tls = [ `Implicit | `Required_starttls | `Plain ]
type raw = [Eio.Flow.two_way_ty | Eio.Resource.close_ty] Eio.Resource.t
type t = {
  dial : sw:Eio.Switch.t -> raw;
  host : string;
  port : int;
  tls : tls;
  tls_config : Tls.Config.client option;
  peer_name : [ `host ] Domain_name.t option;
  ip : Ipaddr.t option;
}

let v ~net ~host ?port ?(tls=`Implicit)
    ?(authenticator : X509.Authenticator.t option @ portable) () =
  if host = "" then invalid_arg "empty IMAP host";
  let port = Option.value port ~default:(if tls = `Implicit then 993 else 143) in
  if port < 1 || port > 65535 then invalid_arg "invalid IMAP port";
  let dial ~sw =
    let rec attempt = function
    | [] -> failwith "IMAP host has no reachable addresses"
    | addr :: rest -> (try Eio.Net.connect ~sw net addr with
        | ex when rest <> [] && Eio.Exn.is_io ex -> attempt rest)
    in
    (attempt (Eio.Net.getaddrinfo_stream net host ~service:(string_of_int port))
      :> raw)
  in
  let peer_name, ip =
    if tls = `Plain then None, None
    else match Ipaddr.of_string host with
      | Ok ip -> None, Some ip
      | Error _ ->
          Some (Domain_name.host_exn (Domain_name.of_string_exn host)), None
  in
  let tls_config = if tls = `Plain then None else (
    let authenticator = match authenticator with
    | Some a -> a
    | None -> (match Ca_certs.system_authenticator () with
        | Ok a -> a | Error (`Msg m) -> failwith m) in
    match Tls.Config.client_no_cert ~authenticator ?peer_name ?ip () with
    | Ok c -> Some c | Error (`Msg m) -> invalid_arg m) in
  { dial; host; port; tls; tls_config; peer_name; ip }

let host t = t.host
let port t = t.port
let tls t = t.tls

type flow = {
  mutable raw : raw;
  mutable deflate : Deflate_flow.t option;
  mutable closed : bool;
}

let secure (t : t @ nonportable) raw =
  let config = match t.tls_config with Some c -> c | None -> invalid_arg "plain transport" in
  let g = Mirage_crypto_rng_unix.fresh_generator () in
  Tls_eio.client_of_flow_with_rng ~g config ?host:t.peer_name ?ip:t.ip raw

let of_flow raw = { raw = (raw :> raw); deflate = None; closed = false }

let connect ~sw (t : t @ nonportable) =
  let raw = t.dial ~sw in
  try
    if t.tls = `Implicit then of_flow (secure t raw) else of_flow raw
  with e ->
    let bt=Printexc.get_raw_backtrace () in
    (try Eio.Cancel.protect (fun () -> Eio.Resource.close raw) with _ -> ());
    Eio.Exn.reraise_with_context e bt "IMAP TLS handshake with %s" t.host

let check_open f = if f.closed then invalid_arg "closed IMAP transport"
let read f b =
  check_open f;
  match f.deflate with
  | None -> Eio.Flow.single_read f.raw b
  | Some codec -> Deflate_flow.read codec b
let write f bs =
  check_open f;
  match f.deflate with
  | None -> Eio.Flow.write f.raw bs
  | Some codec -> Deflate_flow.write codec bs
let close f =
  if not f.closed then (
    f.closed<-true;
    Eio.Cancel.protect (fun () -> match f.deflate with
      | None -> Eio.Resource.close f.raw
      | Some codec -> Deflate_flow.close codec))
let compressed f = Option.is_some f.deflate
let compress_deflate f =
  check_open f;
  if compressed f then invalid_arg "DEFLATE is already active";
  f.deflate <- Some (Deflate_flow.create f.raw)

(* STARTTLS keeps ownership of the original resource: the TLS flow closes
   it, and a failed handshake closes it here. *)
let upgrade (t : t @ nonportable) f =
  check_open f;
  if compressed f then invalid_arg "STARTTLS after COMPRESS is forbidden";
  try f.raw <- (secure t f.raw :> raw)
  with ex ->
    let bt=Printexc.get_raw_backtrace () in
    (try close f with _ -> ());
    Eio.Exn.reraise_with_context ex bt "IMAP STARTTLS with %s" t.host
