open Eio.Std

module Flow = Eio.Flow

exception Tls_alert of Tls.Packet.alert_type @@ portable
exception Tls_failure of string @@ portable

type Eio.Exn.Backend.t += Tls_socket_closed
let () = Eio.Exn.Backend.register_pp (fun f -> function
    | Tls_socket_closed -> Fmt.pf f "TLS_socket_closed"; true
    | _ -> false
  )

type ty = [ `Tls | Eio.Flow.two_way_ty | Eio.Resource.close_ty ]
type t = ty r

module Raw = struct

  (* We could replace [`Eof] with [`Error End_of_file] and then use
     a regular [result] type here. *)
  type t = {
    flow           : [Flow.two_way_ty | Eio.Resource.close_ty] r;
    client_rng     : Mirage_crypto_rng.portable_g option;
    mutable state  : [ `Active of Tls.Engine.state
                     | `Read_closed of Tls.Engine.state
                     | `Write_closed of Tls.Engine.state
                     | `Closed
                     | `Error of exn ] ;
    mutable linger : Cstruct.t option ;
    recv_buf       : Cstruct.t ;
  }

  let half_close state mode =
    match state, mode with
    | `Active tls, `read -> `Read_closed tls
    | `Active tls, `write -> `Write_closed tls
    | `Active _, `read_write -> `Closed
    | `Read_closed tls, `read -> `Read_closed tls
    | `Read_closed _, (`write | `read_write) -> `Closed
    | `Write_closed tls, `write -> `Write_closed tls
    | `Write_closed _, (`read | `read_write) -> `Closed
    | (`Closed | `Error _) as e, (`read | `write | `read_write) -> e

  let inject_state tls = function
    | `Active _ -> `Active tls
    | `Read_closed _ -> `Read_closed tls
    | `Write_closed _ -> `Write_closed tls
    | (`Closed | `Error _) as e -> e

  let (write_t @ portable) t s =
    try Flow.write t.flow (stack_ [ Cstruct.of_string_local s ])
    with exn ->
      (match t.state with
       | `Error _ -> ()
       | _ -> t.state <- `Error exn) ;
      raise exn

  let (try_write_t @ portable) t cs =
    try write_t t cs
    with _ -> ()                       (* Error is in [t.state] *)

  let rec (read_react_with @ portable) handle_tls t =

    let handle tls buf =
      match handle_tls tls buf with
      | Ok (state', eof, `Response resp, `Data data) ->
          let state' = inject_state state' t.state in
          let state' = Option.(value ~default:state' (map (fun `Eof -> half_close state' `read) eof)) in
          t.state <- state' ;
          Option.iter (try_write_t t) resp;
          Option.map Cstruct.of_string data

      | Error (fail, `Response resp) ->
          t.state <-
            `Error
              (match fail with
              | `Alert a -> Tls_alert a
              | `Error _ -> Tls_failure "TLS configuration or authentication failure"
              | `Fatal _ -> Tls_failure "TLS protocol failure"
              | `Alert _ -> Tls_failure "TLS alert") ;
          write_t t resp; read_react_with handle_tls t
    in

    match t.state with
    | `Error e  -> raise e
    | `Closed
    | `Read_closed _ -> raise End_of_file
    | _ ->
        match Flow.single_read t.flow t.recv_buf with
          | exception End_of_file ->
            t.state <- half_close t.state `read;
            raise End_of_file
          | exception exn ->
            (match t.state with
             | `Error _ -> ()
             | _ -> t.state <- `Error exn) ;
            raise exn
          | n ->
            match t.state with
            | `Error e -> raise e
            | `Active tls | `Read_closed tls | `Write_closed tls ->
              handle tls (Cstruct.to_string t.recv_buf ~off:0 ~len:n)
            | `Closed -> raise End_of_file

  let read_react t = read_react_with Tls.Engine.handle_tls t

  let (read_react_client @ portable) ~g t =
    read_react_with (Tls.Engine.handle_tls_client ~g) t

  let rec (single_read_with @ portable) read_react t (buf @ local) =
    let writeout t (buf @ local) res =
      let open Cstruct in
      let rlen = length res in
      let n    = min (length buf) rlen in
      blit res 0 buf 0 n ;
      t.linger <-
        (if n < rlen then Some (sub res n (rlen - n)) else None) ;
      n in

    match t.linger with
    | Some res -> writeout t buf res
    | None     ->
        match read_react t with
          | None     -> single_read_with read_react t buf
          | Some res -> writeout t buf res

  let single_read t buf = single_read_with read_react t buf

  let (client_generator @ portable) t =
    match t.client_rng with
    | Some generator -> generator
    | None -> invalid_arg "TLS flow has no portable client generator"

  let (single_read_client @ portable) t buf =
    let generator = client_generator t in
    single_read_with (read_react_client ~g:generator) t buf

  let (writev_with @ portable) send_application_data t (css @ local) =
    match t.state with
    | `Error err  -> raise err
    | `Write_closed _ | `Closed -> raise (Tls_failure "TLS socket is closed")
    | `Active tls | `Read_closed tls ->
        let rec to_strings (css @ local) =
          match css with
          | [] -> []
          | cs :: rest -> Cstruct.to_string cs :: to_strings rest
        in
        let css = to_strings css in
        match send_application_data tls css with
        | Some (tls, tlsdata) ->
            ( t.state <- inject_state tls t.state ; write_t t tlsdata )
        | None -> invalid_arg "tls: write: socket not ready"

  let writev t css = writev_with Tls.Engine.send_application_data t css

  let (writev_client @ portable) t css =
    writev_with Tls.Engine.send_application_data_client t css

  let single_write t (bufs @ local) =
    writev t bufs;
    Cstruct.lenv bufs

  let (single_write_client @ portable) t (bufs @ local) =
    writev_client t bufs;
    Cstruct.lenv bufs

  (*
   * XXX bad XXX
   * This is a point that should particularly be protected from concurrent r/w.
   * Doing this before a `t` is returned is safe; redoing it during rekeying is
   * not, as the API client already sees the `t` and can mistakenly interleave
   * writes while this is in progress.
   * *)
  let rec (drain_handshake_with @ portable) read_react t =
    let push_linger t mcs =
      match (mcs, t.linger) with
      | (None, _)         -> ()
      | (scs, None)       -> t.linger <- scs
      | (Some cs, Some l) -> t.linger <- Some (Cstruct.append l cs)
    in
    match t.state with
    | `Active tls | `Read_closed tls | `Write_closed tls
      when not (Tls.Engine.handshake_in_progress tls) ->
        t
    | _ ->
        let cs = read_react t in
        push_linger t cs; drain_handshake_with read_react t

  let drain_handshake t = drain_handshake_with read_react t

  let (drain_handshake_client @ portable) ~g t =
    drain_handshake_with (read_react_client ~g) t

  let reneg ?authenticator ?acceptable_cas ?cert ?(drop = true) t =
    match t.state with
    | `Error err  -> raise err
    | `Closed | `Read_closed _ | `Write_closed _ -> invalid_arg "tls: closed socket"
    | `Active tls ->
        match Tls.Engine.reneg ?authenticator ?acceptable_cas ?cert tls with
        | None -> invalid_arg "tls: can't renegotiate"
        | Some (tls', buf) ->
           if drop then t.linger <- None ;
           t.state <- inject_state tls' t.state ;
           write_t t buf;
           ignore (drain_handshake t : t)

  let key_update ?request t =
    match t.state with
    | `Error err  -> raise err
    | `Write_closed _ | `Closed -> invalid_arg "tls: closed socket"
    | `Active tls | `Read_closed tls ->
      match Tls.Engine.key_update ?request tls with
      | Error f -> Fmt.invalid_arg "tls: can't update key: %a" Tls.Engine.pp_failure f
      | Ok (tls', buf) ->
        t.state <- inject_state tls' t.state ;
        write_t t buf

  let (shutdown_with @ portable) send_close_notify t = function
    | `Receive -> ()
    | `Send | `All ->
      match t.state with
      | `Active tls | `Read_closed tls ->
        let tls', buf = send_close_notify tls in
        t.state <- inject_state tls' (half_close t.state `write) ;
        write_t t buf
      | _ -> ()

  let shutdown t command =
    shutdown_with Tls.Engine.send_close_notify t command

  let (shutdown_client @ portable) t command =
    shutdown_with Tls.Engine.send_close_notify_client t command

  let server_of_flow config flow =
    drain_handshake {
      state    = `Active (Tls.Engine.server config) ;
      flow     = (flow :> [Flow.two_way_ty | Eio.Resource.close_ty] r) ;
      client_rng = None;
      linger   = None ;
      recv_buf = Cstruct.create 4096
    }

  let (client_of_flow_with @ portable) make_client drain_handshake
      ?client_rng config ?host ?ip flow =
    let config =
      match host with
      | None -> config
      | Some host -> Tls.Config.peer config host
    in
    let config' =
      match ip with
      | None -> config
      | Some ip -> Tls.Config.ip config ip
    in
    let (tls, init) = make_client config' in
    let t = {
      state    = `Active tls ;
      flow     = (flow :> [Flow.two_way_ty | Eio.Resource.close_ty] r);
      client_rng;
      linger   = None ;
      recv_buf = Cstruct.create 4096
    } in
    write_t t init;
    drain_handshake t

  let client_of_flow config ?host ?ip flow =
    client_of_flow_with Tls.Engine.client drain_handshake config ?host ?ip flow

  let (client_of_flow_with_rng @ portable) ~g config ?host ?ip flow =
    let make_client config = Tls.Engine.client_no_resumption_with_rng ~g config in
    client_of_flow_with make_client (drain_handshake_client ~g)
      ~client_rng:g config ?host ?ip flow


  let epoch t =
    match t.state with
    | `Active tls | `Read_closed tls | `Write_closed tls -> Tls.Engine.epoch tls
    | `Closed | `Error _ -> Error ()

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let (copy_client @ portable) t ~src =
    Eio.Flow.Pi.simple_copy ~single_write:single_write_client t ~src

  let read_methods = []

  let (close @ portable) t = Eio.Resource.close t.flow

  type (_, _, _) Eio.Resource.pi += T : ('t, 't -> t, ty) Eio.Resource.pi
end

let raw (Eio.Resource.T (t, ops)) = Eio.Resource.get ops Raw.T t

let of_t t =
  let handler =
    Eio.Resource.handler [
      H (Eio.Flow.Pi.Source, (module Raw));
      H (Eio.Flow.Pi.Sink, (module Raw));
      H (Eio.Flow.Pi.Shutdown, (module Raw));
      H (Eio.Resource.Close, Raw.close);
      H (Raw.T, Fun.id);
    ]
  in
  Eio.Resource.T (t, handler)

let (of_client_t @ portable) t =
  let module Client = struct
    type nonrec t = Raw.t

    let read_methods = []
    let single_read = Raw.single_read_client
    let single_write = Raw.single_write_client
    let copy = Raw.copy_client
    let shutdown = Raw.shutdown_client
  end in
  let identity value = value in
  let handler =
    Eio.Resource.handler [
      H (Eio.Flow.Pi.Source, (module Client));
      H (Eio.Flow.Pi.Sink, (module Client));
      H (Eio.Flow.Pi.Shutdown, (module Client));
      H (Eio.Resource.Close, Raw.close);
      H (Raw.T, identity);
    ]
  in
  Eio.Resource.T (t, handler)

let server_of_flow config flow =
  Raw.server_of_flow config flow |> of_t

let client_of_flow config ?host ?ip flow =
  Raw.client_of_flow config ?host ?ip flow |> of_t

let (client_of_flow_with_rng @ portable) ~g config ?host ?ip flow =
  Raw.client_of_flow_with_rng ~g config ?host ?ip flow |> of_client_t

let reneg ?authenticator ?acceptable_cas ?cert ?drop (t:t) = Raw.reneg ?authenticator ?acceptable_cas ?cert ?drop (raw t)
let key_update ?request (t:t) = Raw.key_update ?request (raw t)
let epoch (t:t) = Raw.epoch (raw t)

let () =
  Printexc.register_printer (function
      | Tls_alert typ ->
        Some ("TLS alert from peer: " ^ Tls.Packet.alert_type_to_string typ)
      | Tls_failure f ->
        Some ("TLS failure: " ^ f)
      | _ -> None)
