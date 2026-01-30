(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Conpool - Protocol-aware TCP/IP connection pooling library for Eio *)

let src = Logs.Src.create "conpool" ~doc:"Connection pooling library"

module Log = (val Logs.src_log src : Logs.LOG)

(* Re-export submodules *)
module Endpoint = Endpoint
module Config = Config
module Stats = Stats
module Cmd = Cmd

(* Track whether TLS tracing has been suppressed *)
let tls_tracing_suppressed = ref false

(* Suppress TLS tracing debug output (hexdumps) unless explicitly enabled *)
let suppress_tls_tracing () =
  if not !tls_tracing_suppressed then begin
    tls_tracing_suppressed := true;
    match List.find_opt (fun s -> Logs.Src.name s = "tls.tracing") (Logs.Src.list ()) with
    | Some tls_src ->
        (match Logs.Src.level tls_src with
         | Some Logs.Debug -> Logs.Src.set_level tls_src (Some Logs.Warning)
         | _ -> ())
    | None -> ()
  end

(** {1 Error Types} *)

type error =
  | Dns_resolution_failed of { hostname : string }
  | Connection_failed of {
      endpoint : Endpoint.t;
      attempts : int;
      last_error : string;
    }
  | Connection_timeout of { endpoint : Endpoint.t; timeout : float }
  | Invalid_config of string
  | Invalid_endpoint of string

let pp_error ppf = function
  | Dns_resolution_failed { hostname } ->
      Fmt.pf ppf "DNS resolution failed for hostname: %s" hostname
  | Connection_failed { endpoint; attempts; last_error } ->
      Fmt.pf ppf "Failed to connect to %a after %d attempts: %s" Endpoint.pp
        endpoint attempts last_error
  | Connection_timeout { endpoint; timeout } ->
      Fmt.pf ppf "Connection timeout to %a after %.2fs" Endpoint.pp endpoint
        timeout
  | Invalid_config msg -> Fmt.pf ppf "Invalid configuration: %s" msg
  | Invalid_endpoint msg -> Fmt.pf ppf "Invalid endpoint: %s" msg

type Eio.Exn.err += E of error

let err e = Eio.Exn.create (E e)

let () =
  Eio.Exn.register_pp (fun f -> function
      | E e ->
          Fmt.string f "Conpool ";
          pp_error f e;
          true
      | _ -> false)

(** {1 Connection Types} *)

type connection_ty = [Eio.Resource.close_ty | Eio.Flow.two_way_ty]
type connection = connection_ty Eio.Resource.t

(** {1 Internal Types} *)

(** Internal connection wrapper with protocol state and tracking. *)
type 'state pooled_connection = {
  pc_flow : connection;
  pc_tls_flow : Tls_eio.t option;
  pc_state : 'state;
  pc_created_at : float;
  mutable pc_last_used : float;
      (** Last time this connection was used (for idle timeout). *)
  mutable pc_use_count : int;
      (** Number of times this connection has been used. *)
  pc_endpoint : Endpoint.t;
  mutable pc_active_users : int;
  pc_user_available : Eio.Condition.t;
  mutable pc_closed : bool;
  pc_connection_cancel : exn -> unit;
      (** Cancels the connection-lifetime switch, stopping any protocol fibers. *)
}

(** Statistics for an endpoint. *)
type endp_stats = {
  mutable active : int;
  mutable idle : int;
      (** Number of idle connections (active_users = 0). *)
  mutable total_created : int;
  mutable total_reused : int;
  mutable total_closed : int;
  mutable errors : int;
      (** Number of connection errors encountered. *)
}

(** Endpoint pool storing connections. *)
type 'state endpoint_pool = {
  connections : 'state pooled_connection list ref;
  ep_mutex : Eio.Mutex.t;
  stats : endp_stats;
  stats_mutex : Eio.Mutex.t;
}

(** Internal pool representation. *)
type ('state, 'clock, 'net) internal = {
  sw : Eio.Switch.t;
  net : 'net;
  clock : 'clock;
  config : Config.t;
  tls : Tls.Config.client option;
  protocol : 'state Config.protocol_config;
  endpoints : (Endpoint.t, 'state endpoint_pool) Hashtbl.t;
  endpoints_mutex : Eio.Mutex.t;
}

(** {1 Public Types} *)

type 'state t =
  Pool : ('state, 'clock Eio.Time.clock, 'net Eio.Net.t) internal -> 'state t

type 'state connection_info = {
  flow : connection;
  tls_epoch : Tls.Core.epoch_data option;
  state : 'state;
}

(** {1 Default Protocol Handler}

    For simple exclusive-access protocols (HTTP/1.x, Redis, etc.),
    use unit state with no special initialization. *)

let default_protocol : unit Config.protocol_config = {
  Config.init_state = (fun ~sw:_ ~flow:_ ~tls_epoch:_ -> ());
  on_acquire = (fun () -> ());
  on_release = (fun () -> ());
  is_healthy = (fun () -> true);
  on_close = (fun () -> ());
  access_mode = (fun () -> Config.Exclusive);
}

(** {1 Helper Functions} *)

let get_time pool = Eio.Time.now pool.clock

let create_endp_stats () = {
  active = 0;
  idle = 0;
  total_created = 0;
  total_reused = 0;
  total_closed = 0;
  errors = 0;
}

let snapshot_stats (stats : endp_stats) : Stats.t =
  Stats.make ~active:stats.active ~idle:stats.idle
    ~total_created:stats.total_created ~total_reused:stats.total_reused
    ~total_closed:stats.total_closed ~errors:stats.errors

(** {1 Connection Creation} *)

let create_connection pool endpoint =
  Log.debug (fun m -> m "Creating connection to %a" Endpoint.pp endpoint);

  (* DNS resolution *)
  let addr =
    try
      let addrs =
        Eio.Net.getaddrinfo_stream pool.net (Endpoint.host endpoint)
          ~service:(string_of_int (Endpoint.port endpoint))
      in
      match addrs with
      | addr :: _ -> addr
      | [] ->
          raise (err (Dns_resolution_failed { hostname = Endpoint.host endpoint }))
    with Eio.Io _ as ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt "resolving %a" Endpoint.pp endpoint
  in

  (* TCP connection with optional timeout *)
  let socket =
    try
      match Config.connect_timeout pool.config with
      | Some timeout ->
          Eio.Time.with_timeout_exn pool.clock timeout (fun () ->
              Eio.Net.connect ~sw:pool.sw pool.net addr)
      | None -> Eio.Net.connect ~sw:pool.sw pool.net addr
    with Eio.Io _ as ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt "connecting to %a" Endpoint.pp endpoint
  in

  Log.debug (fun m -> m "TCP connection established to %a" Endpoint.pp endpoint);

  (* Optional TLS handshake *)
  let flow, tls_flow =
    match pool.tls with
    | None ->
        ((socket :> connection), None)
    | Some tls_config ->
        try
          Log.debug (fun m ->
              m "Initiating TLS handshake with %a" Endpoint.pp endpoint);
          let host =
            Domain_name.(host_exn (of_string_exn (Endpoint.host endpoint)))
          in
          let tls = Tls_eio.client_of_flow ~host tls_config socket in
          suppress_tls_tracing ();
          Log.info (fun m ->
              m "TLS connection established to %a" Endpoint.pp endpoint);
          ((tls :> connection), Some tls)
        with Eio.Io _ as ex ->
          let bt = Printexc.get_raw_backtrace () in
          Eio.Exn.reraise_with_context ex bt "TLS handshake with %a" Endpoint.pp endpoint
  in

  (* Get TLS epoch if available *)
  let tls_epoch =
    match tls_flow with
    | Some tls_flow -> (
        match Tls_eio.epoch tls_flow with
        | Ok epoch -> Some epoch
        | Error () -> None)
    | None -> None
  in

  (* Create connection-lifetime sub-switch via a fiber.
     This switch lives for the connection's lifetime and can be used
     by the protocol handler to spawn long-running fibers (e.g., HTTP/2 reader). *)
  let conn_sw_ref = ref None in
  let conn_cancel_ref = ref (fun (_ : exn) -> ()) in
  let ready_promise, ready_resolver = Eio.Promise.create () in

  (* Use fork_daemon so connection fibers don't prevent parent switch from completing.
     When the parent switch completes, all connection daemon fibers are cancelled,
     which triggers cleanup of their inner switches and connection resources. *)
  Eio.Fiber.fork_daemon ~sw:pool.sw (fun () ->
    (try
      Eio.Switch.run (fun conn_sw ->
        conn_sw_ref := Some conn_sw;
        conn_cancel_ref := (fun exn -> Eio.Switch.fail conn_sw exn);
        (* Signal that the switch is ready *)
        Eio.Promise.resolve ready_resolver ();
        (* Block until the switch is cancelled *)
        Eio.Fiber.await_cancel ()
      )
    with
    | Eio.Cancel.Cancelled _ -> ()
    | exn ->
        Log.warn (fun m -> m "Connection fiber caught exception: %s" (Printexc.to_string exn)));
    `Stop_daemon
  );

  (* Wait for the switch to be created *)
  Eio.Promise.await ready_promise;
  let conn_sw = Option.get !conn_sw_ref in
  let conn_cancel = !conn_cancel_ref in

  (* Initialize protocol-specific state with connection switch *)
  Log.debug (fun m -> m "Initializing protocol state for %a" Endpoint.pp endpoint);
  let state = pool.protocol.init_state ~sw:conn_sw ~flow ~tls_epoch in

  let now = get_time pool in

  Log.info (fun m -> m "Created connection to %a" Endpoint.pp endpoint);

  {
    pc_flow = flow;
    pc_tls_flow = tls_flow;
    pc_state = state;
    pc_created_at = now;
    pc_last_used = now;
    pc_use_count = 0;
    pc_endpoint = endpoint;
    pc_active_users = 0;
    pc_user_available = Eio.Condition.create ();
    pc_closed = false;
    pc_connection_cancel = conn_cancel;
  }

(** {1 Connection Health Checking} *)

(** Health check result distinguishing errors from normal lifecycle. *)
type health_status =
  | Healthy
  | Unhealthy_error of string
      (** Connection failed due to an error (protocol failure, etc.) *)
  | Unhealthy_lifecycle of string
      (** Connection should close due to normal lifecycle (timeout, max uses, etc.) *)

let check_health pool conn =
  if conn.pc_closed then
    Unhealthy_lifecycle "already closed"
  else
    (* Check protocol-specific health *)
    let protocol_healthy = pool.protocol.is_healthy conn.pc_state in
    if not protocol_healthy then begin
      Log.debug (fun m -> m "Connection unhealthy: protocol check failed");
      Unhealthy_error "protocol check failed"
    end else
      let now = get_time pool in
      (* Check connection age *)
      let age = now -. conn.pc_created_at in
      let max_lifetime = Config.max_connection_lifetime pool.config in
      if age > max_lifetime then begin
        Log.debug (fun m -> m "Connection unhealthy: exceeded max lifetime (%.1fs > %.1fs)"
          age max_lifetime);
        Unhealthy_lifecycle "exceeded max lifetime"
      end else
        (* Check idle time - only for idle connections *)
        let idle_time = now -. conn.pc_last_used in
        let max_idle = Config.max_idle_time pool.config in
        if conn.pc_active_users = 0 && idle_time > max_idle then begin
          Log.debug (fun m -> m "Connection unhealthy: exceeded max idle time (%.1fs > %.1fs)"
            idle_time max_idle);
          Unhealthy_lifecycle "exceeded max idle time"
        end else
          (* Check use count *)
          match Config.max_connection_uses pool.config with
          | Some max_uses when conn.pc_use_count >= max_uses ->
              Log.debug (fun m -> m "Connection unhealthy: exceeded max uses (%d >= %d)"
                conn.pc_use_count max_uses);
              Unhealthy_lifecycle "exceeded max uses"
          | _ ->
              Healthy

let is_healthy pool conn =
  match check_health pool conn with
  | Healthy -> true
  | Unhealthy_error _ | Unhealthy_lifecycle _ -> false

(** {1 Connection Cleanup} *)

let close_connection pool conn =
  if not conn.pc_closed then begin
    conn.pc_closed <- true;
    Log.debug (fun m ->
        m "Closing connection to %a" Endpoint.pp conn.pc_endpoint);

    (* Cancel connection-lifetime switch first - this stops any protocol fibers *)
    (try conn.pc_connection_cancel (Failure "Connection closed")
     with _ -> ());

    (* Call protocol cleanup *)
    pool.protocol.on_close conn.pc_state;

    (* Close the underlying flow *)
    Eio.Cancel.protect (fun () ->
        try Eio.Flow.close conn.pc_flow with _ -> ())
  end

(** {1 Endpoint Pool Management} *)

let get_or_create_endpoint_pool pool endpoint =
  match
    Eio.Mutex.use_ro pool.endpoints_mutex (fun () ->
        Hashtbl.find_opt pool.endpoints endpoint)
  with
  | Some ep_pool -> ep_pool
  | None ->
      Eio.Mutex.use_rw ~protect:true pool.endpoints_mutex (fun () ->
          match Hashtbl.find_opt pool.endpoints endpoint with
          | Some ep_pool -> ep_pool
          | None ->
              Log.info (fun m ->
                  m "Creating endpoint pool for %a" Endpoint.pp endpoint);
              let ep_pool = {
                connections = ref [];
                ep_mutex = Eio.Mutex.create ();
                stats = create_endp_stats ();
                stats_mutex = Eio.Mutex.create ();
              } in
              Hashtbl.add pool.endpoints endpoint ep_pool;
              ep_pool)

(** {1 Connection Acquisition} *)

let rec acquire_connection pool ep_pool endpoint =
  Eio.Mutex.use_rw ~protect:true ep_pool.ep_mutex (fun () ->
    (* Find an existing healthy connection with available capacity *)
    let rec find_available = function
      | [] -> None
      | conn :: rest ->
          if not (is_healthy pool conn) then begin
            conn.pc_closed <- true;
            find_available rest
          end else begin
            match pool.protocol.access_mode conn.pc_state with
            | Config.Exclusive ->
                if conn.pc_active_users = 0 then
                  Some conn
                else
                  find_available rest
            | Config.Shared max_concurrent ->
                if conn.pc_active_users < max_concurrent then
                  Some conn
                else
                  find_available rest
          end
    in

    (* Clean up closed connections *)
    ep_pool.connections := List.filter (fun c -> not c.pc_closed) !(ep_pool.connections);

    match find_available !(ep_pool.connections) with
    | Some conn ->
        (* Reuse existing connection *)
        let was_idle = conn.pc_active_users = 0 in
        conn.pc_active_users <- conn.pc_active_users + 1;
        conn.pc_last_used <- get_time pool;
        conn.pc_use_count <- conn.pc_use_count + 1;

        Eio.Mutex.use_rw ~protect:true ep_pool.stats_mutex (fun () ->
            ep_pool.stats.total_reused <- ep_pool.stats.total_reused + 1;
            ep_pool.stats.active <- ep_pool.stats.active + 1;
            (* Decrement idle count when connection becomes active *)
            if was_idle then
              ep_pool.stats.idle <- max 0 (ep_pool.stats.idle - 1));

        Log.debug (fun m ->
            m "Reusing connection to %a (users=%d)"
              Endpoint.pp endpoint conn.pc_active_users);

        (* Notify protocol handler of acquisition *)
        pool.protocol.on_acquire conn.pc_state;
        conn

    | None ->
        (* Need to create a new connection *)
        let max_conns = Config.max_connections_per_endpoint pool.config in
        let current_conns = List.length !(ep_pool.connections) in

        if current_conns >= max_conns then begin
          (* Wait for a connection to become available *)
          Log.debug (fun m ->
              m "At connection limit for %a (%d), waiting..."
                Endpoint.pp endpoint max_conns);

          (* Find a connection to wait on (prefer shared mode) *)
          let wait_conn = List.find_opt (fun c ->
            match pool.protocol.access_mode c.pc_state with
            | Config.Shared _ -> true
            | Config.Exclusive -> false
          ) !(ep_pool.connections) in

          match wait_conn with
          | Some conn ->
              (* Wait for user slot *)
              while conn.pc_active_users >=
                    (match pool.protocol.access_mode conn.pc_state with
                     | Config.Shared n -> n
                     | Config.Exclusive -> 1)
                    && not conn.pc_closed do
                Eio.Condition.await_no_mutex conn.pc_user_available
              done;
              if conn.pc_closed then
                acquire_connection pool ep_pool endpoint
              else begin
                conn.pc_active_users <- conn.pc_active_users + 1;
                conn.pc_last_used <- get_time pool;
                conn.pc_use_count <- conn.pc_use_count + 1;

                Eio.Mutex.use_rw ~protect:true ep_pool.stats_mutex (fun () ->
                    ep_pool.stats.total_reused <- ep_pool.stats.total_reused + 1;
                    ep_pool.stats.active <- ep_pool.stats.active + 1);

                (* Notify protocol handler of acquisition *)
                pool.protocol.on_acquire conn.pc_state;
                conn
              end
          | None ->
              (* All connections are exclusive and in use - wait for any *)
              let any_conn = List.hd !(ep_pool.connections) in
              while any_conn.pc_active_users > 0 && not any_conn.pc_closed do
                Eio.Condition.await_no_mutex any_conn.pc_user_available
              done;
              if any_conn.pc_closed then
                acquire_connection pool ep_pool endpoint
              else begin
                (* Connection was idle (active_users = 0), now becoming active *)
                Eio.Mutex.use_rw ~protect:true ep_pool.stats_mutex (fun () ->
                    ep_pool.stats.total_reused <- ep_pool.stats.total_reused + 1;
                    ep_pool.stats.active <- ep_pool.stats.active + 1;
                    ep_pool.stats.idle <- max 0 (ep_pool.stats.idle - 1));
                any_conn.pc_active_users <- 1;
                any_conn.pc_last_used <- get_time pool;
                any_conn.pc_use_count <- any_conn.pc_use_count + 1;
                (* Notify protocol handler of acquisition *)
                pool.protocol.on_acquire any_conn.pc_state;
                any_conn
              end
        end else begin
          (* Create new connection *)
          let conn = create_connection pool endpoint in
          conn.pc_active_users <- 1;
          ep_pool.connections := conn :: !(ep_pool.connections);

          Eio.Mutex.use_rw ~protect:true ep_pool.stats_mutex (fun () ->
              ep_pool.stats.total_created <- ep_pool.stats.total_created + 1;
              ep_pool.stats.active <- ep_pool.stats.active + 1);

          Log.info (fun m ->
              m "Created new connection to %a (total=%d)"
                Endpoint.pp endpoint (List.length !(ep_pool.connections)));

          (* Notify protocol handler of acquisition *)
          pool.protocol.on_acquire conn.pc_state;
          conn
        end)

(** {1 Connection Release} *)

let release_connection pool ep_pool conn =
  (* Notify protocol handler of release *)
  pool.protocol.on_release conn.pc_state;

  Eio.Mutex.use_rw ~protect:true ep_pool.ep_mutex (fun () ->
    let was_active = conn.pc_active_users > 0 in
    conn.pc_active_users <- max 0 (conn.pc_active_users - 1);
    let now_idle = conn.pc_active_users = 0 in

    Eio.Mutex.use_rw ~protect:true ep_pool.stats_mutex (fun () ->
        ep_pool.stats.active <- max 0 (ep_pool.stats.active - 1);
        (* Track idle count: increment when connection becomes idle *)
        if was_active && now_idle then
          ep_pool.stats.idle <- ep_pool.stats.idle + 1);

    (* Signal waiting fibers *)
    Eio.Condition.broadcast conn.pc_user_available;

    Log.debug (fun m ->
        m "Released connection to %a (users=%d)"
          Endpoint.pp conn.pc_endpoint conn.pc_active_users);

    (* Check if connection should be closed *)
    match check_health pool conn with
    | Healthy -> ()
    | Unhealthy_error reason ->
        conn.pc_closed <- true;

        Eio.Mutex.use_rw ~protect:true ep_pool.stats_mutex (fun () ->
            ep_pool.stats.total_closed <- ep_pool.stats.total_closed + 1;
            ep_pool.stats.errors <- ep_pool.stats.errors + 1;
            if now_idle then
              ep_pool.stats.idle <- max 0 (ep_pool.stats.idle - 1));

        Log.warn (fun m -> m "Closing connection due to error: %s" reason);
        close_connection pool conn;
        ep_pool.connections := List.filter (fun c -> c != conn) !(ep_pool.connections)

    | Unhealthy_lifecycle reason ->
        conn.pc_closed <- true;

        Eio.Mutex.use_rw ~protect:true ep_pool.stats_mutex (fun () ->
            ep_pool.stats.total_closed <- ep_pool.stats.total_closed + 1;
            if now_idle then
              ep_pool.stats.idle <- max 0 (ep_pool.stats.idle - 1));

        Log.debug (fun m -> m "Closing connection due to lifecycle: %s" reason);
        close_connection pool conn;
        ep_pool.connections := List.filter (fun c -> c != conn) !(ep_pool.connections))

(** {1 Public API} *)

let create ~sw ~(net : 'net Eio.Net.t) ~(clock : 'clock Eio.Time.clock)
    ?tls ?(config = Config.default) ~protocol () =

  Log.info (fun m ->
      m "Creating connection pool (max_per_endpoint=%d)"
        (Config.max_connections_per_endpoint config));

  let pool = {
    sw;
    net;
    clock;
    config;
    tls;
    protocol;
    endpoints = Hashtbl.create 16;
    endpoints_mutex = Eio.Mutex.create ();
  } in

  (* Auto-cleanup on switch release *)
  Eio.Switch.on_release sw (fun () ->
      Eio.Cancel.protect (fun () ->
          Log.info (fun m -> m "Closing connection pool");
          Hashtbl.iter (fun _endpoint ep_pool ->
            List.iter (fun conn ->
              close_connection pool conn
            ) !(ep_pool.connections)
          ) pool.endpoints;
          Hashtbl.clear pool.endpoints));

  Pool pool

let create_basic ~sw ~net ~clock ?tls ?config () =
  create ~sw ~net ~clock ?tls ?config ~protocol:default_protocol ()

let connection ~sw (Pool pool) endpoint =
  Log.debug (fun m -> m "Acquiring connection to %a" Endpoint.pp endpoint);

  let ep_pool = get_or_create_endpoint_pool pool endpoint in
  let conn = acquire_connection pool ep_pool endpoint in

  (* Release connection when switch ends *)
  Eio.Switch.on_release sw (fun () ->
      release_connection pool ep_pool conn);

  (* Get TLS epoch if available *)
  let tls_epoch =
    match conn.pc_tls_flow with
    | Some tls_flow -> (
        match Tls_eio.epoch tls_flow with
        | Ok epoch -> Some epoch
        | Error () -> None)
    | None -> None
  in

  {
    flow = conn.pc_flow;
    tls_epoch;
    state = conn.pc_state;
  }

let with_connection pool endpoint f =
  Eio.Switch.run (fun sw -> f (connection ~sw pool endpoint))

let stats (Pool pool) endpoint =
  match Hashtbl.find_opt pool.endpoints endpoint with
  | Some ep_pool ->
      Eio.Mutex.use_ro ep_pool.stats_mutex (fun () -> snapshot_stats ep_pool.stats)
  | None ->
      Stats.make ~active:0 ~idle:0 ~total_created:0 ~total_reused:0
        ~total_closed:0 ~errors:0

let all_stats (Pool pool) =
  Eio.Mutex.use_ro pool.endpoints_mutex (fun () ->
      Hashtbl.fold
        (fun endpoint ep_pool acc ->
          let stats =
            Eio.Mutex.use_ro ep_pool.stats_mutex (fun () ->
                snapshot_stats ep_pool.stats)
          in
          (endpoint, stats) :: acc)
        pool.endpoints [])

let clear_endpoint (Pool pool) endpoint =
  Log.info (fun m -> m "Clearing endpoint %a from pool" Endpoint.pp endpoint);
  match Hashtbl.find_opt pool.endpoints endpoint with
  | Some ep_pool ->
      Eio.Cancel.protect (fun () ->
          Eio.Mutex.use_rw ~protect:true ep_pool.ep_mutex (fun () ->
            List.iter (fun conn ->
              close_connection pool conn
            ) !(ep_pool.connections);
            ep_pool.connections := []);
          Eio.Mutex.use_rw ~protect:true pool.endpoints_mutex (fun () ->
              Hashtbl.remove pool.endpoints endpoint))
  | None ->
      Log.debug (fun m ->
          m "No endpoint pool found for %a" Endpoint.pp endpoint)
