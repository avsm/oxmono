(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Configuration for connection pools *)

let src = Logs.Src.create "conpool.config" ~doc:"Connection pool configuration"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  max_connections_per_endpoint : int;
  max_idle_time : float;
  max_connection_lifetime : float;
  max_connection_uses : int option;
  health_check :
    ([Eio.Resource.close_ty | Eio.Flow.two_way_ty] Eio.Resource.t -> bool) option;
  connect_timeout : float option;
  connect_retry_count : int;
  connect_retry_delay : float;
  on_connection_created : (Endpoint.t -> unit) option;
  on_connection_closed : (Endpoint.t -> unit) option;
  on_connection_reused : (Endpoint.t -> unit) option;
}

let make ?(max_connections_per_endpoint = 10) ?(max_idle_time = 60.0)
    ?(max_connection_lifetime = 300.0) ?max_connection_uses ?health_check
    ?(connect_timeout = 10.0) ?(connect_retry_count = 3)
    ?(connect_retry_delay = 0.1) ?on_connection_created ?on_connection_closed
    ?on_connection_reused () =
  (* Validate parameters *)
  if max_connections_per_endpoint <= 0 then
    invalid_arg
      (Printf.sprintf "max_connections_per_endpoint must be positive, got %d"
         max_connections_per_endpoint);

  if max_idle_time <= 0.0 then
    invalid_arg
      (Printf.sprintf "max_idle_time must be positive, got %.2f" max_idle_time);

  if max_connection_lifetime <= 0.0 then
    invalid_arg
      (Printf.sprintf "max_connection_lifetime must be positive, got %.2f"
         max_connection_lifetime);

  (match max_connection_uses with
  | Some n when n <= 0 ->
      invalid_arg
        (Printf.sprintf "max_connection_uses must be positive, got %d" n)
  | _ -> ());

  if connect_timeout <= 0.0 then
    invalid_arg
      (Printf.sprintf "connect_timeout must be positive, got %.2f"
         connect_timeout);

  if connect_retry_count < 0 then
    invalid_arg
      (Printf.sprintf "connect_retry_count must be non-negative, got %d"
         connect_retry_count);

  if connect_retry_delay <= 0.0 then
    invalid_arg
      (Printf.sprintf "connect_retry_delay must be positive, got %.2f"
         connect_retry_delay);

  {
    max_connections_per_endpoint;
    max_idle_time;
    max_connection_lifetime;
    max_connection_uses;
    health_check;
    connect_timeout = Some connect_timeout;
    connect_retry_count;
    connect_retry_delay;
    on_connection_created;
    on_connection_closed;
    on_connection_reused;
  }

let default = make ()
let max_connections_per_endpoint t = t.max_connections_per_endpoint
let max_idle_time t = t.max_idle_time
let max_connection_lifetime t = t.max_connection_lifetime
let max_connection_uses t = t.max_connection_uses
let health_check t = t.health_check
let connect_timeout t = t.connect_timeout
let connect_retry_count t = t.connect_retry_count
let connect_retry_delay t = t.connect_retry_delay
let on_connection_created t = t.on_connection_created
let on_connection_closed t = t.on_connection_closed
let on_connection_reused t = t.on_connection_reused

let pp ppf t =
  Fmt.pf ppf
    "@[<v>Config:@,\
     - max_connections_per_endpoint: %d@,\
     - max_idle_time: %.1fs@,\
     - max_connection_lifetime: %.1fs@,\
     - max_connection_uses: %s@,\
     - connect_timeout: %s@,\
     - connect_retry_count: %d@,\
     - connect_retry_delay: %.2fs@]"
    t.max_connections_per_endpoint t.max_idle_time t.max_connection_lifetime
    (match t.max_connection_uses with
    | Some n -> string_of_int n
    | None -> "unlimited")
    (match t.connect_timeout with
    | Some f -> Fmt.str "%.1fs" f
    | None -> "none")
    t.connect_retry_count t.connect_retry_delay

(** {1 Protocol Handler Configuration}

    Protocol handlers define protocol-specific behavior for connection pools.
    This enables different pooling strategies for different protocols
    (e.g., exclusive for HTTP/1.x, shared for HTTP/2). *)

(** Access mode for connections.
    - [Exclusive] - Each connection is used by one request at a time (HTTP/1.x)
    - [Shared] - Multiple requests can share a connection (HTTP/2) *)
type access_mode =
  | Exclusive
      (** Exclusive access - one request per connection at a time *)
  | Shared of int
      (** Shared access - up to n concurrent requests per connection *)

(** Connection type alias for protocol config *)
type connection_flow = [Eio.Resource.close_ty | Eio.Flow.two_way_ty] Eio.Resource.t

(** Protocol configuration for typed connection pools.
    @param 'state The protocol-specific state type (e.g., H2_client.t for HTTP/2) *)
type 'state protocol_config = {
  init_state :
    sw:Eio.Switch.t ->
    flow:connection_flow ->
    tls_epoch:Tls.Core.epoch_data option ->
    'state;
      (** Initialize protocol state when a new connection is created.
          The [sw] parameter is a connection-lifetime switch that can be used
          to spawn long-running fibers (e.g., HTTP/2 frame reader).
          For HTTP/2, this performs the handshake and returns the H2_client.t. *)

  on_acquire : 'state -> unit;
      (** Called when a connection is acquired from the pool.
          For HTTP/2, this can start the background reader fiber if not already running. *)

  on_release : 'state -> unit;
      (** Called when a connection is released back to the pool.
          For HTTP/2, this is typically a no-op since the reader keeps running. *)

  is_healthy : 'state -> bool;
      (** Protocol-specific health check. Return false if connection should be closed.
          For HTTP/2, checks if GOAWAY has been received. *)

  on_close : 'state -> unit;
      (** Cleanup callback when connection is destroyed.
          For HTTP/2, can send GOAWAY frame. *)

  access_mode : 'state -> access_mode;
      (** Get the access mode for this connection.
          For HTTP/2, returns [Shared n] with max_concurrent from peer settings. *)
}
