(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Configuration for connection pools *)

(** {1 Logging} *)

val src : Logs.Src.t
(** Logs source for configuration operations. Configure logging with:
    {[
      Logs.Src.set_level Conpool.Config.src (Some Logs.Debug)
    ]} *)

(** {1 Type} *)

type t
(** Pool configuration *)

(** {1 Construction} *)

val make :
  ?max_connections_per_endpoint:int ->
  ?max_idle_time:float ->
  ?max_connection_lifetime:float ->
  ?max_connection_uses:int ->
  ?health_check:([Eio.Resource.close_ty | Eio.Flow.two_way_ty] Eio.Resource.t -> bool) ->
  ?connect_timeout:float ->
  ?connect_retry_count:int ->
  ?connect_retry_delay:float ->
  ?on_connection_created:(Endpoint.t -> unit) ->
  ?on_connection_closed:(Endpoint.t -> unit) ->
  ?on_connection_reused:(Endpoint.t -> unit) ->
  unit ->
  t
(** Create pool configuration with optional parameters.

    @param max_connections_per_endpoint
      Maximum concurrent connections per endpoint (default: 10)
    @param max_idle_time
      Maximum time a connection can sit idle in seconds (default: 60.0)
    @param max_connection_lifetime
      Maximum connection age in seconds (default: 300.0)
    @param max_connection_uses
      Maximum times a connection can be reused (default: unlimited)
    @param health_check Custom health check function (default: none)
    @param connect_timeout Connection timeout in seconds (default: 10.0)
    @param connect_retry_count Number of connection retry attempts (default: 3)
    @param connect_retry_delay
      Initial retry delay in seconds, with exponential backoff (default: 0.1)
    @param on_connection_created Hook called when a connection is created
    @param on_connection_closed Hook called when a connection is closed
    @param on_connection_reused Hook called when a connection is reused *)

val default : t
(** Sensible defaults for most use cases:
    - max_connections_per_endpoint: 10
    - max_idle_time: 60.0s
    - max_connection_lifetime: 300.0s
    - max_connection_uses: unlimited
    - health_check: none
    - connect_timeout: 10.0s
    - connect_retry_count: 3
    - connect_retry_delay: 0.1s
    - hooks: none *)

(** {1 Accessors} *)

val max_connections_per_endpoint : t -> int
(** Get maximum connections per endpoint. *)

val max_idle_time : t -> float
(** Get maximum idle time in seconds. *)

val max_connection_lifetime : t -> float
(** Get maximum connection lifetime in seconds. *)

val max_connection_uses : t -> int option
(** Get maximum connection uses, if any. *)

val health_check :
  t -> ([Eio.Resource.close_ty | Eio.Flow.two_way_ty] Eio.Resource.t -> bool) option
(** Get custom health check function, if any. *)

val connect_timeout : t -> float option
(** Get connection timeout in seconds, if any. *)

val connect_retry_count : t -> int
(** Get number of connection retry attempts. *)

val connect_retry_delay : t -> float
(** Get initial retry delay in seconds. *)

val on_connection_created : t -> (Endpoint.t -> unit) option
(** Get connection created hook, if any. *)

val on_connection_closed : t -> (Endpoint.t -> unit) option
(** Get connection closed hook, if any. *)

val on_connection_reused : t -> (Endpoint.t -> unit) option
(** Get connection reused hook, if any. *)

(** {1 Pretty-printing} *)

val pp : t Fmt.t
(** Pretty-printer for configuration. *)

(** {1 Protocol Handler Configuration}

    Protocol handlers define protocol-specific behavior for typed connection pools.
    This enables different pooling strategies for different protocols
    (e.g., exclusive for HTTP/1.x, shared for HTTP/2). *)

(** Access mode for connections.
    - [Exclusive] - Each connection is used by one request at a time (HTTP/1.x)
    - [Shared n] - Up to n concurrent requests can share a connection (HTTP/2) *)
type access_mode =
  | Exclusive
      (** Exclusive access - one request per connection at a time *)
  | Shared of int
      (** Shared access - up to n concurrent requests per connection *)

(** Connection flow type for protocol handlers. *)
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
