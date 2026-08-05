(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Conpool - Protocol-aware TCP/IP connection pooling library for Eio

    Conpool provides efficient connection pooling with support for both
    exclusive (HTTP/1.x) and shared (HTTP/2) access modes. All connections
    carry protocol-specific state managed through callbacks.

    {2 Quick Start}

    For simple exclusive-access protocols (HTTP/1.x, Redis, etc.):
    {[
      let pool = Conpool.create_basic ~sw ~net ~clock ~tls () in
      Eio.Switch.run (fun conn_sw ->
        let conn = Conpool.connection ~sw:conn_sw pool endpoint in
        (* Use conn.flow for I/O *)
        Eio.Flow.copy_string "GET / HTTP/1.1\r\n\r\n" conn.flow)
    ]}

    For multiplexed protocols (HTTP/2):
    {[
      let pool = Conpool.create ~sw ~net ~clock ~tls ~protocol:h2_handler () in
      Eio.Switch.run (fun conn_sw ->
        let conn = Conpool.connection ~sw:conn_sw pool endpoint in
        (* conn.state has H2_client.t, multiple streams share the connection *)
        H2_client.request conn.flow conn.state ...)
    ]} *)

(** {1 Logging} *)

val src : Logs.Src.t
(** Logs source for the connection pool. Configure logging with:
    {[
      Logs.Src.set_level Conpool.src (Some Logs.Debug);
      Logs.set_reporter (Logs_fmt.reporter ())
    ]} *)

(** {1 Core Types} *)

module Endpoint = Endpoint
(** Network endpoint representation *)

module Config = Config
(** Configuration for connection pools *)

module Stats = Stats
(** Statistics for connection pool endpoints *)

module Cmd = Cmd
(** Cmdliner terms for connection pool configuration *)

(** {1 Errors} *)

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

type Eio.Exn.err += E of error

val err : error -> exn
(** [err e] creates an Eio exception from a connection pool error. *)

val pp_error : error Fmt.t
(** Pretty-printer for error values. *)

(** {1 Connection Types} *)

type connection_ty = [Eio.Resource.close_ty | Eio.Flow.two_way_ty]
(** Type tags for a pooled connection. *)

type connection = connection_ty Eio.Resource.t
(** A connection resource from the pool. *)

(** {1 Connection Pool}

    All pools are typed - they carry protocol-specific state with each
    connection. For simple exclusive-access protocols, use the default
    [unit] state which requires no protocol handler. *)

type 'state t
(** Connection pool with protocol-specific state ['state].

    - For HTTP/1.x: use [unit t] with exclusive access (one request per connection)
    - For HTTP/2: use [h2_state t] with shared access (multiple streams per connection) *)

(** Connection with protocol-specific state. *)
type 'state connection_info = {
  flow : connection;
      (** The underlying connection flow for I/O. *)
  tls_epoch : Tls.Core.epoch_data option;
      (** TLS epoch data if connection uses TLS. *)
  state : 'state;
      (** Protocol-specific state (e.g., H2_client.t for HTTP/2). *)
}

(** {2 Pool Creation} *)

val default_protocol : unit Config.protocol_config
(** Default protocol configuration for simple exclusive-access protocols.
    Use with {!create} for HTTP/1.x, Redis, and similar protocols where
    each connection handles one request at a time with no extra state. *)

val create :
  sw:Eio.Switch.t ->
  net:'net Eio.Net.t ->
  clock:'clock Eio.Time.clock ->
  ?tls:Tls.Config.client ->
  ?config:Config.t ->
  protocol:'state Config.protocol_config ->
  unit ->
  'state t
(** Create a connection pool with a protocol handler.

    @param sw Switch for resource management
    @param net Network interface for creating connections
    @param clock Clock for timeouts
    @param tls Optional TLS client configuration
    @param config Pool configuration (uses {!Config.default} if not provided)
    @param protocol Protocol handler for state management

    Examples:

    Simple pool for HTTP/1.x (exclusive access, no state):
    {[
      let pool = Conpool.create ~sw ~net ~clock ~tls
        ~protocol:Conpool.default_protocol ()
    ]}

    HTTP/2 pool (shared access with H2 state):
    {[
      let pool = Conpool.create ~sw ~net ~clock ~tls ~protocol:h2_handler ()
    ]} *)

val create_basic :
  sw:Eio.Switch.t ->
  net:'net Eio.Net.t ->
  clock:'clock Eio.Time.clock ->
  ?tls:Tls.Config.client ->
  ?config:Config.t ->
  unit ->
  unit t
(** Create a basic connection pool with no protocol state.

    This is a convenience function equivalent to:
    {[
      Conpool.create ~sw ~net ~clock ?tls ?config
        ~protocol:Conpool.default_protocol ()
    ]}

    Use for simple exclusive-access protocols like HTTP/1.x and Redis.

    Example:
    {[
      let pool = Conpool.create_basic ~sw ~net ~clock ~tls ()
    ]} *)

(** {2 Connection Acquisition} *)

val connection : sw:Eio.Switch.t -> 'state t -> Endpoint.t -> 'state connection_info
(** [connection ~sw pool endpoint] acquires a connection from the pool.

    The connection is automatically released when [sw] finishes:
    - Exclusive mode: connection returns to idle pool
    - Shared mode: user count is decremented

    Behavior depends on access mode:
    - Exclusive: blocks until a connection is available
    - Shared: may share an existing connection if under max_concurrent limit

    Example:
    {[
      Eio.Switch.run (fun sw ->
        let conn = Conpool.connection ~sw pool endpoint in
        (* For HTTP/1.x: conn.state is () *)
        (* For HTTP/2: conn.state is H2_client.t *)
        Eio.Flow.copy_string data conn.flow)
    ]} *)

val with_connection : 'state t -> Endpoint.t -> ('state connection_info -> 'a) -> 'a
(** [with_connection pool endpoint fn] is a convenience wrapper.

    Equivalent to:
    {[
      Eio.Switch.run (fun sw -> fn (connection ~sw pool endpoint))
    ]} *)

(** {1 Statistics & Management} *)

val stats : 'state t -> Endpoint.t -> Stats.t
(** Get statistics for specific endpoint. *)

val all_stats : 'state t -> (Endpoint.t * Stats.t) list
(** Get statistics for all endpoints in pool. *)

val clear_endpoint : 'state t -> Endpoint.t -> unit
(** Clear all connections for an endpoint. *)
