(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Cmdliner terms for connection pool configuration *)

(** {1 Configuration Terms} *)

val max_connections_per_endpoint : int Cmdliner.Term.t
(** Cmdliner term for maximum connections per endpoint. Default: 10 Flag:
    [--max-connections-per-endpoint] *)

val max_idle_time : float Cmdliner.Term.t
(** Cmdliner term for maximum idle time in seconds. Default: 60.0 Flag:
    [--max-idle-time] *)

val max_connection_lifetime : float Cmdliner.Term.t
(** Cmdliner term for maximum connection lifetime in seconds. Default: 300.0
    Flag: [--max-connection-lifetime] *)

val max_connection_uses : int option Cmdliner.Term.t
(** Cmdliner term for maximum connection uses. Default: None (unlimited) Flag:
    [--max-connection-uses] *)

val connect_timeout : float Cmdliner.Term.t
(** Cmdliner term for connection timeout in seconds. Default: 10.0 Flag:
    [--connect-timeout] *)

val connect_retry_count : int Cmdliner.Term.t
(** Cmdliner term for number of connection retry attempts. Default: 3 Flag:
    [--connect-retry-count] *)

val connect_retry_delay : float Cmdliner.Term.t
(** Cmdliner term for initial retry delay in seconds. Default: 0.1 Flag:
    [--connect-retry-delay] *)

(** {1 Combined Terms} *)

val config : Config.t Cmdliner.Term.t
(** Cmdliner term that combines all configuration options into a {!Config.t}.
    This term can be used in your application's main command to accept all
    connection pool configuration options from the command line. *)
