(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Cmdliner terms for connection pool configuration *)

open Cmdliner

let max_connections_per_endpoint =
  let doc = "Maximum concurrent connections per endpoint." in
  Arg.(
    value & opt int 10
    & info [ "max-connections-per-endpoint" ] ~doc ~docv:"NUM")

let max_idle_time =
  let doc = "Maximum time a connection can sit idle in seconds." in
  Arg.(value & opt float 60.0 & info [ "max-idle-time" ] ~doc ~docv:"SECONDS")

let max_connection_lifetime =
  let doc = "Maximum connection age in seconds." in
  Arg.(
    value & opt float 300.0
    & info [ "max-connection-lifetime" ] ~doc ~docv:"SECONDS")

let max_connection_uses =
  let doc = "Maximum times a connection can be reused (omit for unlimited)." in
  Arg.(
    value
    & opt (some int) None
    & info [ "max-connection-uses" ] ~doc ~docv:"NUM")

let connect_timeout =
  let doc = "Connection timeout in seconds." in
  Arg.(value & opt float 10.0 & info [ "connect-timeout" ] ~doc ~docv:"SECONDS")

let connect_retry_count =
  let doc = "Number of connection retry attempts." in
  Arg.(value & opt int 3 & info [ "connect-retry-count" ] ~doc ~docv:"NUM")

let connect_retry_delay =
  let doc = "Initial retry delay in seconds (with exponential backoff)." in
  Arg.(
    value & opt float 0.1 & info [ "connect-retry-delay" ] ~doc ~docv:"SECONDS")

let config =
  let make max_conn max_idle max_lifetime max_uses timeout retry_count
      retry_delay =
    Config.make ~max_connections_per_endpoint:max_conn ~max_idle_time:max_idle
      ~max_connection_lifetime:max_lifetime ?max_connection_uses:max_uses
      ~connect_timeout:timeout ~connect_retry_count:retry_count
      ~connect_retry_delay:retry_delay ()
  in
  Term.(
    const make $ max_connections_per_endpoint $ max_idle_time
    $ max_connection_lifetime $ max_connection_uses $ connect_timeout
    $ connect_retry_count $ connect_retry_delay)
