(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP request timing metrics

    Per Recommendation #12: Detailed timing breakdown for requests,
    similar to curl's --write-out timing variables.

    {2 Timing Phases}

    {[
      |--DNS--|--Connect--|--TLS--|--Request--|--Wait--|--Content--|
              ^           ^       ^           ^        ^           ^
              namelookup  connect ssl_handsh  send     ttfb        total
    ]}

    {2 Example Usage}

    {[
      (* Start timing *)
      let timer = Timing.start () in

      (* DNS resolution *)
      let addrs = Eio.Net.getaddrinfo_stream net host in
      Timing.mark_dns timer;

      (* TCP connect *)
      let flow = Eio.Net.connect ~sw net addr in
      Timing.mark_connect timer;

      (* TLS handshake (for HTTPS) *)
      let tls_flow = Tls_eio.client_of_flow tls_cfg flow in
      Timing.mark_tls timer;

      (* Send request *)
      send_request tls_flow request;
      Timing.mark_send timer;

      (* First byte received *)
      Timing.mark_ttfb timer;

      (* Complete transfer *)
      Timing.mark_transfer_end timer;

      (* Get timing metrics *)
      let metrics = Timing.finish timer in
      Printf.printf "Total: %.3fms\n" (Timing.total metrics *. 1000.0)
    ]}
*)

(** {1 Timing Metrics} *)

type t
(** Timing metrics for a completed request *)

val empty : t
(** Empty timing with all phases unknown *)

val make :
  ?dns_lookup:float ->
  ?tcp_connect:float ->
  ?tls_handshake:float ->
  ?request_sent:float ->
  ?time_to_first_byte:float ->
  ?content_transfer:float ->
  total:float ->
  unit -> t
(** Create timing metrics manually *)

(** {2 Accessors} *)

val dns_lookup : t -> float option
(** Time for DNS resolution in seconds *)

val tcp_connect : t -> float option
(** Time to establish TCP connection in seconds *)

val tls_handshake : t -> float option
(** Time for TLS handshake in seconds (HTTPS only) *)

val request_sent : t -> float option
(** Time to send request in seconds *)

val time_to_first_byte : t -> float option
(** Time from request sent to first response byte in seconds *)

val content_transfer : t -> float option
(** Time to transfer response body in seconds *)

val total : t -> float
(** Total request time in seconds *)

(** {2 Computed Metrics} *)

val connection_time : t -> float option
(** Total connection setup time (DNS + TCP + TLS) *)

val server_time : t -> float option
(** Server processing time (TTFB - request send time) *)

(** {2 Formatting} *)

val pp : Format.formatter -> t -> unit
(** Pretty-print timing in human readable format *)

val to_string : t -> string
(** Convert to string representation *)

val to_assoc : t -> (string * float) list
(** Convert to association list for logging/serialization *)

(** {1 Timer for Collecting Metrics}

    Use this during request execution to collect timing data incrementally. *)

type timer
(** Mutable timer for collecting metrics during request execution *)

val start : unit -> timer
(** Start a new timer *)

val mark_dns : timer -> unit
(** Mark DNS resolution complete *)

val mark_connect : timer -> unit
(** Mark TCP connection established *)

val mark_tls : timer -> unit
(** Mark TLS handshake complete *)

val mark_send : timer -> unit
(** Mark request fully sent *)

val mark_ttfb : timer -> unit
(** Mark first response byte received *)

val mark_transfer_end : timer -> unit
(** Mark response body transfer complete *)

val finish : timer -> t
(** Stop timer and compute final timing metrics *)

(** {1 Logging} *)

val src : Logs.Src.t
(** Log source for timing operations *)

val log_timing : ?level:Logs.level -> t -> unit
(** Log timing metrics at specified level (default: Debug) *)
