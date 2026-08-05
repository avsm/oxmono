(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP request timing metrics

    Per Recommendation #12: Detailed timing breakdown for requests,
    similar to curl's --write-out timing variables.

    Timing phases:
    {[
      |--DNS--|--Connect--|--TLS--|--Request--|--Wait--|--Content--|
              ^           ^       ^           ^        ^           ^
              namelookup  connect ssl_handsh  send     ttfb        total
    ]}
*)

let src = Logs.Src.create "requests.timing" ~doc:"HTTP request timing"
module Log = (val Logs.src_log src : Logs.LOG)

(** Timing metrics for a single request *)
type t = {
  dns_lookup : float option;       (** Time for DNS resolution *)
  tcp_connect : float option;      (** Time to establish TCP connection *)
  tls_handshake : float option;    (** Time for TLS handshake (HTTPS only) *)
  request_sent : float option;     (** Time to send request *)
  time_to_first_byte : float option;  (** Time from request sent to first byte received *)
  content_transfer : float option; (** Time to transfer response body *)
  total : float;                   (** Total request time *)
}

let empty = {
  dns_lookup = None;
  tcp_connect = None;
  tls_handshake = None;
  request_sent = None;
  time_to_first_byte = None;
  content_transfer = None;
  total = 0.0;
}

let make
    ?dns_lookup
    ?tcp_connect
    ?tls_handshake
    ?request_sent
    ?time_to_first_byte
    ?content_transfer
    ~total
    () =
  { dns_lookup; tcp_connect; tls_handshake; request_sent;
    time_to_first_byte; content_transfer; total }

let dns_lookup t = t.dns_lookup
let tcp_connect t = t.tcp_connect
let tls_handshake t = t.tls_handshake
let request_sent t = t.request_sent
let time_to_first_byte t = t.time_to_first_byte
let content_transfer t = t.content_transfer
let total t = t.total

(** Connection setup time (DNS + TCP + TLS) *)
let connection_time t =
  let dns = Option.value t.dns_lookup ~default:0.0 in
  let tcp = Option.value t.tcp_connect ~default:0.0 in
  let tls = Option.value t.tls_handshake ~default:0.0 in
  Some (dns +. tcp +. tls)

(** Server processing time (TTFB - request send time) *)
let server_time t =
  match t.time_to_first_byte, t.request_sent with
  | Some ttfb, Some send -> Some (ttfb -. send)
  | _ -> None

(** Pretty-print timing in human readable format *)
let pp ppf t =
  let pp_opt ppf = function
    | Some v -> Format.fprintf ppf "%.3fms" (v *. 1000.0)
    | None -> Format.fprintf ppf "-"
  in
  Format.fprintf ppf "@[<v>Timing:@,\
    DNS lookup:       %a@,\
    TCP connect:      %a@,\
    TLS handshake:    %a@,\
    Request sent:     %a@,\
    Time to 1st byte: %a@,\
    Content transfer: %a@,\
    Total:            %.3fms@]"
    pp_opt t.dns_lookup
    pp_opt t.tcp_connect
    pp_opt t.tls_handshake
    pp_opt t.request_sent
    pp_opt t.time_to_first_byte
    pp_opt t.content_transfer
    (t.total *. 1000.0)

let to_string t =
  Format.asprintf "%a" pp t

(** Convert to JSON-like association list for logging/debugging *)
let to_assoc t =
  let add_opt name = function
    | Some v -> [(name, v)]
    | None -> []
  in
  add_opt "dns_lookup" t.dns_lookup @
  add_opt "tcp_connect" t.tcp_connect @
  add_opt "tls_handshake" t.tls_handshake @
  add_opt "request_sent" t.request_sent @
  add_opt "time_to_first_byte" t.time_to_first_byte @
  add_opt "content_transfer" t.content_transfer @
  [("total", t.total)]

(** {1 Timer for Collecting Metrics}

    Use this during request execution to collect timing data. *)

type timer = {
  start : float;
  mutable dns_end : float option;
  mutable connect_end : float option;
  mutable tls_end : float option;
  mutable send_end : float option;
  mutable ttfb : float option;
  mutable transfer_end : float option;
}

let start () =
  { start = Unix.gettimeofday ();
    dns_end = None;
    connect_end = None;
    tls_end = None;
    send_end = None;
    ttfb = None;
    transfer_end = None;
  }

let mark_dns timer =
  timer.dns_end <- Some (Unix.gettimeofday ())

let mark_connect timer =
  timer.connect_end <- Some (Unix.gettimeofday ())

let mark_tls timer =
  timer.tls_end <- Some (Unix.gettimeofday ())

let mark_send timer =
  timer.send_end <- Some (Unix.gettimeofday ())

let mark_ttfb timer =
  timer.ttfb <- Some (Unix.gettimeofday ())

let mark_transfer_end timer =
  timer.transfer_end <- Some (Unix.gettimeofday ())

let finish timer =
  let now = Unix.gettimeofday () in
  let total = now -. timer.start in

  let calc_phase start_time end_time =
    Option.map (fun e -> e -. Option.value start_time ~default:timer.start) end_time
  in

  {
    dns_lookup = calc_phase (Some timer.start) timer.dns_end;
    tcp_connect = calc_phase timer.dns_end timer.connect_end;
    tls_handshake = calc_phase timer.connect_end timer.tls_end;
    request_sent = calc_phase (if Option.is_some timer.tls_end then timer.tls_end else timer.connect_end) timer.send_end;
    time_to_first_byte = calc_phase timer.send_end timer.ttfb;
    content_transfer = calc_phase timer.ttfb timer.transfer_end;
    total;
  }

(** Log timing metrics *)
let log_timing ?(level=Logs.Debug) t =
  Log.msg level (fun m -> m "%a" pp t)
