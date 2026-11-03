(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** A live terminal dashboard of a running server.

    The dashboard keeps in-memory counters of the requests the server has
    answered since it started: rates over several windows, latency
    percentiles and buckets, status classes, cache outcomes, the busiest and
    slowest paths, user agent families, distinct visitors, open connections,
    the process's CPU, heap and allocation rate, and a scrolling list of the
    latest requests. A slower panel summarises the last day from the access
    log database. {!run} draws all of it at the bottom of the terminal while
    log lines scroll above it.

    The display is idle between requests. A frame is drawn when a request,
    a connection or an error arrives, bursts coalesced into at most ten
    frames a second, and once a second for a minute after the last request
    so that the rate windows settle to zero. Process and access log figures
    are sampled when a frame is drawn, never on a timer.

    All functions belong to the serving domain. Recording a request never
    yields, so counters stay consistent between fibers. *)

type t
(** The type for dashboards. *)

val create :
  clock:_ Eio.Time.Mono.t ->
  wall:(unit -> float) ->
  site:string ->
  listen:string ->
  content:(string * int) list ->
  search_entries:(unit -> int option) ->
  cache_stats:(unit -> int * int) ->
  max_connections:int ->
  unit ->
  t
(** [create ~clock ~wall ~site ~listen ~content ~search_entries ~cache_stats
    ~max_connections ()] is an empty dashboard. [clock] times rates and
    latencies, [wall ()] is the current Unix time, [site] names the site and
    [listen] its listening address. [content] lists the loaded entry kinds
    and their counts. [search_entries ()] is the size of the search index
    once built, [cache_stats ()] the response cache's hit and miss counts
    and [max_connections] the server's connection limit. *)

val record : t -> Proffer_httpz.event -> unit
(** [record t event] counts one answered request. [event] must already be
    a heap copy ({!Proffer_httpz.globalize_event}). *)

val connection : t -> int -> unit
(** [connection t delta] adds [delta] open connections, as the server's
    [on_connection] callback reports them. *)

val error : t -> exn -> unit
(** [error t exn] records a server error for the dashboard's error line. *)

val run :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.Mono.t ->
  ?reader:Sqlite3_eio.t ->
  t ->
  (unit -> 'a) ->
  'a
(** [run ~sw ~clock ?reader t f] is [f ()] with the dashboard drawn on
    standard error, in that formatter's theme with its motion stopped, until
    [f] returns. [clock] paces the frames. When [reader] is given, the
    access log is summarised through it at most once a minute, in a fiber
    under [sw]. Logs messages print above the dashboard meanwhile, and the
    previous reporter is put back afterwards. SIGINT and SIGTERM exit with
    status 130 after the last frame is printed. *)

val view : t -> Console.View.t
(** [view t] is the dashboard laid out for the room it is drawn in. It drops
    its lower panels in a short terminal and its rightmost panels in a
    narrow one, so the frame never scrolls. *)
