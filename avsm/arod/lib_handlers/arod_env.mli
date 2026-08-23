(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Capabilities for arod handlers.

    A proffer handler is portable. It can therefore capture neither
    domain-bound state, such as a search handle, an access log, a clock or a
    filesystem capability, nor a function whose interface carries no mode
    annotations, which is every function in jsont and Syndic and every
    renderer built on those. Each of them is reached through a closure in {!t}
    instead, and one value of {!t} is built per domain by {!create}, where
    those resources exist.

    A closure field is named for the response it produces, not for the module
    it came from, so a handler reads as a description of the route it answers.
    A response a handler can compute itself has no field: it reads {!t.config}
    or {!t.ctx} and calls {!Arod_render} directly. Every page render is now
    reached that way, as is the sitemap, and the header of {!Arod_render} names
    what keeps the four that are left behind a closure. *)

type t = {
  ctx : Arod.Ctx.t;
      (** The loaded context. It is immutable data, so a handler both captures
          it and passes it to {!Arod_render} directly. A response whose render
          is portable has no closure field and reads this instead. *)
  config : Arod.Config.t;
      (** The loaded configuration. It is immutable data, so a handler reads
          it directly rather than through a closure. *)
  cache : Proffer.Cache.t;
      (** The memoization cache for rendered pages. It crosses domains, so
          every domain shares one cache. *)
  now : unit -> float;
      (** [now ()] is the current time in seconds since the epoch, read from
          this domain's clock. The cache is expired against it. *)
  feed : Arod_render.feed -> string;
      (** [feed which] is the syndication feed [which]. *)
  pagination :
    collection:string option ->
    offset:int ->
    limit:int ->
    types:string list ->
    (Proffer.Body.Sink.t -> unit);
      (** [pagination ~collection ~offset ~limit ~types] is one page of
          [collection] as the JSON the pagination API answers. It is a closure
          because it renders through jsont, whose codecs cannot cross a domain
          boundary. *)
  search :
    q:string -> limit:int -> link_limit:int ->
    (Proffer.Body.Sink.t -> unit) * int;
      (** [search ~q ~limit ~link_limit] is the tiers for [q], at most
          [limit] work hits and [link_limit] links, as the JSON the search
          page reads, paired with the number of hits in both tiers. An empty
          [q] is an empty result set and queries nothing. *)
  log_search : query:string -> limit:int -> results:int option -> unit;
      (** [log_search ~query ~limit ~results] records a search API request.
          [results] is [None] before the query runs and [Some n] once it has
          returned [n] results. A handler is portable and cannot reach a log
          source itself, so the message is written on the domain that built
          this record. *)
  read_image : string list -> string option;
      (** [read_image segs] is the contents of the file named by [segs] under
          the served image directory, and [None] when that file is missing or
          unreadable. The read is confined to that directory, so [segs]
          naming a path that would leave it is [None]. *)
  read_paper : string -> string option;
      (** [read_paper name] is the contents of the file [name] under the
          served paper directory, and [None] when that file is missing or
          unreadable. The read is confined to that directory, so a [name]
          that would leave it is [None]. *)
  report : Arod_render.report -> range:string -> string;
      (** [report which ~range] is the access log view [which] over [range],
          read on this domain's connection to the log database. *)
}
(** The capabilities a handler reaches through its [env] argument. *)

val create :
  ctx:Arod.Ctx.t ->
  cache:Proffer.Cache.t ->
  search:(limit:int -> link_limit:int -> string -> Arod_search.results) ->
  log_search:(query:string -> limit:int -> results:int option -> unit) ->
  read_image:(string list -> string option) ->
  read_paper:(string -> string option) ->
  reader:(unit -> Sqlite3_eio.t) ->
  now:(unit -> float) ->
  t
(** [create ~ctx ~cache ~search ~log_search ~read_image ~read_paper ~reader
    ~now] is the capability record for one domain. The configuration and the
    rendering closures that are left come from [ctx], [search] answers the
    search API, [log_search] reports what it was asked for, [reader] is the
    read-only handle the stats dashboard queries, and [read_image] and
    [read_paper] are the confined reads of the served directories. Call it on
    the domain that owns those resources. *)
