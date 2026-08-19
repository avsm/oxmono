(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Capabilities for arod handlers.

    A proffer handler is portable. It can therefore capture neither
    domain-bound state, such as a search handle, an access log, a clock or a
    filesystem capability, nor a function whose interface carries no mode
    annotations, which is every function in Uri, Cmarkit and Ezjsonm, and any
    Bushel accessor, including the ones that date an entry through the
    annotated Ptime, and every renderer built on those. Each of them is
    reached through a closure in {!t} instead, and one value of {!t} is built
    per domain by {!create}, where those resources exist.

    A closure field is named for the response it produces, not for the module
    it came from, so a handler reads as a description of the route it answers.
    A response a handler can compute itself from plain configuration has no
    field. It reads {!t.config} instead. *)

type t = {
  config : Arod.Config.t;
      (** The loaded configuration. It is immutable data, so a handler reads
          it directly rather than through a closure. *)
  cache : Proffer.Cache.t;
      (** The memoization cache for rendered pages. It crosses domains, so
          every domain shares one cache. *)
  now : unit -> float;
      (** [now ()] is the current time in seconds since the epoch, read from
          this domain's clock. The cache is expired against it. *)
  listing : Arod_render.listing -> Arod_render.flavour -> string;
      (** [listing which flavour] is the page [which] rendered as
          [flavour]. *)
  entry : Arod_render.entry_kind -> string -> Arod_render.flavour -> string;
      (** [entry kind slug flavour] is the entry [slug] of collection [kind]
          rendered as [flavour]. A [slug] that names nothing is the empty
          string, which is what arod has always answered such a URL with. *)
  entry_markdown : string -> string option;
      (** [entry_markdown slug] is the entry [slug] as markdown, and [None]
          when no entry has that slug. *)
  paper_bib : string -> string option;
      (** [paper_bib slug] is the BibTeX entry for the paper [slug], and
          [None] when [slug] names no paper. *)
  feed : Arod_render.feed -> string;
      (** [feed which] is the syndication feed [which]. *)
  sitemap : unit -> string;
      (** [sitemap ()] is the XML sitemap of every entry. *)
  blogroll : unit -> string;
      (** [blogroll ()] is the OPML blogroll of every contact with a feed. *)
  pagination :
    collection:string option ->
    offset:int ->
    limit:int ->
    types:string list ->
    string;
      (** [pagination ~collection ~offset ~limit ~types] is one page of
          [collection] as the JSON the pagination script reads. *)
  search : q:string -> limit:int -> string * int;
      (** [search ~q ~limit] is at most [limit] results for [q] as the JSON
          the search box reads, paired with the number of results it holds. An
          empty [q] is an empty result set and queries nothing. *)
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
  search:(limit:int -> string -> Arod_search.result list) ->
  log_search:(query:string -> limit:int -> results:int option -> unit) ->
  read_image:(string list -> string option) ->
  read_paper:(string -> string option) ->
  reader:(unit -> Sqlite3_eio.t) ->
  now:(unit -> float) ->
  t
(** [create ~ctx ~cache ~search ~log_search ~read_image ~read_paper ~reader
    ~now] is the capability record for one domain. The configuration and every
    rendering closure come from [ctx], [search] answers the search API,
    [log_search] reports what it was asked for, [reader] is the read-only
    handle the stats dashboard queries, and [read_image] and [read_paper] are
    the confined reads of the served directories. Call it on the domain that
    owns those resources. *)
