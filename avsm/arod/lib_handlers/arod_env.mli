(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Per-domain capabilities used by route handlers. *)

type t = {
  ctx : Arod.Ctx.t;
      (** The loaded site context. *)
  config : Arod.Config.t;
      (** The loaded configuration. *)
  cache : Proffer.Cache.t;
      (** The shared response cache. *)
  now : unit -> float;
      (** [now ()] is the current Unix time. *)
  feed : Arod_render.feed -> string;
      (** [feed which] is the syndication feed [which]. *)
  pagination :
    collection:string option ->
    offset:int ->
    limit:int ->
    types:string list ->
    (Proffer.Body.Sink.t -> unit);
      (** [pagination ~collection ~offset ~limit ~types] is one page of
          [collection] as streamed JSON. *)
  search :
    q:string -> limit:int -> link_limit:int -> order:Arod_search.order ->
    (Proffer.Body.Sink.t -> unit) * int;
      (** [search ~q ~limit ~link_limit ~order] is the tiers for [q], at most
          [limit] work hits and [link_limit] links, as the JSON [/api/search]
          serves, paired with their total count. *)
  search_page :
    q:string -> limit:int -> link_limit:int -> order:Arod_search.order ->
    fragment:bool -> string;
      (** [search_page ~q ~limit ~link_limit ~order ~fragment] is the page
          for [q], or only its results region when [fragment] is [true]. *)
  log_search : query:string -> limit:int -> results:int option -> unit;
      (** [log_search ~query ~limit ~results] records a search API request.
          [results] is [None] before execution and [Some n] afterwards. *)
  read_image : string list -> string option;
      (** [read_image segs] is the contents of the file named by [segs] under
          the served image directory, or [None] if it cannot be read safely. *)
  read_paper : string -> string option;
      (** [read_paper name] is the contents of the file [name] under the
          served paper directory, or [None] if it cannot be read safely. *)
  report : Arod_render.report -> range:string -> string;
      (** [report which ~range] is the access log view [which] over [range],
          read on this domain's connection to the log database. *)
}
(** The capabilities of one serving domain. *)

val create :
  ctx:Arod.Ctx.t ->
  cache:Proffer.Cache.t ->
  search:
    (limit:int -> link_limit:int -> order:Arod_search.order -> string ->
     Arod_search.results) ->
  log_search:(query:string -> limit:int -> results:int option -> unit) ->
  read_image:(string list -> string option) ->
  read_paper:(string -> string option) ->
  reader:(unit -> Sqlite3_eio.t) ->
  now:(unit -> float) ->
  t
(** [create ~ctx ~cache ~search ~log_search ~read_image ~read_paper ~reader
    ~now] is the capability record for the calling domain. *)
