(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Response rendering without serving policy. *)

type flavour = [ `Html | `Markdown ]
(** Which rendering of a page is wanted. *)

type listing =
  [ `Index
  | `Papers
  | `Notes
  | `Ideas
  | `Projects
  | `Videos
  | `Links
  | `Network ]
(** A page that lists a collection, or the front page. *)

type entry_kind = [ `Paper | `Note | `Idea | `Project | `Video ]
(** The collection an entry URL names. *)

type feed = [ `Atom of string | `Json | `Perma_atom | `Perma_json ]
(** A syndication feed. [`Atom path] names itself [path], since two paths
    serve it. *)

type report = [ `Dashboard | `Overview | `Traffic | `Recent ]
(** One view of the access log. *)

val listing : ctx:Arod.Ctx.t -> listing -> flavour -> string @@ portable
(** [listing ~ctx which flavour] is the page [which] rendered as [flavour]. *)

val entry :
  ctx:Arod.Ctx.t -> entry_kind -> string -> flavour -> string @@ portable
(** [entry ~ctx kind slug flavour] is the entry [slug] rendered as [flavour].
    A [slug] that names nothing is the empty string, and one whose entry is
    not of [kind] renders as a bare page. *)

val entry_markdown : ctx:Arod.Ctx.t -> string -> string option @@ portable
(** [entry_markdown ~ctx slug] is the entry [slug] as markdown, and [None]
    when no entry has that slug. *)

val paper_bib : ctx:Arod.Ctx.t -> string -> string option @@ portable
(** [paper_bib ~ctx slug] is the BibTeX entry for the paper [slug], and
    [None] when [slug] names no paper. *)

val feed : ctx:Arod.Ctx.t -> feed -> string
(** [feed ~ctx which] is the feed [which]. *)

val sitemap : ctx:Arod.Ctx.t -> string @@ portable
(** [sitemap ~ctx] is the XML sitemap of every entry and listing page. *)

val llms_txt : ctx:Arod.Ctx.t -> string @@ portable
(** [llms_txt ~ctx] is the llms.txt index of every entry's Markdown twin. *)

val blogroll : ctx:Arod.Ctx.t -> string @@ portable
(** [blogroll ~ctx] is the OPML blogroll of every contact that has a feed. *)

val pagination :
  ctx:Arod.Ctx.t ->
  collection:string option ->
  offset:int ->
  limit:int ->
  types:string list ->
  (Proffer.Body.Sink.t -> unit)
(** [pagination ~ctx ~collection ~offset ~limit ~types] is a writer for one
    JSON page of [collection]. [types] filters entry collections. *)

val search :
  ctx:Arod.Ctx.t -> Arod_search.results -> (Proffer.Body.Sink.t -> unit)
(** [search ~ctx results] is a writer for [results] as search API JSON. *)

val search_page :
  ctx:Arod.Ctx.t -> q:string -> order:Arod_search.order -> fragment:bool ->
  Arod_search.results -> string
(** [search_page ~ctx ~q ~order ~fragment r] is the search page for [q]
    showing [r] with the [order] toggle marked, or with [fragment] only
    the results region the page script swaps in. *)

val report : db:Sqlite3_eio.t -> report -> range:string -> string
(** [report ~db which ~range] is the access log view [which] over [range]. An
    unknown range defaults to seven days. *)
