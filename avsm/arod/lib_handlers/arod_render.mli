(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Rendering, separated from serving.

    Every function here turns the context into the bytes of one response. None
    of them decides a status, a content type or a cache policy, which is
    {!Arod_handlers}' business.

    {!paper_bib} and {!blogroll} are portable, so the handlers that serve them
    call them directly, passing the context out of {!Arod_env.t}. The rest are
    not, and a proffer handler, which is portable, reaches those through the
    closures in {!Arod_env} instead, built once on the domain that owns the
    context.

    Most of what used to hold them back is gone. Htmlit, Ptime, Cmarkit and
    uriz are annotated, {!Arod.Ctx}, {!Arod.Md}, {!Arod.Jsonld},
    {!Arod.Text} and the Bushel accessors are annotated, the URL matching
    that ran on opam Uri is settled when the context is built, and the link
    graph is now an immutable field of {!Bushel.Entry.t} rather than a
    module-level [ref], so a portable render reads backlinks straight out of
    the collection. One thing remains.

    {!Bushel.Md.note_references} scans a note body with [Re] and decodes a DOI
    with [Uri.pct_decode], at [bushel_md.ml:996-1002] and again at
    [:1019-1023]. [Re] is an opam library with no mode annotations, so nothing
    short of vendoring it or precomputing the references reaches past that.

    It blocks {!entry} and nothing else here: {!entry} reaches it through a
    note page, and {!listing} and {!entry_markdown} are now held back by
    nothing, which was measured by annotating all three and asking the
    compiler.

    The rest of this module is blocked elsewhere and always was: {!feed} by
    {!Arod.Feed}, {!sitemap} by the vendored [Sitemap], which carries no
    annotations yet, {!pagination} and {!search} by Ezjsonm, and {!report} by
    the log database. Each was measured the same way. *)

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

val listing : ctx:Arod.Ctx.t -> listing -> flavour -> string
(** [listing ~ctx which flavour] is the page [which] rendered as [flavour]. *)

val entry : ctx:Arod.Ctx.t -> entry_kind -> string -> flavour -> string
(** [entry ~ctx kind slug flavour] is the entry [slug] rendered as [flavour].
    A [slug] that names nothing is the empty string, and one whose entry is
    not of [kind] renders as a bare page. *)

val entry_markdown : ctx:Arod.Ctx.t -> string -> string option
(** [entry_markdown ~ctx slug] is the entry [slug] as markdown, and [None]
    when no entry has that slug. *)

val paper_bib : ctx:Arod.Ctx.t -> string -> string option @@ portable
(** [paper_bib ~ctx slug] is the BibTeX entry for the paper [slug], and
    [None] when [slug] names no paper. *)

val feed : ctx:Arod.Ctx.t -> feed -> string
(** [feed ~ctx which] is the feed [which]. *)

val sitemap : ctx:Arod.Ctx.t -> string
(** [sitemap ~ctx] is the XML sitemap of every entry. *)

val blogroll : ctx:Arod.Ctx.t -> string @@ portable
(** [blogroll ~ctx] is the OPML blogroll of every contact that has a feed. *)

val pagination :
  ctx:Arod.Ctx.t ->
  collection:string option ->
  offset:int ->
  limit:int ->
  types:string list ->
  string
(** [pagination ~ctx ~collection ~offset ~limit ~types] is one page of
    [collection] as rendered HTML with the counts a client needs to ask for
    the next. [types] filters an entry collection and is ignored by the
    others. An unknown or absent [collection] is a JSON error object. *)

val search : ctx:Arod.Ctx.t -> Arod_search.result list -> string
(** [search ~ctx results] is [results] as the JSON the search box reads. *)

val report : db:Sqlite3_eio.t -> report -> range:string -> string
(** [report ~db which ~range] is the access log view [which] over [range],
    which names a time span such as ["7d"]. An unrecognised [range] is seven
    days. [`Recent] ignores it. *)
