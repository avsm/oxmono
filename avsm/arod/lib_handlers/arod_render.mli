(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Rendering, separated from serving.

    Every function here turns the context into the bytes of one response. None
    of them decides a status, a content type or a cache policy, which is
    {!Arod_handlers}' business.

    Every page render is portable: {!listing}, {!entry}, {!entry_markdown},
    {!paper_bib} and {!blogroll}. A proffer handler is portable too, so it
    calls them directly, passing the context out of {!Arod_env.t}. None of
    them has a closure field in {!Arod_env} any more.

    Getting there took the whole render path. Htmlit, Ptime, Cmarkit and uriz
    are annotated, {!Arod.Ctx}, {!Arod.Md}, {!Arod.Jsonld}, {!Arod.Text} and
    the Bushel accessors are annotated, the URL matching that ran on opam Uri
    is settled when the context is built, the link graph is an immutable field
    of {!Bushel.Entry.t} rather than a module-level [ref], and
    {!Bushel.Md.note_references}, which scans a note body with [Re] and
    decodes a DOI with [Uri.pct_decode], is settled once per note when the
    context is built.

    {!sitemap} is portable too, since the vendored sitemap library carries a
    floating [@@ portable] and its [url] type is a private record whose kind
    the compiler reads off its fields.

    Four renders are still not portable, and each was measured by annotating it
    and reading what the compiler named. {!feed} closes over
    [Arod.Feed.feed_string], {!pagination} and {!search} over
    [Arod_json.stream], and {!report} over
    [Arod_handlers_stats.render_dashboard]. The first three reach jsont, whose
    codecs cannot be given a kind that crosses a domain boundary; [TODO.md]
    records what that would take and where it stops. {!report} also takes a
    database handle, which is bound to the domain that opened it, so
    annotating the renderer alone would not free the route. Those four keep
    their closures in {!Arod_env}. *)

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
(** [sitemap ~ctx] is the XML sitemap of every entry. *)

val blogroll : ctx:Arod.Ctx.t -> string @@ portable
(** [blogroll ~ctx] is the OPML blogroll of every contact that has a feed. *)

val pagination :
  ctx:Arod.Ctx.t ->
  collection:string option ->
  offset:int ->
  limit:int ->
  types:string list ->
  (Proffer.Body.Sink.t -> unit)
(** [pagination ~ctx ~collection ~offset ~limit ~types] writes one page of
    [collection] as rendered HTML with the counts a client needs to ask for
    the next. [types] filters an entry collection and is ignored by the
    others. An unknown or absent [collection] writes a JSON error object.

    The page is selected and its HTML rendered when this is called, so a
    caller holds the cost of the page as soon as it has the writer. Only the
    JSON is deferred, and it goes to the socket a slice at a time rather than
    through a string: this route answers over a megabyte, which used to exist
    twice. *)

val search :
  ctx:Arod.Ctx.t -> Arod_search.result list -> (Proffer.Body.Sink.t -> unit)
(** [search ~ctx results] writes [results] as the JSON the search box reads,
    streamed as {!pagination} is. *)

val report : db:Sqlite3_eio.t -> report -> range:string -> string
(** [report ~db which ~range] is the access log view [which] over [range],
    which names a time span such as ["7d"]. An unrecognised [range] is seven
    days. [`Recent] ignores it. *)
