(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** FTS5 full-text search index for Arod content.

    Uses one FTS5 table per entry kind so that kind filtering queries only
    the relevant tables. *)

type t
(** A handle to the search database. *)

type goto_kind = [ `Section | `Project | `Tag ]
(** What a {!goto} entry points at. *)

type goto = {
  label : string;
  url : string;
  detail : string;
  goto_kind : goto_kind;
}
(** A page the query names rather than describes: a site section, a
    project or a tag. A section matches when some query word prefixes its
    name. A project or a tag matches only when every query word prefixes
    its slug or tag, or one of its title's words. [detail] is the short
    line shown beside it, such as ["2012 project"] or ["2 entries"]. *)

type hit = {
  slug : string;
  kind : string;
  url : string;
  title : string;
  snippet : string;
  date : string;
  tags : string list;
  parent_slugs : string list;
  score : float;
}
(** One ranked hit. [score] is the tier's combined score, larger is better.
    For a link, [parent_slugs] names the entries that cite it. *)

type results = {
  terms : string list;
  goto : goto list;
  work : hit list;
  work_total : int;
  links : hit list;
  links_total : int;
  kinds : (string * int) list;
  years : (int * int) list;
  tags : (string * int) list;
}
(** The tiers of one search. [work] and [links] are ranked and cut to the
    caller's limits, and [work_total] and [links_total] count the matches
    before that cut. Each per-kind query fetches at most a fixed depth (200
    for a local kind, 500 for links), so a total is bounded by what was
    fetched, not by what exists. [kinds], [years] and [tags] count the same
    way, over every work match before the cut, except on a tags-only query,
    where [tags] counts only the shown page of [work]. [kinds] is sorted by
    name and [years] ascending. [tags] is sorted by count descending then
    name, cut to the 8 most used. [terms] is the query's words, lowercased,
    for marking matches. *)

val empty : results
(** [empty] is the result of a query that asked for nothing. *)

val create : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [create ~sw path] opens or creates the search database at [path]. *)

val create_memory : sw:Eio.Switch.t -> unit -> t
(** [create_memory ~sw ()] creates an in-memory search database.
    Ideal for the server where the index is rebuilt on startup. *)

val open_readonly : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [open_readonly ~sw path] opens the search database read-only for queries.
    It never calls {!index}, so [own_host], the tag counts and the projects
    a {!search} needs are read back from what the last {!index} left in
    [path] rather than computed. *)

val rebuild : t -> Arod.Ctx.t -> unit
(** [rebuild t ctx] drops and rebuilds all per-kind search tables from all
    entries and links in [ctx]. *)

val index :
  t ->
  own_host:string ->
  contact_name:(string -> string option) ->
  entries:Bushel.Entry.entry list ->
  links:Bushel.Link.t list ->
  unit
(** [index t ~own_host ~contact_name ~entries ~links] drops every table and
    indexes [entries] and [links]. [contact_name handle] is the display name
    a body mention of [handle] expands to. [own_host] is the host of the
    site's own base URL, and links on it are left out of search results. It
    is what {!rebuild} calls with a context's contents. *)

type order = [ `Relevance | `Date ]
(** How the work and links tiers are ordered. [`Relevance] is the ranking
    described under {!search}. [`Date] shows the same matched sets newest
    first. *)

val search :
  t -> ?today:int * int * int -> ?limit:int -> ?link_limit:int ->
  ?order:order -> string -> results
(** [search t ?today ?limit ?link_limit ?order input] ranks what matches
    [input] in three strict tiers. Papers, notes, projects, ideas and
    videos are ordered by [bm25 × kind prior × freshness]. Links are
    ordered by [bm25 × freshness × citation bonus], deduplicated by
    normalised URL and by host and title, and never include the site's own
    host. [order] defaults to [`Relevance], and [`Date] re-sorts both
    tiers newest first after ranking, so link dedupe still keeps the
    better-scoring copy. [today] defaults to the current date and fixes
    freshness for tests. [limit] defaults to 20 and [link_limit] to 12.
    The syntax is as before: words,
    ["exact phrase"], [prefix*], [kind:paper] and [#tag]. A query with only
    filters browses the filtered set by date, and a browse leaves [goto]
    empty since there is no query text to match a page's name against. *)

val kind_prior : string -> float
(** [kind_prior kind] is the multiplier the work tier applies to [kind]. *)

val freshness : today:int * int * int -> string -> float
(** [freshness ~today date] is between 1.0 and 1.25, largest for [today]
    and 1.0 from eight years before it. *)

val citation_bonus : int -> float
(** [citation_bonus n] is the multiplier for a link cited by [n] entries. *)

val normalise_url : string -> string
(** [normalise_url u] is [u] lowercased without scheme, leading [www.] or
    trailing [/] and [#]. Two links with one normalised URL are one page. *)

val host_of_url : string -> string
(** [host_of_url u] is the host of {!normalise_url}[ u]. *)

val search_tags :
  t -> ?kinds:string list -> ?limit:int -> string list -> hit list
(** [search_tags t ?kinds ?limit tags] returns entries matching ALL given
    tags exactly. Uses the entry_tags table for exact matching. *)

val all_tags : t -> (string * int) list
(** [all_tags t] returns all unique tags with their counts, sorted by
    count descending. *)

val kinds : string list
(** The valid kind values: paper, note, project, idea, video, link. *)

val pp_results : Format.formatter -> results -> unit
(** [pp_results ppf r] prints each tier under a heading. *)
