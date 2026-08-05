(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** FTS5 full-text search index for Arod content.

    Uses one FTS5 table per entry kind so that kind filtering queries only
    the relevant tables. *)

type t
(** A handle to the search database. *)

type result = {
  slug : string;
  kind : string;
  url : string;
  title : string;
  snippet : string;
  date : string;
  rank : float;
  parent_slugs : string list;
  tags : string list;
}
(** A search result with BM25 ranking and snippet.
    For links, [parent_slugs] lists the Bushel entry slugs that contain
    this link (e.g. notes or papers). Empty for non-link entries.
    [tags] lists the entry's tags (exact, from entry_tags table). *)

val create : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [create ~sw path] opens or creates the search database at [path]. *)

val create_memory : sw:Eio.Switch.t -> unit -> t
(** [create_memory ~sw ()] creates an in-memory search database.
    Ideal for the server where the index is rebuilt on startup. *)

val open_readonly : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [open_readonly ~sw path] opens the search database read-only for queries. *)

val rebuild : t -> Arod.Ctx.t -> unit
(** [rebuild t ctx] drops and rebuilds all per-kind search tables from all
    entries and links in [ctx]. *)

val search : t -> ?limit:int -> string -> result list
(** [search t ?limit input] parses [input] using the search syntax and
    returns results from the relevant per-kind FTS5 tables. The syntax
    supports:

    - Plain words: matched against title, body, and tags
    - [kind:paper] (or [kind:note], [kind:project], [kind:idea],
      [kind:video], [kind:link]) — restrict to specific entry types
    - ["exact phrase"] — match the exact phrase
    - [prefix*] — prefix matching
    - [#tag] — exact tag matching (uses entry_tags table, not FTS)

    Multiple kind filters are supported (queries their union).
    [#tag] tokens can be mixed with text: [#ocaml memory] finds entries
    tagged "ocaml" that also match FTS "memory".
    Returns empty if no text query or tags are provided. *)

val search_tags : t -> ?kinds:string list -> ?limit:int -> string list -> result list
(** [search_tags t ?kinds ?limit tags] returns entries matching ALL given
    tags exactly. Uses the entry_tags table for exact matching. *)

val all_tags : t -> (string * int) list
(** [all_tags t] returns all unique tags with their counts, sorted by
    count descending. *)

val kinds : string list
(** The valid kind values: paper, note, project, idea, video, link. *)

val pp_result : Format.formatter -> result -> unit
(** Pretty-print a single search result. *)
