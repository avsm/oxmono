(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** External link tracking for Bushel.

    A link records one external URL the site points at, together with what
    the Karakeep bookmarking service knows about it and which Bushel entries
    mention it. Links live in one YAML file that the sync pipeline rewrites.

    The record and its accessors are portable, which is what lets a renderer
    read a link from inside a function marked [portable]. The two file
    operations are not, and neither are the three URL predicates. That last
    one costs something, because {!is_paper_url} runs at render time on the
    links listing. {!is_doi_url} is held back by [Astring] alone.
    {!is_academic_url} is held back twice over, by [Astring] and by the opam
    [Uri] that the rest of this render path has moved off, and {!is_paper_url}
    inherits both. Moving the host and path reads onto [uriz] and the three
    affix tests onto [String] would lift all three. *)

@@ portable

(** {1 Types} *)

type karakeep_data = {
  remote_url : string;  (** URL as Karakeep stores it. *)
  id : string;  (** Karakeep bookmark identifier. *)
  tags : string list;
  metadata : (string * string) list;  (** In increasing key order. *)
}
(** What Karakeep knows about a link. *)

type bushel_data = {
  slugs : string list;  (** Entries that mention the link. *)
  tags : string list;
}
(** What this site knows about a link. *)

type t = {
  url : string;
  date : Ptime.date;
  description : string;
  karakeep : karakeep_data option;
  bushel : bushel_data option;
}
(** An external link. The record is public because the sync pipeline builds
    one field by field and the renderers read fields directly. *)

type ts = t list
(** A list of links. *)

(** {1 Accessors} *)

val url : t -> string
(** [url l] is the URL of [l]. *)

val date : t -> Ptime.date
(** [date l] is the date [l] was first recorded. *)

val description : t -> string
(** [description l] is the description of [l], or the empty string if the
    file records none. *)

(** {1 URL classification} *)

val is_doi_url : string -> bool @@ nonportable
(** [is_doi_url u] is [true] if [u] resolves through a DOI resolver. *)

val is_academic_url : string -> bool @@ nonportable
(** [is_academic_url u] is [true] if [u] is on a publisher or preprint host
    that the Zotero translation server can resolve. Matching ignores a
    leading ["www."] and accepts any subdomain of a listed host. Some hosts
    match only under a given path prefix, so that a journal's front page is
    not taken for an article. *)

val is_paper_url : string -> bool @@ nonportable
(** [is_paper_url u] is [is_doi_url u || is_academic_url u]. *)

(** {1 Files and merging} *)

val load_links_file : string -> t list @@ nonportable
(** [load_links_file path] is the links recorded in the YAML file at [path],
    or the empty list if the file is missing or does not parse. *)

val save_links_file : string -> t list -> unit @@ nonportable
(** [save_links_file path links] writes [links] to [path] as YAML,
    overwriting what was there. *)

val merge_links : ?prefer_new_date:bool -> t list -> t list -> t list
(** [merge_links existing new_links] is the union of the two lists keyed by
    URL, sorted most recent first. Where both sides hold a URL the new
    description wins unless it is empty, and the date kept is the earlier of
    the two, which is the date the link was first recorded. The Bushel slugs
    and tags are unioned. The Karakeep tags and metadata are unioned only when
    both records name the same [remote_url], and the new record replaces the
    old one wholesale when they do not. Set [prefer_new_date] to take the new
    date whatever the old one was, which is what a re-import from an
    authoritative source wants. *)
