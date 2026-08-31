(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** External links and their imported metadata. *)

@@ portable

(** {1 Types} *)

type karakeep_data = {
  remote_url : string;  (** URL as Karakeep stores it. *)
  id : string;  (** Karakeep bookmark identifier. *)
  tags : string list;
  metadata : (string * string) list;  (** In increasing key order. *)
}
(** Karakeep metadata for a link. *)

type bushel_data = {
  slugs : string list;  (** Entries that mention the link. *)
  tags : string list;
}
(** Bushel metadata for a link. *)

type t = {
  url : string;
  date : Ptime.date;
  description : string;
  karakeep : karakeep_data option;
  bushel : bushel_data option;
}
(** An external link. *)

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

val is_doi_url : string -> bool
(** [is_doi_url u] is [true] if [u] resolves through a DOI resolver. *)

val is_academic_url : string -> bool
(** [is_academic_url u] is [true] if [u] is on a publisher or preprint host
    supported by the Zotero translation server. Subdomains are accepted and
    malformed URI references return [false]. *)

val is_paper_url : string -> bool
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
    URL, sorted most recent first. New non-empty descriptions win, dates keep
    the earliest value, and tags, slugs and compatible Karakeep metadata are
    combined. [prefer_new_date] always keeps the new date. *)
