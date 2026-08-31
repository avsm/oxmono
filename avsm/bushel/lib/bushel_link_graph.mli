(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Links between entries and external URLs. *)

@@ portable

type entry_type = [ `Paper | `Project | `Note | `Idea | `Video | `Contact ]
(** The kind of an internal link target. *)

type internal_link = {
  source : string;  (** Slug of the entry the link is written in. *)
  target : string;  (** Slug or contact handle the link names. *)
  target_type : entry_type;  (** Kind of the target. *)
}
(** A link from one entry to another entry or to a contact. *)

type external_link = {
  source : string;  (** Slug of the entry the link is written in. *)
  domain : string;  (** Host of {!url}, as {!Bushel_util.extract_domain} reads it. *)
  url : string;  (** The link as written. *)
}
(** A link from an entry out to the web. *)

type t : immutable_data
(** A link graph. *)

(** {1 Constructors} *)

val empty : t
(** [empty] is a graph with no links. *)

val v :
  internal_links:internal_link list ->
  external_links:external_link list ->
  t
  @@ nonportable
(** [v ~internal_links ~external_links] is the graph over the supplied links. *)

(** {1 Queries} *)

val backlinks : t -> string -> string list
(** [backlinks g slug] is the slugs of the entries that link to [slug], sorted
    and without repeats. *)

val outbound : t -> string -> string list
(** [outbound g slug] is the slugs and contact handles that [slug] links to,
    sorted and without repeats. *)

val external_urls : t -> string -> string list
(** [external_urls g slug] is the web URLs that [slug] links to, sorted and
    without repeats. *)

val all_external_links : t -> external_link list
(** [all_external_links g] is every external link of [g], in the order it was
    built with. *)

(** {1 Utilities} *)

val entry_type_of_entry :
  [< `Paper of 'a | `Project of 'b | `Note of 'c | `Idea of 'd | `Video of 'e ] ->
  entry_type
(** [entry_type_of_entry e] is the {!entry_type} of [e]. *)

val entry_type_to_string : entry_type -> string
(** [entry_type_to_string k] is [k] in lower case. *)

val pp : Format.formatter -> t -> unit @@ nonportable
(** [pp ppf g] prints the size of [g] on [ppf]. *)
