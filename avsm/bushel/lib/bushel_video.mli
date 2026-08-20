(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Video entry type for Bushel.

    A video is a talk recording or screencast held on a PeerTube instance.
    Its slug is its UUID. Everything here is portable except
    {!of_frontmatter} and {!pp}, which is what lets a renderer read a video
    from inside a function marked [portable]. *)

@@ portable

(** {1 Types} *)

type t = {
  slug : string;  (** Equal to {!field-uuid}. *)
  title : string;
  published_date : Ptime.t;
  uuid : string;  (** PeerTube identifier. *)
  description : string;  (** Bushel markdown body. *)
  url : string;  (** Watch URL on the hosting instance. *)
  talk : bool;  (** Recording of a talk rather than a screencast. *)
  vertical : bool;  (** Portrait aspect ratio, rendered narrow. *)
  paper : string option;  (** Slug of the paper this talk presents. *)
  project : string option;  (** Slug of the owning project. *)
  tags : string list;
  social : Bushel_types.social option;
}
(** A video entry. The record is public because the loader builds one field by
    field and the renderers read fields directly. *)

type ts = t list
(** A list of videos, in load order. *)

(** {1 Accessors} *)

val slug : t -> string
(** [slug v] is the slug of [v], which is its UUID. *)

val title : t -> string
(** [title v] is the title of [v]. *)

val uuid : t -> string
(** [uuid v] is the identifier of [v] on its hosting instance. *)

val url : t -> string
(** [url v] is the watch URL of [v]. *)

val description : t -> string
(** [description v] is the Bushel markdown body of [v]. *)

val talk : t -> bool
(** [talk v] is [true] if [v] records a talk rather than a screencast. *)

val vertical : t -> bool
(** [vertical v] is [true] if [v] is portrait and must be rendered narrow. *)

val paper : t -> string option
(** [paper v] is the slug of the paper [v] presents, if any. *)

val project : t -> string option
(** [project v] is the slug of the project owning [v], if any. *)

val tags : t -> string list
(** [tags v] is the tags of [v]. *)

val social : t -> Bushel_types.social option
(** [social v] is the discussion links recorded for [v]. *)

val date : t -> Ptime.date
(** [date v] is the publication date of [v]. *)

val datetime : t -> Ptime.t
(** [datetime v] is the publication timestamp of [v]. *)

(** {1 Ordering} *)

val compare : t -> t -> int
(** [compare a b] orders by publication timestamp, most recent first. *)

(** {1 Parsing and printing} *)

val of_frontmatter : Frontmatter.t -> (t, string) result @@ nonportable
(** [of_frontmatter fm] is the video described by [fm]. The slug is taken
    from the [uuid] field and the description from the body. *)

val pp : Format.formatter -> t -> unit @@ nonportable
(** [pp ppf v] prints [v] to [ppf] as a styled multi-line summary. *)
