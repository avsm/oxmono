(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Feed items in a JSON Feed.

    An item represents a single entry in a feed, such as a blog post, podcast
    episode, or microblog entry. Each item must have a unique identifier and
    content.

    @see <https://www.jsonfeed.org/version/1.1/> JSON Feed Specification *)

type t
(** The type representing a feed item. *)

type content = [ `Html of string | `Text of string | `Both of string * string ]
(** Content representation for an item.

    The JSON Feed specification requires that each item has at least one form of
    content. This type enforces that requirement at compile time.

    - [`Html s]: Item has HTML content only
    - [`Text s]: Item has plain text content only
    - [`Both (html, text)]: Item has both HTML and plain text versions *)

(** {1 Unknown Fields} *)

module Unknown : sig
  type t = Jsont.json
  (** Unknown/unrecognized JSON object members as a generic JSON object. Useful
      for preserving fields from custom extensions or future spec versions. *)

  val empty : t
  (** [empty] is the empty list of unknown fields. *)

  val is_empty : t -> bool
  (** [is_empty u] returns [true] if there are no unknown fields. *)
end

(** {1 Jsont Type} *)

val jsont : t Jsont.t
(** Declarative JSON type for feed items.

    Maps JSON objects with "id" (required), content fields, and various optional
    metadata. The content must have at least one of "content_html" or
    "content_text". *)

(** {1 Construction} *)

val create :
  id:string ->
  content:content ->
  ?url:string ->
  ?external_url:string ->
  ?title:string ->
  ?summary:string ->
  ?image:string ->
  ?banner_image:string ->
  ?date_published:Ptime.t ->
  ?date_modified:Ptime.t ->
  ?authors:Author.t list ->
  ?tags:string list ->
  ?language:string ->
  ?attachments:Attachment.t list ->
  ?references:Reference.t list ->
  ?unknown:Unknown.t ->
  unit ->
  t

(** {1 Accessors} *)

val id : t -> string
val content : t -> content
val content_html : t -> string option
val content_text : t -> string option
val url : t -> string option
val external_url : t -> string option
val title : t -> string option
val summary : t -> string option
val image : t -> string option
val banner_image : t -> string option
val date_published : t -> Ptime.t option
val date_modified : t -> Ptime.t option
val authors : t -> Author.t list option
val tags : t -> string list option
val language : t -> string option
val attachments : t -> Attachment.t list option
val references : t -> Reference.t list option
val unknown : t -> Unknown.t

(** {1 Comparison} *)

val equal : t -> t -> bool
val compare : t -> t -> int

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
val pp_summary : Format.formatter -> t -> unit
