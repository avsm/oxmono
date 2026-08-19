(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Author information for JSON Feed items and feeds.

    An author object provides information about the creator of a feed or item.
    According to the JSON Feed 1.1 specification, at least one field must be
    present when an author object is included.

    @see <https://www.jsonfeed.org/version/1.1/> JSON Feed Specification *)

type t
(** The type representing an author. *)

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
(** Declarative JSON type for authors.

    Maps JSON objects with optional "name", "url", and "avatar" fields. At least
    one field must be present during decoding. *)

(** {1 Construction} *)

val create :
  ?name:string ->
  ?url:string ->
  ?avatar:string ->
  ?unknown:Unknown.t ->
  unit ->
  t
(** [create ?name ?url ?avatar ?unknown ()] creates an author.

    At least one of the optional parameters must be provided, otherwise the
    function will raise [Invalid_argument].

    @param name The author's name
    @param url URL of the author's website or profile
    @param avatar
      URL of the author's avatar image (should be square, 512x512 or larger)
    @param unknown Unknown/custom fields for extensions (default: empty)

    {b Examples:}
    {[
      let author = Author.create ~name:"Jane Doe" ()
      let author = Author.create ~name:"Jane Doe" ~url:"https://janedoe.com" ()

      let author =
        Author.create ~name:"Jane Doe" ~url:"https://janedoe.com"
          ~avatar:"https://janedoe.com/avatar.png" ()
    ]} *)

(** {1 Accessors} *)

val name : t -> string option
(** [name t] returns the author's name, if set. *)

val url : t -> string option
(** [url t] returns the author's URL, if set. *)

val avatar : t -> string option
(** [avatar t] returns the author's avatar URL, if set. *)

val unknown : t -> Unknown.t
(** [unknown t] returns unrecognized fields from the JSON. *)

(** {1 Predicates} *)

val is_valid : t -> bool
(** [is_valid t] checks if the author has at least one field set.

    This should always return [true] for authors created via {!create}, but may
    be useful when parsing from external sources. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] tests equality between two authors. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] pretty prints an author to the formatter.

    The output is human-readable and suitable for debugging.

    {b Example output:}
    {v Jane Doe <https://janedoe.com> v} *)
