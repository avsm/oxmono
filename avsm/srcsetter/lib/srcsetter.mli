(** Image entry management for responsive image generation.

    This module provides types and functions for managing image entries
    that contain metadata about responsive images including their dimensions
    and size variants. *)

(** {1 Types} *)

(** String map for storing image variants keyed by filename.

    This is not [Map.Make (String)]. A variant map is built once and then only
    walked in key order, and [Map.S] declares no kind on its [t], which would
    leave {!t} unable to cross into a function marked [portable]. *)
module MS : sig
  type 'a t : immutable_data with 'a
  (** A finite map from filenames to ['a], in increasing filename order. *)

  val empty : 'a t
  (** [empty] is the map with no bindings. *)

  val of_list : (string * 'a) list -> 'a t
  (** [of_list l] is the map holding the bindings of [l]. A key bound more than
      once in [l] keeps the binding that appears last. *)

  val bindings : 'a t -> (string * 'a) list @@ portable
  (** [bindings m] is the bindings of [m] in increasing key order. *)

  val cardinal : 'a t -> int
  (** [cardinal m] is the number of bindings in [m]. *)

  val fold : (string -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  (** [fold f m acc] folds [f] over the bindings of [m] in increasing key
      order. *)
end

(** An image entry representing a source image and its generated variants.

    Each entry tracks:
    - The output filename ([name])
    - A URL-safe identifier ([slug])
    - The original source path ([origin])
    - Image dimensions as [(width, height)]
    - A map of variant filenames to their dimensions *)
type t = {
  name : string;
  slug : string;
  origin : string;
  dims : int * int;
  variants : (int * int) MS.t;
}

(** {1 Constructors} *)

val v : string -> string -> string -> (int * int) MS.t -> int * int -> t
(** [v name slug origin variants dims] creates a new image entry.

    @param name The output filename (e.g., ["photo.webp"])
    @param slug A URL-safe identifier derived from the filename
    @param origin The original source file path
    @param variants Map of variant filenames to their [(width, height)] dimensions
    @param dims The base image dimensions as [(width, height)] *)

(** {1 Accessors} *)

val name : t -> string
(** [name entry] returns the output filename. *)

val slug : t -> string
(** [slug entry] returns the URL-safe identifier. *)

val origin : t -> string
(** [origin entry] returns the original source file path. *)

val dims : t -> int * int
(** [dims entry] returns the base image dimensions as [(width, height)]. *)

val variants : t -> (int * int) MS.t
(** [variants entry] returns the map of variant filenames to dimensions. *)

(** {1 JSON Serialization} *)

val json_t : t Jsont.t
(** JSON codec for a single image entry. *)

val list : t list Jsont.t
(** JSON codec for a list of image entries. *)

val list_to_json : t list -> (string, string) result
(** [list_to_json entries] serializes a list of entries to a JSON string.

    Returns [Ok json_string] on success, or [Error message] if encoding fails. *)

val list_of_json : string -> (t list, string) result
(** [list_of_json json_string] parses a JSON string into a list of entries.

    Returns [Ok entries] on success, or [Error message] if parsing fails. *)
