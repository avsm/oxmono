(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML tags for type information *)

type t = {
  handle : string;  (** e.g., "!" or "!!" or "!foo!" *)
  suffix : string;  (** e.g., "str", "int", "custom/type" *)
}

val make : handle:string -> suffix:string -> t
(** Create a tag from handle and suffix *)

val of_string : string -> t option
(** Parse a tag string *)

val to_string : t -> string
(** Convert tag to string representation *)

val to_uri : t -> string
(** Convert tag to full URI representation *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a tag *)

val equal : t -> t -> bool
(** Test equality of two tags *)

val compare : t -> t -> int
(** Compare two tags *)

(** {2 Standard Tags} *)

val null : t
val bool : t
val int : t
val float : t
val str : t
val seq : t
val map : t
val binary : t
val timestamp : t

(** {2 Tag Predicates} *)

val is_null : t -> bool
val is_bool : t -> bool
val is_int : t -> bool
val is_float : t -> bool
val is_str : t -> bool
val is_seq : t -> bool
val is_map : t -> bool
