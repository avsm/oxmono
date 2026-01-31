(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Unknown fields for preserving extra JSON object members during
    round-tripping.

    This module provides an opaque type for storing unknown JSON fields as an
    association list. This is useful for preserving fields that are not part of
    the defined schema but should be maintained when reading and writing JSON.
*)

type t
(** The opaque type of unknown fields, stored as an association list of field
    names to JSON values. *)

val empty : t
(** [empty] is an empty set of unknown fields. *)

val is_empty : t -> bool
(** [is_empty t] returns [true] if there are no unknown fields stored in [t]. *)

val of_assoc : (string * Jsont.json) list -> t
(** [of_assoc assoc] creates unknown fields from an association list. *)

val to_assoc : t -> (string * Jsont.json) list
(** [to_assoc t] returns the association list of unknown fields. *)

val jsont : t Jsont.t
(** [jsont] is a codec for encoding and decoding unknown fields to/from JSON. *)

val mems : (t, Jsont.json, Jsont.mem list) Jsont.Object.Mems.map
(** [mems] is a mems codec for use with [Jsont.Object.keep_unknown]. *)
