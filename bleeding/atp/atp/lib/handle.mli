(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** AT Protocol handle validation.

    Handles are domain-name-like identifiers for AT Protocol users. They follow
    DNS hostname rules with some AT Protocol-specific constraints.

    Examples: ["alice.bsky.social"], ["example.com"]

    Rules:
    - Max 253 characters total
    - Segments separated by [.]
    - Each segment: 1-63 chars, alphanumeric + hyphen
    - No leading/trailing hyphen in segments
    - At least 2 segments
    - TLD cannot be all numeric *)

(** {1 Types} *)

type t
(** Opaque handle value. *)

type error = [ `Invalid_handle of string ]
(** Handle validation error. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

(** {1 Parsing} *)

val of_string : string -> (t, error) result
(** [of_string s] validates and parses a handle string. *)

val of_string_exn : string -> t
(** [of_string_exn s] parses a handle, raising on invalid format.

    @raise Invalid_argument on invalid format. *)

(** {1 Serialization} *)

val to_string : t -> string
(** [to_string h] returns the handle string. *)

(** {1 Validation} *)

val is_valid : string -> bool
(** [is_valid s] is [true] if [s] is a valid handle. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
val compare : t -> t -> int

(** {1 Pretty Printing} *)

val pp : t Fmt.t
