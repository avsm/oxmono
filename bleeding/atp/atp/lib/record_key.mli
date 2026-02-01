(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** AT Protocol record key validation.

    Record keys identify individual records within a collection. They are used
    as the final component of AT-URIs.

    Rules:
    - 1-512 characters
    - Allowed chars: [a-zA-Z0-9._:~-]
    - Cannot be [.] or [..]

    Examples: ["self"], ["3jzfcijpj2z2a"], ["example.com"] *)

(** {1 Types} *)

type t
(** Opaque record key value. *)

type error = [ `Invalid_record_key of string ]
(** Record key validation error. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

(** {1 Parsing} *)

val of_string : string -> (t, error) result
(** [of_string s] validates and parses a record key string. *)

val of_string_exn : string -> t
(** [of_string_exn s] parses a record key, raising on invalid format.

    @raise Invalid_argument on invalid format. *)

(** {1 Serialization} *)

val to_string : t -> string
(** [to_string rkey] returns the record key string. *)

(** {1 Validation} *)

val is_valid : string -> bool
(** [is_valid s] is [true] if [s] is a valid record key. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
val compare : t -> t -> int

(** {1 Pretty Printing} *)

val pp : t Fmt.t
