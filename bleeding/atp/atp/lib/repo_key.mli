(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** AT Protocol repository key validation.

    Keys in AT Protocol repositories have the format [collection/rkey] where:
    - [collection] is a reverse-DNS namespace (e.g., [app.bsky.feed.post])
    - [rkey] is a record key within that collection (often a TID)

    Both parts must be non-empty and contain only valid characters:
    [a-zA-Z0-9_~-:.] and total length at most 1024. *)

(** {1 Types} *)

type t = string
(** A validated repository key. *)

type error = [ `Invalid_repo_key of string ]
(** Repository key validation error. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

(** {1 Validation} *)

val is_valid : string -> bool
(** [is_valid key] returns [true] if [key] is a valid repository key. *)

val of_string : string -> (t, error) result
(** [of_string s] validates and returns a repository key. *)

val of_string_exn : string -> t
(** [of_string_exn s] validates and returns a repository key.
    @raise Invalid_argument if invalid. *)

val validate_exn : string -> unit
(** [validate_exn s] validates a repository key.
    @raise Invalid_argument if invalid.

    This is a convenience for validation-only use cases. *)

val to_string : t -> string
(** [to_string t] returns the string representation. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : t Fmt.t
