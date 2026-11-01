(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Namespaced Identifier (NSID) validation.

    NSIDs are reverse-DNS-style identifiers used for AT Protocol methods and
    record types.

    Format: [<authority>.<name>] where authority is reverse domain.

    Examples:
    - [com.atproto.repo.createRecord]
    - [app.bsky.feed.post]

    Rules:
    - Max 317 characters total
    - Segments separated by [.]
    - At least 3 segments (authority + name)
    - Authority segments: alphanumeric + hyphen, can start with digit
    - Name segment (last): alphanumeric only, must start with letter
    - Each segment max 63 characters *)

(** {1 Types} *)

type t
(** Opaque NSID value. *)

type error = [ `Invalid_nsid of string ]
(** NSID validation error. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

(** {1 Parsing} *)

val of_string : string -> (t, error) result
(** [of_string s] validates and parses an NSID string. *)

val of_string_exn : string -> t
(** [of_string_exn s] parses an NSID, raising on invalid format.

    @raise Invalid_argument on invalid format. *)

(** {1 Serialization} *)

val to_string : t -> string
(** [to_string nsid] returns the NSID string. *)

(** {1 Accessors} *)

val authority : t -> string
(** [authority nsid] returns the authority part (all but last segment). For
    [com.atproto.repo.createRecord], returns [com.atproto.repo]. *)

val name : t -> string
(** [name nsid] returns the name part (last segment). For
    [com.atproto.repo.createRecord], returns [createRecord]. *)

val segments : t -> string list
(** [segments nsid] returns all segments as a list. *)

(** {1 Validation} *)

val is_valid : string -> bool
(** [is_valid s] is [true] if [s] is a valid NSID. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
val compare : t -> t -> int

(** {1 Pretty Printing} *)

val pp : t Fmt.t
