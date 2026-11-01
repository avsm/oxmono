(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Decentralized Identifier (DID) validation.

    DIDs are globally unique identifiers that resolve to DID Documents. AT
    Protocol supports [did:plc] and [did:web] methods.

    Format: [did:<method>:<method-specific-id>]

    Examples:
    - [did:plc:z72i7hdynmk6r22z27h6tvur]
    - [did:web:example.com] *)

(** {1 Types} *)

type t
(** Opaque DID value. *)

type error = [ `Invalid_did of string ]
(** DID validation error. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

(** {1 Parsing} *)

val of_string : string -> (t, error) result
(** [of_string s] validates and parses a DID string. *)

val of_string_exn : string -> t
(** [of_string_exn s] parses a DID, raising on invalid format.

    @raise Invalid_argument on invalid format. *)

(** {1 Serialization} *)

val to_string : t -> string
(** [to_string did] returns the DID string. *)

(** {1 Accessors} *)

val method_ : t -> string
(** [method_ did] returns the DID method (e.g., "plc", "web"). *)

val method_specific_id : t -> string
(** [method_specific_id did] returns the method-specific identifier. *)

(** {1 Validation} *)

val is_valid : string -> bool
(** [is_valid s] is [true] if [s] is a valid DID. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
val compare : t -> t -> int

(** {1 Pretty Printing} *)

val pp : t Fmt.t
