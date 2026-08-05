(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** AT Protocol URI validation and parsing.

    AT-URIs identify resources in the AT Protocol network.

    Format: [at://<authority>/<collection>/<rkey>]

    Where:
    - authority: DID or handle
    - collection: NSID (optional)
    - rkey: record key (optional, requires collection)

    Examples:
    - [at://did:plc:z72i7hdynmk6r22z27h6tvur]
    - [at://alice.bsky.social/app.bsky.feed.post/3jzfcijpj2z2a] *)

(** {1 Types} *)

type t
(** Opaque AT-URI value. *)

type error = [ `Invalid_at_uri of string ]
(** AT-URI validation error. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

(** {1 Parsing} *)

val of_string : string -> (t, error) result
(** [of_string s] validates and parses an AT-URI string. *)

val of_string_exn : string -> t
(** [of_string_exn s] parses an AT-URI, raising on invalid format.

    @raise Invalid_argument on invalid format. *)

(** {1 Serialization} *)

val to_string : t -> string
(** [to_string uri] returns the AT-URI string. *)

(** {1 Construction} *)

val make :
  authority:string ->
  ?collection:string ->
  ?rkey:string ->
  unit ->
  (t, error) result
(** [make ~authority ?collection ?rkey ()] constructs an AT-URI.

    @param authority A DID or handle string.
    @param collection An NSID string (optional).
    @param rkey A record key string (optional, requires collection). *)

(** {1 Accessors} *)

val authority : t -> string
(** [authority uri] returns the authority (DID or handle). *)

val collection : t -> string option
(** [collection uri] returns the collection NSID if present. *)

val rkey : t -> string option
(** [rkey uri] returns the record key if present. *)

(** {1 Validation} *)

val is_valid : string -> bool
(** [is_valid s] is [true] if [s] is a valid AT-URI. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
val compare : t -> t -> int

(** {1 Pretty Printing} *)

val pp : t Fmt.t
