(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Timestamp-based identifiers for AT Protocol.

    TIDs are 13-character identifiers using a custom base32 charset. They encode
    a microsecond timestamp (11 chars) and a clock ID (2 chars).

    Format: "234567abcdefghijklmnopqrstuvwxyz" charset
    - Characters 0-10: microsecond timestamp (base32)
    - Characters 11-12: clock ID 0-1023 (base32) *)

(** {1 Types} *)

type t
(** Opaque TID value. *)

(** {1 Errors} *)

type error =
  [ `Tid_invalid_length of int
  | `Tid_invalid_format of string
  | `Tid_timestamp_out_of_range
  | `Tid_clockid_out_of_range of int ]
(** TID errors. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

type Eio.Exn.err += E of error  (** Eio exception wrapper for TID errors. *)

(** {1 Creation} *)

val now : _ Eio.Time.clock -> t
(** [now clock] generates a TID from the current time with random clock ID. *)

val create : unit -> t
(** [create ()] generates a TID from the current time with random clock ID. Uses
    [Unix.gettimeofday] for the timestamp. Prefer [now] when an Eio clock is
    available. *)

val of_timestamp_us : ?clockid:int -> int64 -> t
(** [of_timestamp_us ?clockid us] creates a TID from microsecond timestamp.

    @param clockid Clock ID 0-1023 (default: random).
    @raise Eio.Io if timestamp out of range (0 to 2{^53}-1) or clockid invalid. *)

val of_timestamp_ms : ?clockid:int -> int64 -> t
(** [of_timestamp_ms ?clockid ms] creates a TID from millisecond timestamp.

    Adds random microseconds within the millisecond.

    @param clockid Clock ID 0-1023 (default: random).
    @raise Eio.Io if timestamp out of range. *)

(** {1 Parsing} *)

val of_string : string -> t
(** [of_string s] parses a 13-character TID string.

    @raise Eio.Io on invalid format. *)

val of_string_opt : string -> t option
(** [of_string_opt s] parses a TID, returning [None] on invalid format. *)

(** {1 Serialization} *)

val to_string : t -> string
(** [to_string tid] returns the 13-character TID string. *)

(** {1 Accessors} *)

val to_timestamp_us : t -> int64 * int
(** [to_timestamp_us tid] returns [(microsecond_timestamp, clockid)]. *)

val to_timestamp_ms : t -> int64 * int
(** [to_timestamp_ms tid] returns [(millisecond_timestamp, clockid)]. *)

val timestamp_us : t -> int64
(** [timestamp_us tid] returns just the microsecond timestamp. *)

val clock_id : t -> int
(** [clock_id tid] returns just the clock ID. *)

(** {1 Validation} *)

val is_valid : string -> bool
(** [is_valid s] is [true] if [s] is a valid TID string. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

(** {1 Pretty Printing} *)

val pp : t Fmt.t
