@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JMAP dates and times.

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-1.4} RFC 8620
     Section 1.4} defines two string types. A [Date] is an
    {{:https://datatracker.ietf.org/doc/html/rfc3339} RFC 3339} date and time in
    any time zone. A [UTCDate] is one whose time zone offset must be written
    [Z]. Both are in the normalised form the section requires, so any letter in
    them is upper case.

    @canonical Jmap.Proto.Date *)

type t = Ptime.t
(** The type for instants. The type equality is exposed so that a caller may use
    every [Ptime] operation on a decoded value. *)

val of_string : string -> (t, string) result
(** [of_string s] is the instant [s], an RFC 3339 date and time in any time
    zone. The error holds a human readable message when [s] is not one. *)

val to_string : t -> string
(** [to_string t] is [t] as an RFC 3339 date and time with the time zone offset
    [Z]. The fraction of a second is written with the least number of decimal
    digits that represents it exactly, and omitted when it is zero, as RFC 8620
    Section 1.4 requires. *)

val of_utc_string : string -> (t, string) result
(** [of_utc_string s] is {!of_string} except that the time zone offset of [s]
    must be written [Z]. The error holds a human readable message otherwise. *)

val to_utc_string : t -> string
(** [to_utc_string t] is {!to_string}, which already writes the time zone offset
    as [Z]. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a [Date], decoding with {!of_string} and encoding
    with {!to_string}. *)

val utc_jsont : t Jsont.t
(** [utc_jsont] is the codec for a [UTCDate], decoding with {!of_utc_string} and
    encoding with {!to_utc_string}. *)
