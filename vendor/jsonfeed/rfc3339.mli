(*---------------------------------------------------------------------------
   Copyright (c) 2024 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** RFC 3339 date/time handling for JSON Feed.

    Provides parsing, formatting, and jsont combinators for RFC 3339 timestamps
    as required by the JSON Feed specification.

    @see <https://www.rfc-editor.org/rfc/rfc3339> RFC 3339 *)

val jsont : Ptime.t Jsont.t
(** [jsont] is a bidirectional JSON type for RFC 3339 timestamps.

    On decode: accepts JSON strings in RFC 3339 format (e.g.,
    "2024-11-03T10:30:00Z") On encode: produces UTC timestamps with 'Z' suffix

    {b Example:}
    {[
      let time = Ptime.of_float_s (Unix.time ()) |> Option.get in
      Jsont_bytesrw.encode_string Rfc3339.jsont time
    ]} *)

val parse : string -> Ptime.t option
(** [parse s] parses an RFC 3339 timestamp string.

    Accepts various formats:
    - "2024-11-03T10:30:00Z" (UTC)
    - "2024-11-03T10:30:00-08:00" (with timezone offset)
    - "2024-11-03T10:30:00.123Z" (with fractional seconds)

    Returns [None] if the string is not valid RFC 3339. *)

val format : Ptime.t -> string
(** [format t] formats a timestamp as RFC 3339.

    Always uses UTC timezone (Z suffix) and includes fractional seconds if the
    timestamp has sub-second precision.

    {b Example output:} ["2024-11-03T10:30:45.123Z"] *)

val pp : Format.formatter -> Ptime.t -> unit
(** [pp ppf t] pretty prints a timestamp in RFC 3339 format. *)
