(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Temporal validity support for contact fields.

    This module provides types and functions for managing time-bounded
    information in contacts, such as emails valid only during certain
    employment periods. *)

(** Date represented as a Ptime.date tuple (year, month, day).

    When parsing from strings, partial dates are normalized:
    - Year: ["2001"] → (2001, 1, 1)
    - Year-Month: ["2001-01"] → (2001, 1, 1)
    - Full date: ["2001-01-15"] → (2001, 1, 15) *)
type date = Ptime.date

(** {1 Date Conversion} *)

(** [parse_date_string s] parses an ISO 8601 date string.

    Accepts various formats with partial date support:
    - "2001" (year only) → (2001, 1, 1)
    - "2001-01" (year-month) → (2001, 1, 1)
    - "2001-01-15" (full date) → (2001, 1, 15)

    Returns [None] if the string is not a valid date format. *)
val parse_date_string : string -> date option

(** [format_date date] formats a date as ISO 8601 (YYYY-MM-DD).

    {b Example:} [format_date (2001, 1, 15)] returns ["2001-01-15"] *)
val format_date : date -> string

(** {1 Temporal Ranges} *)

(** A temporal range indicating validity period. *)
type range = {
  from: date option;   (** Start date (inclusive). [None] means from the beginning. *)
  until: date option;  (** End date (exclusive). [None] means continuing/indefinite. *)
}

(** {1 Range Construction} *)

(** [make ?from ?until ()] creates a temporal range. *)
val make : ?from:date -> ?until:date -> unit -> range

(** [always] is a range that is always valid (no from/until bounds). *)
val always : range

(** {1 Range Queries} *)

(** [valid_at range ~date] checks if [range] is valid at the given [date].

    - [None] range means always valid
    - [None] from means valid from beginning
    - [None] until means valid continuing *)
val valid_at : range option -> date:date -> bool

(** [overlaps r1 r2] checks if two ranges overlap in time. *)
val overlaps : range -> range -> bool

(** [is_current range] checks if range is valid at the current date.
    Uses today's date for the check. *)
val is_current : range option -> bool

(** {1 List Filtering} *)

(** [current ~get list] returns the first current/valid item from [list].

    @param get Function to extract the temporal range from an item.
    Returns the first item where the range is currently valid,
    or the first item without temporal bounds if none are current. *)
val current : get:('a -> range option) -> 'a list -> 'a option

(** [at_date ~get ~date list] filters [list] to items valid at [date].

    @param get Function to extract the temporal range from an item.
    @param date The date to check validity against. *)
val at_date : get:('a -> range option) -> date:date -> 'a list -> 'a list

(** [filter ~get ~from ~until list] filters [list] to items overlapping the period.

    Returns items whose temporal range overlaps with the given period. *)
val filter : get:('a -> range option) -> from:date option -> until:date option ->
             'a list -> 'a list

(** {1 JSON Encoding} *)

(** [json_t] is the jsont encoder/decoder for temporal ranges.

    Encodes as a JSON object with optional [from] and [until] fields:
    {[ { "from": "2001-01", "until": "2003-12" } ]}

    Empty object [\{\}] or missing field represents [always]. *)
val json_t : range Jsont.t
