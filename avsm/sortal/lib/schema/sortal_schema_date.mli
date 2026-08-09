(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** ISO 8601 calendar dates.

    Dates are used only to bound an affiliation. No other field in the
    schema carries a date. *)

type t = Ptime.date
(** A date as a [(year, month, day)] triple. *)

val parse : string -> t option
(** [parse s] is the date [s] denotes, or [None] if [s] is not an ISO 8601
    date. A year alone and a year and month are accepted, and are completed
    with the first day of the implied period, so ["2001"] and ["2001-01"]
    are both [(2001, 1, 1)]. A date that names a day outside its month, such
    as ["2001-02-30"], is rejected. *)

val to_string : t -> string
(** [to_string d] is [d] as an ISO 8601 date, always in [YYYY-MM-DD] form. *)

val compare : t -> t -> int
(** [compare a b] orders dates chronologically. *)

val json_t : t Jsont.t
(** [json_t] maps a date to and from its {!to_string} form. Decoding a string
    that {!parse} rejects is a decoding error. *)
