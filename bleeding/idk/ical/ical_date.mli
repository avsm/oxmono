@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Dates and times.

    The DATE, TIME, DATE-TIME and UTC-OFFSET value types of
    {{:https://www.rfc-editor.org/rfc/rfc5545.html} RFC 5545} Sections 3.3.4,
    3.3.5, 3.3.12 and 3.3.14. A date-time is floating, in UTC when it ends in
    [Z], or local to the time zone a [TZID] parameter names, which the value
    itself does not carry.

    @canonical Ical.Date *)

type date = { year : int; month : int; day : int }
(** The type for DATE values, [YYYYMMDD]. The reader rejects a day the month
    does not have, and a record built by hand is the caller's to keep valid. *)

type time = { hour : int; minute : int; second : int; utc : bool }
(** The type for TIME values, [HHMMSS] with [Z] when [utc]. *)

type date_time = { date : date; time : time }
(** The type for DATE-TIME values. *)

(** The type for a value that is a date or a date-time, as DTSTART and its kin
    allow. *)
type t =
  | Date of date  (** A calendar date alone. *)
  | Date_time of date_time  (** A date with a time of day. *)

(** {1 Reading and writing} *)

val date_of_string : string -> (date, string) result
(** [date_of_string s] is the DATE [s]. *)

val date_to_string : date -> string
(** [date_to_string d] is [d] as [YYYYMMDD]. *)

val time_of_string : string -> (time, string) result
(** [time_of_string s] is the TIME [s]. *)

val time_to_string : time -> string
(** [time_to_string t] is [t] as [HHMMSS], with [Z] when it is UTC. *)

val date_time_of_string : string -> (date_time, string) result
(** [date_time_of_string s] is the DATE-TIME [s], a date, [T] and a time. *)

val date_time_to_string : date_time -> string
(** [date_time_to_string dt] is [dt] in the form of
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3.5} RFC 5545
     Section 3.3.5}. *)

val of_string : string -> (t, string) result
(** [of_string s] is the date or date-time [s], told apart by the [T]. *)

val to_string : t -> string
(** [to_string t] is [t] in its
    {{:https://www.rfc-editor.org/rfc/rfc5545.html} RFC 5545} form. *)

(** {1 Conversions} *)

val to_ptime : date_time -> Ptime.t option
(** [to_ptime dt] is the instant of [dt] read in UTC, whether or not [dt] is
    UTC, or [None] if [dt] is not a valid date. A floating or local time is the
    caller's to place in a zone. *)

val of_ptime : Ptime.t -> date_time
(** [of_ptime p] is [p] as a UTC date-time. *)

val start_of_day : date -> date_time
(** [start_of_day d] is midnight at the start of [d], floating. *)

val utc_offset_of_string : string -> (int, string) result
(** [utc_offset_of_string s] is the UTC-OFFSET [s] in seconds east of UTC,
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3.14} RFC 5545
     Section 3.3.14}. *)

val utc_offset_to_string : int -> string
(** [utc_offset_to_string n] is [n] seconds as [+HHMM] or [+HHMMSS]. *)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b] by their instant read in UTC, with a date
    at the start of its day. Values at the same instant are ordered by their
    parts, so that [compare a b] is [0] exactly when {!equal} holds. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same parts. *)

val equal_date : date -> date -> bool
(** [equal_date a b] is [true] if [a] and [b] are the same calendar date. *)

val equal_time : time -> time -> bool
(** [equal_time a b] is [true] if [a] and [b] have the same parts and are both
    UTC or both not. *)

val equal_date_time : date_time -> date_time -> bool
(** [equal_date_time a b] is [true] if [a] and [b] have the same date and time.
*)

val compare_date_time : date_time -> date_time -> int
(** [compare_date_time a b] orders [a] and [b] by their instant read in UTC,
    whether or not they are UTC. Values at the same instant are ordered by their
    parts, so that [compare_date_time a b] is [0] exactly when
    {!equal_date_time} holds. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints [t] as {!to_string} does. *)
