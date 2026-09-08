@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Durations.

    The DURATION value type of
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3.6} RFC 5545
     Section 3.3.6}, such as [P15DT5H0M20S].

    @canonical Ical.Duration *)

type t = {
  negative : bool;
  weeks : int;
  days : int;
  hours : int;
  minutes : int;
  seconds : int;
}
(** The type for durations. A duration in weeks has no other part, and one in
    days may have a time. *)

val zero : t
(** [zero] is [PT0S]. *)

val of_string : string -> (t, string) result
(** [of_string s] is the DURATION [s]. A component above 1e9 is refused, so that
    {!to_seconds} stays within an [int]. *)

val to_string : t -> string
(** [to_string d] is [d] in the form of
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3.6} RFC 5545
     Section 3.3.6}. *)

val to_seconds : t -> int
(** [to_seconds d] is the length of [d] in seconds, with a week as seven days
    and a day as 24 hours, negative when [d] is. *)

val of_seconds : int -> t
(** [of_seconds n] is the duration of [n] seconds in days, hours, minutes and
    seconds, negative when [n] is. *)

val add : Ical_date.date_time -> t -> Ical_date.date_time
(** [add dt d] is [dt] moved by [d], with days counted on the calendar and the
    time parts as seconds. It is [dt] itself if [dt] is not a date the calendar
    has, or if moving it would leave the range [Ptime] represents. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same parts. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf d] prints [d] as {!to_string} does. *)
