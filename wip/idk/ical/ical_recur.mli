@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Recurrence rules.

    The RECUR value type of
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3.10} RFC 5545
     Section 3.3.10}, as an RRULE carries it. A rule is read and written with
    every part it names, and {!validate} checks the constraints the section
    states. The instances a rule produces are not computed here. A CalDAV server
    expands them on request,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.6.5} RFC 4791
     Section 9.6.5}.

    @canonical Ical.Recur *)

type freq =
  [ `Secondly | `Minutely | `Hourly | `Daily | `Weekly | `Monthly | `Yearly ]
(** The type for the [FREQ] part, the unit a rule repeats by. *)

type weekday = [ `Su | `Mo | `Tu | `We | `Th | `Fr | `Sa ]
(** The type for the days of the week, as [BYDAY] and [WKST] name them. *)

type t = {
  freq : freq;  (** The unit the rule repeats by. Mandatory. *)
  until : Ical_date.t option;
      (** The last instant an instance may fall on. At most one of [until] and
          [count] is set. *)
  count : int option;  (** The number of instances the rule produces. *)
  interval : int option;
      (** The number of [freq] units between instances. Defaults to [1] when
          absent. *)
  by_second : int list;  (** The seconds of the minute, 0 to 60. *)
  by_minute : int list;  (** The minutes of the hour, 0 to 59. *)
  by_hour : int list;  (** The hours of the day, 0 to 23. *)
  by_day : (int option * weekday) list;
      (** Each with its ordinal, such as [-1] for the last, when given. *)
  by_month_day : int list;
      (** The days of the month, 1 to 31, or -1 to -31 counting from the end. *)
  by_year_day : int list;
      (** The days of the year, 1 to 366, or -1 to -366 counting from the end.
      *)
  by_week_no : int list;
      (** The weeks of the year, 1 to 53, or -1 to -53 counting from the end. *)
  by_month : int list;  (** The months of the year, 1 to 12. *)
  by_set_pos : int list;
      (** The positions to keep within each interval's instances, counting from
          one, or from the end when negative. *)
  wkst : weekday option;
      (** The day a week starts on. Defaults to [`Mo] when absent. *)
  other : (string * string) list;
      (** The parts this library does not name, kept in order. *)
}
(** The type for recurrence rules. *)

val v : ?until:Ical_date.t -> ?count:int -> ?interval:int -> freq -> t
(** [v ~until ~count ~interval freq] is the rule repeating by [freq] with every
    other part empty. *)

val of_string : string -> (t, string) result
(** [of_string s] is the rule [s], a list of [NAME=value] parts separated by
    semicolons. A part that appears twice, or an unknown part with an invalid
    form, is an error. *)

val to_string : t -> string
(** [to_string t] is [t] with [FREQ] first, as
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3.10} RFC 5545
     Section 3.3.10} requires, and the other parts in the order the section
    lists them. *)

val validate : t -> (t, string) result
(** [validate t] checks that [t] has at most one of [until] and [count], that
    [interval] and [count], when set, are positive, that each numeric part is in
    the range its rule allows, that an ordinal on a [by_day] entry appears only
    with a monthly or yearly frequency and never with [by_week_no], that
    [by_week_no] appears only with a yearly frequency, that [by_year_day] does
    not appear with a daily, weekly or monthly frequency, that [by_month_day]
    does not appear with a weekly frequency, and that [by_set_pos] appears only
    with another by part. *)

val weekday_of_string : string -> weekday option
(** [weekday_of_string s] is the weekday [s] names, such as ["MO"]. *)

val weekday_to_string : weekday -> string
(** [weekday_to_string d] is the two letter name of [d]. *)

val freq_of_string : string -> freq option
(** [freq_of_string s] is the frequency [s] names, such as ["WEEKLY"]. *)

val freq_to_string : freq -> string
(** [freq_to_string f] is the name of [f]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same parts. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints [t] as {!to_string} does. *)
