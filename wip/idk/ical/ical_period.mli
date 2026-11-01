@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Periods.

    The PERIOD value type of
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3.9} RFC 5545
     Section 3.3.9}, a start and either an end or a duration.

    @canonical Ical.Period *)

type t = {
  start : Ical_date.date_time;  (** The start of the period. *)
  ends : [ `End of Ical_date.date_time | `Duration of Ical_duration.t ];
      (** The end of the period, given either outright or as a duration from
          [start]. {!val-finish} resolves it either way. *)
}
(** The type for periods. *)

val of_string : string -> (t, string) result
(** [of_string s] is the PERIOD [s]. A duration form with a negative duration is
    an error, since Section 3.3.9 gives the form a positive duration. The
    explicit form is not held to its start preceding its end. *)

val to_string : t -> string
(** [to_string p] is [p] in the form of
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3.9} RFC 5545
     Section 3.3.9}. *)

val finish : t -> Ical_date.date_time
(** [finish p] is the end of [p], computed from its duration when it has one. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same start and end. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf p] prints [p] as {!to_string} does. *)
