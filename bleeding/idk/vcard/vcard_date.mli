@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Dates and times.

    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4.3} RFC 6350 Section
     4.3} defines the date and time value types on the basic format of ISO 8601,
    with the reduced accuracy and truncated representations the section permits.
    A date may lack its year, its day, or both, and a time may lack its hour or
    its hour and minute. Such a value denotes a set of instants rather than one,
    so {!Cal_date}, {!Time} and {!Date_time} carry no [compare]. {!Timestamp}
    and {!Utc_offset}, which are complete by construction, carry one.

    @canonical Vcard.Date *)

(** The type for the zone of a time. *)
type zone =
  | Utc  (** The [Z] designator. *)
  | Offset of int  (** A UTC offset, in minutes east of UTC. *)

(** UTC offsets, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4.7} Section 4.7}. *)
module Utc_offset : sig
  type t = int
  (** The type for an offset, in minutes east of UTC. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the offset [s], a sign, two hour digits and two optional
      minute digits. *)

  val to_string : t -> string
  (** [to_string o] is [o] as a sign, two hour digits and two minute digits. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] are the same offset. *)

  val compare : t -> t -> int
  (** [compare a b] orders offsets from west to east. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf o] formats [o] on [ppf]. *)
end

(** Calendar dates, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4.3.1} Section 4.3.1}.
*)
module Cal_date : sig
  type t = { year : int option; month : int option; day : int option }
  (** The type for a date. A date with a year and a day has a month. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the date [s], in one of the shapes [YYYYMMDD], [YYYY-MM],
      [YYYY], [--MMDD], [--MM] and [---DD]. *)

  val to_string : t -> string
  (** [to_string d] is [d] in the shape {!of_string} reads, and the empty string
      if {!validate} rejects [d]. *)

  val validate : t -> (t, string) result
  (** [validate d] is [Ok d] if the parts of [d] are in range and in one of the
      shapes {!of_string} reads. *)

  val is_complete : t -> bool
  (** [is_complete d] is [true] if [d] has a year, a month and a day. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same parts. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf d] formats [d] on [ppf]. *)
end

(** Times of day, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4.3.2} Section 4.3.2}.
*)
module Time : sig
  type t = {
    hour : int option;
    minute : int option;
    second : int option;
    zone : zone option;
  }
  (** The type for a time. A time with an hour and a second has a minute. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the time [s], in one of the shapes [HHMMSS], [HHMM],
      [HH], [-MMSS], [-MM] and [--SS], each followed by an optional zone. *)

  val to_string : t -> string
  (** [to_string t] is [t] in the shape {!of_string} reads, and its zone alone
      if {!validate} rejects [t]. *)

  val validate : t -> (t, string) result
  (** [validate t] is [Ok t] if the parts of [t] are in range and in one of the
      shapes {!of_string} reads. *)

  val is_complete : t -> bool
  (** [is_complete t] is [true] if [t] has an hour, a minute and a second. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same parts and zone. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] on [ppf]. *)
end

(** Dates with a time of day, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4.3.3} Section 4.3.3}.
*)
module Date_time : sig
  type t = { date : Cal_date.t; time : Time.t }
  (** The type for a date-time. The date has a day and the time an hour. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the date-time [s], a date, a [T] and a time. *)

  val to_string : t -> string
  (** [to_string t] is [t] in the shape {!of_string} reads. *)

  val validate : t -> (t, string) result
  (** [validate t] is [Ok t] if the parts of [t] are in range, its date has a
      day and its time an hour, as Section 4.3.3 requires. *)

  val is_complete : t -> bool
  (** [is_complete t] is [true] if the date of [t] has a year, a month and a
      day, and its time an hour, a minute and a second. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same date and time. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] on [ppf]. *)
end

(** Complete timestamps, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4.3.5} Section 4.3.5}.
*)
module Timestamp : sig
  type t = {
    year : int;
    month : int;
    day : int;
    hour : int;
    minute : int;
    second : int;
    zone : zone option;
  }
  (** The type for a timestamp. A timestamp without a zone is a local time. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the timestamp [s], a complete date, a [T], a complete
      time and an optional zone. *)

  val to_string : t -> string
  (** [to_string t] is [t] in the shape {!of_string} reads. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same parts and zone. *)

  val compare : t -> t -> int
  (** [compare a b] orders timestamps by the instant they denote, reading a
      timestamp without a zone in UTC. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] on [ppf]. *)

  val to_ptime : ?zone:zone -> t -> Ptime.t option
  (** [to_ptime ~zone t] is the instant [t] denotes, or [None] if [t] is not a
      valid Gregorian date and time. [zone] is the zone of a timestamp that has
      none and defaults to [Utc]. *)

  val of_ptime : ?zone:zone -> Ptime.t -> t
  (** [of_ptime ~zone p] is the timestamp of the instant [p] in [zone], which
      defaults to [Utc]. *)
end

(** The type for a [date-and-or-time] value, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4.3.4} Section 4.3.4}.
*)
type t =
  | Date of Cal_date.t  (** A calendar date alone. *)
  | Time of Time.t  (** A time of day alone. *)
  | Date_time of Date_time.t  (** A date with a time of day. *)

val of_string : string -> (t, string) result
(** [of_string s] is the value [s]. A stand-alone time starts with [T]. *)

val to_string : t -> string
(** [to_string t] is [t] in the shape {!of_string} reads. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same value. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf]. *)
