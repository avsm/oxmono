@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Dates and timestamps.

    JSContact records a point in time as a UTCDateTime, and the date of an
    anniversary as either a partial calendar date or a complete timestamp.

    @canonical Jscontact.Date *)

(** UTC date-times.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.4.5} RFC 9553
     Section 1.4.5} types a UTCDateTime as an
    {{:https://www.rfc-editor.org/rfc/rfc3339.html} RFC 3339} [date-time] whose
    letters are uppercase and whose offset is the character ["Z"]. A fractional
    second is written only when it is non-zero, and without trailing zeros, so
    that each instant has a single spelling. *)
module Utc : sig
  type t = Ptime.t
  (** The type for a UTC date-time, equal to [Ptime.t]. A value can be compared
      and formatted with the [ptime] library. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the date-time [s]. Parsing is strict about the case of
      the ["T"] and ["Z"] characters and about the [hh:mm] shape of an offset,
      as Section 1.4.5 requires. An offset other than ["Z"] is accepted and
      converted to UTC. {!is_canonical} is the stricter test. Any fractional
      part beyond the picosecond is truncated. *)

  val to_string : t -> string
  (** [to_string t] is the canonical spelling of [t]. The letters are uppercase,
      the offset is ["Z"], and a fractional second appears only when [t] has
      one, written with no trailing zeros. *)

  val is_canonical : string -> bool
  (** [is_canonical s] is [true] if [s] is the spelling {!to_string} produces
      for the instant it denotes, which is what Section 1.4.5 requires of a
      UTCDateTime on the wire. A well formed [s] that is not canonical, such as
      ["2010-10-10T10:10:10.000Z"] or an offset of ["+01:00"], still decodes. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] are the same instant. *)

  val compare : t -> t -> int
  (** [compare a b] orders instants. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] on [ppf] in its canonical spelling. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a UTCDateTime. Decoding is {!of_string} and
      encoding is {!to_string}. A non-canonical spelling on the wire is
      canonicalised by a decode and encode cycle. *)
end

(** Partial calendar dates.

    A PartialDate is a complete date, a year, a month in a year, or a day in a
    month, in the Gregorian calendar, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.8.1} RFC 9553
     Section 2.8.1}. *)
module Partial_date : sig
  type t = {
    year : int option;  (** The calendar year. *)
    month : int option;
        (** The calendar month, 1 to 12. If set, then either [year] or [day]
            must be set. *)
    day : int option;
        (** The day of the month, 1 to 31. If set, then [month] must be set. *)
    calendar_scale : string option;
        (** The calendar system the date occurs in, in lowercase, either a CLDR
            calendar system name registered by
            {{:https://www.rfc-editor.org/rfc/rfc7529.html} RFC 7529} or a
            vendor-specific value. The year, month and day are still those of
            the Gregorian calendar. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for a partial date. *)

  val make :
    ?year:int ->
    ?month:int ->
    ?day:int ->
    ?calendar_scale:string ->
    ?unknown:Jscontact_unknown.t ->
    unit ->
    t
  (** [make ()] is a partial date with the given properties. Every optional
      argument defaults to the property being unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf d] formats [d] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate d] checks the ranges of [d] and the dependencies between its
      properties that Section 2.8.1 states. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a PartialDate. *)
end

(** Complete timestamps.

    A Timestamp identifies a UTCDateTime as distinct from a {!Partial_date.t}
    where either may occur. *)
module Timestamp : sig
  type t = {
    utc : Utc.t;  (** The point in time. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for a timestamp. *)

  val make : ?unknown:Jscontact_unknown.t -> Utc.t -> t
  (** [make utc] is the timestamp at [utc]. [unknown] defaults to the property
      being unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] are the same instant. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf ts] formats [ts] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate t] checks the unknown members of [t]. A timestamp has no rule of
      its own beyond its shape. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a Timestamp. *)
end

(** The type for the value of a property typed [PartialDate|Timestamp], such as
    the [date] of an anniversary. *)
type t =
  | Partial of Partial_date.t  (** A whole or partial calendar date. *)
  | Timestamp of Timestamp.t  (** A complete UTC timestamp. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same date. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf d] formats [d] on [ppf]. *)

val validate : t -> t Jscontact_valid.t
(** [validate d] validates the date [d] holds. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a [PartialDate|Timestamp] property.
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.3.4} Section 1.3.4}
    makes PartialDate the default type of such a property. An object with no
    [@type] member decodes as a partial date and encodes without one, while a
    timestamp must and does carry [@type]. *)
