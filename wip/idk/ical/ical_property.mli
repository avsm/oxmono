@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Properties.

    A property is a content line of
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.1} RFC 5545 Section
     3.1}, which is the content line of
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-3.3} RFC 6350 Section
     3.3} without its group, so a property is a {!Vcard.Property.t} and the
    parameter and text escapes are those of the vcard library. The readers here
    are for the value types of
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3} RFC 5545 Section
     3.3}.

    @canonical Ical.Property *)

type t = Vcard.Property.t
(** The type for properties.
    {{:https://www.rfc-editor.org/rfc/rfc5545.html} RFC 5545} gives a content
    line no group, and {!Ical.of_string} rejects one, but the type is the vCard
    content line and so can carry a group a caller puts there with
    {!Vcard.Property.v}. *)

val v : ?params:Vcard.Param.t list -> string -> string -> t
(** [v ~params name value] is the property [name] with the wire value [value].
    [name] is uppercased. *)

val of_text : ?params:Vcard.Param.t list -> string -> string -> t
(** [of_text ~params name s] is the property [name] with the TEXT value [s],
    escaped as
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3.11} RFC 5545
     Section 3.3.11} requires. *)

val of_date_time :
  ?params:Vcard.Param.t list -> ?tzid:string -> string -> Ical_date.t -> t
(** [of_date_time ~params ~tzid name d] is the property [name] holding [d], with
    [VALUE=DATE] when [d] is a date and [TZID] set to [tzid] when given. *)

val name : t -> string
(** [name p] is the name of [p], in upper case. *)

val params : t -> Vcard.Param.t list
(** [params p] are the parameters of [p], in order. *)

val value : t -> string
(** [value p] is the wire value of [p], as it appears on the content line. No
    escape is undone. {!text} reads it as a TEXT value. *)

val find_values : t -> string -> string list option
(** [find_values p name] are the values of the parameter [name] of [p]. *)

val find_first : t -> string -> string option
(** [find_first p name] is the first value of the parameter [name] of [p]. *)

val value_type : t -> Ical_value_type.t
(** [value_type p] is the [VALUE] parameter of [p], or the default type of the
    property from {!Ical.Registry.value_type}, or [Text]. *)

val tzid : t -> string option
(** [tzid p] is the [TZID] parameter of [p]. *)

(** {1 Readers} *)

val text : t -> string
(** [text p] is the TEXT value of [p] unescaped. *)

val text_list : t -> string list
(** [text_list p] are the comma separated TEXT values of [p], for CATEGORIES and
    RESOURCES. *)

val date_time : t -> (Ical_date.t, string) result
(** [date_time p] is the DATE or DATE-TIME value of [p], by its value type. *)

val date_times : t -> (Ical_date.t list, string) result
(** [date_times p] are the comma separated dates or date-times of [p], for
    EXDATE and RDATE. *)

val duration : t -> (Ical_duration.t, string) result
(** [duration p] is the DURATION value of [p]. *)

val period : t -> (Ical_period.t, string) result
(** [period p] is the PERIOD value of [p]. *)

val periods : t -> (Ical_period.t list, string) result
(** [periods p] are the comma separated periods of [p], for FREEBUSY and an
    RDATE with [VALUE=PERIOD]. *)

val recur : t -> (Ical_recur.t, string) result
(** [recur p] is the RECUR value of [p]. *)

val integer : t -> (int, string) result
(** [integer p] is the INTEGER value of [p]. *)

val float : t -> (float, string) result
(** [float p] is the FLOAT value of [p]. *)

val boolean : t -> (bool, string) result
(** [boolean p] is the BOOLEAN value of [p]. *)

val utc_offset : t -> (int, string) result
(** [utc_offset p] is the UTC-OFFSET value of [p], in seconds east of UTC. *)

val uri : t -> string
(** [uri p] is the URI or CAL-ADDRESS value of [p], as it is. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same name, parameters, value
    and group. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf p] prints [p] as a content line. *)
