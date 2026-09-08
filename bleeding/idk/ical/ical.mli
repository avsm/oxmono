@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** iCalendar, {{:https://www.rfc-editor.org/rfc/rfc5545.html} RFC 5545}.

    A calendar object is a [VCALENDAR] component with its properties and the
    components it holds. It is read and written losslessly. Every property is
    kept with its parameters and wire value, and the typed readers of
    {!Property} read a value on demand. The content line syntax is that of
    {{:https://www.rfc-editor.org/rfc/rfc6350.html} RFC 6350} and comes from the
    vcard library. *)

module Value_type = Ical_value_type
(** The value data types of
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3} RFC 5545 Section
     3.3} and the [VALUE] parameter. *)

module Date = Ical_date
(** The DATE, TIME, DATE-TIME and UTC-OFFSET value types,
    {{:https://www.rfc-editor.org/rfc/rfc5545.html} RFC 5545} Sections 3.3.4,
    3.3.5, 3.3.12 and 3.3.14. *)

module Duration = Ical_duration
(** The DURATION value type,
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3.6} RFC 5545
     Section 3.3.6}. *)

module Period = Ical_period
(** The PERIOD value type,
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3.9} RFC 5545
     Section 3.3.9}. *)

module Recur = Ical_recur
(** The RECUR value type,
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3.10} RFC 5545
     Section 3.3.10}. *)

module Property = Ical_property
(** Content lines,
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.1} RFC 5545 Section
     3.1}, and the readers for their value types. *)

module Registry = Ical_registry
(** The registered properties and components, with their default value types and
    the properties each component requires and allows. *)

module Component = Ical_component
(** Components, the runs between [BEGIN] and [END],
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.6} RFC 5545 Section
     3.6}. *)

module Param = Vcard.Param
(** Property parameters, as the vcard library reads and writes them. *)

type t = private Component.t
(** The type for calendar objects, a component named [VCALENDAR]. A value comes
    from {!v}, {!with_components} or {!of_string}, and reads as a component. *)

val v : ?prodid:string -> ?properties:Property.t list -> Component.t list -> t
(** [v ~prodid ~properties components] is the calendar object holding
    [components], with [VERSION:2.0], a PRODID of [prodid], which defaults to
    this library's, and [properties]. *)

val with_components : t -> Component.t list -> t
(** [with_components t cs] is [t] holding [cs] in place of its components. *)

val properties : t -> Property.t list
(** [properties t] are the properties of the VCALENDAR itself. *)

val components : t -> Component.t list
(** [components t] are the components [t] holds, in order. *)

val prodid : t -> string option
(** [prodid t] is the PRODID of [t]. *)

val version : t -> string option
(** [version t] is the VERSION of [t]. *)

val events : t -> Component.t list
(** [events t] are the VEVENT components of [t]. *)

val todos : t -> Component.t list
(** [todos t] are the VTODO components of [t]. *)

val journals : t -> Component.t list
(** [journals t] are the VJOURNAL components of [t]. *)

val free_busy : t -> Component.t list
(** [free_busy t] are the VFREEBUSY components of [t]. *)

val timezones : t -> Component.t list
(** [timezones t] are the VTIMEZONE components of [t]. *)

val uid : t -> string option
(** [uid t] is the UID shared by the components of [t] other than time zones,
    which
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-4.1} RFC 4791 Section
     4.1} requires of a calendar object resource, or [None] if they have none or
    differ. *)

val find_timezone : t -> string -> Component.t option
(** [find_timezone t tzid] is the VTIMEZONE of [t] whose TZID is [tzid]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] hold the same properties and
    components. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints [t] as {!to_string} does. *)

(** {1 Validation} *)

val validate : t -> (t, string) result
(** [validate t] checks the rules of
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.6} RFC 5545 Section
     3.6} that the syntax does not. VERSION is [2.0] and PRODID is present, at
    least one component is, each component holds the properties
    [Registry.required] lists and no property more often than
    [Registry.cardinality] allows, an event has at most one of DTEND and
    DURATION, a to-do at most one of DUE and DURATION, a time zone at least one
    STANDARD or DAYLIGHT, and every date, date-time, duration, period,
    recurrence rule and offset value reads. *)

(** {1 Reading and writing} *)

val of_string : string -> (t list, string) result
(** [of_string s] are the calendar objects in [s], each a [VCALENDAR]. Lines are
    unfolded first,
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.1} RFC 5545 Section
     3.1}, and a bare line feed ends a line as a carriage return and line feed
    does. A property outside a component, a mismatched END, or a component left
    open is an error naming the line. *)

val one_of_string : string -> (t, string) result
(** [one_of_string s] is the single calendar object in [s]. *)

val to_string : t -> string
(** [to_string t] is [t] as content lines ending in a carriage return and line
    feed, folded at 75 octets. *)
