@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The registered properties and components.

    The properties of {{:https://www.rfc-editor.org/rfc/rfc5545.html} RFC 5545}
    Sections 3.7 and 3.8 and
    {{:https://www.rfc-editor.org/rfc/rfc7986.html#section-5} RFC 7986 Section
     5}, with their default value types, and the components of Section 3.6 with
    the properties each requires and allows.

    @canonical Ical.Registry *)

val value_type : string -> Ical_value_type.t option
(** [value_type name] is the default value type of the property [name], such as
    [Date_time] for DTSTART, or [None] if [name] is not registered. *)

(** The type for how often a property may appear in a component. *)
type cardinality =
  | One  (** Exactly one instance per component. *)
  | At_most_one  (** At most one instance per component. *)
  | Many  (** Any number of instances. *)

val cardinality : component:string -> string -> cardinality option
(** [cardinality ~component name] is how often the property [name] may appear in
    [component], or [None] if
    {{:https://www.rfc-editor.org/rfc/rfc5545.html} RFC 5545} does not list it
    there. A property with no listing is allowed any number of times, as Section
    3.6 allows extension properties.

    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.6.6} RFC 5545
     Section 3.6.6} gives VALARM three property sets, one per ACTION, and this
    table merges them. Where they disagree the permissive count is kept, so
    [ATTACH] is [Many] although an AUDIO alarm allows one. *)

val required : string -> string list
(** [required component] are the properties [component] must hold, such as UID
    and DTSTAMP for VEVENT, or TZID for VTIMEZONE. *)

val components : string list
(** [components] are VCALENDAR, VEVENT, VTODO, VJOURNAL, VFREEBUSY, VTIMEZONE,
    STANDARD, DAYLIGHT and VALARM. *)

val children : string -> string list
(** [children component] are the components [component] may hold. *)

val names : string list
(** [names] are the registered property names. {!Vcard.Registry.all} is the
    vCard counterpart and holds entries rather than names, since a vCard
    property has one default value type and one cardinality, while an iCalendar
    property's cardinality depends on the component holding it. *)
