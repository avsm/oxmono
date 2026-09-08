(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CalDAV names and properties.

    The elements of {{:https://www.rfc-editor.org/rfc/rfc4791.html} RFC 4791}
    live in the namespace [urn:ietf:params:xml:ns:caldav]. The names here are
    its properties, Sections 5.2 and 6.2, its resource type, Section 4.2, and
    its reports, Section 7.

    @canonical Caldav.Property *)

type name = Httpz_dav.name
(** The type for names. *)

val caldav : string -> name
(** [caldav local] is the name [local] in the CalDAV namespace. *)

(** {1 Resource types and reports} *)

val calendar : name
(** [calendar] is the resource type of a calendar collection,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-4.2} RFC 4791 Section
     4.2}. *)

val calendar_query : name
(** [calendar_query] is the report of
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.8} RFC 4791 Section
     7.8}. *)

val calendar_multiget : name
(** [calendar_multiget] is the report of
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.9} RFC 4791 Section
     7.9}. *)

val free_busy_query : name
(** [free_busy_query] is the report of
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.10} RFC 4791
     Section 7.10}. *)

(** {1 Properties} *)

val calendar_home_set : name
(** [calendar_home_set] is the property of a principal listing the collections
    its calendars live in,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-6.2.1} RFC 4791
     Section 6.2.1}. *)

val calendar_description : name
(** [calendar_description] is
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.2.1} RFC 4791
     Section 5.2.1}. *)

val calendar_timezone : name
(** [calendar_timezone] is
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.2.2} RFC 4791
     Section 5.2.2}. *)

val supported_calendar_component_set : name
(** [supported_calendar_component_set] is
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.2.3} RFC 4791
     Section 5.2.3}. *)

val supported_calendar_data : name
(** [supported_calendar_data] is
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.2.4} RFC 4791
     Section 5.2.4}. *)

val max_resource_size : name
(** [max_resource_size] is
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.2.5} RFC 4791
     Section 5.2.5}. *)

val min_date_time : name
(** [min_date_time] is
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.2.6} RFC 4791
     Section 5.2.6}. *)

val max_date_time : name
(** [max_date_time] is
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.2.7} RFC 4791
     Section 5.2.7}. *)

val max_instances : name
(** [max_instances] is
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.2.8} RFC 4791
     Section 5.2.8}. *)

val max_attendees_per_instance : name
(** [max_attendees_per_instance] is
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.2.9} RFC 4791
     Section 5.2.9}. *)

val supported_collation_set : name
(** [supported_collation_set] is
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.5.1} RFC 4791
     Section 7.5.1}. *)

val calendar_user_address_set : name
(** [calendar_user_address_set] is
    {{:https://www.rfc-editor.org/rfc/rfc6638.html#section-2.4.1} RFC 6638
     Section 2.4.1}, which a server with scheduling sets on a principal. *)

val getctag : name
(** [getctag] is the [getctag] property of the [http://calendarserver.org/ns/]
    namespace, which many servers set on a collection and change whenever a
    member does. It is not in an RFC. *)

(** {1 Readers} *)

val is_calendar : Httpz_dav.element -> bool
(** [is_calendar p] is [true] if the [DAV:resourcetype] [p] holds
    [CALDAV:calendar]. *)

val component_set : Httpz_dav.element -> string list
(** [component_set p] are the component names a
    [CALDAV:supported-calendar-component-set] lists, such as
    [["VEVENT"; "VTODO"]]. A calendar without the property accepts every
    component,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.2.3} RFC 4791
     Section 5.2.3}. *)

val calendar_data_types : Httpz_dav.element -> (string * string) list
(** [calendar_data_types p] are the content type and version pairs a
    [CALDAV:supported-calendar-data] lists, each defaulting to [text/calendar]
    and [2.0]. *)

val timezone : Httpz_dav.element -> string option
(** [timezone p] is the iCalendar text a [CALDAV:calendar-timezone] holds. *)

val collations : Httpz_dav.element -> string list
(** [collations p] are the collations a [CALDAV:supported-collation-set] lists.
*)

val max_size : Httpz_dav.element -> int option
(** [max_size p] is the value of a [CALDAV:max-resource-size]. *)

val date_time_bound : Httpz_dav.element -> Ical.Date.date_time option
(** [date_time_bound p] is the UTC date-time a [CALDAV:min-date-time] or
    [CALDAV:max-date-time] holds. *)

val integer : Httpz_dav.element -> int option
(** [integer p] is the value of a [CALDAV:max-instances] or
    [CALDAV:max-attendees-per-instance]. *)

(** {1 Constructors} *)

val description : ?lang:string -> string -> Httpz_dav.element
(** [description ~lang s] is a [CALDAV:calendar-description] of [s], tagged
    [xml:lang] [lang] when given. *)

val timezone_prop : string -> Httpz_dav.element
(** [timezone_prop ical] is a [CALDAV:calendar-timezone] holding [ical]. *)

val supported_components : string list -> Httpz_dav.element
(** [supported_components names] is a [CALDAV:supported-calendar-component-set]
    with a [comp] per name. *)

val collection_props : name list
(** [collection_props] are the properties a client asks of a collection to tell
    calendars apart and describe them. *)

val principal_props : name list
(** [principal_props] are [CALDAV:calendar-home-set],
    [CALDAV:calendar-user-address-set], [DAV:displayname] and
    [DAV:principal-URL]. *)
