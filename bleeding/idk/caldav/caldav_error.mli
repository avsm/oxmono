(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CalDAV conditions.

    The preconditions and postconditions of
    {{:https://www.rfc-editor.org/rfc/rfc4791.html} RFC 4791} a server names in
    a [DAV:error] body, Sections 5.3.1, 5.3.2.1, 7.8, 7.9 and 7.10.

    @canonical Caldav.Error *)

val supported_calendar_data : Httpz_dav.name
(** [supported_calendar_data] is [CALDAV:supported-calendar-data], which a
    server names when the resource's media type is not one the targeted calendar
    collection accepts,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.3.2.1} RFC 4791
     Section 5.3.2.1}. *)

val valid_calendar_data : Httpz_dav.name
(** [valid_calendar_data] is [CALDAV:valid-calendar-data], which a server names
    when the submitted data is not valid iCalendar data,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.3.2.1} RFC 4791
     Section 5.3.2.1}. *)

val valid_calendar_object_resource : Httpz_dav.name
(** [valid_calendar_object_resource] is [CALDAV:valid-calendar-object-resource],
    which a server names when the calendar object resource does not obey the
    restrictions of Section 4.1,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.3.2.1} RFC 4791
     Section 5.3.2.1}. *)

val supported_calendar_component : Httpz_dav.name
(** [supported_calendar_component] is [CALDAV:supported-calendar-component],
    which a server names when the calendar object resource holds a component
    type the targeted calendar collection does not accept,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.3.2.1} RFC 4791
     Section 5.3.2.1}. *)

val no_uid_conflict : Httpz_dav.name
(** [no_uid_conflict] is [CALDAV:no-uid-conflict], which a server names when the
    UID is already in use by another resource in the targeted calendar
    collection,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.3.2.1} RFC 4791
     Section 5.3.2.1}. *)

val calendar_collection_location_ok : Httpz_dav.name
(** [calendar_collection_location_ok] is
    [CALDAV:calendar-collection-location-ok], which a server names when a
    calendar collection cannot be created at the destination,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.3.2.1} RFC 4791
     Section 5.3.2.1}. *)

val max_resource_size : Httpz_dav.name
(** [max_resource_size] is [CALDAV:max-resource-size], which a server names when
    the resource is larger than the targeted calendar collection accepts,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.3.2.1} RFC 4791
     Section 5.3.2.1}. *)

val min_date_time : Httpz_dav.name
(** [min_date_time] is [CALDAV:min-date-time], which a server names when a date
    or date-time in the request is earlier than the targeted calendar collection
    accepts,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.3.2.1} RFC 4791
     Section 5.3.2.1}. *)

val max_date_time : Httpz_dav.name
(** [max_date_time] is [CALDAV:max-date-time], which a server names when a date
    or date-time in the request is later than the targeted calendar collection
    accepts,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.3.2.1} RFC 4791
     Section 5.3.2.1}. *)

val max_instances : Httpz_dav.name
(** [max_instances] is [CALDAV:max-instances], which a server names when the
    resource generates more recurrence instances than the targeted calendar
    collection accepts,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.3.2.1} RFC 4791
     Section 5.3.2.1}. *)

val max_attendees_per_instance : Httpz_dav.name
(** [max_attendees_per_instance] is [CALDAV:max-attendees-per-instance], which a
    server names when an instance of the resource has more ATTENDEE properties
    than the targeted calendar collection accepts,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.3.2.1} RFC 4791
     Section 5.3.2.1}. *)

val supported_filter : Httpz_dav.name
(** [supported_filter] is [CALDAV:supported-filter], which a server names when a
    filter names a calendar component, property or parameter the server does not
    support querying,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.8} RFC 4791 Section
     7.8}. *)

val valid_filter : Httpz_dav.name
(** [valid_filter] is [CALDAV:valid-filter], which a server names when the
    filter is not a valid [CALDAV:filter] element,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.8} RFC 4791 Section
     7.8}. *)

val supported_collation : Httpz_dav.name
(** [supported_collation] is [CALDAV:supported-collation], which a server names
    when the collation named is not one the server supports,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.8} RFC 4791 Section
     7.8}. *)

val conflicting_uid : Httpz_dav.element list -> string option
(** [conflicting_uid e] is the href of the resource already using the UID when
    [e] names [CALDAV:no-uid-conflict]. *)

val describe : Httpz_dav.name -> string
(** [describe n] is a sentence explaining the condition [n]. *)
