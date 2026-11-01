(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CalDAV, {{:https://www.rfc-editor.org/rfc/rfc4791.html} RFC 4791}.

    The properties, reports, filters and conditions of the calendaring
    extensions to WebDAV, built on {!Httpz_dav}. Requests are described and
    responses read here without an HTTP client. The [caldav.eio] library sends
    them. *)

module Property = Caldav_property
(** Names and properties. *)

module Filter = Caldav_filter
(** Query filters. *)

module Calendar_data = Caldav_calendar_data
(** The calendar-data element. *)

module Report = Caldav_report
(** The calendar-query, calendar-multiget and free-busy-query reports. *)

module Error = Caldav_error
(** Conditions. *)

module Data = Caldav_data
(** Calendar object representations. *)

module Calendar = Caldav_calendar
(** Calendar collections. *)
