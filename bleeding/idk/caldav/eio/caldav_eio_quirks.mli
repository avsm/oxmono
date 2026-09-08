(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What a server does differently.

    {{:https://www.rfc-editor.org/rfc/rfc4791.html} RFC 4791} leaves a server
    room a client has to live with, and some depart from it outright. A profile
    names the departures of one service, and {!Caldav_eio.Client} consults it in
    the functions the departure affects and nowhere else, so that the protocol
    layer stays what the RFC says.

    @canonical Caldav_eio.Quirks *)

type t = {
  fixed_components : bool;
      (** A new calendar stores VEVENT only, whatever component set the
          MKCALENDAR asked for, and answers a VTODO or VJOURNAL with 403. The
          client checks the calendar's component set before a PUT and reports a
          [Data] error naming the component instead. *)
  param_filter : bool;
      (** The server answers a [param-filter] with a [text-match] correctly,
          {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.7.3} RFC 4791
           Section 9.7.3}. When it does not, the client sends the query without
          its parameter filters and applies {!Caldav.Filter.matches} to what
          comes back. *)
  recurrence_id_on_first : bool;
      (** An expanded recurrence carries [RECURRENCE-ID] on every instance,
          {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.6.5} RFC 4791
           Section 9.6.5}. When the first instance lacks it, the client adds one
          from its DTSTART. *)
  lenient_hrefs : bool;
      (** The server writes member names into hrefs unencoded, which the strict
          validator of {!Httpz_dav} rejects. See {!Fetch_dav.v}. *)
}
(** The type for profiles. A field naming a departure, [fixed_components] and
    [lenient_hrefs], is [false] in {!standard}. A field naming a report the RFC
    requires, [param_filter] and [recurrence_id_on_first], is [true] there and
    cleared for a service that does not answer it. *)

val standard : t
(** [standard] expects the RFC and is the default. *)

val fastmail : t
(** [fastmail] is Fastmail's CalDAV, which it serves through Cyrus.
    [fixed_components] is set, [param_filter] and [recurrence_id_on_first] are
    clear. Its calendars honour conditional PUT and DELETE, store a
    calendar-timezone, refuse a second resource with the same UID and invalid
    data, expand recurrences, answer free-busy-query and support
    sync-collection. The profile records the service as it behaved on
    2026-09-08. *)

val of_url : string -> t
(** [of_url url] is {!fastmail} for a [fastmail.com] or [messagingengine.com]
    host and {!standard} otherwise. *)
