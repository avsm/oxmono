(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Calendar collections.

    A calendar is a collection whose resource type holds [CALDAV:calendar],
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-4.2} RFC 4791 Section
     4.2}. The record here is what a PROPFIND of
    {!Caldav.Property.collection_props} says about one.

    @canonical Caldav.Calendar *)

type t = {
  href : string;  (** The URL of the calendar. *)
  display_name : string option;  (** The [DAV:displayname], if set. *)
  description : string option;
      (** The [CALDAV:calendar-description], if set. *)
  timezone : string option;  (** The VTIMEZONE of the calendar, as text. *)
  components : string list;
      (** The component names the calendar accepts, or every one when the server
          does not say. *)
  etag : string option;  (** The [DAV:getetag] of the collection, if set. *)
  ctag : string option;
      (** The [getctag] of the collection, if the server sets one. *)
  sync_token : string option;
      (** The [DAV:sync-token] of the collection, if the server supports the
          sync-collection report of
          {{:https://www.rfc-editor.org/rfc/rfc6578.html} RFC 6578}. *)
  reports : Httpz_dav.name list;
      (** The reports the collection supports,
          {{:https://www.rfc-editor.org/rfc/rfc3253.html#section-3.1.5} RFC 3253
           Section 3.1.5}. *)
  data_types : (string * string) list;
      (** The content type and version pairs the calendar accepts,
          {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.2.4} RFC 4791
           Section 5.2.4}, or [text/calendar] at [2.0] alone when the server
          does not say. *)
  max_size : int option;
      (** The [CALDAV:max-resource-size] of the collection, if set. *)
  min_date_time : Ical.Date.date_time option;
      (** The [CALDAV:min-date-time] of the collection, if set. *)
  max_date_time : Ical.Date.date_time option;
      (** The [CALDAV:max-date-time] of the collection, if set. *)
  max_instances : int option;
      (** The [CALDAV:max-instances] of the collection, if set. *)
  max_attendees : int option;
      (** The [CALDAV:max-attendees-per-instance] of the collection, if set. *)
  collations : string list;
      (** The collations the [CALDAV:supported-collation-set] of the collection
          lists. *)
  privileges : Httpz_dav.name list;
      (** The privileges of the current user,
          {{:https://www.rfc-editor.org/rfc/rfc3744.html#section-5.4} RFC 3744
           Section 5.4}, or none if the server does not say. *)
}
(** The type for calendars. *)

val of_response : Httpz_dav.response -> t option
(** [of_response r] is the calendar [r] describes, or [None] if [r] is not a
    calendar collection. *)

val of_multistatus : Httpz_dav.multistatus -> t list
(** [of_multistatus m] are the calendars among the responses of [m], in order.
*)

val supports : Httpz_dav.name -> t -> bool
(** [supports report t] is [true] if [t] advertises [report]. *)

val accepts : component:string -> t -> bool
(** [accepts ~component t] is [true] if [t] stores the component [component],
    such as ["VEVENT"]. *)

val mkcalendar :
  ?display_name:string ->
  ?description:string ->
  ?timezone:string ->
  ?components:string list ->
  unit ->
  Httpz_dav.element list
(** [mkcalendar ~display_name ~description ~timezone ~components ()] are the
    properties of the MKCALENDAR that creates a calendar. *)

val propfind : Httpz_dav.propfind
(** [propfind] asks for {!Caldav.Property.collection_props}. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints the href and display name of [t]. *)
