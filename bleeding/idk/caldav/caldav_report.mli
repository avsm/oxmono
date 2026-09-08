(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The calendar reports.

    [CALDAV:calendar-query] finds the calendar objects of a collection that
    match a filter,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.8} RFC 4791 Section
     7.8}, [CALDAV:calendar-multiget] fetches the ones named, Section 7.9, and
    [CALDAV:free-busy-query] is the busy time of a collection in a range,
    Section 7.10, answered with a VFREEBUSY rather than a multistatus.

    @canonical Caldav.Report *)

(** The type for the properties a report asks for. *)
type props =
  | Prop of Httpz_dav.name list * Caldav_calendar_data.t option
      (** The named properties, and the calendar object when calendar data is
          given. *)
  | Allprop
      (** Every dead property of each resource and the live properties
          {{:https://www.rfc-editor.org/rfc/rfc4918.html} RFC 4918} defines,
          Section 14.2. The calendar object is not among them, since
          {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.6} RFC 4791
           Section 9.6} carries calendar-data inside [DAV:prop] alone. *)
  | Propname  (** The names of the matching properties, not their values. *)

type query = {
  props : props;  (** The properties asked for. *)
  filter : Caldav_filter.t;  (** The filter matching resources must satisfy. *)
  timezone : string option;
      (** An iCalendar VTIMEZONE to read floating times in,
          {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.8} RFC 4791
           Section 9.8}. *)
}
(** The type for [CALDAV:calendar-query] requests,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.8} RFC 4791 Section
     7.8}. *)

type multiget = {
  props : props;  (** The properties asked for. *)
  hrefs : string list;  (** The hrefs asked for. *)
}
(** The type for [CALDAV:calendar-multiget] requests,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.9} RFC 4791 Section
     7.9}. *)

val query :
  ?props:Httpz_dav.name list ->
  ?data:Caldav_calendar_data.t ->
  ?timezone:string ->
  Caldav_filter.t ->
  query
(** [query ~props ~data ~timezone filter] asks for [props], which default to
    [DAV:getetag], and for the calendar object as [data] describes, which
    defaults to the whole object. *)

val multiget :
  ?props:Httpz_dav.name list ->
  ?data:Caldav_calendar_data.t ->
  string list ->
  multiget
(** [multiget ~props ~data hrefs] asks for [hrefs] with the same defaults as
    {!val-query}. *)

val equal_query : query -> query -> bool
(** [equal_query a b] is [true] if [a] and [b] ask for the same properties, the
    same filter and the same time zone. *)

val equal_multiget : multiget -> multiget -> bool
(** [equal_multiget a b] is [true] if [a] and [b] ask for the same properties
    and the same hrefs, in the same order. *)

val query_to_xml : query -> Httpz_dav.element
(** [query_to_xml q] is [q] as a [CALDAV:calendar-query] element. *)

val multiget_to_xml : multiget -> Httpz_dav.element
(** [multiget_to_xml m] is [m] as a [CALDAV:calendar-multiget] element. *)

val free_busy_to_xml : Caldav_filter.time_range -> Httpz_dav.element
(** [free_busy_to_xml tr] is [tr] as a [CALDAV:free-busy-query] element. *)

val query_of_xml : Httpz_dav.element -> (query, string) result
(** [query_of_xml x] is the query [x] holds. *)

val multiget_of_xml : Httpz_dav.element -> (multiget, string) result
(** [multiget_of_xml x] is the multiget [x] holds. *)

val free_busy_of_xml :
  Httpz_dav.element -> (Caldav_filter.time_range, string) result
(** [free_busy_of_xml x] is the time range [x] holds. *)

(** {1 Responses} *)

type entry = {
  href : string;  (** The href of the calendar object. *)
  etag : string option;  (** Its ETag, if the response carried one. *)
  data : string option;
      (** The calendar object text, if asked for and returned. *)
  response : Httpz_dav.response;  (** The underlying response. *)
}
(** The type for one matching calendar object. *)

type outcome = {
  entries : entry list;  (** The matching calendar objects. *)
  truncated : bool;
}
(** The type for what a report returns. [truncated] is [true] if the collection
    itself answered [507],
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.8} RFC 4791 Section
     7.8}. *)

val outcome_of_multistatus : base:string -> Httpz_dav.multistatus -> outcome
(** [outcome_of_multistatus ~base m] reads the entries of [m], a response to a
    report on [base]. A response for [base] itself, or one with a failure
    status, is not an entry. *)

val missing : Httpz_dav.multistatus -> string list
(** [missing m] are the hrefs [m] answers with [404], which a multiget uses for
    the resources that do not exist. *)
