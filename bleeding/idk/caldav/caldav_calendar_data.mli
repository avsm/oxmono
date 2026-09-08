(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The calendar-data element.

    [CALDAV:calendar-data] appears among the properties of a report request to
    ask for the calendar object of each matching resource, and among the
    properties of a response to carry it,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.6} RFC 4791 Section
     9.6}. A request may narrow the components and properties returned, expand
    recurrences into instances, or limit the instances and free-busy periods
    returned.

    @canonical Caldav.Calendar_data *)

val name : Httpz_dav.name
(** [name] is [CALDAV:calendar-data]. *)

type comp = {
  comp_name : string;  (** The component name, such as ["VEVENT"]. *)
  props : [ `All | `Props of (string * bool) list ];
      (** The properties wanted, each with whether only its name is wanted. *)
  comps : [ `All | `Comps of comp list ];
      (** The components wanted inside this one. *)
}
(** The type for [CALDAV:comp] elements,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.6.1} RFC 4791
     Section 9.6.1}. *)

type t = {
  content_type : string option;
      (** The media type wanted, defaulting to [text/calendar]. *)
  version : string option;  (** The version wanted, defaulting to [2.0]. *)
  comp : comp option;  (** The root component wanted, normally VCALENDAR. *)
  expand : Caldav_filter.time_range option;
      (** Asks for recurrences expanded into instances in the range,
          {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.6.5} RFC 4791
           Section 9.6.5}. *)
  limit_recurrence_set : Caldav_filter.time_range option;
      (** Restricts a recurring component to its overridden instances that
          overlap the range,
          {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.6.6} RFC 4791
           Section 9.6.6}. *)
  limit_freebusy_set : Caldav_filter.time_range option;
      (** Restricts FREEBUSY values to those overlapping the range,
          {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.6.7} RFC 4791
           Section 9.6.7}. *)
}
(** The type for the request form. *)

val v :
  ?content_type:string ->
  ?version:string ->
  ?comp:comp ->
  ?expand:Caldav_filter.time_range ->
  ?limit_recurrence_set:Caldav_filter.time_range ->
  ?limit_freebusy_set:Caldav_filter.time_range ->
  unit ->
  t
(** [v ()] asks for the whole calendar object. Section 9.6 admits one of
    [expand] and [limit_recurrence_set], so [v] refuses both. A record built by
    hand escapes that check, and {!to_xml} then writes [expand] alone.

    @raise Invalid_argument
      if [expand] and [limit_recurrence_set] are both given. *)

val comp : ?props:(string * bool) list -> ?comps:comp list -> string -> comp
(** [comp ~props ~comps name] asks for the component [name] with [props] and
    [comps], each defaulting to all. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] ask for the same media type, version,
    components and ranges, the nested components compared in order. *)

val to_xml : t -> Httpz_dav.element
(** [to_xml t] is [t] as a [CALDAV:calendar-data] element. *)

val of_xml : Httpz_dav.element -> (t, string) result
(** [of_xml x] is the request [x] holds. *)

(** {1 Responses} *)

val data : Httpz_dav.element -> string option
(** [data p] is the iCalendar text a response [CALDAV:calendar-data] carries,
    with bare line feeds restored to carriage return and line feed, or [None] if
    [p] is another property. Whitespace around the value is dropped, so an
    object that was indented by a pretty-printing server reads correctly and one
    that was not loses its final line break. *)

val content_type_of : Httpz_dav.element -> string * string
(** [content_type_of p] are the [content-type] and [version] attributes of [p],
    with their defaults. *)
