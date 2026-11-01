(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Calendar query filters.

    A filter selects calendar object resources by the components they hold, the
    properties and parameters of those components, the text of either, and the
    time range a component overlaps,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.7} RFC 4791 Section
     9.7}. A filter is a tree of component filters rooted at VCALENDAR.

    @canonical Caldav.Filter *)

type time_range = {
  start : Ical.Date.date_time option;  (** The lower bound, if given. *)
  finish : Ical.Date.date_time option;  (** The upper bound, if given. *)
}
(** The type for [CALDAV:time-range] elements,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.9} RFC 4791 Section
     9.9}. Both bounds are UTC, and at least one is given. A component matches
    if it overlaps the half open interval. *)

type text_match = {
  text : string;  (** The text to match against. *)
  collation : string option;
  negate : bool;  (** [true] matches when the text does not match. *)
}
(** The type for [CALDAV:text-match] elements,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.7.5} RFC 4791
     Section 9.7.5}. A match is a substring match under the collation, which
    defaults to [i;ascii-casemap]. *)

type param_filter = {
  param : string;  (** The parameter name, such as ["TZID"]. *)
  param_test : [ `Defined | `Not_defined | `Match of text_match ];
      (** Whether [param] must be present, absent, or match. *)
}
(** The type for [CALDAV:param-filter] elements,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.7.3} RFC 4791
     Section 9.7.3}. *)

type prop_filter = {
  prop : string;  (** The property name, such as ["ATTENDEE"]. *)
  prop_condition :
    [ `Defined  (** The property exists. *)
    | `Not_defined  (** The property does not exist. *)
    | `Matches of
      [ `Time_range of time_range | `Text of text_match ] option
      * param_filter list
      (** The property exists and satisfies the time range or text match, when
          given, and the parameter filters. *) ];
}
(** The type for [CALDAV:prop-filter] elements,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.7.2} RFC 4791
     Section 9.7.2}. *)

type comp_filter = {
  comp : string;  (** The component name, such as ["VEVENT"]. *)
  comp_condition :
    [ `Defined  (** The component exists. *)
    | `Not_defined  (** The component does not exist. *)
    | `Matches of time_range option * comp_filter list * prop_filter list
      (** The component exists and satisfies the time range, when given, and the
          nested component and property filters. *) ];
}
(** The type for [CALDAV:comp-filter] elements,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.7.1} RFC 4791
     Section 9.7.1}. *)

type t = comp_filter
(** The type for [CALDAV:filter] elements, which hold one component filter,
    normally for VCALENDAR. *)

(** {1 Constructors} *)

val time_range :
  ?start:Ical.Date.date_time ->
  ?finish:Ical.Date.date_time ->
  unit ->
  time_range
(** [time_range ~start ~finish ()] is the range from [start] to [finish].

    @raise Invalid_argument if neither bound is given. *)

val text_match : ?collation:string -> ?negate:bool -> string -> text_match
(** [text_match ~collation ~negate text] is a match for [text]. [collation]
    defaults to [i;ascii-casemap]. [negate] defaults to [false]. *)

val prop :
  ?params:param_filter list ->
  string ->
  [ `Time_range of time_range | `Text of text_match ] option ->
  prop_filter
(** [prop ~params name matched] selects the property [name] whose value
    satisfies [matched], when given, and whose parameters satisfy [params].
    [params] defaults to the empty list. With neither, it selects the property's
    presence. *)

val prop_not_defined : string -> prop_filter
(** [prop_not_defined name] selects components without the property [name]. *)

val param : string -> text_match option -> param_filter
(** [param name m] selects the parameter [name], with the value matching [m]
    when given. *)

val param_not_defined : string -> param_filter
(** [param_not_defined name] selects properties without the parameter [name]. *)

val comp :
  ?time_range:time_range ->
  ?comps:comp_filter list ->
  ?props:prop_filter list ->
  string ->
  comp_filter
(** [comp ~time_range ~comps ~props name] selects the component [name] that
    overlaps [time_range] and whose components and properties satisfy [comps]
    and [props]. *)

val comp_not_defined : string -> comp_filter
(** [comp_not_defined name] selects components without a nested component named
    [name]. *)

val v : comp_filter list -> t
(** [v comps] is the filter for a VCALENDAR holding [comps]. *)

val components :
  ?time_range:time_range -> ?props:prop_filter list -> string -> t
(** [components ~time_range ~props name] selects the calendar objects with a
    component [name], such as ["VEVENT"], overlapping [time_range] and
    satisfying [props]. *)

val all : t
(** [all] matches every calendar object. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] name the same component under the same
    condition, their nested filters compared in order. *)

val equal_time_range : time_range -> time_range -> bool
(** [equal_time_range a b] is [true] if [a] and [b] have the same bounds. *)

(** {1 XML} *)

val to_xml : t -> Httpz_dav.element
(** [to_xml t] is [t] as a [CALDAV:filter] element. *)

val of_xml : Httpz_dav.element -> (t, string) result
(** [of_xml x] is the filter [x] holds. *)

val time_range_to_xml : time_range -> Httpz_dav.element
(** [time_range_to_xml tr] is [tr] as a [CALDAV:time-range] element. *)

val time_range_of_xml : Httpz_dav.element -> (time_range, string) result
(** [time_range_of_xml x] is the time range [x] holds. *)

(** {1 Matching} *)

val matches : t -> Ical.t -> bool
(** [matches t cal] is [true] if [cal] satisfies [t] under the rules of
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.7} RFC 4791 Section
     9.7}, with a time range tested against the DTSTART and
    {!Ical.Component.dtend} of a component as Section 9.9 defines for the
    instance the component states. Recurrence instances are not expanded, so a
    recurring component matches only when its first instance does. A collation
    this library does not know compares bytes. A server instead answers such a
    request with the [CALDAV:supported-collation] precondition, Section 7.5.

    Of the tables of Section 9.9, the rows on DTSTART, DTEND, DURATION and DUE
    are implemented. A to-do carrying none of those, and a free-busy carrying
    only FREEBUSY properties, do not match a time range. *)
