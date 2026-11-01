@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Components.

    A component is a run of properties between [BEGIN:name] and [END:name], with
    the components nested in it,
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.6} RFC 5545 Section
     3.6}. Every property and component is kept, known or not.

    @canonical Ical.Component *)

type t = {
  name : string;
      (** The name, such as ["VEVENT"], which {!v} and the reader store in upper
          case. *)
  properties : Ical_property.t list;
      (** The properties of the component, in the order they were read. *)
  components : t list;
      (** The components nested in this one, in the order they were read. *)
}
(** The type for components. *)

val v : ?properties:Ical_property.t list -> ?components:t list -> string -> t
(** [v ~properties ~components name] is the component [name]. *)

val find : t -> string -> Ical_property.t option
(** [find c name] is the first property [name] of [c]. *)

val find_all : t -> string -> Ical_property.t list
(** [find_all c name] are the properties [name] of [c], in order. *)

val text : t -> string -> string option
(** [text c name] is the TEXT value of the first property [name] of [c]. *)

val uid : t -> string option
(** [uid c] is the UID of [c]. *)

val children : t -> string -> t list
(** [children c name] are the components [name] of [c]. *)

val add : t -> Ical_property.t -> t
(** [add c p] is [c] with [p] appended. *)

val replace : t -> Ical_property.t -> t
(** [replace c p] is [c] with every property named as [p] removed and [p]
    appended. *)

val remove : t -> string -> t
(** [remove c name] is [c] without the properties [name]. *)

val dtstart : t -> (Ical_date.t option, string) result
(** [dtstart c] is the DTSTART of [c]. It is [Ok None] if [c] has no DTSTART,
    and an error if the property it has does not read. *)

val dtend : t -> (Ical_date.t option, string) result
(** [dtend c] is the end of [c], which is its DTEND or DUE, or its DTSTART moved
    by its DURATION, or for an event with a DATE start the next day and with a
    DATE-TIME start the start itself,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.9} RFC 4791 Section
     9.9}. It is [Ok None] if [c] has none of those properties, and an error if
    the one it has does not read. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same name, properties and
    components. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf c] prints [c] as content lines. *)
