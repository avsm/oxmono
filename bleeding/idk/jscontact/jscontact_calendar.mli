@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Calendaring and scheduling.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.4} RFC 9553 Section
     2.4} defines two properties for scheduling calendar events with the entity
    the Card represents: [calendars], whose values are the Calendar objects of
    this module, and [schedulingAddresses], whose values are its
    {!Scheduling_address.t}.

    A Calendar is a {!Jscontact.Resource.type-base}. A SchedulingAddress is not.
    Section 2.4.2 gives it neither a kind nor a media type.

    @canonical Jscontact.Calendar *)

(** The kinds of calendaring resource.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.4.1} RFC 9553
     Section 2.4.1} makes the kind of a Calendar mandatory and enumerates its
    values. *)
module Kind : sig
  type t =
    [ `Calendar
      (** A calendar holding entries such as calendar events or tasks. *)
    | `Free_busy
      (** A resource allowing free-busy lookups, for example to schedule group
          events. Its wire spelling is ["freeBusy"]. *)
    | `Vendor of string  (** A vendor-specific kind. *) ]
  (** The type for the kind of a calendaring resource. *)

  include Jscontact_enum.S with type t := t
end

type t = Kind.t Jscontact_resource.base
(** The type for a calendaring resource, the value type of the [calendars]
    property of a Card, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.4.1} RFC 9553
     Section 2.4.1}. It has the properties of a Resource, with the kind made
    mandatory. *)

val make :
  ?uri:string ->
  ?media_type:string ->
  ?contexts:Jscontact_context.t list ->
  ?pref:int ->
  ?label:string ->
  ?unknown:Jscontact_unknown.t ->
  Kind.t ->
  t
(** [make kind] is the calendaring resource of kind [kind] with the given
    properties. Every optional argument defaults to the property being unset. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same properties. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf c] formats [c] on [ppf]. *)

val validate : t -> t Jscontact_valid.t
(** [validate c] is {!Jscontact.Resource.validate} on [c]. Section 2.4.1 adds no
    rule of its own. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a Calendar. The [kind] member is mandatory. *)

(** Scheduling addresses.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.4.2} RFC 9553
     Section 2.4.2} defines the SchedulingAddress object, the value type of the
    [schedulingAddresses] property of a Card. A SchedulingAddress is an address
    at which the entity receives calendar scheduling invitations. *)
module Scheduling_address : sig
  type t = {
    uri : string;
        (** The address to use for calendar scheduling with the contact, a URI
            as defined by
            {{:https://www.rfc-editor.org/rfc/rfc3986.html#section-3} RFC 3986
             Section 3}. *)
    contexts : Jscontact_context.t list option;
        (** The contexts in which to use the scheduling address. *)
    pref : int option;
        (** The preference of the scheduling address in relation to other
            scheduling addresses, from 1 to 100. *)
    label : string option;  (** A custom label for the scheduling address. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for a scheduling address. *)

  val make :
    ?contexts:Jscontact_context.t list ->
    ?pref:int ->
    ?label:string ->
    ?unknown:Jscontact_unknown.t ->
    string ->
    t
  (** [make uri] is the scheduling address [uri] with the given properties.
      Every optional argument defaults to the property being unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf a] formats [a] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate a] checks the [uri] of [a] with {!Jscontact.Uri.validate}, and
      its contexts, its preference and its unknown members. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a SchedulingAddress. *)
end
