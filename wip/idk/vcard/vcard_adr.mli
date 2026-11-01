@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The ADR property.

    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-6.3.1} RFC 6350
     Section 6.3.1} defines the seven components of a delivery address, and
    {{:https://www.rfc-editor.org/rfc/rfc9554.html#section-2.1} RFC 9554 Section
     2.1} adds eleven more that separate the parts a street address component
    combines. Each component may hold several values.

    @canonical Vcard.Adr *)

type t = {
  po_box : string list;  (** The post office box. *)
  extended : string list;  (** The extended address, such as a suite. *)
  street : string list;  (** The street address. *)
  locality : string list;  (** The locality, such as a city. *)
  region : string list;  (** The region, such as a state or province. *)
  postal_code : string list;  (** The postal code. *)
  country : string list;  (** The country name. *)
  room : string list;  (** The room, suite number or identifier. *)
  apartment : string list;
      (** The apartment number, extension designation or box number. *)
  floor : string list;  (** The building floor or level. *)
  street_number : string list;  (** The street number. *)
  street_name : string list;  (** The street name. *)
  building : string list;  (** The building, tower or condominium. *)
  block : string list;  (** The block name or number. *)
  subdistrict : string list;  (** The subdistrict. *)
  district : string list;  (** The district. *)
  landmark : string list;
      (** A publicly known prominent feature that can substitute the street name
          and number. *)
  direction : string list;  (** The cardinal direction or quadrant. *)
}
(** The type for the components of an address. An absent component is the empty
    list. The components from [room] onwards are those of
    {{:https://www.rfc-editor.org/rfc/rfc9554.html} RFC 9554}. *)

val empty : t
(** [empty] is the address with no component. *)

val v :
  ?po_box:string list ->
  ?extended:string list ->
  ?street:string list ->
  ?locality:string list ->
  ?region:string list ->
  ?postal_code:string list ->
  ?country:string list ->
  ?room:string list ->
  ?apartment:string list ->
  ?floor:string list ->
  ?street_number:string list ->
  ?street_name:string list ->
  ?building:string list ->
  ?block:string list ->
  ?subdistrict:string list ->
  ?district:string list ->
  ?landmark:string list ->
  ?direction:string list ->
  unit ->
  t
(** [v ()] is the address with the given components. Every component defaults to
    the empty list. *)

val of_value : string -> t
(** [of_value s] is the address whose wire value is [s]. A component beyond the
    eighteenth is dropped. *)

val to_value : t -> string
(** [to_value a] is the wire value of [a], with all eighteen components written,
    as {{:https://www.rfc-editor.org/rfc/rfc9554.html} RFC 9554} recommends. *)

val has_extended : t -> bool
(** [has_extended a] is [true] if [a] sets a component
    {{:https://www.rfc-editor.org/rfc/rfc9554.html} RFC 9554} defines, in which
    case Section 2.1 of that RFC has a reader ignore [street]. *)

val of_property : Vcard_property.t -> (t, string) result
(** [of_property p] is the address [p] holds, or an error if [p] is not an [ADR]
    property. *)

val to_property :
  ?group:string -> ?params:Vcard_param.t list -> t -> Vcard_property.t
(** [to_property ~group ~params a] is the [ADR] property holding [a]. [group]
    defaults to none and [params] to the empty list. *)

val to_components : t -> string list list
(** [to_components a] are the eighteen components of [a], in wire order, each
    the list of its values. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same components. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf a] formats the wire value of [a] on [ppf]. *)
