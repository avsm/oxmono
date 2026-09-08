@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Postal addresses and geographical locations.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.5} RFC 9553 Section
     2.5} types the [addresses] property of a Card as [Id[Address]]. An Address
    holds the components of a postal address, a full address as a single string,
    a country code, a [geo:] URI, a time zone, or any combination of them.

    @canonical Jscontact.Address *)

(** The parts of an address.

    An AddressComponent is one element of a postal address, such as a street
    name or a postal code, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.5.1.2} RFC 9553
     Section 2.5.1.2}. *)
module Component : sig
  (** The kinds of address component.

      The enumerated values of the [kind] property of an AddressComponent, as
      listed in
      {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.5.1.2} RFC 9553
       Section 2.5.1.2}. *)
  module Kind : sig
    type t =
      [ `Room  (** The room, suite number, or identifier. *)
      | `Apartment
        (** The extension designation, such as the apartment number, unit, or
            box number. *)
      | `Floor  (** The floor or level the address is located on. *)
      | `Building
        (** The building, tower, or condominium the address is located in. *)
      | `Number
        (** The street number. It is not restricted to numeric values. A range
            ["112-10"], a grid style ["39.2 RD"], an alphanumeric ["N6W23001"]
            and a fraction ["123 1/2"] are all street numbers. *)
      | `Name  (** The street name. *)
      | `Block  (** The block name or number. *)
      | `Subdistrict
        (** The subdistrict, ward, or other subunit of a district. *)
      | `District  (** The district name. *)
      | `Locality
        (** The municipality, city, town, village, post town, or other locality.
        *)
      | `Region
        (** The administrative area, such as a province, state, prefecture,
            county, or canton. *)
      | `Postcode
        (** The postal code, post code, ZIP code, or other short code the
            relevant country's postal system associates with the address. *)
      | `Country  (** The country name. *)
      | `Direction  (** The cardinal direction or quadrant, such as "north". *)
      | `Landmark
        (** The publicly known prominent feature that can substitute the street
            name and number, such as "White House". *)
      | `Post_office_box  (** The post office box number or identifier. *)
      | `Separator
        (** A formatting separator between two ordered non-separator components.
            The [value] of the component is the verbatim separator, which may be
            the empty string, and takes precedence over the [default_separator]
            of the address. Two separator components must not be consecutive. A
            single one holding the combined value is written instead. This kind
            must not be set if the address is not ordered. *)
      | `Vendor of string  (** A vendor-specific kind. *) ]
    (** The type for the kind of an address component. *)

    include Jscontact_enum.S with type t := t
  end

  type t = {
    value : string;  (** The value of the address component. *)
    kind : Kind.t;  (** The kind of the address component. *)
    phonetic : string option;
        (** The pronunciation of the component value. If this is set, then at
            least one of the [phonetic_script] and [phonetic_system] properties
            of the address holding the component must be set. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for an address component. *)

  val make :
    ?phonetic:string -> ?unknown:Jscontact_unknown.t -> Kind.t -> string -> t
  (** [make kind value] is the component of kind [kind] with value [value].
      Every optional argument defaults to the property being unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf c] formats [c] on [ppf] as its kind and value. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate c] checks the kind of [c] and its unknown members. That a
      [phonetic] requires a phonetic script or system is a rule of the address
      holding [c], and {!Jscontact.Address.validate} checks it there. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an AddressComponent. *)
end

type t = {
  components : Component.t list option;
      (** The components that make up the address. The list must have at least
          one entry whose kind is not [`Separator]. *)
  is_ordered : bool;
      (** The indicator of whether the components are ordered, that is whether
          joining their values as a string produces a valid full address.
          Defaults to [false]. *)
  country_code : string option;  (** The Alpha-2 country code of ISO 3166-1. *)
  coordinates : string option;
      (** A {{:https://www.rfc-editor.org/rfc/rfc5870.html} [geo:]} URI for the
          address. *)
  time_zone : string option;
      (** The time zone the address is located in, a name registered in the
          {{:https://www.iana.org/time-zones} IANA Time Zone Database}. *)
  contexts : Jscontact_context.t list option;
      (** The contexts in which to use the address. Besides the common contexts,
          an address may use [`Billing] and [`Delivery]. *)
  full : string option;
      (** The full address, including street, region or country. It defines an
          address even when the individual components are not known. *)
  default_separator : string option;
      (** The default separator to insert between component values when
          concatenating them to a single string. It must not be set if the
          address is not ordered or if [components] is not set. *)
  pref : int option;
      (** The preference of the address in relation to other addresses, in the
          range 1 to 100. *)
  phonetic_script : string option;
      (** The script used in the [phonetic] of the components, a script subtag
          as defined by
          {{:https://www.rfc-editor.org/rfc/rfc5646.html#section-2.2.3} RFC 5646
           Section 2.2.3}. *)
  phonetic_system : Jscontact_phonetic.t option;
      (** The phonetic system used in the [phonetic] of the components. *)
  unknown : Jscontact_unknown.t;  (** The members no property above names. *)
}
(** The type for an address, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.5.1.1} RFC 9553
     Section 2.5.1.1}. At least one of [components], [coordinates],
    [country_code], [full] and [time_zone] must be set. *)

val make :
  ?components:Component.t list ->
  ?is_ordered:bool ->
  ?country_code:string ->
  ?coordinates:string ->
  ?time_zone:string ->
  ?contexts:Jscontact_context.t list ->
  ?full:string ->
  ?default_separator:string ->
  ?pref:int ->
  ?phonetic_script:string ->
  ?phonetic_system:Jscontact_phonetic.t ->
  ?unknown:Jscontact_unknown.t ->
  unit ->
  t
(** [make ()] is an address with the given properties. Every optional argument
    defaults to the property being unset, except [is_ordered], which defaults to
    [false]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same properties. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf a] formats [a] on [ppf] as its [full] value, if set, and otherwise
    as the values of its components. *)

val validate : t -> t Jscontact_valid.t
(** [validate a] checks [a] against the rules of
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.5.1.1} Section
     2.5.1.1}. One of [components], [coordinates], [country_code], [full] and
    [time_zone] must be set. The components must have an entry that is not a
    separator. Two separator components must not be consecutive. Neither a
    separator component nor [default_separator] appears unless [is_ordered] is
    [true]. [default_separator] is set only alongside [components]. A component
    [phonetic] is accompanied by [phonetic_script] or [phonetic_system].
    [validate] checks the shape of [country_code], [coordinates],
    [phonetic_script], [phonetic_system] and [pref]. It recurses into the
    components and the unknown members.

    A [time_zone] is not checked against the IANA Time Zone Database, which the
    library does not carry. *)

val jsont : t Jsont.t
(** [jsont] is the codec for an Address. *)
