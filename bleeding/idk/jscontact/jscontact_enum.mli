@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Enumerated values.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.7.5} RFC 9553
     Section 1.7.5} restricts several properties to a list of string values, and
    lets a vendor add values of its own in the syntax of {!Jscontact.Vendor}.
    Each such property is typed as a polymorphic variant with one case per
    registered value and a [`Vendor] case holding the wire spelling of anything
    else, and its module is built with {!Make}.

    Decoding never rejects a value, so a Card that carries an enumerated value
    from a later JSContact version still decodes. {!VALUE.validate} is where a
    value outside the registry is held to the vendor grammar.

    @canonical Jscontact.Enum *)

(** The input of {!Make}. *)
module type VALUES = sig
  type t : immutable_data
  (** The type for the values. *)

  val kind : string @@ portable
  (** [kind] is the property name, which error messages cite. *)

  val to_string : t -> string @@ portable
  (** [to_string v] is the wire spelling of [v]. *)

  val of_string : string -> t @@ portable
  (** [of_string s] is the value spelled [s], and the [`Vendor] case holding [s]
      if no registered value is. *)

  val is_vendor : t -> bool @@ portable
  (** [is_vendor v] is [true] if [v] is the [`Vendor] case. *)
end

(** The surface of an enumeration over single values.

    It is {!S} without {!S.validate_set}, for an enumeration whose sets are held
    to a rule of their own. {!Jscontact.Context} is the one such, since
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.5.1.1} Section
     2.5.1.1} admits two of its values on an Address alone. *)
module type VALUE = sig
  type t : immutable_data
  (** The type for the values. *)

  val to_string : t -> string @@ portable
  (** [to_string v] is the wire spelling of [v]. *)

  val of_string : string -> t @@ portable
  (** [of_string s] is the value spelled [s], and the [`Vendor] case holding [s]
      if no registered value is. *)

  val is_vendor : t -> bool @@ portable
  (** [is_vendor v] is [true] if [v] is the [`Vendor] case. *)

  val equal : t -> t -> bool @@ portable
  (** [equal a b] is [true] if [a] and [b] have the same wire spelling. *)

  val compare : t -> t -> int @@ portable
  (** [compare a b] orders values by their wire spelling. *)

  val pp : Format.formatter -> t -> unit @@ portable
  (** [pp ppf v] formats the wire spelling of [v] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t @@ portable
  (** [validate v] is [Ok v] if [v] is a registered value, or a vendor-specific
      one whose spelling {!Jscontact.Vendor.validate_extension} accepts. *)

  val equal_set : t list -> t list -> bool @@ portable
  (** [equal_set a b] is [true] if [a] and [b] hold the same values, whatever
      their order or repetition. *)

  val jsont : t Jsont.t @@ portable
  (** [jsont] is the codec for one value. Decoding maps any string with
      {!of_string}. Encoding errors on a vendor-specific value whose spelling
      {!validate} rejects, since such a value cannot be written back as valid
      JSContact. *)

  val set_jsont : t list Jsont.t @@ portable
  (** [set_jsont] is the codec for a [String[Boolean]] set of values, such as
      the [contexts] of a phone number. See {!Jscontact.Json.Map.bool_set} for
      the shape of such a set. Decoding orders the list by wire spelling. *)
end

(** The surface of an enumeration. *)
module type S = sig
  type t : immutable_data
  (** The type for the values. *)

  include VALUE with type t := t

  val validate_set : t list -> t list Jscontact_valid.t @@ portable
  (** [validate_set vs] is {!validate} over [vs], reporting the index of the
      first value that fails. *)
end

(** [Make (V)] is the enumeration over the values of [V]. *)
module Make (V : VALUES) : S with type t := V.t
