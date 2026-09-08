@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The N property.

    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-6.2.2} RFC 6350
     Section 6.2.2} defines the structured value of the [N] property, and
    {{:https://www.rfc-editor.org/rfc/rfc9554.html#section-2.2} RFC 9554 Section
     2.2} extends it with a secondary surname and a generation component. Each
    component may hold several values.

    @canonical Vcard.N *)

type t = {
  family : string list;  (** The family names, also known as surnames. *)
  given : string list;  (** The given names. *)
  additional : string list;  (** The additional names. *)
  prefixes : string list;  (** The honorific prefixes. *)
  suffixes : string list;  (** The honorific suffixes. *)
  surname2 : string list;
      (** The secondary surnames, per
          {{:https://www.rfc-editor.org/rfc/rfc9554.html} RFC 9554}. *)
  generation : string list;
      (** The generation markers such as ["Jr."], per
          {{:https://www.rfc-editor.org/rfc/rfc9554.html} RFC 9554}. *)
}
(** The type for the components of a name. An absent component is the empty
    list. *)

val empty : t
(** [empty] is the name with no component. *)

val v :
  ?family:string list ->
  ?given:string list ->
  ?additional:string list ->
  ?prefixes:string list ->
  ?suffixes:string list ->
  ?surname2:string list ->
  ?generation:string list ->
  unit ->
  t
(** [v ()] is the name with the given components. Every component defaults to
    the empty list. *)

val of_value : string -> t
(** [of_value s] is the name whose wire value is [s]. A component beyond the
    seventh is dropped. *)

val to_value : t -> string
(** [to_value n] is the wire value of [n], with all seven components written, as
    {{:https://www.rfc-editor.org/rfc/rfc9554.html} RFC 9554} recommends. *)

val of_property : Vcard_property.t -> (t, string) result
(** [of_property p] is the name [p] holds, or an error if [p] is not an [N]
    property. *)

val to_property :
  ?group:string -> ?params:Vcard_param.t list -> t -> Vcard_property.t
(** [to_property ~group ~params n] is the [N] property holding [n]. [group]
    defaults to none and [params] to the empty list. *)

val to_components : t -> string list list
(** [to_components n] are the seven components of [n], in wire order, each the
    list of its values. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same components. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf n] formats the wire value of [n] on [ppf]. *)
