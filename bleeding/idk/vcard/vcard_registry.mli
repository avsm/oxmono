@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The property registry.

    The properties of the IANA "vCard Properties" registry that this library
    knows, with the value type each defaults to and the cardinality its defining
    section states. The entries come from
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-6} RFC 6350 Section
     6}, {{:https://www.rfc-editor.org/rfc/rfc6474.html} RFC 6474},
    {{:https://www.rfc-editor.org/rfc/rfc6715.html} RFC 6715},
    {{:https://www.rfc-editor.org/rfc/rfc8605.html} RFC 8605},
    {{:https://www.rfc-editor.org/rfc/rfc9554.html} RFC 9554} and
    {{:https://www.rfc-editor.org/rfc/rfc9555.html} RFC 9555}.

    @canonical Vcard.Registry *)

(** The type for cardinalities, in the notation of
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-3.3} RFC 6350 Section
     3.3}. Instances that share an [ALTID] parameter count as one. *)
type cardinality =
  | One  (** Exactly one instance per card. *)
  | At_most_one  (** At most one instance per card. *)
  | One_or_more  (** At least one instance per card. *)
  | Many  (** Any number of instances. *)

type entry = {
  name : string;  (** The property name, in uppercase. *)
  value_type : Vcard_value_type.t;
      (** The value type of the property when it carries no [VALUE] parameter.
      *)
  cardinality : cardinality;  (** How many instances a card may hold. *)
  reference : string;  (** The RFC and section that define the property. *)
}
(** The type for registry entries. *)

val all : entry list
(** [all] are the entries, in the order of their defining documents. *)

val find : string -> entry option
(** [find name] is the entry of the property [name], compared case
    insensitively, or [None] if this library does not know it. *)

val value_type : string -> Vcard_value_type.t
(** [value_type name] is the default value type of the property [name], and
    [Text] for a property this library does not know. *)

val cardinality : string -> cardinality
(** [cardinality name] is the cardinality of the property [name], and [Many] for
    a property this library does not know. *)
