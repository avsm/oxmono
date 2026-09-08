(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** jCard properties.

    {{:https://www.rfc-editor.org/rfc/rfc9555.html#section-2.15.1} RFC 9555
     Section 2.15.1} carries a vCard property that converts to no JSContact
    property as a JCardProp, the jCard array of
    {{:https://www.rfc-editor.org/rfc/rfc7095.html#section-3.3} RFC 7095 Section
     3.3}. The array holds the lowercase property name, an object of the
    parameters with the group as a [group] member, the value type name, and the
    values.

    @canonical Jscontact_vcard.Jcard *)

val of_property : Vcard.Property.t -> Jsont.json
(** [of_property p] is the JCardProp of [p]. The type is the [VALUE] parameter
    of [p], the default type of its name, or ["unknown"] for a property this
    library does not know, in which case the value is the unprocessed text. A
    boolean is a JSON boolean and an integer or float a JSON number. A
    structured text value is an array with one element per component, and a text
    list one value per element. The group is written in lowercase. *)

val to_property : Jsont.json -> (Vcard.Property.t, string) result
(** [to_property j] is the property the JCardProp [j] holds. A type other than
    the default of the name becomes a [VALUE] parameter. *)
