(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Conversion between JSContact and vCard.

    An implementation of
    {{:https://www.rfc-editor.org/rfc/rfc9555.html} RFC 9555}, which converts a
    vCard 4.0 to a JSContact Card by the rules of its Section 2 and a Card to a
    vCard by the reverse rules of its Section 3.

    A vCard property with no JSContact counterpart survives as a JCardProp in
    the [vCardProps] member of the Card, a parameter no rule converts in the
    [vCardParams] member of the object its property became, and a JSContact
    property with no vCard counterpart as a [JSPROP] property. A Card converted
    to a vCard and back is therefore the same Card, and a vCard converted to a
    Card and back holds the same properties. *)

module Jcard = Jscontact_vcard_jcard
(** The jCard encoding of a property,
    {{:https://www.rfc-editor.org/rfc/rfc9555.html#section-2.15.1} RFC 9555
     Section 2.15.1}. *)

val of_vcard :
  ?uid:(unit -> string) -> Vcard.t -> (Jscontact.Card.t, string) result
(** [of_vcard ~uid v] is the Card the vCard [v] converts to. The [uid] of the
    Card is the [UID] property of [v], or else [uid ()], which defaults to a URN
    derived from the text of [v] so that the same vCard yields the same uid, as
    Section 2.1.1 recommends.

    Each object of a map is keyed by the [PROP-ID] of its property, or else by
    [k1], [k2] and so on in the order of the properties. A property in a
    language other than the Card's, marked by [ALTID] and [LANGUAGE], becomes a
    patch of the [localizations] property, one per property.

    The error holds a message if the [JSPROP] properties of [v] do not form a
    valid PatchObject or if the Card they patch does not decode. *)

val to_vcard : Jscontact.Card.t -> (Vcard.t, string) result
(** [to_vcard c] is the vCard the Card [c] converts to. Every property built
    from a map entry carries the entry's key as [PROP-ID]. The [FN] property is
    the full name of [c], or else is derived from its name components and marked
    [DERIVED], or else is empty.

    A name or address whose components are ordered carries a [JSCOMPS]
    parameter. A localization that patches a converted property becomes a
    [LANGUAGE] variant of that property sharing its [ALTID], and any other
    patch, like any unknown member, becomes a [JSPROP]. The error holds a
    message if a localization of [c] cannot be applied. *)
