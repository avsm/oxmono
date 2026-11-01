(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JSContact to vCard.

    The conversion of
    {{:https://www.rfc-editor.org/rfc/rfc9555.html#section-3} RFC 9555 Section
     3}, which writes a JSContact Card as a vCard 4.0. A JSContact property with
    no vCard counterpart survives as a [JSPROP] property, and the [vCardProps]
    and [vCardParams] members a conversion from vCard left behind are put back
    on the properties they came from. *)

val convert_card : Jscontact.Card.t -> (Vcard.t, string) result
(** [convert_card c] is the vCard that the Card [c] converts to. See
    {!Jscontact_vcard.to_vcard}, which is this function. *)
