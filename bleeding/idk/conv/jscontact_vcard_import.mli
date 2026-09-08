(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** vCard to JSContact.

    The conversion of
    {{:https://www.rfc-editor.org/rfc/rfc9555.html#section-2} RFC 9555 Section
     2}, which reads a vCard 4.0 as a JSContact Card. A property with no
    JSContact counterpart survives as a JCardProp in the [vCardProps] member of
    the Card, and a parameter no rule converts in the [vCardParams] member of
    the object its property became. *)

val convert_card :
  ?uid:(unit -> string) -> Vcard.t -> (Jscontact.Card.t, string) result
(** [convert_card ~uid card] is the Card that [card] converts to. See
    {!Jscontact_vcard.of_vcard}, which is this function. *)
