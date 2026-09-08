(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JSContact cards as CardDAV address objects.

    A CardDAV server stores vCards, and a program that works in JSContact reads
    and writes them through the conversion of
    {{:https://www.rfc-editor.org/rfc/rfc9555.html} RFC 9555}. The codec here is
    given to any [Carddav_eio.Client] function that takes one, and the card it
    yields carries the vCard properties, parameters and names that have no
    JSContact counterpart, so that writing it back loses nothing. *)

val card : Jscontact.Card.t Carddav.Data.t
(** [card] reads a vCard 4.0 as a {!Jscontact.Card.t} by
    {!Jscontact_vcard.of_vcard} and writes one by {!Jscontact_vcard.to_vcard}. A
    server that serves vCard 3.0 is read through the upgrade of
    [Carddav_eio.Quirks], which the client applies before this codec. *)

val json : Jsont.json Carddav.Data.t
(** [json] is {!card} as the JSON value of
    {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553}, for a program
    that passes cards on rather than reading them. *)
