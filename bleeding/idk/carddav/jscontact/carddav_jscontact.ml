(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let card =
  Carddav.Data.map ~decode:Jscontact_vcard.of_vcard
    ~encode:Jscontact_vcard.to_vcard Carddav.Data.vcard

let json =
  Carddav.Data.map
    ~decode:(Jsont.Json.encode Jscontact.Card.jsont)
    ~encode:(Jsont.Json.decode Jscontact.Card.jsont)
    card
