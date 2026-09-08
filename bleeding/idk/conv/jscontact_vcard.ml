(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Jcard = Jscontact_vcard_jcard

let of_vcard ?uid v = Jscontact_vcard_import.convert_card ?uid v
let to_vcard c = Jscontact_vcard_export.convert_card c
