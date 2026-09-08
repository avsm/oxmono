(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let validate p =
  Jscontact_valid.(
    let* () =
      check (p >= 1 && p <= 100) "pref: %d is outside the range 1 to 100" p
    in
    ok p)

let jsont = Jscontact_json.unsigned ~kind:"pref"
