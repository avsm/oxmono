(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let validate s =
  let subtag t =
    t <> "" && String.length t <= 8 && String.for_all Jscontact_ascii.is_alnum t
  in
  Jscontact_valid.(
    let* () =
      check
        (List.for_all subtag (String.split_on_char '-' s))
        "%S is not a well formed language tag, which is hyphen separated \
         subtags of 1 to 8 alphanumerics"
        s
    in
    ok s)
