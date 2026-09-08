(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = [ `Ipa | `Jyut | `Piny | `Vendor of string ]

include Jscontact_enum.Make (struct
  type nonrec t = t

  let kind = "phoneticSystem"

  let to_string = function
    | `Ipa -> "ipa"
    | `Jyut -> "jyut"
    | `Piny -> "piny"
    | `Vendor s -> s

  let of_string = function
    | "ipa" -> `Ipa
    | "jyut" -> `Jyut
    | "piny" -> `Piny
    | s -> `Vendor s

  let is_vendor = function `Vendor _ -> true | _ -> false
end)

let validate_script s =
  Jscontact_valid.(
    let* () =
      check
        (String.length s = 4 && String.for_all Jscontact_ascii.is_alpha s)
        "phoneticScript: %S is not a script subtag, which is four letters" s
    in
    ok s)
