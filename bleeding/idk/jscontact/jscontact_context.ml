(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = [ `Private | `Work | `Billing | `Delivery | `Vendor of string ]

include Jscontact_enum.Make (struct
  type nonrec t = t

  let kind = "contexts"

  let to_string = function
    | `Private -> "private"
    | `Work -> "work"
    | `Billing -> "billing"
    | `Delivery -> "delivery"
    | `Vendor s -> s

  let of_string = function
    | "private" -> `Private
    | "work" -> `Work
    | "billing" -> `Billing
    | "delivery" -> `Delivery
    | s -> `Vendor s

  let is_vendor = function `Vendor _ -> true | _ -> false
end)

let validate_set ?(address = false) cs =
  let one c =
    match c with
    | (`Billing | `Delivery) when not address ->
        Jscontact_valid.error "contexts: %S is defined for an Address alone"
          (to_string c)
    | _ -> validate c
  in
  Jscontact_valid.list one cs
