(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

include Zarrz.Consolidated

(* ["utm"] and exactly two digits. The zone arrays sit one path
   component deeper, so nothing else in the store is this shape. *)
let zone_of_name p =
  if String.length p = 5 && String.sub p 0 3 = "utm" then
    let d0 = p.[3] and d1 = p.[4] in
    if d0 >= '0' && d0 <= '9' && d1 >= '0' && d1 <= '9' then
      let z = ((Char.code d0 - 48) * 10) + (Char.code d1 - 48) in
      if z >= 1 && z <= 60 then Some z else None
    else None
  else None

let zones t =
  let z =
    List.filter_map
      (function name, `Group -> zone_of_name name | _, `Array -> None)
      (children t "")
  in
  List.sort_uniq Int.compare z
