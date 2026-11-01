(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = Jsont.json

let empty = Jsont.Object ([], Jsont.Meta.none)
let is_empty = function Jsont.Object (mems, _) -> mems = [] | _ -> true

let find u name =
  match u with
  | Jsont.Object (mems, _) -> Option.map snd (Jsont.Json.find_mem name mems)
  | _ -> None

let mems = Jsont.json_mems
