(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  | String
  | Int
  | Float
  | Bool
  | Datetime
  | Datetime_local
  | Date
  | Time
  | Array
  | Table

let to_string = function
  | String -> "string"
  | Int -> "integer"
  | Float -> "float"
  | Bool -> "boolean"
  | Datetime -> "datetime"
  | Datetime_local -> "datetime-local"
  | Date -> "date-local"
  | Time -> "time-local"
  | Array -> "array"
  | Table -> "table"

let pp fmt t = Fmt.string fmt (to_string t)
let or_kind ~kind sort = if kind = "" then to_string sort else kind
let kinded_string ~kind:k s = if k = "" then s else k ^ " " ^ s
let kinded ~kind sort = kinded_string ~kind (to_string sort)
