(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  | Binary
  | Boolean
  | Cal_address
  | Date
  | Date_time
  | Duration
  | Float
  | Integer
  | Period
  | Recur
  | Text
  | Time
  | Uri
  | Utc_offset
  | Other of string

let to_string = function
  | Binary -> "BINARY"
  | Boolean -> "BOOLEAN"
  | Cal_address -> "CAL-ADDRESS"
  | Date -> "DATE"
  | Date_time -> "DATE-TIME"
  | Duration -> "DURATION"
  | Float -> "FLOAT"
  | Integer -> "INTEGER"
  | Period -> "PERIOD"
  | Recur -> "RECUR"
  | Text -> "TEXT"
  | Time -> "TIME"
  | Uri -> "URI"
  | Utc_offset -> "UTC-OFFSET"
  | Other s -> s

let of_string s =
  match String.uppercase_ascii s with
  | "BINARY" -> Binary
  | "BOOLEAN" -> Boolean
  | "CAL-ADDRESS" -> Cal_address
  | "DATE" -> Date
  | "DATE-TIME" -> Date_time
  | "DURATION" -> Duration
  | "FLOAT" -> Float
  | "INTEGER" -> Integer
  | "PERIOD" -> Period
  | "RECUR" -> Recur
  | "TEXT" -> Text
  | "TIME" -> Time
  | "URI" -> Uri
  | "UTC-OFFSET" -> Utc_offset
  | s -> Other s

let equal a b = String.equal (to_string a) (to_string b)
let pp ppf t = Format.pp_print_string ppf (to_string t)
