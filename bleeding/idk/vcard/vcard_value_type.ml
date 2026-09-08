(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  | Text
  | Uri
  | Date
  | Time
  | Date_time
  | Date_and_or_time
  | Timestamp
  | Boolean
  | Integer
  | Float
  | Utc_offset
  | Language_tag
  | Other of string

let to_string = function
  | Text -> "text"
  | Uri -> "uri"
  | Date -> "date"
  | Time -> "time"
  | Date_time -> "date-time"
  | Date_and_or_time -> "date-and-or-time"
  | Timestamp -> "timestamp"
  | Boolean -> "boolean"
  | Integer -> "integer"
  | Float -> "float"
  | Utc_offset -> "utc-offset"
  | Language_tag -> "language-tag"
  | Other s -> s

let of_string s =
  match String.lowercase_ascii s with
  | "text" -> Text
  | "uri" -> Uri
  | "date" -> Date
  | "time" -> Time
  | "date-time" -> Date_time
  | "date-and-or-time" -> Date_and_or_time
  | "timestamp" -> Timestamp
  | "boolean" -> Boolean
  | "integer" -> Integer
  | "float" -> Float
  | "utc-offset" -> Utc_offset
  | "language-tag" -> Language_tag
  | other -> Other other

let equal a b = String.equal (to_string a) (to_string b)
let pp ppf t = Format.pp_print_string ppf (to_string t)
