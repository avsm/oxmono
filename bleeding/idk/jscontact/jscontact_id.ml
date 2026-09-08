(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string

let is_char c =
  (c >= 'A' && c <= 'Z')
  || (c >= 'a' && c <= 'z')
  || (c >= '0' && c <= '9')
  || c = '-' || c = '_'

let of_string s =
  let len = String.length s in
  if len = 0 then Error "an Id must be at least 1 octet"
  else if len > 255 then
    Error
      (Printf.sprintf "an Id must be at most 255 octets, but %S is %d" s len)
  else
    let rec check i =
      if i >= len then Ok s
      else if is_char s.[i] then check (i + 1)
      else
        Error
          (Printf.sprintf
             "invalid octet %C at position %d of Id %S: an Id is made of \
              A-Za-z0-9, the hyphen and the underscore"
             s.[i] i s)
    in
    check 0

let v s =
  match of_string s with
  | Ok id -> id
  | Error msg -> invalid_arg ("Jscontact_id: " ^ msg)

let to_string id = id
let equal = String.equal
let compare = String.compare
let pp ppf id = Format.pp_print_string ppf id
let jsont = Jsont.of_of_string ~kind:"Id" ~enc:to_string of_string
