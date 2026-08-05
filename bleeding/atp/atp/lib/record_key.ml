(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string
type error = [ `Invalid_record_key of string ]

let pp_error ppf = function
  | `Invalid_record_key s -> Fmt.pf ppf "invalid record key: %S" s

(* Allowed characters: a-zA-Z0-9._:~- *)
let is_valid_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '.' || c = '_' || c = ':' || c = '~' || c = '-'

let is_valid s =
  let len = String.length s in
  (* Length check: 1-512 *)
  if len < 1 || len > 512 then false (* Cannot be "." or ".." *)
  else if s = "." || s = ".." then false
  (* All characters must be valid *)
    else String.for_all is_valid_char s

let of_string s = if is_valid s then Ok s else Error (`Invalid_record_key s)

let of_string_exn s =
  match of_string s with
  | Ok t -> t
  | Error e -> invalid_arg (Format.asprintf "%a" pp_error e)

let to_string t = t
let equal = String.equal
let compare = String.compare
let pp ppf t = Fmt.string ppf t
