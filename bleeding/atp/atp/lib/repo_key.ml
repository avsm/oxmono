(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string
type error = [ `Invalid_repo_key of string ]

let pp_error ppf = function
  | `Invalid_repo_key s -> Fmt.pf ppf "invalid repository key: %S" s

let valid_chars_regex = Re.Pcre.regexp "^[a-zA-Z0-9_~\\-:.]*$"

let is_valid key =
  match String.split_on_char '/' key with
  | [ coll; rkey ]
    when String.length key <= 1024
         && coll <> "" && rkey <> ""
         && Re.execp valid_chars_regex coll
         && Re.execp valid_chars_regex rkey ->
      true
  | _ -> false

let of_string s = if is_valid s then Ok s else Error (`Invalid_repo_key s)

let of_string_exn s =
  match of_string s with
  | Ok t -> t
  | Error e -> invalid_arg (Format.asprintf "%a" pp_error e)

let validate_exn s = ignore (of_string_exn s)
let to_string t = t
let equal = String.equal
let compare = String.compare
let pp ppf t = Fmt.string ppf t
