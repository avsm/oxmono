(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { raw : string; method_ : string; method_specific_id : string }
type error = [ `Invalid_did of string ]

let pp_error ppf = function `Invalid_did s -> Fmt.pf ppf "invalid DID: %S" s

(* DID method must be lowercase letters only *)
let is_valid_method_char c = c >= 'a' && c <= 'z'

(* Method-specific ID can contain: a-z A-Z 0-9 . - _ : % *)
let is_valid_msid_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '.' || c = '-' || c = '_' || c = ':' || c = '%'

(* Validate percent encoding - must be %XX where X is hex *)
let validate_percent_encoding s =
  let len = String.length s in
  let rec check i =
    if i >= len then true
    else if s.[i] = '%' then
      if i + 2 >= len then false
      else
        let is_hex c =
          (c >= '0' && c <= '9')
          || (c >= 'a' && c <= 'f')
          || (c >= 'A' && c <= 'F')
        in
        if is_hex s.[i + 1] && is_hex s.[i + 2] then check (i + 3) else false
    else check (i + 1)
  in
  check 0

let is_valid s =
  let len = String.length s in
  (* Must start with "did:" and have reasonable length *)
  len > 4 && len <= 2048
  && String.sub s 0 4 = "did:"
  &&
  let rest = String.sub s 4 (len - 4) in
  match String.index_opt rest ':' with
  | None -> false
  | Some colon_pos ->
      (* Method must not be empty *)
      colon_pos > 0
      &&
      let method_ = String.sub rest 0 colon_pos in
      let msid =
        String.sub rest (colon_pos + 1) (String.length rest - colon_pos - 1)
      in
      (* Method must be lowercase letters only *)
      String.for_all is_valid_method_char method_
      (* Method-specific ID must not be empty and must have valid characters *)
      && String.length msid > 0
      && String.for_all is_valid_msid_char msid
      (* Method-specific ID must not end with colon *)
      && msid.[String.length msid - 1] <> ':'
      &&
      (* Validate percent encoding *)
      validate_percent_encoding msid

let of_string s =
  if not (is_valid s) then Error (`Invalid_did s)
  else
    let rest = String.sub s 4 (String.length s - 4) in
    match String.index_opt rest ':' with
    | None -> Error (`Invalid_did s)
    | Some colon_pos ->
        let method_ = String.sub rest 0 colon_pos in
        let method_specific_id =
          String.sub rest (colon_pos + 1) (String.length rest - colon_pos - 1)
        in
        Ok { raw = s; method_; method_specific_id }

let of_string_exn s =
  match of_string s with
  | Ok t -> t
  | Error e -> invalid_arg (Format.asprintf "%a" pp_error e)

let to_string t = t.raw
let method_ t = t.method_
let method_specific_id t = t.method_specific_id
let equal a b = String.equal a.raw b.raw
let compare a b = String.compare a.raw b.raw
let pp ppf t = Fmt.string ppf t.raw
