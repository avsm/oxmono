(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* RFC 9553 Section 1.8.1: alnum-int = ALPHA / DIGIT / NON-ASCII, where
   NON-ASCII is the UTF-8 encoding of a scalar value above U+007F. JSON strings
   are UTF-8 by construction, so any byte above 0x7f is part of such an
   encoding. *)
let is_alnum_int c =
  (c >= 'A' && c <= 'Z')
  || (c >= 'a' && c <= 'z')
  || (c >= '0' && c <= '9')
  || Char.code c > 0x7f

(* v-label = alnum-int / alnum-int *(alnum-int / "-") alnum-int *)
let is_label s =
  let len = String.length s in
  if len = 0 then false
  else if not (is_alnum_int s.[0] && is_alnum_int s.[len - 1]) then false
  else
    let rec check i =
      if i >= len - 1 then true
      else if is_alnum_int s.[i] || s.[i] = '-' then check (i + 1)
      else false
    in
    check 1

(* v-prefix = v-label *("." v-label) *)
let is_prefix s = List.for_all is_label (String.split_on_char '.' s)

(* v-name = 1*(WSP / "!" / %x23-2e / %x30-7d / NON-ASCII) *)
let is_name_char c =
  let c = Char.code c in
  c = 0x09 || c = 0x20 || c = 0x21
  || (c >= 0x23 && c <= 0x2e)
  || (c >= 0x30 && c <= 0x7d)
  || c > 0x7f

let is_name s = s <> "" && String.for_all is_name_char s

let split s =
  match String.index_opt s ':' with
  | None -> None
  | Some i ->
      Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

let parts s =
  match split s with
  | Some (p, n) when is_prefix p && is_name n -> Some (p, n)
  | _ -> None

let is_extension s = Option.is_some (parts s)
let prefix s = Option.map fst (parts s)
let name s = Option.map snd (parts s)

let bad_prefix =
  "a vendor prefix is dot separated labels of alphanumerics and hyphens, each \
   starting and ending with an alphanumeric"

let bad_name =
  "a vendor name is a non-empty run of characters other than the controls, the \
   double quote, the solidus and the tilde"

let validate_extension s =
  match split s with
  | None ->
      Jscontact_valid.error
        "%S is not a vendor-specific name. It holds no colon separating the \
         vendor prefix from the name"
        s
  | Some (p, n) ->
      if not (is_prefix p) then
        Jscontact_valid.error "%S is not a valid vendor prefix in %S. %s" p s
          bad_prefix
      else if not (is_name n) then
        Jscontact_valid.error "%S is not a valid vendor name in %S. %s" n s
          bad_name
      else Ok s
