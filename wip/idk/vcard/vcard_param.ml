(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { name : string; values : string list }

let v name values = { name = String.uppercase_ascii name; values }
let name p = p.name
let values p = p.values

let equal a b =
  String.equal a.name b.name && List.equal String.equal a.values b.values

let values_named ps name =
  let name = String.uppercase_ascii name in
  List.concat_map
    (fun p -> if String.equal p.name name then p.values else [])
    ps

let find_values ps name =
  let name = String.uppercase_ascii name in
  if List.exists (fun p -> String.equal p.name name) ps then
    Some (values_named ps name)
  else None

let find_first ps name =
  match values_named ps name with [] -> None | v :: _ -> Some v

let decode_value s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i >= n then ()
    else if s.[i] = '^' && i + 1 < n then (
      (match s.[i + 1] with
      | 'n' -> Buffer.add_char b '\n'
      | '^' -> Buffer.add_char b '^'
      | '\'' -> Buffer.add_char b '"'
      | c ->
          Buffer.add_char b '^';
          Buffer.add_char b c);
      go (i + 2))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let needs_quotes s = String.exists (fun c -> c = ':' || c = ';' || c = ',') s

let encode_value s =
  let b = Buffer.create (String.length s + 2) in
  let quote = needs_quotes s in
  if quote then Buffer.add_char b '"';
  String.iter
    (function
      | '\n' -> Buffer.add_string b "^n"
      | '^' -> Buffer.add_string b "^^"
      | '"' -> Buffer.add_string b "^'"
      | '\r' -> ()
      | c -> Buffer.add_char b c)
    s;
  if quote then Buffer.add_char b '"';
  Buffer.contents b

let to_string p =
  match p.values with
  | [] -> p.name
  | vs -> p.name ^ "=" ^ String.concat "," (List.map encode_value vs)

let pp ppf p = Format.pp_print_string ppf (to_string p)
