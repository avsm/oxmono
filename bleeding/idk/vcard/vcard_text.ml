(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let unescape s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i >= n then ()
    else if s.[i] = '\\' && i + 1 < n then (
      (match s.[i + 1] with
      | 'n' | 'N' -> Buffer.add_char b '\n'
      | ('\\' | ',' | ';') as c -> Buffer.add_char b c
      | c ->
          Buffer.add_char b '\\';
          Buffer.add_char b c);
      go (i + 2))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let escape_with ~semicolon s =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (function
      | '\\' -> Buffer.add_string b "\\\\"
      | ',' -> Buffer.add_string b "\\,"
      | ';' when semicolon -> Buffer.add_string b "\\;"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> ()
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let escape s = escape_with ~semicolon:false s
let escape_component s = escape_with ~semicolon:true s

let split sep s =
  let n = String.length s in
  let rec go acc start i =
    if i >= n then List.rev (String.sub s start (n - start) :: acc)
    else if s.[i] = '\\' && i + 1 < n then go acc start (i + 2)
    else if s.[i] = sep then
      go (String.sub s start (i - start) :: acc) (i + 1) (i + 1)
    else go acc start (i + 1)
  in
  go [] 0 0

let list_of_string s = List.map unescape (split ',' s)
let list_to_string l = String.concat "," (List.map escape l)

let structured_of_string s =
  List.map (fun field -> List.map unescape (split ',' field)) (split ';' s)

let structured_to_string fields =
  String.concat ";"
    (List.map
       (fun values -> String.concat "," (List.map escape_component values))
       fields)

let component cs i =
  if i < 0 then []
  else match List.nth_opt cs i with Some [ "" ] | None -> [] | Some vs -> vs
