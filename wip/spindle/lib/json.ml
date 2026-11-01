(* SPDX-License-Identifier: ISC *)

exception Invalid of string

let invalid message = raise (Invalid message)
let str s = Jsont.Json.string s
let int n = Jsont.Json.int n
let bool b = Jsont.Json.bool b
let arr xs = Jsont.Json.list xs
let obj xs =
  Jsont.Json.object' (List.map (fun (k, v) -> (Jsont.Json.name k, v)) xs)

let members = function
  | Jsont.Object (xs, _) -> List.map (fun ((k, _), v) -> k, v) xs
  | _ -> invalid "expected an object"

let field k j = List.assoc_opt k (members j)
let required k j =
  match field k j with Some v -> v | None -> invalid (k ^ " is required")

let string = function
  | Jsont.String (s, _) -> s
  | _ -> invalid "expected a string"

let number = function
  | Jsont.Number (n, _) when Float.is_finite n -> n
  | _ -> invalid "expected a number"

let list = function
  | Jsont.Array (xs, _) -> xs
  | _ -> invalid "expected an array"

let get k j = string (required k j)
let strings k j =
  match field k j with None -> [] | Some v -> List.map string (list v)

let rec validate depth j =
  if depth > 32 then invalid "JSON nesting exceeds 32";
  match j with
  | Jsont.Object _ ->
      let xs = members j in
      let names = List.map fst xs in
      if List.length names <> List.length (List.sort_uniq String.compare names)
      then invalid "duplicate object member";
      List.iter (fun (_, v) -> validate (depth + 1) v) xs
  | Jsont.Array (xs, _) -> List.iter (validate (depth + 1)) xs
  | _ -> ()

let check_depth s =
  let mutable depth = 0 in
  let mutable quoted = false in
  let mutable escaped = false in
  for i = 0 to String.length s - 1 do
    let c = String.unsafe_get s i in
    if quoted then (
      if escaped then escaped <- false
      else if c = '\\' then escaped <- true
      else if c = '"' then quoted <- false)
    else if c = '"' then quoted <- true
    else if c = '{' || c = '[' then (
      depth <- depth + 1;
      if depth > 32 then invalid "JSON nesting exceeds 32")
    else if c = '}' || c = ']' then depth <- depth - 1
  done

let decode s =
  check_depth s;
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Error _ -> invalid "invalid JSON"
  | Ok j -> validate 0 j; j

let encode j =
  match Jsont_bytesrw.encode_string Jsont.json j with
  | Ok s -> s
  | Error e -> failwith e

let sha s =
  if String.length s <> 40 || not (String.for_all (function
      | '0' .. '9' | 'a' .. 'f' -> true | _ -> false) s)
  then invalid "commit must be a lowercase 40-digit SHA-1";
  s

let did s =
  if not (Atp.Did.is_valid s) then invalid "invalid DID";
  s
