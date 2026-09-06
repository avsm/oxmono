open Base
module Name = Header_name
module Syntax = Httpz_syntax

type t =
  { name : Name.t
  ; name_span : Span.t
  ; value : Span.t
  }

let rec find_known (headers : t list @ local) name = exclave_
  match headers with
  | [] -> None
  | hdr :: rest -> if Name.equal name hdr.name then Some hdr else find_known rest name
;;

let find (headers : t list @ local) name = exclave_
  if Name.equal name Name.Other then None else find_known headers name
;;

let rec find_lowercase (local_ (buf : bytes)) (headers : t list @ local) name = exclave_
  match headers with
  | [] -> None
  | hdr :: rest ->
    if Span.equal_caseless buf hdr.name_span name
    then Some hdr
    else find_lowercase buf rest name
;;

(* [Span.equal_caseless] folds only the buffer side, so [name] is lowered once here rather
   than once per field. *)
let find_string (local_ (buf : bytes)) (headers : t list @ local) name =
  let name =
    if String.exists name ~f:Char.is_uppercase then String.lowercase name else name
  in
  exclave_ find_lowercase buf headers name
;;

let to_string_pair (buf : bytes) t =
  let name =
    match t.name with
    | Name.Other -> Span.to_string buf t.name_span
    | known -> Name.canonical known
  in
  let value = Span.to_string buf t.value in
  name, value
;;

let to_string_pairs (buf : bytes) headers = List.map headers ~f:(to_string_pair buf)

let rec to_string_pairs_local (buf : bytes) (headers : t list @ local) =
  match headers with
  | [] -> []
  | hdr :: rest ->
    let pair = to_string_pair buf hdr in
    pair :: to_string_pairs_local buf rest
;;

let to_rev_string_pairs_local (buf : bytes) (headers : t list @ local) =
  let rec go acc (headers : t list @ local) =
    match headers with
    | [] -> acc
    | hdr :: rest -> go (to_string_pair buf hdr :: acc) rest
  in
  go [] headers

let pp_with_buf (buf : bytes) fmt t =
  let name, value = to_string_pair buf t in
  Stdlib.Format.fprintf fmt "%s: %s" name value
;;

let pp fmt t =
  Stdlib.Format.fprintf
    fmt
    "{ name = %a; name_span = #{ off = %d; len = %d }; value = #{ off = %d; len = %d } }"
    Name.pp
    t.name
    (Span.off t.name_span)
    (Span.len t.name_span)
    (Span.off t.value)
    (Span.len t.value)
;;
