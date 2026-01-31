(* header.ml - HTTP header type and operations *)

open Base

module Name = Header_name

type t =
  { name : Name.t
  ; name_span : Span.t
  ; value : Span.t
  }

let rec find (headers : t list @ local) name = exclave_
  match headers with
  | [] -> None
  | hdr :: rest ->
    let matches =
      match name, hdr.name with
      | Name.Other, _ | _, Name.Other -> false
      | n1, n2 -> Poly.( = ) n1 n2
    in
    if matches then Some hdr else find rest name
;;

let rec find_string (buf : bytes) (headers : t list @ local) name = exclave_
  match headers with
  | [] -> None
  | hdr :: rest ->
    let matches =
      match hdr.name with
      | Name.Other -> Span.equal_caseless buf hdr.name_span name
      | known ->
        let canonical = Name.lowercase known in
        String.( = ) (String.lowercase name) canonical
    in
    if matches then Some hdr else find_string buf rest name
;;

let to_string_pair (buf : bytes) t =
  let name =
    match t.name with
    | Name.Other -> Span.to_string buf t.name_span
    | known -> Name.canonical known
  in
  let value = Span.to_string buf t.value in
  (name, value)
;;

let to_string_pairs (buf : bytes) headers =
  List.map headers ~f:(to_string_pair buf)
;;

let pp_with_buf (buf : bytes) fmt t =
  Stdlib.Format.fprintf fmt "%s: %s"
    (Name.canonical t.name)
    (Span.to_string buf t.value)
;;

let pp fmt t =
  Stdlib.Format.fprintf fmt "{ name = %a; name_span = #{ off = %d; len = %d }; value = #{ off = %d; len = %d } }"
    Name.pp t.name
    (Span.off t.name_span) (Span.len t.name_span)
    (Span.off t.value) (Span.len t.value)
;;
