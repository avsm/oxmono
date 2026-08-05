(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  | Int of Z.t
  | Bytes of string
  | Text of string
  | Array of t list
  | Map of (t * t) list
  | Tag of int * t
  | Bool of bool
  | Null
  | Undefined
  | Simple of int
  | Float of float

(* Constructors *)
let int n = Int (Z.of_int n)
let int64 n = Int (Z.of_int64 n)
let bigint n = Int n
let string s = Text s
let bytes s = Bytes s
let array items = Array items
let map pairs = Map pairs
let tag n item = Tag (n, item)
let bool b = Bool b
let null = Null
let undefined = Undefined
let float f = Float f

(* Map operations *)
let find key = function Map pairs -> List.assoc_opt key pairs | _ -> None
let find_text key t = find (Text key) t
let mem key = function Map pairs -> List.mem_assoc key pairs | _ -> false
let mem_text key t = mem (Text key) t

(* Array operations *)
let nth i = function Array items -> List.nth_opt items i | _ -> None

let length = function
  | Array items -> Some (List.length items)
  | Map pairs -> Some (List.length pairs)
  | _ -> None

(* Type predicates *)
let is_int = function Int _ -> true | _ -> false
let is_bytes = function Bytes _ -> true | _ -> false
let is_text = function Text _ -> true | _ -> false
let is_array = function Array _ -> true | _ -> false
let is_map = function Map _ -> true | _ -> false
let is_tag = function Tag _ -> true | _ -> false
let is_bool = function Bool _ -> true | _ -> false
let is_null = function Null -> true | _ -> false
let is_undefined = function Undefined -> true | _ -> false
let is_simple = function Simple _ -> true | _ -> false
let is_float = function Float _ -> true | _ -> false

(* Type-safe accessors *)
let to_int = function Int n -> Some n | _ -> None

let to_int_exn = function
  | Int n -> n
  | _ -> invalid_arg "Cbor.to_int_exn: not an Int"

let to_int64 = function
  | Int n -> if Z.fits_int64 n then Some (Z.to_int64 n) else None
  | _ -> None

let to_int64_exn = function
  | Int n ->
      if Z.fits_int64 n then Z.to_int64 n
      else invalid_arg "Cbor.to_int64_exn: value doesn't fit in int64"
  | _ -> invalid_arg "Cbor.to_int64_exn: not an Int"

let to_bytes = function Bytes s -> Some s | _ -> None

let to_bytes_exn = function
  | Bytes s -> s
  | _ -> invalid_arg "Cbor.to_bytes_exn: not Bytes"

let to_text = function Text s -> Some s | _ -> None

let to_text_exn = function
  | Text s -> s
  | _ -> invalid_arg "Cbor.to_text_exn: not Text"

let to_array = function Array items -> Some items | _ -> None

let to_array_exn = function
  | Array items -> items
  | _ -> invalid_arg "Cbor.to_array_exn: not Array"

let to_map = function Map pairs -> Some pairs | _ -> None

let to_map_exn = function
  | Map pairs -> pairs
  | _ -> invalid_arg "Cbor.to_map_exn: not Map"

let to_tag = function Tag (n, v) -> Some (n, v) | _ -> None

let to_tag_exn = function
  | Tag (n, v) -> (n, v)
  | _ -> invalid_arg "Cbor.to_tag_exn: not Tag"

let to_bool = function Bool b -> Some b | _ -> None

let to_bool_exn = function
  | Bool b -> b
  | _ -> invalid_arg "Cbor.to_bool_exn: not Bool"

let to_float = function Float f -> Some f | _ -> None

let to_float_exn = function
  | Float f -> f
  | _ -> invalid_arg "Cbor.to_float_exn: not Float"

(* Numeric conversions *)
let to_number = function
  | Int n -> Some (Z.to_float n)
  | Float f -> Some f
  | _ -> None

let to_int_of_float = function
  | Int n -> Some n
  | Float f ->
      let n = Z.of_float f in
      if Z.to_float n = f then Some n else None
  | _ -> None

(* Comparison *)
let rec equal a b =
  match (a, b) with
  | Int x, Int y -> Z.equal x y
  | Bytes x, Bytes y -> x = y
  | Text x, Text y -> x = y
  | Array xs, Array ys ->
      List.length xs = List.length ys && List.for_all2 equal xs ys
  | Map xs, Map ys ->
      List.length xs = List.length ys
      && List.for_all2
           (fun (k1, v1) (k2, v2) -> equal k1 k2 && equal v1 v2)
           xs ys
  | Tag (n1, v1), Tag (n2, v2) -> n1 = n2 && equal v1 v2
  | Bool x, Bool y -> x = y
  | Null, Null -> true
  | Undefined, Undefined -> true
  | Simple x, Simple y -> x = y
  | Float x, Float y -> x = y
  | _ -> false

let major_type_order = function
  | Int n when Z.sign n >= 0 -> 0
  | Int _ -> 1
  | Bytes _ -> 2
  | Text _ -> 3
  | Array _ -> 4
  | Map _ -> 5
  | Tag _ -> 6
  | Bool _ | Null | Undefined | Simple _ | Float _ -> 7

let rec compare a b =
  let ma = major_type_order a and mb = major_type_order b in
  if ma <> mb then Int.compare ma mb
  else
    match (a, b) with
    | Int x, Int y -> Z.compare x y
    | Bytes x, Bytes y -> String.compare x y
    | Text x, Text y -> String.compare x y
    | Array xs, Array ys -> compare_lists xs ys
    | Map xs, Map ys -> compare_maps xs ys
    | Tag (n1, v1), Tag (n2, v2) ->
        let c = Int.compare n1 n2 in
        if c <> 0 then c else compare v1 v2
    | Bool x, Bool y -> Bool.compare x y
    | Null, Null -> 0
    | Undefined, Undefined -> 0
    | Simple x, Simple y -> Int.compare x y
    | Float x, Float y -> Float.compare x y
    | _ -> 0

and compare_lists xs ys =
  match (xs, ys) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs', y :: ys' ->
      let c = compare x y in
      if c <> 0 then c else compare_lists xs' ys'

and compare_maps xs ys =
  match (xs, ys) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | (k1, v1) :: xs', (k2, v2) :: ys' ->
      let c = compare k1 k2 in
      if c <> 0 then c
      else
        let c = compare v1 v2 in
        if c <> 0 then c else compare_maps xs' ys'

(* Pretty printing - diagnostic notation per RFC 8949 Section 8 *)
let rec pp ppf = function
  | Int n -> Format.fprintf ppf "%s" (Z.to_string n)
  | Bytes s -> pp_bytes ppf s
  | Text s -> pp_text ppf s
  | Array items -> pp_array ppf items
  | Map pairs -> pp_map ppf pairs
  | Tag (n, v) -> Format.fprintf ppf "%d(%a)" n pp v
  | Bool true -> Format.fprintf ppf "true"
  | Bool false -> Format.fprintf ppf "false"
  | Null -> Format.fprintf ppf "null"
  | Undefined -> Format.fprintf ppf "undefined"
  | Simple n -> Format.fprintf ppf "simple(%d)" n
  | Float f -> pp_float ppf f

and pp_bytes ppf s =
  Format.fprintf ppf "h'";
  String.iter (fun c -> Format.fprintf ppf "%02x" (Char.code c)) s;
  Format.fprintf ppf "'"

and pp_text ppf s =
  Format.fprintf ppf "\"";
  String.iter
    (fun c ->
      match c with
      | '"' -> Format.fprintf ppf "\\\""
      | '\\' -> Format.fprintf ppf "\\\\"
      | '\n' -> Format.fprintf ppf "\\n"
      | '\r' -> Format.fprintf ppf "\\r"
      | '\t' -> Format.fprintf ppf "\\t"
      | c when Char.code c < 32 -> Format.fprintf ppf "\\u%04x" (Char.code c)
      | c -> Format.fprintf ppf "%c" c)
    s;
  Format.fprintf ppf "\""

and pp_array ppf items =
  Format.fprintf ppf "[";
  List.iteri
    (fun i v ->
      if i > 0 then Format.fprintf ppf ", ";
      pp ppf v)
    items;
  Format.fprintf ppf "]"

and pp_map ppf pairs =
  Format.fprintf ppf "{";
  List.iteri
    (fun i (k, v) ->
      if i > 0 then Format.fprintf ppf ", ";
      Format.fprintf ppf "%a: %a" pp k pp v)
    pairs;
  Format.fprintf ppf "}"

and pp_float ppf f =
  match classify_float f with
  | FP_nan -> Format.fprintf ppf "NaN"
  | FP_infinite ->
      if f > 0.0 then Format.fprintf ppf "Infinity"
      else Format.fprintf ppf "-Infinity"
  | FP_zero when 1.0 /. f < 0.0 -> Format.fprintf ppf "-0.0"
  | _ -> Format.fprintf ppf "%g" f

let to_diagnostic v = Format.asprintf "%a" pp v
