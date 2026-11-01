(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** JSON-compatible YAML value representation *)

type t =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of t list
  | `O of (string * t) list ]

(* Type equality is ensured by structural compatibility with Yamlrw.value *)

(** Constructors *)

let null : t = `Null
let bool b : t = `Bool b
let int n : t = `Float (Float.of_int n)
let float f : t = `Float f
let string s : t = `String s
let list f xs : t = `A (List.map f xs)
let obj pairs : t = `O pairs

(** Type name for error messages *)
let type_name : t -> string = function
  | `Null -> "null"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
  | `String _ -> "string"
  | `A _ -> "array"
  | `O _ -> "object"

(** Safe accessors (return option) *)

let as_null = function `Null -> Some () | _ -> None
let as_bool = function `Bool b -> Some b | _ -> None
let as_float = function `Float f -> Some f | _ -> None
let as_string = function `String s -> Some s | _ -> None
let as_list = function `A l -> Some l | _ -> None
let as_assoc = function `O o -> Some o | _ -> None

let as_int = function
  | `Float f ->
      let i = Float.to_int f in
      if Float.equal (Float.of_int i) f then Some i else None
  | _ -> None

(** Unsafe accessors (raise on type mismatch) *)

let unwrap_or_type_error expected_type extractor v =
  match extractor v with
  | Some x -> x
  | None -> Error.raise (Type_mismatch (expected_type, type_name v))

let to_null v = unwrap_or_type_error "null" as_null v
let to_bool v = unwrap_or_type_error "bool" as_bool v
let to_float v = unwrap_or_type_error "float" as_float v
let to_string v = unwrap_or_type_error "string" as_string v
let to_list v = unwrap_or_type_error "array" as_list v
let to_assoc v = unwrap_or_type_error "object" as_assoc v
let to_int v = unwrap_or_type_error "int" as_int v

(** Object access *)

let mem key = function
  | `O pairs -> List.exists (fun (k, _) -> k = key) pairs
  | _ -> false

let find key = function `O pairs -> List.assoc_opt key pairs | _ -> None

let get key v =
  match find key v with Some v -> v | None -> Error.raise (Key_not_found key)

let keys = function
  | `O pairs -> List.map fst pairs
  | v -> Error.raise (Type_mismatch ("object", type_name v))

let values = function
  | `O pairs -> List.map snd pairs
  | v -> Error.raise (Type_mismatch ("object", type_name v))

(** Combinators *)

let combine v1 v2 =
  match (v1, v2) with
  | `O o1, `O o2 -> `O (o1 @ o2)
  | v1, _ -> Error.raise (Type_mismatch ("object", type_name v1))

let map f = function
  | `A l -> `A (List.map f l)
  | v -> Error.raise (Type_mismatch ("array", type_name v))

let filter pred = function
  | `A l -> `A (List.filter pred l)
  | v -> Error.raise (Type_mismatch ("array", type_name v))

(** Pretty printing *)

let rec pp fmt (v : t) =
  match v with
  | `Null -> Format.pp_print_string fmt "null"
  | `Bool b -> Format.pp_print_bool fmt b
  | `Float f ->
      if Float.is_integer f && Float.abs f < 1e15 then
        Format.fprintf fmt "%.0f" f
      else Format.fprintf fmt "%g" f
  | `String s -> Format.fprintf fmt "%S" s
  | `A [] -> Format.pp_print_string fmt "[]"
  | `A items ->
      Format.fprintf fmt "@[<hv 2>[@,%a@]@,]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           pp)
        items
  | `O [] -> Format.pp_print_string fmt "{}"
  | `O pairs ->
      Format.fprintf fmt "@[<hv 2>{@,%a@]@,}"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt (k, v) -> Format.fprintf fmt "@[<hv 2>%S:@ %a@]" k pp v))
        pairs

(** Equality and comparison *)

let rec equal (a : t) (b : t) =
  match (a, b) with
  | `Null, `Null -> true
  | `Bool a, `Bool b -> a = b
  | `Float a, `Float b -> Float.equal a b
  | `String a, `String b -> String.equal a b
  | `A a, `A b -> List.equal equal a b
  | `O a, `O b ->
      List.length a = List.length b
      && List.for_all2 (fun (k1, v1) (k2, v2) -> k1 = k2 && equal v1 v2) a b
  | _ -> false

let rec compare (a : t) (b : t) =
  match (a, b) with
  | `Null, `Null -> 0
  | `Null, _ -> -1
  | _, `Null -> 1
  | `Bool a, `Bool b -> Bool.compare a b
  | `Bool _, _ -> -1
  | _, `Bool _ -> 1
  | `Float a, `Float b -> Float.compare a b
  | `Float _, _ -> -1
  | _, `Float _ -> 1
  | `String a, `String b -> String.compare a b
  | `String _, _ -> -1
  | _, `String _ -> 1
  | `A a, `A b -> List.compare compare a b
  | `A _, _ -> -1
  | _, `A _ -> 1
  | `O a, `O b ->
      let cmp_pair (k1, v1) (k2, v2) =
        let c = String.compare k1 k2 in
        if c <> 0 then c else compare v1 v2
      in
      List.compare cmp_pair a b
