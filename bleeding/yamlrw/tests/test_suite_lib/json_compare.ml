(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Parse JSON using jsonm and compare parsed structures *)

type json =
  | Null
  | Bool of bool
  | Float of float
  | String of string
  | Array of json list
  | Object of (string * json) list

let rec equal a b =
  match (a, b) with
  | Null, Null -> true
  | Bool a, Bool b -> a = b
  | Float a, Float b -> Float.equal a b
  | String a, String b -> String.equal a b
  | Array a, Array b -> List.equal equal a b
  | Object a, Object b ->
      (* Compare objects as sets of key-value pairs (order independent) *)
      let sorted_a =
        List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) a
      in
      let sorted_b =
        List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) b
      in
      List.length sorted_a = List.length sorted_b
      && List.for_all2
           (fun (k1, v1) (k2, v2) -> k1 = k2 && equal v1 v2)
           sorted_a sorted_b
  | _ -> false

(* Parse JSON string using jsonm *)
let parse_json s =
  let decoder = Jsonm.decoder (`String s) in
  let rec parse_value () =
    match Jsonm.decode decoder with
    | `Lexeme `Null -> Ok Null
    | `Lexeme (`Bool b) -> Ok (Bool b)
    | `Lexeme (`Float f) -> Ok (Float f)
    | `Lexeme (`String s) -> Ok (String s)
    | `Lexeme `As -> parse_array []
    | `Lexeme `Os -> parse_object []
    | `Lexeme _ -> Error "unexpected lexeme"
    | `Error e -> Error (Format.asprintf "%a" Jsonm.pp_error e)
    | `End -> Error "unexpected end"
    | `Await -> Error "unexpected await"
  and parse_array acc =
    match Jsonm.decode decoder with
    | `Lexeme `Ae -> Ok (Array (List.rev acc))
    | `Lexeme _ as lex -> (
        (* Push back and parse value *)
        let result = parse_value_with_lex lex in
        match result with Ok v -> parse_array (v :: acc) | Error _ as e -> e)
    | `Error e -> Error (Format.asprintf "%a" Jsonm.pp_error e)
    | `End -> Error "unexpected end in array"
    | `Await -> Error "unexpected await"
  and parse_object acc =
    match Jsonm.decode decoder with
    | `Lexeme `Oe -> Ok (Object (List.rev acc))
    | `Lexeme (`Name key) -> (
        match parse_value () with
        | Ok v -> parse_object ((key, v) :: acc)
        | Error _ as e -> e)
    | `Lexeme _ -> Error "expected object key"
    | `Error e -> Error (Format.asprintf "%a" Jsonm.pp_error e)
    | `End -> Error "unexpected end in object"
    | `Await -> Error "unexpected await"
  and parse_value_with_lex lex =
    match lex with
    | `Lexeme `Null -> Ok Null
    | `Lexeme (`Bool b) -> Ok (Bool b)
    | `Lexeme (`Float f) -> Ok (Float f)
    | `Lexeme (`String s) -> Ok (String s)
    | `Lexeme `As -> parse_array []
    | `Lexeme `Os -> parse_object []
    | `Lexeme _ -> Error "unexpected lexeme"
    | `Error e -> Error (Format.asprintf "%a" Jsonm.pp_error e)
    | `End -> Error "unexpected end"
    | `Await -> Error "unexpected await"
  in
  parse_value ()

(* Parse multiple JSON values (for multi-document YAML) *)
let parse_json_multi s =
  let decoder = Jsonm.decoder (`String s) in
  let rec parse_value () =
    match Jsonm.decode decoder with
    | `Lexeme `Null -> Some Null
    | `Lexeme (`Bool b) -> Some (Bool b)
    | `Lexeme (`Float f) -> Some (Float f)
    | `Lexeme (`String s) -> Some (String s)
    | `Lexeme `As -> parse_array []
    | `Lexeme `Os -> parse_object []
    | `Lexeme _ -> None
    | `Error _ -> None
    | `End -> None
    | `Await -> None
  and parse_array acc =
    match Jsonm.decode decoder with
    | `Lexeme `Ae -> Some (Array (List.rev acc))
    | `Lexeme _ as lex -> (
        match parse_value_with_lex lex with
        | Some v -> parse_array (v :: acc)
        | None -> None)
    | _ -> None
  and parse_object acc =
    match Jsonm.decode decoder with
    | `Lexeme `Oe -> Some (Object (List.rev acc))
    | `Lexeme (`Name key) -> (
        match parse_value () with
        | Some v -> parse_object ((key, v) :: acc)
        | None -> None)
    | _ -> None
  and parse_value_with_lex lex =
    match lex with
    | `Lexeme `Null -> Some Null
    | `Lexeme (`Bool b) -> Some (Bool b)
    | `Lexeme (`Float f) -> Some (Float f)
    | `Lexeme (`String s) -> Some (String s)
    | `Lexeme `As -> parse_array []
    | `Lexeme `Os -> parse_object []
    | _ -> None
  in
  let rec collect acc =
    match parse_value () with
    | Some v -> collect (v :: acc)
    | None -> List.rev acc
  in
  collect []

(* Compare two JSON strings *)
let compare_json_strings expected actual =
  (* Handle empty strings *)
  let expected_trimmed = String.trim expected in
  let actual_trimmed = String.trim actual in
  if expected_trimmed = "" && actual_trimmed = "" then true
  else if expected_trimmed = "" || actual_trimmed = "" then false
  else
    (* Parse as potentially multiple JSON values *)
    let expected_values = parse_json_multi expected in
    let actual_values = parse_json_multi actual in
    List.length expected_values = List.length actual_values
    && List.for_all2 equal expected_values actual_values
