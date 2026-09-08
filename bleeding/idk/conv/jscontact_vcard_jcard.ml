(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module P = Vcard.Property

let structured_names = [ "N"; "ADR"; "ORG"; "GENDER"; "CLIENTPIDMAP" ]
let list_names = [ "NICKNAME"; "CATEGORIES" ]
let str s = Jsont.Json.string s
let mem k v = ((k, Jsont.Meta.none), v)

(* RFC 7095 Section 3.4: parameter names in lowercase and a list for several
   values. Section 3.3.1.2.1 carries the group as a member of its own. *)
let params_json ?group params =
  let group =
    match group with
    | Some g -> [ mem "group" (str (String.lowercase_ascii g)) ]
    | None -> []
  in
  let params =
    List.map
      (fun p ->
        let name = String.lowercase_ascii (Vcard.Param.name p) in
        match Vcard.Param.values p with
        | [ v ] -> mem name (str v)
        | vs -> mem name (Jsont.Json.list (List.map str vs)))
      params
  in
  Jsont.Json.object' (group @ params)

(* RFC 7095 Section 5.1: a property with no VALUE parameter whose default type
   is not known has the type "unknown" and its value text unprocessed. *)
let type_name p =
  match P.find_first p "VALUE" with
  | Some v -> String.lowercase_ascii v
  | None -> (
      match Vcard.Registry.find (P.name p) with
      | Some e -> Vcard.Value_type.to_string e.value_type
      | None -> "unknown")

(* RFC 7095 Sections 3.5.8 to 3.5.10: a boolean is a JSON boolean and a number
   a JSON number. Any other type is a JSON string. *)
let values p typ =
  let name = P.name p in
  match typ with
  | "unknown" -> [ str (P.value p) ]
  | "boolean" -> (
      match P.boolean p with
      | Ok b -> [ Jsont.Json.bool b ]
      | Error _ -> [ str (P.value p) ])
  | "integer" -> (
      match P.integers p with
      | Ok is ->
          (* Beyond 2^53 a double no longer holds every integer, so such a
             value stays the text it arrived as rather than being rounded. *)
          let safe = 9007199254740992L in
          List.map
            (fun i ->
              if Int64.abs i < safe then Jsont.Json.number (Int64.to_float i)
              else str (Int64.to_string i))
            is
      | Error _ -> [ str (P.value p) ])
  | "float" -> (
      match P.floats p with
      | Ok fs -> List.map Jsont.Json.number fs
      | Error _ -> [ str (P.value p) ])
  | "text" ->
      if List.mem name structured_names then
        [
          Jsont.Json.list
            (List.map
               (function
                 | [ v ] -> str v | vs -> Jsont.Json.list (List.map str vs))
               (P.structured p));
        ]
      else if List.mem name list_names then List.map str (P.text_list p)
      else [ str (P.text p) ]
  | _ -> [ str (P.value p) ]

let of_property p =
  let name = P.name p in
  let params =
    List.filter (fun x -> Vcard.Param.name x <> "VALUE") (P.params p)
  in
  let typ = type_name p in
  Jsont.Json.list
    (str (String.lowercase_ascii name)
    :: params_json ?group:(P.group p) params
    :: str typ :: values p typ)

let string_of_json ?(integer = false) = function
  | Jsont.String (s, _) -> Ok s
  | Jsont.Bool (b, _) -> Ok (string_of_bool b)
  | Jsont.Number (f, _) ->
      (* RFC 7095 Section 3.5.9: a decimal point or exponent is eliminated when
         an integer value converts back to vCard. *)
      Ok
        (if integer || Float.is_integer f then Printf.sprintf "%.0f" f
         else Float.to_string f)
  | _ -> Error "a jCard value is a string, a boolean or a number"

let ( let* ) = Result.bind

let rec strings = function
  | [] -> Ok []
  | j :: js ->
      let* s = string_of_json j in
      let* ss = strings js in
      Ok (s :: ss)

let to_property = function
  | Jsont.Array
      ( Jsont.String (name, _)
        :: Jsont.Object (pmems, _)
        :: Jsont.String (typ, _)
        :: values,
        _ ) ->
      let name = String.uppercase_ascii name in
      let group = ref None in
      let params =
        List.filter_map
          (fun ((k, _), v) ->
            if String.equal k "group" then (
              (* Section 3.3.1.2.1: the name returns to uppercase. *)
              (match v with
              | Jsont.String (g, _) -> group := Some (String.uppercase_ascii g)
              | _ -> ());
              None)
            else
              match v with
              | Jsont.Array (vs, _) -> (
                  match strings vs with
                  | Ok vs -> Some (Vcard.Param.v k vs)
                  | Error _ -> None)
              | v -> (
                  match string_of_json v with
                  | Ok s -> Some (Vcard.Param.v k [ s ])
                  | Error _ -> None))
          pmems
      in
      (* RFC 7095 Section 5.2 omits VALUE only when the jCard type is the
         property's default. A name this library does not know has no default,
         so its VALUE is kept whatever the type. *)
      let default =
        Option.map
          (fun (e : Vcard.Registry.entry) -> e.value_type)
          (Vcard.Registry.find name)
      in
      let params =
        if
          typ = "unknown"
          ||
          match default with
          | Some d -> Vcard.Value_type.equal (Vcard.Value_type.of_string typ) d
          | None -> false
        then params
        else params @ [ Vcard.Param.v "VALUE" [ typ ] ]
      in
      let* value =
        match (typ, values) with
        | "unknown", [ v ] -> string_of_json v
        | "text", [ Jsont.Array (components, _) ]
          when List.mem name structured_names ->
            let component = function
              | Jsont.Array (vs, _) -> strings vs
              | v -> Result.map (fun s -> [ s ]) (string_of_json v)
            in
            let rec all = function
              | [] -> Ok []
              | c :: cs ->
                  let* c = component c in
                  let* cs = all cs in
                  Ok (c :: cs)
            in
            Result.map Vcard.Text.structured_to_string (all components)
        | "text", vs -> Result.map Vcard.Text.list_to_string (strings vs)
        | "integer", vs ->
            let rec go = function
              | [] -> Ok []
              | j :: js ->
                  let* s = string_of_json ~integer:true j in
                  let* ss = go js in
                  Ok (s :: ss)
            in
            Result.map (String.concat ",") (go vs)
        | _, vs -> Result.map (String.concat ",") (strings vs)
      in
      Ok (P.v ?group:!group ~params name value)
  | _ ->
      Error "a JCardProp is an array of a name, parameters, a type and values"
