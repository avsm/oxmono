(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  group : string option;
  name : string;
  params : Vcard_param.t list;
  value : string;
}

let v ?group ?(params = []) name value =
  { group; name = String.uppercase_ascii name; params; value }

let group p = p.group
let name p = p.name
let params p = p.params
let value p = p.value

let equal a b =
  Option.equal
    (fun x y ->
      String.equal (String.uppercase_ascii x) (String.uppercase_ascii y))
    a.group b.group
  && String.equal a.name b.name
  && List.equal Vcard_param.equal a.params b.params
  && String.equal a.value b.value

let is_name_char c =
  (c >= 'A' && c <= 'Z')
  || (c >= 'a' && c <= 'z')
  || (c >= '0' && c <= '9')
  || c = '-'

open Vcard_result

(* RFC 6350 Section 3.3: contentline = [group "."] name *(";" param) ":"
   value, param = name "=" param-value *("," param-value), and a param-value
   is either quoted or free of ":", ";" and ",". *)
let of_string line =
  let n = String.length line in
  let rec scan_while pred i =
    if i < n && pred line.[i] then scan_while pred (i + 1) else i
  in
  let name_end = scan_while (fun c -> is_name_char c || c = '.') 0 in
  if name_end = 0 then
    error "a content line does not start with a property name"
  else
    let group, name =
      let s = String.sub line 0 name_end in
      match String.index_opt s '.' with
      | None -> (None, s)
      | Some i ->
          ( Some (String.sub s 0 i),
            String.sub s (i + 1) (String.length s - i - 1) )
    in
    if name = "" || (not (String.for_all is_name_char name)) || group = Some ""
    then error "%S is not a property name" (String.sub line 0 name_end)
    else
      let rec param_value i acc =
        if i >= n then error "a parameter value ends before the property value"
        else if line.[i] = '"' then
          match String.index_from_opt line (i + 1) '"' with
          | None -> error "a quoted parameter value is not closed"
          | Some j ->
              let v =
                Vcard_param.decode_value (String.sub line (i + 1) (j - i - 1))
              in
              next_value (j + 1) (v :: acc)
        else
          let j =
            scan_while (fun c -> c <> ',' && c <> ';' && c <> ':' && c <> '"') i
          in
          if j < n && line.[j] = '"' then
            error "a double quote may not appear inside a parameter value"
          else
            let v = Vcard_param.decode_value (String.sub line i (j - i)) in
            next_value j (v :: acc)
      and next_value i acc =
        if i < n && line.[i] = ',' then param_value (i + 1) acc
        else Ok (List.rev acc, i)
      in
      let rec params i acc =
        if i >= n then error "a content line holds no \":\""
        else
          match line.[i] with
          | ':' -> Ok (List.rev acc, String.sub line (i + 1) (n - i - 1))
          | ';' ->
              let j = scan_while is_name_char (i + 1) in
              let pname = String.sub line (i + 1) (j - i - 1) in
              if pname = "" then error "a parameter has no name"
              else if j < n && line.[j] = '=' then
                match param_value (j + 1) [] with
                | Error _ as e -> e
                | Ok (values, k) -> params k (Vcard_param.v pname values :: acc)
              else params j (Vcard_param.v pname [] :: acc)
          | c -> error "unexpected %C in the parameters" c
      in
      match params name_end [] with
      | Error _ as e -> e
      | Ok (params, value) ->
          Ok { group; name = String.uppercase_ascii name; params; value }

let to_string p =
  let b = Buffer.create (String.length p.value + 32) in
  Option.iter
    (fun g ->
      Buffer.add_string b g;
      Buffer.add_char b '.')
    p.group;
  Buffer.add_string b p.name;
  List.iter
    (fun prm ->
      Buffer.add_char b ';';
      Buffer.add_string b (Vcard_param.to_string prm))
    p.params;
  Buffer.add_char b ':';
  Buffer.add_string b p.value;
  Buffer.contents b

let pp ppf p = Format.pp_print_string ppf (to_string p)
let find_values p name = Vcard_param.find_values p.params name
let find_first p name = Vcard_param.find_first p.params name
let is_digit c = c >= '0' && c <= '9'
let unsigned s = s <> "" && String.for_all is_digit s

let signed s =
  s <> ""
  &&
  match s.[0] with
  | '+' | '-' -> unsigned (String.sub s 1 (String.length s - 1))
  | _ -> unsigned s

let value_type p =
  match find_first p "VALUE" with
  | Some v -> Vcard_value_type.of_string v
  | None -> Vcard_registry.value_type p.name

let pref p =
  match find_first p "PREF" with
  | None -> None
  | Some s -> if unsigned s then int_of_string_opt s else None

(* A quoted parameter value is one value on the wire, but RFC 6350 writes the
   lists of TYPE, PID and SORT-AS as one quoted value, as in
   TYPE="voice,home", so those are split on the comma when read. *)
let list_param p name =
  List.concat_map (String.split_on_char ',')
    (Vcard_param.values_named p.params name)

let types p = List.map String.lowercase_ascii (list_param p "TYPE")
let sort_as p = list_param p "SORT-AS"
let pids p = list_param p "PID"
let language p = find_first p "LANGUAGE"
let altid p = find_first p "ALTID"
let prop_id p = find_first p "PROP-ID"
let media_type p = find_first p "MEDIATYPE"
let text p = Vcard_text.unescape p.value
let text_list p = Vcard_text.list_of_string p.value
let structured p = Vcard_text.structured_of_string p.value
let uri p = p.value

let boolean p =
  match String.lowercase_ascii p.value with
  | "true" -> Ok true
  | "false" -> Ok false
  | _ -> error "%S is not a boolean" p.value

let values_of ~one p =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | v :: vs ->
        let* v = one v in
        go (v :: acc) vs
  in
  go [] (String.split_on_char ',' p.value)

(* RFC 6350 Section 4.5 and 4.6: integer = [sign] 1*DIGIT and
   float = [sign] 1*DIGIT ["." 1*DIGIT], so neither the base prefixes nor the
   separators int_of_string accepts are allowed. *)
let integers p =
  values_of p ~one:(fun s ->
      match Int64.of_string_opt s with
      | Some i when signed s -> Ok i
      | _ -> error "%S is not an integer" s)

let floats p =
  let is_float s =
    match String.index_opt s '.' with
    | None -> signed s
    | Some i ->
        signed (String.sub s 0 i)
        && unsigned (String.sub s (i + 1) (String.length s - i - 1))
  in
  values_of p ~one:(fun s ->
      match float_of_string_opt s with
      | Some f when is_float s && Float.is_finite f -> Ok f
      | _ -> error "%S is not a float" s)

let date_and_or_time p = Vcard_date.of_string p.value
let timestamp p = Vcard_date.Timestamp.of_string p.value
let utc_offset p = Vcard_date.Utc_offset.of_string p.value
let of_text ?group ?params name s = v ?group ?params name (Vcard_text.escape s)

let of_text_list ?group ?params name l =
  v ?group ?params name (Vcard_text.list_to_string l)

let of_structured ?group ?params name cs =
  v ?group ?params name (Vcard_text.structured_to_string cs)

let of_uri ?group ?params name u = v ?group ?params name u
