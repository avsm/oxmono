(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Token = struct
  type t = string

  let validate_with_meta meta s =
    if not (String.is_valid_utf_8 s) then
      Jsont.Error.msgf meta "Invalid JSON Pointer: input is not valid UTF-8"

  let escape s =
    validate_with_meta Jsont.Meta.none s;
    let b = Buffer.create (String.length s) in
    String.iter
      (function
        | '~' -> Buffer.add_string b "~0"
        | '/' -> Buffer.add_string b "~1"
        | c -> Buffer.add_char b c)
      s;
    Buffer.contents b

  let unescape_with_meta meta s =
    validate_with_meta meta s;
    let len = String.length s in
    let b = Buffer.create len in
    let rec loop i =
      if i = len then Buffer.contents b
      else
        match s.[i] with
        | '~' when i + 1 = len ->
            Jsont.Error.msgf meta
              "Invalid JSON Pointer: incomplete escape sequence at end"
        | '~' ->
            (match s.[i + 1] with
            | '0' -> Buffer.add_char b '~'
            | '1' -> Buffer.add_char b '/'
            | c ->
                Jsont.Error.msgf meta
                  "Invalid JSON Pointer: invalid escape sequence ~%c" c);
            loop (i + 2)
        | c ->
            Buffer.add_char b c;
            loop (i + 1)
    in
    loop 0

  let unescape s = unescape_with_meta Jsont.Meta.none s
end

type t = Token.t list

let root = []
let is_root = function [] -> true | _ -> false

let of_tokens tokens =
  List.iter (Token.validate_with_meta Jsont.Meta.none) tokens;
  tokens

let tokens p = p

let append p token =
  Token.validate_with_meta Jsont.Meta.none token;
  p @ [ token ]

let ( / ) = append
let concat = ( @ )

let parent p =
  match List.rev p with [] -> None | _ :: rest -> Some (List.rev rest)

let last p = match List.rev p with [] -> None | token :: _ -> Some token

let parse_with_meta meta s =
  Token.validate_with_meta meta s;
  if String.equal s "" then root
  else if s.[0] <> '/' then
    Jsont.Error.msgf meta
      "Invalid JSON Pointer: must be empty or start with '/': %s" s
  else
    let tokens = String.sub s 1 (String.length s - 1) in
    List.map (Token.unescape_with_meta meta) (String.split_on_char '/' tokens)

let of_string s = parse_with_meta Jsont.Meta.none s

let error_to_string f v =
  try Ok (f v) with Jsont.Error e -> Error (Jsont.Error.to_string e)

let of_string_result s = error_to_string of_string s

let hex_digit = function
  | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
  | _ -> false

(* Validate escapes first to retain precise JSON Pointer diagnostics. *)
let validate_pct_encoding meta s =
  let len = String.length s in
  let rec loop i =
    if i = len then ()
    else if s.[i] <> '%' then loop (i + 1)
    else if i + 2 >= len then
      Jsont.Error.msgf meta "Incomplete percent-encoding at position %d" i
    else if not (hex_digit s.[i + 1] && hex_digit s.[i + 2]) then
      Jsont.Error.msgf meta "Invalid percent-encoding at position %d" i
    else loop (i + 3)
  in
  loop 0

let of_uri_fragment_with_meta meta s =
  validate_pct_encoding meta s;
  let decoded = match Uriz.pct_decode s with
  | This s -> s
  | Null -> Jsont.Error.msgf meta "Invalid percent-encoding" in
  parse_with_meta meta decoded

let of_uri_fragment s = of_uri_fragment_with_meta Jsont.Meta.none s
let of_uri_fragment_result s = error_to_string of_uri_fragment s

let to_string p =
  match p with
  | [] -> ""
  | tokens ->
      let b = Buffer.create 64 in
      List.iter
        (fun token ->
          Buffer.add_char b '/';
          Buffer.add_string b (Token.escape token))
        tokens;
      Buffer.contents b

let to_uri_fragment p =
  Uriz.pct_encode ~component:`Fragment (to_string p)

let pp ppf p = Format.pp_print_string ppf (to_string p)
let equal a b = List.equal String.equal a b
let compare a b = List.compare String.compare a b

let of_path path =
  let of_index = function
    | Jsont.Path.Mem (name, meta) ->
        Token.validate_with_meta meta name;
        name
    | Jsont.Path.Nth (n, meta) when n < 0 ->
        Jsont.Error.msgf meta
          "JSON Pointer: cannot convert negative array index %d" n
    | Jsont.Path.Nth (n, _) -> string_of_int n
  in
  List.rev_map of_index (Jsont.Path.rev_indices path)

type array_index = Index of int | End | Invalid | Too_large

let array_index token =
  let len = String.length token in
  let rec digits i =
    i = len || match token.[i] with '0' .. '9' -> digits (i + 1) | _ -> false
  in
  if String.equal token "-" then End
  else if len = 1 && token.[0] = '0' then Index 0
  else if len > 0 && token.[0] >= '1' && token.[0] <= '9' && digits 1 then
    match int_of_string_opt token with Some n -> Index n | None -> Too_large
  else Invalid

let json_sort_string json = Jsont.Sort.to_string (Jsont.Json.sort json)
let pointer_kind jmap = if jmap then "JMAP Pointer" else "JSON Pointer"

exception Missing of Jsont.Error.t

let missingf meta fmt =
  Format.kasprintf
    (fun message ->
      raise
        (Missing (Jsont.Error.make_msg Jsont.Error.Context.empty meta message)))
    fmt

let find_unique_member ~jmap json name members =
  let rec loop found = function
    | [] -> found
    | (((member_name, _), _) as member) :: rest -> (
        if not (String.equal name member_name) then loop found rest
        else
          match found with
          | None -> loop (Some member) rest
          | Some _ ->
              Jsont.Error.msgf (Jsont.Json.meta json)
                "%s: member '%s' is not unique" (pointer_kind jmap) name)
  in
  loop None members

let invalid_array_index ?(jmap = false) json token =
  let kind = pointer_kind jmap in
  match array_index token with
  | End ->
      Jsont.Error.msgf (Jsont.Json.meta json)
        "%s: '-' refers to a nonexistent array element" kind
  | Too_large ->
      Jsont.Error.msgf (Jsont.Json.meta json) "%s: array index is too large: %s"
        kind token
  | Invalid ->
      Jsont.Error.msgf (Jsont.Json.meta json) "%s: invalid array index '%s'"
        kind token
  | Index _ -> assert false

type eval_result =
  | Value of Jsont.json
  | Wildcard of Jsont.json list * Jsont.Meta.t

type result_budget =
  | Unlimited
  | Limited of { limit : int; mutable used : int }

let result_budget = function
  | None -> Unlimited
  | Some limit when limit < 0 ->
      invalid_arg "Json_pointer.Jmap: max_results must not be negative"
  | Some limit -> Limited { limit; used = 0 }

let consume_result budget meta =
  match budget with
  | Unlimited -> ()
  | Limited b when b.used < b.limit -> b.used <- b.used + 1
  | Limited b ->
      Jsont.Error.msgf meta "JMAP Pointer: wildcard result limit %d exceeded"
        b.limit

let json_of_eval_result = function
  | Value json -> json
  | Wildcard (elements, meta) -> Jsont.Array (elements, meta)

(* [Wildcard] keeps a wildcard-produced array distinct from an ordinary JSON
   array. This lets an enclosing wildcard splice an already-counted result
   without first materialising and then counting it again. In particular, one
   budget is shared by all paths through a manually shared JSON DAG. *)
let rec eval_tokens ~jmap ~budget tokens json =
  match tokens with
  | [] -> Value json
  | token :: rest -> (
      match json with
      | Jsont.Object (members, _) -> (
          match find_unique_member ~jmap json token members with
          | Some (_, value) -> eval_tokens ~jmap ~budget rest value
          | None ->
              missingf (Jsont.Json.meta json) "%s: member '%s' not found"
                (pointer_kind jmap) token)
      | Jsont.Array (elements, meta) when jmap && String.equal token "*" ->
          let rev_results = ref [] in
          let add value =
            consume_result budget meta;
            rev_results := value :: !rev_results
          in
          List.iter
            (fun element ->
              match eval_tokens ~jmap ~budget rest element with
              | Value (Jsont.Array (values, _)) -> List.iter add values
              | Value value -> add value
              | Wildcard (values, _) ->
                  (* The nested wildcard consumed the shared budget as it
                     produced these values. The current wildcard only changes
                     the array carrying them. *)
                  rev_results := List.rev_append values !rev_results)
            elements;
          Wildcard (List.rev !rev_results, meta)
      | Jsont.Array (elements, _) -> (
          match array_index token with
          | Index n -> (
              match List.nth_opt elements n with
              | Some value -> eval_tokens ~jmap ~budget rest value
              | None ->
                  missingf (Jsont.Json.meta json)
                    "%s: index %d out of bounds (array has %d elements)"
                    (pointer_kind jmap) n (List.length elements))
          | End ->
              missingf (Jsont.Json.meta json)
                "%s: '-' refers to a nonexistent array element"
                (pointer_kind jmap)
          | Invalid | Too_large -> invalid_array_index ~jmap json token)
      | _ ->
          Jsont.Error.msgf (Jsont.Json.meta json)
            "%s: cannot index into %s with '%s'" (pointer_kind jmap)
            (json_sort_string json) token)

let get_with ?max_results ~jmap p json =
  let budget = result_budget max_results in
  try json_of_eval_result (eval_tokens ~jmap ~budget p json)
  with Missing error -> raise (Jsont.Error error)

let find_valid_with ?max_results ~jmap p json =
  let budget = result_budget max_results in
  try Some (json_of_eval_result (eval_tokens ~jmap ~budget p json))
  with Missing _ -> None

let get p json = get_with ~jmap:false p json
let find_valid p json = find_valid_with ~jmap:false p json

let get_result_with get p json =
  try Ok (get p json) with Jsont.Error error -> Error error

let find_with get p json = try Some (get p json) with Jsont.Error _ -> None
let get_result = get_result_with get
let find = find_with get

let map_nth n f values =
  let rec loop n rev_prefix = function
    | [] -> None
    | value :: rest when n = 0 ->
        Some (List.rev_append rev_prefix (f value :: rest))
    | value :: rest -> loop (n - 1) (value :: rev_prefix) rest
  in
  loop n [] values

let insert_nth n value values =
  let rec loop n rev_prefix = function
    | values when n = 0 -> Some (List.rev_append rev_prefix (value :: values))
    | [] -> None
    | head :: rest -> loop (n - 1) (head :: rev_prefix) rest
  in
  loop n [] values

let remove_nth n values =
  let rec loop n rev_prefix = function
    | [] -> None
    | _ :: rest when n = 0 -> Some (List.rev_append rev_prefix rest)
    | head :: rest -> loop (n - 1) (head :: rev_prefix) rest
  in
  loop n [] values

let map_member name f members =
  let rec loop rev_prefix = function
    | [] -> List.rev rev_prefix
    | ((((member_name, _) as named), value) as member) :: rest ->
        if String.equal name member_name then
          List.rev_append rev_prefix ((named, f value) :: rest)
        else loop (member :: rev_prefix) rest
  in
  loop [] members

let put_member json name value (members : Jsont.object') =
  match find_unique_member ~jmap:false json name members with
  | None -> members @ [ Jsont.Json.mem (Jsont.Json.name name) value ]
  | Some _ -> map_member name (Fun.const value) members

let cannot_navigate json token =
  Jsont.Error.msgf (Jsont.Json.meta json)
    "JSON Pointer: cannot index into %s with '%s'" (json_sort_string json) token

let map_child token f json =
  match json with
  | Jsont.Object (members, meta) -> (
      match find_unique_member ~jmap:false json token members with
      | None ->
          Jsont.Error.msgf (Jsont.Json.meta json)
            "JSON Pointer: member '%s' not found" token
      | Some _ -> Jsont.Object (map_member token f members, meta))
  | Jsont.Array (elements, meta) -> (
      match array_index token with
      | Index n -> (
          match map_nth n f elements with
          | Some elements -> Jsont.Array (elements, meta)
          | None ->
              Jsont.Error.msgf (Jsont.Json.meta json)
                "JSON Pointer: index %d out of bounds (array has %d elements)" n
                (List.length elements))
      | End | Invalid | Too_large -> invalid_array_index json token)
  | _ -> cannot_navigate json token

let rec update_existing tokens f json =
  match tokens with
  | [] -> f json
  | token :: rest -> map_child token (update_existing rest f) json

let replace p json ~value = update_existing p (Fun.const value) json

let rec add_at tokens value json =
  match tokens with
  | [] -> value
  | [ token ] -> (
      match json with
      | Jsont.Object (members, meta) ->
          Jsont.Object (put_member json token value members, meta)
      | Jsont.Array (elements, meta) -> (
          match array_index token with
          | End -> Jsont.Array (elements @ [ value ], meta)
          | Index n -> (
              match insert_nth n value elements with
              | Some elements -> Jsont.Array (elements, meta)
              | None ->
                  Jsont.Error.msgf (Jsont.Json.meta json)
                    "JSON Pointer: index %d out of bounds for add (array has \
                     %d elements)"
                    n (List.length elements))
          | Invalid | Too_large -> invalid_array_index json token)
      | _ -> cannot_navigate json token)
  | token :: rest -> map_child token (add_at rest value) json

let add p json ~value = add_at p value json

let rec remove_at ~allow_absent tokens json =
  match tokens with
  | [] ->
      Jsont.Error.msgf (Jsont.Json.meta json)
        "JSON Pointer: cannot remove root document"
  | [ token ] -> (
      match json with
      | Jsont.Object (members, meta) -> (
          match find_unique_member ~jmap:false json token members with
          | None when allow_absent -> json
          | None ->
              Jsont.Error.msgf (Jsont.Json.meta json)
                "JSON Pointer: member '%s' not found for remove" token
          | Some _ -> Jsont.Object (Jsont.Json.remove_mem token members, meta))
      | Jsont.Array (elements, meta) -> (
          match array_index token with
          | Index n -> (
              match remove_nth n elements with
              | Some elements -> Jsont.Array (elements, meta)
              | None when allow_absent -> json
              | None ->
                  Jsont.Error.msgf (Jsont.Json.meta json)
                    "JSON Pointer: index %d out of bounds for remove" n)
          | End when allow_absent -> json
          | End | Invalid | Too_large -> invalid_array_index json token)
      | _ -> cannot_navigate json token)
  | token :: rest -> map_child token (remove_at ~allow_absent rest) json

let remove p json = remove_at ~allow_absent:false p json

let rec is_proper_prefix prefix path =
  match (prefix, path) with
  | [], _ :: _ -> true
  | [], [] | _ :: _, [] -> false
  | p :: ps, q :: qs -> String.equal p q && is_proper_prefix ps qs

let move ~from ~path json =
  if is_proper_prefix from path then
    Jsont.Error.msgf (Jsont.Json.meta json)
      "JSON Pointer: move source is a proper prefix of its destination";
  let value = get from json in
  if equal from path then json else add path (remove from json) ~value

let copy ~from ~path json = add path json ~value:(get from json)

let test p json ~expected =
  Option.fold ~none:false ~some:(Jsont.Json.equal expected) (find p json)

let jsont : t Jsont.t =
  let dec meta s = parse_with_meta meta s in
  Jsont.Base.string
    (Jsont.Base.map ~kind:"JSON Pointer" ~doc:"RFC 6901 JSON Pointer" ~dec
       ~enc:to_string ())

let jsont_uri_fragment : t Jsont.t =
  let dec meta s = of_uri_fragment_with_meta meta s in
  Jsont.Base.string
    (Jsont.Base.map ~kind:"JSON Pointer URI fragment"
       ~doc:"RFC 6901 JSON Pointer in URI fragment encoding" ~dec
       ~enc:to_uri_fragment ())

let or_raise = function
  | Ok value -> value
  | Error error -> raise (Jsont.Error error)

let decode_json codec json = or_raise (Jsont.Json.decode' codec json)
let encode_json codec value = or_raise (Jsont.Json.encode' codec value)

let query_path (type a : value mod portable contended)
    ?(absent : a option) ~(codec_name : string) ~get ~find_valid p (codec : a Jsont.t) =
  let dec json =
    match absent with
    | None -> decode_json codec (get p json)
    | Some absent -> (
        match find_valid p json with
        | Some value -> decode_json codec value
        | None -> absent)
  in
  Jsont.map Jsont.json ~dec ~enc:(fun _ ->
      Jsont.Error.msgf Jsont.Meta.none "%s: encode not supported" codec_name)

let path ?absent p codec =
  query_path ?absent ~codec_name:"Json_pointer.path" ~get ~find_valid p codec

let set_path (type a : value mod portable contended)
    ?(allow_absent = false) (codec : a Jsont.t) p (value : a) =
  let dec json =
    let value = encode_json codec value in
    if (not allow_absent) || Option.is_some (find_valid p json) then
      update_existing p
        (fun previous -> Jsont.Json.copy_layout previous ~dst:value)
        json
    else add p json ~value
  in
  Jsont.map Jsont.json ~dec ~enc:Fun.id

let update_path (type a : value mod portable contended)
    ?(absent : a option) p (codec : a Jsont.t) =
  let update value =
    let updated = Jsont.Json.update codec value in
    Jsont.Json.copy_layout value ~dst:updated
  in
  let dec json =
    match absent with
    | None -> update_existing p update json
    | Some absent ->
        if Option.is_some (find_valid p json) then update_existing p update json
        else add p json ~value:(encode_json codec absent)
  in
  Jsont.map Jsont.json ~dec ~enc:Fun.id

let delete_path ?(allow_absent = false) p =
  let dec json = remove_at ~allow_absent p json in
  Jsont.map Jsont.json ~dec ~enc:Fun.id

module Jmap = struct
  let get ?max_results = get_with ?max_results ~jmap:true
  let find_valid ?max_results = find_valid_with ?max_results ~jmap:true
  let get_result ?max_results = get_result_with (get ?max_results)
  let find ?max_results = find_with (get ?max_results)

  let path ?max_results ?absent p codec =
    (* Validate at codec construction rather than at its first decode. Each
       decode still gets a fresh mutable counter through [get]/[find_valid]. *)
    Option.iter
      (fun limit ->
        if limit < 0 then
          invalid_arg "Json_pointer.Jmap.path: max_results must not be negative")
      max_results;
    query_path ?absent ~codec_name:"Json_pointer.Jmap.path"
      ~get:(get ?max_results) ~find_valid:(find_valid ?max_results) p codec

  let path_list (type a : value mod portable contended)
      ?max_results p (codec : a Jsont.t) = path ?max_results p (Jsont.list codec)
end
