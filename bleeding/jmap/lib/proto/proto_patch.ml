(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type entry = { pointer : Json_pointer.t; value : Jsont.json option }

(* PatchObject keys omit the leading slash of their JSON Pointer. *)
let key_of_pointer pointer =
  String.concat "/"
    (List.map Json_pointer.Token.escape (Json_pointer.tokens pointer))

let no_token = "a patch path must not be empty or a single empty token"

let path tokens value =
  match tokens with
  | [] | [ "" ] -> invalid_arg ("Proto_patch: " ^ no_token)
  | tokens -> (
      match Json_pointer.of_tokens tokens with
      | pointer -> { pointer; value }
      | exception Jsont.Error e ->
          invalid_arg ("Proto_patch: " ^ Jsont.Error.to_string e))

let pointer_result key value =
  if String.equal key "" then Error no_token
  else
    Result.map
      (fun pointer -> { pointer; value })
      (Json_pointer.of_string_result ("/" ^ key))

let pointer key value =
  match pointer_result key value with
  | Ok e -> e
  | Error msg -> invalid_arg ("Proto_patch: " ^ msg)

let entry_key e = key_of_pointer e.pointer
let entry_value e = e.value
let set_field name v = path [ name ] (Some v)
let remove_field name = path [ name ] None

module Pmap = Map.MakePortable (Json_pointer)

type t = { rev : entry list; index : entry Pmap.t }

let empty = { rev = []; index = (Pmap.of_list []) }
let is_empty p = List.is_empty p.rev
let entries p = List.rev p.rev
let to_list p = List.rev_map (fun e -> (entry_key e, e.value)) p.rev

(* RFC 8620 Section 5.3: "There MUST NOT be two patches in the PatchObject
   where the pointer of one is the prefix of the pointer of the other".  A
   prefix is taken token-wise, so "keywords" is a prefix of "keywords/$seen"
   but "key" is not. *)
let rec is_prefix a b =
  match (a, b) with
  | [], _ -> true
  | _, [] -> false
  | x :: a, y :: b -> String.equal x y && is_prefix a b

let prefix_error short long =
  Printf.sprintf
    "patch path %S is a prefix of %S; RFC 8620 Section 5.3 forbids two such \
     patches in one PatchObject"
    short long

(* Token lists are compared lexicographically, so a pointer that is a prefix
   of another sorts immediately before every pointer it is a prefix of, with
   nothing that is not an extension of it in between.  A new entry therefore
   conflicts with an entry already in the patch only if it conflicts with its
   immediate neighbour on one side or the other, which the map finds in
   logarithmic time. *)
let check_against e p =
  let tokens = Json_pointer.tokens e.pointer in
  let key = entry_key e in
  if Pmap.mem e.pointer p.index then
    Error (Printf.sprintf "duplicate patch path %S" key)
  else
    let before =
      match
        Pmap.find_last_opt
          (fun pointer -> Json_pointer.compare pointer e.pointer < 0)
          p.index
      with
      | Some (pointer, e') when is_prefix (Json_pointer.tokens pointer) tokens
        ->
          Some (entry_key e', key)
      | Some _ | None -> None
    in
    let after () =
      match
        Pmap.find_first_opt
          (fun pointer -> Json_pointer.compare pointer e.pointer > 0)
          p.index
      with
      | Some (pointer, e') when is_prefix tokens (Json_pointer.tokens pointer)
        ->
          Some (key, entry_key e')
      | Some _ | None -> None
    in
    match if Option.is_some before then before else after () with
    | Some (short, long) -> Error (prefix_error short long)
    | None -> Ok ()

let add e p =
  match check_against e p with
  | Error _ as err -> err
  | Ok () -> Ok { rev = e :: p.rev; index = Pmap.add e.pointer e p.index }

(* The meta of the offending entry is kept so that a decode error points at
   the source text rather than at the whole document. *)
let add_all entries =
  let rec go p = function
    | [] -> Ok p
    | (meta, e) :: entries -> (
        match add e p with
        | Error msg -> Error (meta, msg)
        | Ok p -> go p entries)
  in
  go { rev = []; index = Pmap.of_list [] } entries

let of_entries es =
  Result.map_error snd (add_all (List.map (fun e -> (Jsont.Meta.none, e)) es))

let v es =
  match of_entries es with
  | Ok p -> p
  | Error msg -> invalid_arg ("Proto_patch: " ^ msg)

let of_json_meta = function
  | Jsont.Object (mems, _) ->
      let rec entries acc = function
        | [] -> add_all (List.rev acc)
        | ((n, meta), value) :: mems -> (
            let value = match value with Jsont.Null _ -> None | j -> Some j in
            match pointer_result n value with
            | Error msg -> Error (meta, msg)
            | Ok e -> entries ((meta, e) :: acc) mems)
      in
      entries [] mems
  | j -> Error (Jsont.Json.meta j, "a PatchObject must be a JSON object")

let of_json j = Result.map_error snd (of_json_meta j)

let to_json p =
  Jsont.Json.object'
    (List.map
       (fun e ->
         let value =
           match e.value with Some j -> j | None -> Jsont.Json.null ()
         in
         Jsont.Json.mem (Jsont.Json.name (entry_key e)) value)
       (entries p))

let jsont =
  let dec j =
    match of_json_meta j with
    | Ok p -> p
    | Error (meta, msg) -> Jsont.Error.msgf meta "%s" msg
  in
  Jsont.map ~kind:"PatchObject" ~dec ~enc:to_json Jsont.json
