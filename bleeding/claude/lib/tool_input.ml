(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Opaque tool input with typed accessors. *)

type t = Jsont.json

(** {1 Escape Hatch} *)

let to_json t = t
let of_json json = json

(** {1 Helper Functions} *)

(* Extract members from JSON object, or return empty list if not an object *)
let get_members = function Jsont.Object (members, _) -> members | _ -> []

(* Find a member by key in the object *)
let find_member key members =
  List.find_map
    (fun ((name, _), value) -> if name = key then Some value else None)
    members

(** {1 Typed Accessors} *)

let get_string t key =
  let members = get_members t in
  match find_member key members with
  | Some json -> (
      match Jsont.Json.decode Jsont.string json with
      | Ok s -> Some s
      | Error _ -> None)
  | None -> None

let get_int t key =
  let members = get_members t in
  match find_member key members with
  | Some json -> (
      match Jsont.Json.decode Jsont.int json with
      | Ok i -> Some i
      | Error _ -> None)
  | None -> None

let get_bool t key =
  let members = get_members t in
  match find_member key members with
  | Some json -> (
      match Jsont.Json.decode Jsont.bool json with
      | Ok b -> Some b
      | Error _ -> None)
  | None -> None

let get_float t key =
  let members = get_members t in
  match find_member key members with
  | Some json -> (
      match Jsont.Json.decode Jsont.number json with
      | Ok f -> Some f
      | Error _ -> None)
  | None -> None

let get_string_list t key =
  let members = get_members t in
  match find_member key members with
  | Some json -> (
      match json with
      | Jsont.Array (items, _) ->
          let strings =
            List.filter_map
              (fun item ->
                match Jsont.Json.decode Jsont.string item with
                | Ok s -> Some s
                | Error _ -> None)
              items
          in
          (* Only return Some if all items were strings *)
          if List.length strings = List.length items then Some strings else None
      | _ -> None)
  | None -> None

let keys t =
  let members = get_members t in
  List.map (fun ((name, _), _) -> name) members

let is_empty t =
  match t with
  | Jsont.Object ([], _) -> true
  | Jsont.Object _ -> false
  | _ -> true

(** {1 Construction} *)

let empty = Jsont.Object ([], Jsont.Meta.none)

let add_member key value t =
  let members = get_members t in
  let new_member = ((key, Jsont.Meta.none), value) in
  (* Replace existing member or add new one *)
  let filtered_members =
    List.filter (fun ((name, _), _) -> name <> key) members
  in
  Jsont.Object (new_member :: filtered_members, Jsont.Meta.none)

let add_string key value t =
  let json_value =
    match Jsont.Json.encode Jsont.string value with
    | Ok json -> json
    | Error _ -> failwith "add_string: encoding failed"
  in
  add_member key json_value t

let add_int key value t =
  let json_value =
    match Jsont.Json.encode Jsont.int value with
    | Ok json -> json
    | Error _ -> failwith "add_int: encoding failed"
  in
  add_member key json_value t

let add_bool key value t =
  let json_value =
    match Jsont.Json.encode Jsont.bool value with
    | Ok json -> json
    | Error _ -> failwith "add_bool: encoding failed"
  in
  add_member key json_value t

let add_float key value t =
  let json_value =
    match Jsont.Json.encode Jsont.number value with
    | Ok json -> json
    | Error _ -> failwith "add_float: encoding failed"
  in
  add_member key json_value t

let of_assoc assoc =
  let members =
    List.map (fun (key, json) -> ((key, Jsont.Meta.none), json)) assoc
  in
  Jsont.Object (members, Jsont.Meta.none)

let of_string_pairs pairs =
  let assoc =
    List.map
      (fun (key, value) -> (key, Jsont.String (value, Jsont.Meta.none)))
      pairs
  in
  of_assoc assoc
