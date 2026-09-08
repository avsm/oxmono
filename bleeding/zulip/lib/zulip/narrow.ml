(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = {
  operator : string;
  operand : [ `String of string | `Int of int | `Strings of string list ];
  negated : bool;
}

let make ?(negated = false) operator operand = { operator; operand; negated }
let stream name = make "stream" (`String name)
let stream_id id = make "stream" (`Int (Id.Channel.to_int id))
let topic name = make "topic" (`String name)
let channel = stream
let sender email = make "sender" (`String email)
let sender_id id = make "sender" (`Int (Id.User.to_int id))

type is_operand =
  [ `Alerted | `Dm | `Mentioned | `Private | `Resolved | `Starred | `Unread ]

let is_operand_to_string = function
  | `Alerted -> "alerted"
  | `Dm -> "dm"
  | `Mentioned -> "mentioned"
  | `Private -> "private"
  | `Resolved -> "resolved"
  | `Starred -> "starred"
  | `Unread -> "unread"

let is operand = make "is" (`String (is_operand_to_string operand))

type has_operand = [ `Attachment | `Image | `Link | `Reaction ]

let has_operand_to_string = function
  | `Attachment -> "attachment"
  | `Image -> "image"
  | `Link -> "link"
  | `Reaction -> "reaction"

let has operand = make "has" (`String (has_operand_to_string operand))
let search query = make "search" (`String query)
let id msg_id = make "id" (`Int (Id.Message.to_int msg_id))
let near msg_id = make "near" (`Int (Id.Message.to_int msg_id))
let dm emails = make "dm" (`Strings emails)
let dm_including email = make "dm-including" (`String email)
let not_ filter = { filter with negated = not filter.negated }

let operand_to_json = function
  | `String s -> Jsont.String (s, Jsont.Meta.none)
  | `Int i -> Jsont.Json.int i
  | `Strings ss ->
      Jsont.Array
        ( List.map (fun s -> Jsont.String (s, Jsont.Meta.none)) ss,
          Jsont.Meta.none )

let operand_of_json = function
  | Jsont.String (s, _) -> `String s
  | Jsont.Array (items, _) ->
      `Strings
        (List.map
           (function
             | Jsont.String (s, _) -> s
             | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.String json)
           items)
  | json -> (
      match Jsont.Json.decode' Json_integer.jsont json with
      | Ok i -> `Int i
      | Error _ ->
          Jsont.Error.msgf (Jsont.Json.meta json)
            "narrow operand must be a string, integer, or string array")

let operand_jsont =
  Jsont.map ~kind:"Zulip narrow operand" ~dec:operand_of_json
    ~enc:operand_to_json Jsont.json

let jsont =
  let kind = "Narrow" in
  let make operator operand negated = { operator; operand; negated } in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "operator" Jsont.string ~enc:(fun t -> t.operator)
  |> Jsont.Object.mem "operand" operand_jsont ~enc:(fun t -> t.operand)
  |> Jsont.Object.mem "negated" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun t -> t.negated)
  |> Jsont.Object.finish

let list_jsont = Jsont.list jsont

let pp fmt t =
  let neg = if t.negated then "-" else "" in
  let operand =
    match t.operand with
    | `String s -> s
    | `Int i -> string_of_int i
    | `Strings ss -> String.concat "," ss
  in
  Format.fprintf fmt "%s%s:%s" neg t.operator operand
