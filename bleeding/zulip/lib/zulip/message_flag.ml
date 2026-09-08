(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type modifiable = [ `Read | `Starred | `Collapsed ]

type t =
  [ modifiable
  | `Mentioned
  | `Wildcard_mentioned
  | `Has_alert_word
  | `Historical
  | `Other of string ]

let to_string = function
  | `Read -> "read"
  | `Starred -> "starred"
  | `Collapsed -> "collapsed"
  | `Mentioned -> "mentioned"
  | `Wildcard_mentioned -> "wildcard_mentioned"
  | `Has_alert_word -> "has_alert_word"
  | `Historical -> "historical"
  | `Other value -> value

let of_string = function
  | "read" -> `Read
  | "starred" -> `Starred
  | "collapsed" -> `Collapsed
  | "mentioned" -> `Mentioned
  | "wildcard_mentioned" -> `Wildcard_mentioned
  | "has_alert_word" -> `Has_alert_word
  | "historical" -> `Historical
  | value -> `Other value

let modifiable_of_string value =
  match of_string value with #modifiable as flag -> Some flag | _ -> None

type op = Add | Remove

let op_to_string = function Add -> "add" | Remove -> "remove"
let pp fmt t = Format.fprintf fmt "%s" (to_string t)

let jsont =
  Jsont.map ~kind:"message flag" ~dec:of_string ~enc:to_string Jsont.string

let modifiable_jsont =
  Jsont.map ~kind:"modifiable Zulip message flag"
    ~dec:(fun s ->
      match modifiable_of_string s with
      | Some t -> t
      | None ->
          Jsont.Error.msgf Jsont.Meta.none "unknown modifiable message flag %S"
            s)
    ~enc:to_string Jsont.string
