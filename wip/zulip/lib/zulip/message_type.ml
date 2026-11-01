(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = [ `Direct | `Channel ]

let to_string = function `Direct -> "direct" | `Channel -> "stream"

let of_string = function
  | "direct" | "private" -> Some `Direct
  | "stream" | "channel" -> Some `Channel
  | _ -> None

let pp fmt t = Format.fprintf fmt "%s" (to_string t)

let jsont =
  Jsont.map ~kind:"Zulip message type"
    ~dec:(fun value ->
      match of_string value with
      | Some t -> t
      | None -> Jsont.Error.msgf Jsont.Meta.none "unknown message type %S" value)
    ~enc:to_string Jsont.string
