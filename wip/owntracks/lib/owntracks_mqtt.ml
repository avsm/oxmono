(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  topic : string;
  user : string option;
  device : string option;
  message : Owntracks_message.t;
}

let topic t = t.topic
let user t = t.user
let device t = t.device
let message t = t.message

let[@zero_alloc] level_end topic start =
  let mutable pos = start in
  while pos < String.length topic && topic.[pos] <> '/' do
    pos <- pos + 1
  done;
  pos

let parse_topic topic =
  if not (String.starts_with ~prefix:"owntracks/" topic) then None
  else
    let user_start = 10 in
    let user_end = level_end topic user_start in
    let device_start = user_end + 1 in
    let device_end = level_end topic device_start in
    if user_end = user_start || device_start >= device_end then None
    else
      Some
        ( String.sub topic user_start (user_end - user_start),
          String.sub topic device_start (device_end - device_start) )

let of_mqtt ~topic ~payload =
  match Owntracks_message.decode payload with
  | Error _ as e -> e
  | Ok message ->
      let message =
        match message with
        | Owntracks_message.Location loc ->
            Owntracks_message.Location (Owntracks_location.with_topic topic loc)
        | other -> other
      in
      let user, device =
        match parse_topic topic with
        | Some (u, d) -> (Some u, Some d)
        | None -> (None, None)
      in
      Ok { topic; user; device; message }

let default_topic = "owntracks/#"

let check_level name level =
  if
    level = ""
    || String.exists (function '/' | '+' | '#' -> true | _ -> false) level
  then invalid_arg (name ^ " must be one nonempty MQTT topic level");
  if not (Mqttz.Topic.Name.validate level) then
    invalid_arg (name ^ " is not a valid MQTT topic level")

let user_topic user =
  check_level "user" user;
  "owntracks/" ^ user ^ "/#"

let device_topic ~user ~device =
  check_level "user" user;
  check_level "device" device;
  "owntracks/" ^ user ^ "/" ^ device

let pp ppf t =
  Format.fprintf ppf "[%s] %a" t.topic Owntracks_message.pp t.message
