(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let set name = match Sys.getenv_opt name with Some "" -> None | v -> v
let file () = set "CONSOLE_CAST"

let deterministic () =
  Option.is_some (set "CONSOLE_DETERMINISTIC_CLOCK") || Option.is_some (file ())

type header = { width : int; height : int; term : string option }

let env =
  Jsont.Object.map ~kind:"asciicast env" Fun.id
  |> Jsont.Object.opt_mem "TERM" Jsont.string ~enc:Fun.id
  |> Jsont.Object.finish

let version =
  Jsont.map Jsont.int
    ~dec:(function
      | 2 -> () | n -> Jsont.Error.msgf Jsont.Meta.none "asciicast version %d" n)
    ~enc:(fun () -> 2)

let header =
  Jsont.Object.map ~kind:"asciicast header" (fun () width height term ->
      { width; height; term })
  |> Jsont.Object.mem "version" version ~enc:ignore
  |> Jsont.Object.mem "width" Jsont.int ~enc:(fun h -> h.width)
  |> Jsont.Object.mem "height" Jsont.int ~enc:(fun h -> h.height)
  |> Jsont.Object.mem "env" env
       ~dec_absent:(fun () -> None)
       ~enc_omit:Option.is_none
       ~enc:(fun h -> h.term)
  |> Jsont.Object.finish

type event = { time : float; data : string }

let field t json =
  match Jsont.Json.decode t json with
  | Ok v -> v
  | Error e -> Jsont.Error.msg Jsont.Meta.none e

(* [s] with each byte that is not part of a UTF-8 character as U+FFFD. *)
let utf_8 = Event.replaced (fun _ -> true)

let event =
  let dec time code data =
    match field Jsont.string code with
    | "o" -> { time = field Jsont.number time; data = field Jsont.string data }
    | c -> Jsont.Error.msgf Jsont.Meta.none "asciicast event %S" c
  in
  let enc e = function
    | 0 -> Jsont.Json.number (Float.round (e.time *. 1e6) /. 1e6)
    | 1 -> Jsont.Json.string "o"
    | _ -> Jsont.Json.string (utf_8 e.data)
  in
  Jsont.t3 ~kind:"asciicast event" ~dec ~enc Jsont.json

(* A time to the microsecond is written as short as it can be: 1.5, or 0. *)
let to_line t v =
  match Jsont_bytesrw.encode_string ~number_format:"%.15g" t v with
  | Ok s -> s ^ "\n"
  | Error e -> invalid_arg e
