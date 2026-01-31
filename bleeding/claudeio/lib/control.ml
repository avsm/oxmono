(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "claude.control" ~doc:"Claude control messages"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  request_id : string;
  subtype : string;
  data : Jsont.json;
  unknown : Unknown.t;
}

let jsont =
  Jsont.Object.map ~kind:"Control" (fun request_id subtype data unknown ->
      { request_id; subtype; data; unknown })
  |> Jsont.Object.mem "request_id" Jsont.string ~enc:(fun t -> t.request_id)
  |> Jsont.Object.mem "subtype" Jsont.string ~enc:(fun t -> t.subtype)
  |> Jsont.Object.mem "data" Jsont.json ~enc:(fun t -> t.data)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun t -> t.unknown)
  |> Jsont.Object.finish

let create ~request_id ~subtype ~data =
  { request_id; subtype; data; unknown = Unknown.empty }

let request_id t = t.request_id
let subtype t = t.subtype
let data t = t.data

let to_json t =
  Jsont_bytesrw.encode_string ~format:Jsont.Minify jsont t
  |> Err.get_ok ~msg:"Control.to_json: "
  |> Jsont_bytesrw.decode_string' Jsont.json
  |> Result.map_error Jsont.Error.to_string
  |> Err.get_ok ~msg:"Control.to_json: "

let of_json json =
  Jsont_bytesrw.encode_string ~format:Jsont.Minify Jsont.json json
  |> Err.get_ok' ~msg:"Control.of_json: "
  |> Jsont_bytesrw.decode_string jsont
  |> Err.get_ok' ~msg:"Control.of_json: "

let log_received t =
  Log.debug (fun m ->
      m "Received control message: %a" (Jsont.pp_value jsont ()) t)

let log_sending t =
  Log.debug (fun m ->
      m "Sending control message: %a" (Jsont.pp_value jsont ()) t)
