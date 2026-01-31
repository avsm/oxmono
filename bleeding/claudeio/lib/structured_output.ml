(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "claude.structured_output" ~doc:"Structured output"

module Log = (val Logs.src_log src : Logs.LOG)

type t = { json_schema : Jsont.json }

let json_to_string json =
  match Jsont_bytesrw.encode_string' Jsont.json json with
  | Ok str -> str
  | Error err -> failwith (Jsont.Error.to_string err)

let of_json_schema schema =
  Log.debug (fun m ->
      m "Created output format from JSON schema: %s" (json_to_string schema));
  { json_schema = schema }

let json_schema t = t.json_schema

(* Codec for serializing structured output format *)
let jsont : t Jsont.t =
  Jsont.Object.map ~kind:"StructuredOutput" (fun json_schema -> { json_schema })
  |> Jsont.Object.mem "jsonSchema" Jsont.json ~enc:(fun t -> t.json_schema)
  |> Jsont.Object.finish

let to_json t =
  match Jsont.Json.encode jsont t with
  | Ok json -> json
  | Error msg -> failwith ("Structured_output.to_json: " ^ msg)

let of_json json =
  match Jsont.Json.decode jsont json with
  | Ok t -> t
  | Error msg -> raise (Invalid_argument ("Structured_output.of_json: " ^ msg))
