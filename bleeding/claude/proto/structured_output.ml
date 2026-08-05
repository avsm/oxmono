(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Structured output wire format implementation. *)

type t = { json_schema : Jsont.json }

let of_json_schema schema = { json_schema = schema }
let to_json_schema t = t.json_schema

(* Codec for serializing structured output format to wire protocol *)
let jsont : t Jsont.t =
  Jsont.Object.map ~kind:"StructuredOutput" (fun json_schema -> { json_schema })
  |> Jsont.Object.mem "jsonSchema" Jsont.json ~enc:(fun t -> t.json_schema)
  |> Jsont.Object.finish
