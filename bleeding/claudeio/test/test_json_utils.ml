(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Helper functions for JSON operations in tests using jsont codecs *)

let to_string ?(minify = false) json =
  let format = if minify then Jsont.Minify else Jsont.Indent in
  match Jsont_bytesrw.encode_string' ~format Jsont.json json with
  | Ok s -> s
  | Error err -> Jsont.Error.to_string err

(* Helper to decode an optional field with a given codec *)
let get_opt (type a) (codec : a Jsont.t) json key : a option =
  let field_codec =
    Jsont.Object.map ~kind:"field" (fun v -> v)
    |> Jsont.Object.opt_mem key codec ~enc:Fun.id
    |> Jsont.Object.finish
  in
  match Jsont.Json.decode field_codec json with Ok v -> v | Error _ -> None

let get_string json key = get_opt Jsont.string json key
let get_int json key = get_opt Jsont.int json key
let get_bool json key = get_opt Jsont.bool json key
let get_array json key = get_opt (Jsont.list Jsont.json) json key

let as_string json =
  match Jsont.Json.decode Jsont.string json with
  | Ok s -> Some s
  | Error _ -> None
