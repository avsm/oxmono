(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Jsont codecs for TOML tagged JSON format.

    This module provides bidirectional codecs between TOML values and
    the tagged JSON format used by {{:https://github.com/toml-lang/toml-test}
    toml-test}. *)

module Toml = Tomlt.Toml
module String_map = Map.Make(String)

(* The tagged JSON format wraps scalar values as {"type": "T", "value": "V"}
   while arrays and objects are passed through with their contents recursively
   encoded. *)

(* Encode TOML -> JSON (string representation) using Tomlt_bytesrw's encoder *)
let encode (v : Toml.t) : string =
  Tomlt_bytesrw.Tagged_json.encode v

(* Decode JSON (string) -> TOML using Tomlt_bytesrw's decoder *)
let decode (s : string) : Toml.t =
  Tomlt_bytesrw.Tagged_json.decode s

(* Convenience result-based decode *)
let decode_result (s : string) : (Toml.t, string) result =
  try Ok (decode s)
  with Failure msg -> Error msg

(* Tagged value type for scalar types *)
type tagged_value = {
  typ : string;
  value : string;
}

(* Convert tagged value to TOML *)
let tagged_to_toml (t : tagged_value) : Toml.t =
  match t.typ with
  | "string" -> Toml.String t.value
  | "integer" -> Toml.Int (Int64.of_string t.value)
  | "float" ->
      let f =
        match t.value with
        | "nan" -> Float.nan
        | "inf" | "+inf" -> Float.infinity
        | "-inf" -> Float.neg_infinity
        | s -> float_of_string s
      in
      Toml.Float f
  | "bool" -> Toml.Bool (t.value = "true")
  | "datetime" -> Toml.Datetime t.value
  | "datetime-local" -> Toml.Datetime_local t.value
  | "date-local" -> Toml.Date_local t.value
  | "time-local" -> Toml.Time_local t.value
  | typ -> failwith ("Unknown tagged type: " ^ typ)

(* Convert TOML scalar to tagged value *)
let toml_to_tagged (v : Toml.t) : tagged_value =
  match v with
  | Toml.String s -> { typ = "string"; value = s }
  | Toml.Int i -> { typ = "integer"; value = Int64.to_string i }
  | Toml.Float f ->
      let value =
        if Float.is_nan f then "nan"
        else if f = Float.infinity then "inf"
        else if f = Float.neg_infinity then "-inf"
        else if f = 0.0 && 1.0 /. f = Float.neg_infinity then "-0"
        else Printf.sprintf "%g" f
      in
      { typ = "float"; value }
  | Toml.Bool b -> { typ = "bool"; value = if b then "true" else "false" }
  | Toml.Datetime s -> { typ = "datetime"; value = s }
  | Toml.Datetime_local s -> { typ = "datetime-local"; value = s }
  | Toml.Date_local s -> { typ = "date-local"; value = s }
  | Toml.Time_local s -> { typ = "time-local"; value = s }
  | Toml.Array _ | Toml.Table _ ->
      failwith "Cannot convert non-scalar TOML value to tagged value"

(* Jsont codec for tagged values (scalars only) *)
let tagged_jsont : tagged_value Jsont.t =
  Jsont.Object.(
    map (fun typ value -> { typ; value })
    |> mem "type" Jsont.string ~enc:(fun t -> t.typ)
    |> mem "value" Jsont.string ~enc:(fun t -> t.value)
    |> finish
  )

(* The main recursive TOML value codec.

   This is a bit tricky because:
   - When decoding an object, we need to determine if it's a tagged scalar
     (has "type" and "value" keys) or a table (keys map to tagged values)
   - When encoding, scalars become {"type": ..., "value": ...}, arrays become
     [...], and tables become {"key": <tagged>, ...}
*)

let rec toml_jsont : Toml.t Jsont.t Lazy.t = lazy (
  Jsont.any
    ~dec_array:(Lazy.force toml_array)
    ~dec_object:(Lazy.force toml_object)
    ~enc:(fun v ->
      match v with
      | Toml.Array _ -> Lazy.force toml_array
      | Toml.Table _ -> Lazy.force toml_table_enc
      | _ -> Lazy.force toml_scalar_enc)
    ()
)

and toml_array : Toml.t Jsont.t Lazy.t = lazy (
  Jsont.map
    ~dec:(fun items -> Toml.Array items)
    ~enc:(function
      | Toml.Array items -> items
      | _ -> failwith "Expected array")
    (Jsont.list (Jsont.rec' toml_jsont))
)

and toml_object : Toml.t Jsont.t Lazy.t = lazy (
  (* Try to decode as tagged scalar first, fall back to table *)
  Jsont.Object.(
    map (fun typ_opt value_opt rest ->
      match typ_opt, value_opt with
      | Some typ, Some value when String_map.is_empty rest ->
          (* Tagged scalar value *)
          tagged_to_toml { typ; value }
      | _ ->
          (* Regular table - include type/value if present but not a valid tagged pair *)
          let pairs = String_map.bindings rest in
          let pairs =
            match typ_opt with
            | Some typ ->
                let typ_toml = Toml.String typ in
                ("type", typ_toml) :: pairs
            | None -> pairs
          in
          let pairs =
            match value_opt with
            | Some value ->
                let value_toml = Toml.String value in
                ("value", value_toml) :: pairs
            | None -> pairs
          in
          Toml.Table pairs)
    |> opt_mem "type" Jsont.string ~enc:(fun _ -> None)
    |> opt_mem "value" Jsont.string ~enc:(fun _ -> None)
    |> keep_unknown
        (Mems.string_map (Jsont.rec' toml_jsont))
        ~enc:(fun _ -> String_map.empty)  (* Encoding handled by toml_table_enc *)
    |> finish
  )
)

and toml_scalar_enc : Toml.t Jsont.t Lazy.t = lazy (
  Jsont.map
    ~dec:(fun t -> tagged_to_toml t)
    ~enc:toml_to_tagged
    tagged_jsont
)

and toml_table_enc : Toml.t Jsont.t Lazy.t = lazy (
  Jsont.Object.(
    map (fun m -> Toml.Table (String_map.bindings m))
    |> keep_unknown
        (Mems.string_map (Jsont.rec' toml_jsont))
        ~enc:(function
          | Toml.Table pairs ->
              List.fold_left (fun m (k, v) -> String_map.add k v m)
                String_map.empty pairs
          | _ -> failwith "Expected table")
    |> finish
  )
)

(* Main codec *)
let toml : Toml.t Jsont.t = Jsont.rec' toml_jsont

(* Convenience functions using jsont *)

let encode_jsont (v : Toml.t) : (string, string) result =
  Jsont_bytesrw.encode_string toml v

let decode_jsont (s : string) : (Toml.t, string) result =
  Jsont_bytesrw.decode_string toml s

let decode_jsont' (s : string) : (Toml.t, Jsont.Error.t) result =
  Jsont_bytesrw.decode_string' toml s

let decode_jsont_exn (s : string) : Toml.t =
  match decode_jsont' s with
  | Ok v -> v
  | Error e -> raise (Jsont.Error e)
