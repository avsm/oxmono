(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "requests.mime" ~doc:"MIME Type Handling"
module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  type_ : string;
  subtype : string;
  parameters : (string * string) list;
}

let make type_ subtype = {
  type_;
  subtype;
  parameters = [];
}

let of_string s =
  let parts = String.split_on_char ';' s in
  match parts with
  | [] -> make "text" "plain"
  | mime :: params ->
      let mime_parts = String.split_on_char '/' (String.trim mime) in
      let type_, subtype = match mime_parts with
        | [t; s] -> String.trim t, String.trim s
        | [t] -> String.trim t, "*"
        | _ -> "text", "plain"
      in
      let parse_param p =
        match String.split_on_char '=' (String.trim p) with
        | [k; v] ->
            let k = String.trim k in
            let v = String.trim v in
            let v =
              if String.length v >= 2 && v.[0] = '"' && v.[String.length v - 1] = '"'
              then String.sub v 1 (String.length v - 2)
              else v
            in
            Some (String.lowercase_ascii k, v)
        | _ -> None
      in
      let parameters = List.filter_map parse_param params in
      { type_; subtype; parameters }

let to_string t =
  let base = Printf.sprintf "%s/%s" t.type_ t.subtype in
  match t.parameters with
  | [] -> base
  | params ->
      let param_str =
        List.map (fun (k, v) ->
          if String.contains v ' ' || String.contains v ';'
          then Printf.sprintf "%s=\"%s\"" k v
          else Printf.sprintf "%s=%s" k v
        ) params
        |> String.concat "; "
      in
      Printf.sprintf "%s; %s" base param_str

let pp ppf t = Format.fprintf ppf "%s" (to_string t)

let charset t =
  List.assoc_opt "charset" t.parameters

let with_charset charset t =
  let parameters =
    ("charset", charset) ::
    List.filter (fun (k, _) -> k <> "charset") t.parameters
  in
  { t with parameters }

let with_param key value t =
  let key_lower = String.lowercase_ascii key in
  let parameters =
    (key_lower, value) ::
    List.filter (fun (k, _) -> k <> key_lower) t.parameters
  in
  { t with parameters }

(* Common MIME types *)
let json = make "application" "json"
let text = make "text" "plain"
let html = make "text" "html"
let xml = make "application" "xml"
let form = make "application" "x-www-form-urlencoded"
let octet_stream = make "application" "octet-stream"
let multipart_form = make "multipart" "form-data"