(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common
open Mapping

let label = function
  | "FN" -> "Name"
  | "N" -> "Structured name"
  | "EMAIL" -> "Email"
  | "TEL" -> "Phone"
  | "URL" -> "URL"
  | "ORG" -> "Organisation"
  | "TITLE" -> "Title"
  | "ADR" -> "Address"
  | "NOTE" -> "Note"
  | "BDAY" -> "Birthday"
  | "CATEGORIES" -> "Tags"
  | "PHOTO" -> "Photo"
  | "UID" -> "UID"
  | "X-SORTAL-ID" -> "Sortal ID"
  | "X-SORTAL-ALT-NAME" -> "Other name"
  | "X-ABLABEL" -> "Label"
  | "X-FEED-PAUSED" -> "Feed paused"
  | "X-FEED-HINT" -> "Feed hint"
  | "X-SORTAL-PHOTO-PATH" -> "Original photo"
  | s -> s

let fields props =
  let grouped_label p =
    if p.group = "" then None
    else
      List.find_map
        (fun q ->
          if q.group = p.group && q.name = "X-ABLABEL" then
            Some (untext q.value)
          else None)
        props
  in
  List.filter_map
    (fun p ->
      if
        List.mem p.name
          [
            "BEGIN";
            "END";
            "VERSION";
            "X-SORTAL-MAPPING";
            "X-SORTAL-STORE";
            "X-SORTAL-SCHEMA";
            "X-SORTAL-VCARD-KEY";
          ]
      then None
      else
        let name, value =
          if p.name = "X-SORTAL-FIELD" then
            ( unpercent (required p "X-SORTAL-PATH"),
              json_string ~pretty:true (json (untext p.value)) )
          else if
            p.name = "PHOTO"
            && (param p "ENCODING" <> None
               || String.starts_with ~prefix:"data:" p.value)
          then ("Photo", "Embedded image")
          else
            ( label p.name,
              String.concat " · "
                (if List.mem p.name [ "N"; "ORG"; "ADR" ] then
                   components p.value
                 else [ untext p.value ]) )
        in
        let annotations =
          List.filter_map
            (fun (k, v) ->
              if String.starts_with ~prefix:"X-SORTAL-" k then None
              else Some (if k = "TYPE" then v else k ^ "=" ^ v))
            p.params
        in
        let annotations =
          match grouped_label p with
          | Some s when p.name <> "X-ABLABEL" -> s :: annotations
          | _ -> annotations
        in
        Some
          ( (name
            ^
            if annotations = [] then ""
            else " (" ^ String.concat ", " annotations ^ ")"),
            value ))
    props
