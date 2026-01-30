(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** DOI entries resolved from external sources via Zotero Translation Server *)

type status =
  | Resolved
  | Failed of string

type t = {
  doi : string;
  title : string;
  authors : string list;
  year : int;
  bibtype : string;
  publisher : string;
  resolved_at : string;
  source_urls : string list;
  status : status;
  ignore : bool;
}

type ts = t list

let get_string key fields =
  match List.assoc_opt key fields with
  | Some (`String s) -> s
  | _ -> ""

let get_string_opt key fields =
  match List.assoc_opt key fields with
  | Some (`String s) -> Some s
  | _ -> None

let get_int key fields =
  match List.assoc_opt key fields with
  | Some (`Float f) -> int_of_float f
  | _ -> 0

let get_bool key fields =
  match List.assoc_opt key fields with
  | Some (`Bool b) -> b
  | _ -> false

let get_strings key fields =
  match List.assoc_opt key fields with
  | Some (`A items) ->
    List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

let of_yaml_value = function
  | `O fields ->
    let doi = get_string "doi" fields in
    let resolved_at = get_string "resolved_at" fields in
    let source_urls =
      match get_strings "source_urls" fields with
      | [] ->
        (match get_string_opt "source_url" fields with
         | Some u -> [u]
         | None -> [])
      | urls -> urls
    in
    let ignore = get_bool "ignore" fields in
    let error = get_string_opt "error" fields in
    (match error with
     | Some err ->
       Some { doi; title = ""; authors = []; year = 0; bibtype = "";
              publisher = ""; resolved_at; source_urls;
              status = Failed err; ignore }
     | None ->
       let title = get_string "title" fields in
       let authors = get_strings "authors" fields in
       let year = get_int "year" fields in
       let bibtype = get_string "bibtype" fields in
       let publisher = get_string "publisher" fields in
       Some { doi; title; authors; year; bibtype; publisher;
              resolved_at; source_urls; status = Resolved; ignore })
  | _ -> None

(** Load DOI entries from a YAML string *)
let of_yaml_string str =
  try
    match Yamlrw.of_string str with
    | `A entries -> List.filter_map of_yaml_value entries
    | _ -> []
  with Yamlrw.Yamlrw_error _ -> []

(** Find entry by DOI (excludes ignored entries) *)
let find_by_doi entries doi =
  List.find_opt (fun entry -> not entry.ignore && entry.doi = doi) entries

(** Find entry by source URL (excludes ignored entries) *)
let find_by_url entries url =
  List.find_opt (fun entry ->
    not entry.ignore && List.mem url entry.source_urls
  ) entries
