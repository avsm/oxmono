(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** DOI metadata cached from Zotero Translation Server. *)

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

(** [of_yaml_string s] is the DOI cache encoded by [s], or the empty list if
    [s] is invalid. *)
let of_yaml_string str =
  try
    match Yamlrw.of_string str with
    | `A entries -> List.filter_map of_yaml_value entries
    | _ -> []
  with Yamlrw.Yamlrw_error _ -> []

(** [find_by_doi entries doi] is the non-ignored entry for [doi], if any. *)
let find_by_doi entries doi =
  List.find_opt (fun entry -> not entry.ignore && entry.doi = doi) entries

(** [find_by_url entries url] is the non-ignored entry sourced from [url], if
    any. *)
let find_by_url entries url =
  List.find_opt (fun entry ->
    not entry.ignore && List.mem url entry.source_urls
  ) entries

(** [is_failed entry] is [true] if resolving [entry] failed. *)
let is_failed entry =
  match entry.status with Failed _ -> true | Resolved -> false

(** [remove_failed entries] is [entries] without failed resolutions. *)
let remove_failed entries =
  List.filter (fun entry -> not (is_failed entry)) entries

let status_to_yaml = function
  | Resolved -> []
  | Failed err -> [("error", `String err)]

let to_yaml t =
  let base = [
    ("doi", `String t.doi);
    ("resolved_at", `String t.resolved_at);
  ] in
  let source_url_field = match t.source_urls with
    | [] -> []
    | [url] -> [("source_url", `String url)]
    | urls -> [("source_urls", `A (List.map (fun u -> `String u) urls))]
  in
  let status_fields = status_to_yaml t.status in
  let metadata = if t.status <> Resolved then [] else [
    ("title", `String t.title);
    ("authors", `A (List.map (fun a -> `String a) t.authors));
    ("year", `Float (float_of_int t.year));
    ("bibtype", `String t.bibtype);
    ("publisher", `String t.publisher);
  ] in
  let ignore_field = if t.ignore then [("ignore", `Bool true)] else [] in
  `O (base @ source_url_field @ status_fields @ metadata @ ignore_field)

(** [to_yaml_string entries] is [entries] encoded as YAML. *)
let to_yaml_string entries =
  Yamlrw.to_string (`A (List.map to_yaml entries))

(** [load_file path] is the DOI cache in [path], or the empty list if it cannot
    be read. *)
let load_file path =
  try In_channel.(with_open_bin path input_all) |> of_yaml_string
  with _ -> []

(** [save_file path entries] writes [entries] to [path]. *)
let save_file path entries =
  Out_channel.with_open_bin path (fun oc ->
    output_string oc (to_yaml_string entries))

(** [merge_entries old fresh] is their union by DOI. Existing metadata wins
    and source URLs are combined. *)
let merge_entries existing new_entries =
  let tbl = Hashtbl.create (List.length existing) in
  List.iter (fun e -> Hashtbl.replace tbl e.doi e) existing;
  List.iter (fun e ->
    match Hashtbl.find_opt tbl e.doi with
    | Some old ->
      let merged_urls =
        List.sort_uniq String.compare (old.source_urls @ e.source_urls)
      in
      Hashtbl.replace tbl e.doi { old with source_urls = merged_urls }
    | None -> Hashtbl.add tbl e.doi e
  ) new_entries;
  Hashtbl.to_seq_values tbl |> List.of_seq
