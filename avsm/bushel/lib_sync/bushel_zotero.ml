(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Zotero Translation Server client for DOI resolution *)

let src = Logs.Src.create "bushel.zotero" ~doc:"Zotero DOI resolution"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Types} *)

type paper_metadata = {
  title : string;
  authors : string list;
  year : int;
  month : int;
  bibtype : string;
  publisher : string;
  booktitle : string;
  journal : string;
  institution : string;
  pages : string;
  volume : string option;
  number : string option;
  doi : string option;
  url : string option;
  abstract : string option;
  bib : string;
}

(** {1 Month Parsing} *)

let month_of_string s =
  match String.lowercase_ascii s with
  | "jan" | "january" -> 1
  | "feb" | "february" -> 2
  | "mar" | "march" -> 3
  | "apr" | "april" -> 4
  | "may" -> 5
  | "jun" | "june" -> 6
  | "jul" | "july" -> 7
  | "aug" | "august" -> 8
  | "sep" | "september" -> 9
  | "oct" | "october" -> 10
  | "nov" | "november" -> 11
  | "dec" | "december" -> 12
  | _ -> 1

let string_of_month = function
  | 1 -> "jan" | 2 -> "feb" | 3 -> "mar" | 4 -> "apr"
  | 5 -> "may" | 6 -> "jun" | 7 -> "jul" | 8 -> "aug"
  | 9 -> "sep" | 10 -> "oct" | 11 -> "nov" | 12 -> "dec"
  | _ -> "jan"

(** {1 JSON Helpers for Zotero JSON}

    Zotero returns complex JSON with varying structure.
    We use pattern matching on the generic json type. *)

type creator = {
  first_name : string;
  last_name : string;
}

let creator_jsont : creator Jsont.t =
  let open Jsont in
  let open Object in
  map ~kind:"creator" (fun first_name last_name -> { first_name; last_name })
  |> mem "firstName" string ~dec_absent:"" ~enc:(fun c -> c.first_name)
  |> mem "lastName" string ~dec_absent:"" ~enc:(fun c -> c.last_name)
  |> finish

(** Extract string from generic JSON, returning None if missing or wrong type *)
let rec find_in_json json path =
  match path with
  | [] -> Some json
  | key :: path_rest ->
    match json with
    | Jsont.Object (mems, _) ->
      let rec find_mem = function
        | [] -> None
        | ((name, _), value) :: mems_rest ->
          if name = key then find_in_json value path_rest
          else find_mem mems_rest
      in
      find_mem mems
    | _ -> None

let get_string json path =
  match find_in_json json path with
  | Some (Jsont.String (s, _)) -> Some s
  | _ -> None

let get_string_exn json path ~default =
  get_string json path |> Option.value ~default

let get_int json path ~default =
  match find_in_json json path with
  | Some (Jsont.Number (f, _)) -> int_of_float f
  | Some (Jsont.String (s, _)) -> (try int_of_string s with _ -> default)
  | _ -> default

let get_creators json =
  match find_in_json json ["creators"] with
  | Some (Jsont.Array (items, _)) ->
    List.filter_map (fun item ->
      match Jsont.Json.decode creator_jsont item with
      | Ok c -> Some c
      | Error _ -> None
    ) items
  | _ -> []

(** {1 BibTeX Parsing} *)

(** Simple BibTeX field extraction *)
let extract_bibtex_field bib field =
  let pattern = Printf.sprintf "%s\\s*=\\s*[{\"](.*?)[}\"]" field in
  try
    let re = Re.Pcre.regexp ~flags:[`CASELESS] pattern in
    let groups = Re.exec re bib in
    Some (Re.Group.get groups 1)
  with _ -> None

let extract_bibtex_type bib =
  try
    let re = Re.Pcre.regexp "@(\\w+)\\s*\\{" in
    let groups = Re.exec re bib in
    String.lowercase_ascii (Re.Group.get groups 1)
  with _ -> "misc"

(** {1 Author Parsing} *)

(** Split "Last, First and Last2, First2" into list of names *)
let parse_authors author_str =
  let parts = String.split_on_char '&' author_str in
  let parts = List.concat_map (fun s ->
    Astring.String.cuts ~empty:false ~sep:" and " s
  ) parts in
  List.map (fun name ->
    let name = String.trim name in
    (* Handle "Last, First" format *)
    match Astring.String.cut ~sep:"," name with
    | Some (last, first) ->
      Printf.sprintf "%s %s" (String.trim first) (String.trim last)
    | None -> name
  ) parts

(** {1 Zotero Translation Server API} *)

let web_endpoint base_url =
  if String.ends_with ~suffix:"/" base_url then base_url ^ "web"
  else base_url ^ "/web"

let export_endpoint base_url =
  if String.ends_with ~suffix:"/" base_url then base_url ^ "export"
  else base_url ^ "/export"

let resolve_doi ~http ~server_url doi =
  Log.info (fun m -> m "Resolving DOI: %s" doi);
  let url = web_endpoint server_url in
  let body = "https://doi.org/" ^ doi in
  match Bushel_http.post ~http ~content_type:"text/plain" ~body url with
  | Error e -> Error e
  | Ok json_str ->
    match Jsont_bytesrw.decode_string Jsont.json json_str with
    | Ok json -> Ok json
    | Error e -> Error (Printf.sprintf "JSON parse error: %s" e)

let export_bibtex ~http ~server_url json =
  let url = export_endpoint server_url ^ "?format=bibtex" in
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Error e -> Error e
  | Ok body -> Bushel_http.post ~http ~content_type:"application/json" ~body url

(** {1 DOI Resolution} *)

let resolve ~http ~server_url ~slug doi =
  match resolve_doi ~http ~server_url doi with
  | Error e -> Error e
  | Ok json ->
    (* Export to BibTeX *)
    match export_bibtex ~http ~server_url json with
    | Error e -> Error (Printf.sprintf "BibTeX export failed: %s" e)
    | Ok bib ->
      Log.debug (fun m -> m "Got BibTeX: %s" bib);
      (* Parse the JSON response for metadata *)
      let item =
        match json with
        | Jsont.Array (first :: _, _) -> first
        | _ -> json
      in

      (* Extract fields from JSON and BibTeX *)
      let title = get_string_exn item ["title"] ~default:"Untitled" in
      let authors =
        let creators = get_creators item in
        List.filter_map (fun c ->
          let first = c.first_name in
          let last = c.last_name in
          if first = "" && last = "" then None
          else Some (String.trim (first ^ " " ^ last))
        ) creators
      in
      let authors = if authors = [] then
        (* Fallback to BibTeX author field *)
        match extract_bibtex_field bib "author" with
        | Some a -> parse_authors a
        | None -> []
      else authors in

      let year = get_int item ["date"] ~default:(
        match extract_bibtex_field bib "year" with
        | Some y -> (try int_of_string y with _ -> 2024)
        | None -> 2024
      ) in
      let month = match extract_bibtex_field bib "month" with
        | Some m -> month_of_string m
        | None -> 1
      in

      let bibtype = extract_bibtex_type bib in
      let publisher = get_string_exn item ["publisher"] ~default:(
        extract_bibtex_field bib "publisher" |> Option.value ~default:""
      ) in
      let booktitle = extract_bibtex_field bib "booktitle" |> Option.value ~default:"" in
      let journal = get_string_exn item ["publicationTitle"] ~default:(
        extract_bibtex_field bib "journal" |> Option.value ~default:""
      ) in
      let institution = extract_bibtex_field bib "institution" |> Option.value ~default:"" in
      let pages = extract_bibtex_field bib "pages" |> Option.value ~default:"" in
      let volume = extract_bibtex_field bib "volume" in
      let number = extract_bibtex_field bib "number" in
      let url = get_string item ["url"] in
      let abstract = get_string item ["abstractNote"] in

      (* Generate clean BibTeX with slug as cite key *)
      let cite_key = Astring.String.map (function '-' -> '_' | x -> x) slug in
      let bib = Re.replace_string (Re.Pcre.regexp "@\\w+\\{[^,]+,")
        ~by:(Printf.sprintf "@%s{%s," bibtype cite_key) bib in

      Ok {
        title;
        authors;
        year;
        month;
        bibtype;
        publisher;
        booktitle;
        journal;
        institution;
        pages;
        volume;
        number;
        doi = Some doi;
        url;
        abstract;
        bib = String.trim bib;
      }

(** {1 Paper File Generation} *)

let to_yaml_frontmatter ~slug:_ ~ver:_ metadata =
  let buf = Buffer.create 1024 in
  let add key value =
    if value <> "" then
      Buffer.add_string buf (Printf.sprintf "%s: %s\n" key value)
  in
  let add_opt key = function
    | Some v when v <> "" -> add key v
    | _ -> ()
  in
  let add_quoted key value =
    if value <> "" then
      Buffer.add_string buf (Printf.sprintf "%s: \"%s\"\n" key value)
  in

  Buffer.add_string buf "---\n";
  add "title" metadata.title;

  (* Authors as list *)
  Buffer.add_string buf "author:\n";
  List.iter (fun a ->
    Buffer.add_string buf (Printf.sprintf "  - %s\n" a)
  ) metadata.authors;

  add_quoted "year" (string_of_int metadata.year);
  add "month" (string_of_month metadata.month);
  add "bibtype" metadata.bibtype;

  if metadata.publisher <> "" then add "publisher" metadata.publisher;
  if metadata.booktitle <> "" then add "booktitle" metadata.booktitle;
  if metadata.journal <> "" then add "journal" metadata.journal;
  if metadata.institution <> "" then add "institution" metadata.institution;
  if metadata.pages <> "" then add "pages" metadata.pages;
  add_opt "volume" metadata.volume;
  add_opt "number" metadata.number;
  add_opt "doi" metadata.doi;
  add_opt "url" metadata.url;

  (* BibTeX entry *)
  Buffer.add_string buf "bib: |\n";
  String.split_on_char '\n' metadata.bib |> List.iter (fun line ->
    Buffer.add_string buf (Printf.sprintf "  %s\n" line)
  );

  Buffer.add_string buf "---\n";

  (* Abstract as body *)
  (match metadata.abstract with
   | Some abstract when abstract <> "" ->
     Buffer.add_string buf "\n";
     Buffer.add_string buf abstract;
     Buffer.add_string buf "\n"
   | _ -> ());

  Buffer.contents buf

(** {1 Merging with Existing Papers} *)

let merge_with_existing ~existing metadata =
  (* Preserve fields from existing paper if new ones are empty *)
  {
    metadata with
    abstract = (match metadata.abstract with
      | Some a when a <> "" -> Some a
      | _ -> if Bushel.Paper.abstract existing <> "" then Some (Bushel.Paper.abstract existing) else None);
  }
  (* Note: tags, projects, selected, slides, video are preserved at a higher level
     when writing the file - they're not part of paper_metadata *)
