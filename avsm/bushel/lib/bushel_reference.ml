(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Structured reference types for citation management *)

type source = Paper | Note | External

type t = {
  doi : string;
  title : string;
  authors : string list;
  year : int;
  publisher : string option;
  source : source;
  citation : string;
}

type ts = t list

(** Conversion from Bushel_md.reference_source *)
let source_of_md_source = function
  | Bushel_md.Paper -> Paper
  | Bushel_md.Note -> Note
  | Bushel_md.External -> External

let source_to_string = function
  | Paper -> "paper" | Note -> "note" | External -> "external"

(** Extract structured references from a note *)
let of_note ~entries ~default_author note =
  let raw_refs = Bushel_md.note_references entries default_author note in
  let doi_entries = Bushel_entry.doi_entries entries in
  List.map (fun (doi, citation, md_source) ->
    let source = source_of_md_source md_source in
    match Bushel_doi_entry.find_by_doi doi_entries doi with
    | Some e when e.Bushel_doi_entry.status = Bushel_doi_entry.Resolved ->
      { doi; title = e.title; authors = e.authors; year = e.year;
        publisher = Some e.publisher; source; citation }
    | _ ->
      { doi; title = ""; authors = []; year = 0;
        publisher = None; source; citation }
  ) raw_refs

(** {1 YAML Serialization} *)

let to_yaml t =
  let base = [
    ("doi", `String t.doi);
    ("citation", `String t.citation);
    ("source", `String (source_to_string t.source));
  ] in
  let meta = if t.title = "" then [] else [
    ("title", `String t.title);
    ("authors", `A (List.map (fun a -> `String a) t.authors));
    ("year", `Float (float_of_int t.year));
  ] in
  let pub = match t.publisher with Some p -> [("publisher", `String p)] | None -> [] in
  `O (base @ meta @ pub)

let to_yaml_string refs = Yamlrw.to_string (`A (List.map to_yaml refs))

(** {1 JSON Codec} *)

let source_jsont =
  Jsont.enum ~kind:"source" [("paper", Paper); ("note", Note); ("external", External)]

let jsont =
  Jsont.Object.map ~kind:"reference"
    (fun doi title authors year publisher source citation ->
      { doi; title; authors; year; publisher; source; citation })
  |> Jsont.Object.mem "doi" Jsont.string ~enc:(fun r -> r.doi)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.mem "authors" Jsont.(list string) ~enc:(fun r -> r.authors)
  |> Jsont.Object.mem "year" Jsont.int ~enc:(fun r -> r.year)
  |> Jsont.Object.mem "publisher" Jsont.(option string) ~dec_absent:None ~enc:(fun r -> r.publisher)
  |> Jsont.Object.mem "source" source_jsont ~enc:(fun r -> r.source)
  |> Jsont.Object.mem "citation" Jsont.string ~enc:(fun r -> r.citation)
  |> Jsont.Object.finish

let list_jsont = Jsont.list jsont
