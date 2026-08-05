(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Zotero Translation Server Client - Eio/Requests implementation *)

module J = Jsont.Json

(* From the ZTS source code:  https://github.com/zotero/translation-server/blob/master/src/formats.js
  bibtex: "9cb70025-a888-4a29-a210-93ec52da40d4",
	biblatex: "b6e39b57-8942-4d11-8259-342c46ce395f",
	bookmarks: "4e7119e0-02be-4848-86ef-79a64185aad8",
	coins: "05d07af9-105a-4572-99f6-a8e231c0daef",
	csljson: "bc03b4fe-436d-4a1f-ba59-de4d2d7a63f7",
	csv: "25f4c5e2-d790-4daa-a667-797619c7e2f2",
	endnote_xml: "eb7059a4-35ec-4961-a915-3cf58eb9784b",
	evernote: "18dd188a-9afc-4cd6-8775-1980c3ce0fbf",
	mods: "0e2235e7-babf-413c-9acf-f27cce5f059c",
	rdf_bibliontology: "14763d25-8ba0-45df-8f52-b8d1108e7ac9",
	rdf_dc: "6e372642-ed9d-4934-b5d1-c11ac758ebb7",
	rdf_zotero: "14763d24-8ba0-45df-8f52-b8d1108e7ac9",
	refer: "881f60f2-0802-411a-9228-ce5f47b64c7d",
	refworks_tagged: "1a3506da-a303-4b0a-a1cd-f216e6138d86",
	ris: "32d59d2d-b65a-4da4-b0a3-bdd3cfb979e7",
	tei: "032ae9b7-ab90-9205-a479-baf81f49184a",
	wikipedia: "3f50aaac-7acc-4350-acd0-59cb77faf620"
 *)

type format =
  | Bibtex
  | Biblatex
  | Bookmarks
  | Coins
  | Csljson
  | Csv
  | Endnote_xml
  | Evernote
  | Mods
  | Rdf_bibliontology
  | Rdf_dc
  | Rdf_zotero
  | Refer
  | Refworks_tagged
  | Ris
  | Tei
  | Wikipedia

let format_to_string = function
  | Bibtex -> "bibtex"
  | Biblatex -> "biblatex"
  | Bookmarks -> "bookmarks"
  | Coins -> "coins"
  | Csljson -> "csljson"
  | Csv -> "csv"
  | Endnote_xml -> "endnote_xml"
  | Evernote -> "evernote"
  | Mods -> "mods"
  | Rdf_bibliontology -> "rdf_bibliontology"
  | Rdf_dc -> "rdf_dc"
  | Rdf_zotero -> "rdf_zotero"
  | Refer -> "refer"
  | Refworks_tagged -> "refworks_tagged"
  | Ris -> "ris"
  | Tei -> "tei"
  | Wikipedia -> "wikipedia"

let format_of_string = function
  | "bibtex" -> Some Bibtex
  | "biblatex" -> Some Biblatex
  | "bookmarks" -> Some Bookmarks
  | "coins" -> Some Coins
  | "csljson" -> Some Csljson
  | "csv" -> Some Csv
  | "endnote_xml" -> Some Endnote_xml
  | "evernote" -> Some Evernote
  | "mods" -> Some Mods
  | "rdf_bibliontology" -> Some Rdf_bibliontology
  | "rdf_dc" -> Some Rdf_dc
  | "rdf_zotero" -> Some Rdf_zotero
  | "refer" -> Some Refer
  | "refworks_tagged" -> Some Refworks_tagged
  | "ris" -> Some Ris
  | "tei" -> Some Tei
  | "wikipedia" -> Some Wikipedia
  | _ -> None

(* Session type *)
type t = { session : Requests.t; base_url : string }

let create ?session ~sw env ~base_url =
  let session = Option.value session ~default:(Requests.create ~sw env) in
  { session; base_url }

let base_url t = t.base_url
let http_session t = t.session

(* Logging *)
let log_src = Logs.Src.create "zotero_translation" ~doc:"Zotero Translation Server client"
module Log = (val Logs.src_log log_src : Logs.LOG)

exception Api_error of int * string

(* URL construction *)
let endpoint base_uri path =
  if String.ends_with ~suffix:"/" base_uri then base_uri ^ path
  else base_uri ^ "/" ^ path

let web_endp base_uri = endpoint base_uri "web"
let export_endp base_uri = endpoint base_uri "export"
let search_endp base_uri = endpoint base_uri "search"

(* HTTP helpers *)
let post_text_get_json t ~url ~body =
  Log.debug (fun m -> m "POST %s (text body)" url);
  let headers = Requests.Headers.(empty |> content_type Requests.Mime.text) in
  let response = Requests.post t.session url ~headers ~body:(Requests.Body.text body) in
  let status = Requests.Response.status_code response in
  if Requests.Response.ok response then begin
    Log.debug (fun m -> m "Response: %d OK" status);
    let body = Requests.Response.text response in
    match Jsont_bytesrw.decode_string Jsont.json body with
    | Ok result -> result
    | Error e ->
        Log.err (fun m -> m "JSON parse error: %s" e);
        failwith (Fmt.str "JSON parse error: %s" e)
  end
  else begin
    let body = Requests.Response.text response in
    Log.err (fun m -> m "API error %d: %s" status body);
    raise (Api_error (status, body))
  end

let post_json_get_text t ~url ~json =
  Log.debug (fun m -> m "POST %s (JSON body)" url);
  let headers = Requests.Headers.(empty |> content_type Requests.Mime.json) in
  let body_str =
    match Jsont_bytesrw.encode_string ~format:Jsont.Minify Jsont.json json with
    | Ok s -> s
    | Error e -> failwith (Fmt.str "JSON encode error: %s" e)
  in
  let response = Requests.post t.session url ~headers ~body:(Requests.Body.text body_str) in
  let status = Requests.Response.status_code response in
  if Requests.Response.ok response then begin
    Log.debug (fun m -> m "Response: %d OK" status);
    Requests.Response.text response
  end
  else begin
    let body = Requests.Response.text response in
    Log.err (fun m -> m "API error %d: %s" status body);
    raise (Api_error (status, body))
  end

(* API operations *)
let resolve_doi t doi =
  let body = "https://doi.org/" ^ doi in
  let url = web_endp t.base_url in
  Log.info (fun m -> m "Resolving DOI: %s" doi);
  post_text_get_json t ~url ~body

let resolve_url t target_url =
  let url = web_endp t.base_url in
  Log.info (fun m -> m "Resolving URL: %s" target_url);
  post_text_get_json t ~url ~body:target_url

let search_id t doi =
  let body = "https://doi.org/" ^ doi in
  let url = search_endp t.base_url in
  Log.info (fun m -> m "Searching DOI: %s" doi);
  post_text_get_json t ~url ~body

let export t format json =
  let url =
    let base = export_endp t.base_url in
    let uri = Uri.of_string base in
    Uri.add_query_param' uri ("format", format_to_string format) |> Uri.to_string
  in
  Log.info (fun m -> m "Exporting to format: %s" (format_to_string format));
  let result = post_json_get_text t ~url ~json in
  match format with
  | Bibtex -> Astring.String.trim result
  | _ -> result

(* JSON helper functions for dynamic JSON manipulation *)

let json_find key json =
  match json with
  | Jsont.Object (mems, _) -> Option.map snd (J.find_mem key mems)
  | _ -> None

let json_get_string key json =
  Option.bind (json_find key json) (function
    | Jsont.String (s, _) -> Some s
    | _ -> None)

let json_update key value json =
  match json with
  | Jsont.Object (mems, meta) ->
      let get_name ((name, _), _) = name in
      let mems' =
        match value with
        | None -> List.filter (fun m -> get_name m <> key) mems
        | Some v ->
            let exists = List.exists (fun m -> get_name m = key) mems in
            if exists then
              List.map
                (fun ((name, nmeta), old_v) ->
                  if name = key then ((name, nmeta), v) else ((name, nmeta), old_v))
                mems
            else mems @ [ J.mem (J.name key) v ]
      in
      Jsont.Object (mems', meta)
  | _ -> json

(* BibTeX processing *)
let unescape_hex s =
  let buf = Buffer.create (String.length s) in
  let rec aux i =
    if i >= String.length s then Buffer.contents buf
    else if s.[i] = '\\' && i + 3 < String.length s && s.[i + 1] = 'x' then begin
      let hex = String.sub s (i + 2) 2 in
      let char_code = int_of_string ("0x" ^ hex) in
      Buffer.add_char buf (char_of_int char_code);
      aux (i + 4)
    end
    else begin
      Buffer.add_char buf s.[i];
      aux (i + 1)
    end
  in
  aux 0

let unescape_bibtex s =
  unescape_hex s |> String.split_on_char '{' |> String.concat ""
  |> String.split_on_char '}' |> String.concat ""

let fields_of_bib bib =
  match Bibtex.of_string bib with
  | Error e ->
      Log.err (fun m -> m "BibTeX parse error: %a" Bibtex.pp_error e);
      failwith "BibTeX parse error"
  | Ok [ bib ] ->
      let f =
        Bibtex.fields bib |> Bibtex.SM.bindings
        |> List.map (fun (k, v) -> (k, unescape_bibtex v))
      in
      let ty =
        match Bibtex.type' bib with "inbook" -> "book" | x -> x
      in
      let mems =
        List.fold_left
          (fun acc (k, v) -> (J.name k, J.string v) :: acc)
          [ (J.name "bibtype", J.string ty) ]
          f
      in
      J.object' mems
  | Ok _ -> failwith "Expected exactly one BibTeX entry"

let bib_of_doi t doi =
  Log.info (fun m -> m "Fetching BibTeX for DOI: %s" doi);
  let json =
    try resolve_doi t doi
    with Api_error _ ->
      Log.info (fun m -> m "%s failed on /web, trying /search" doi);
      search_id t doi
  in
  let result = export t Bibtex json in
  Log.debug (fun m -> m "BibTeX result: %s" result);
  result

let split_authors json =
  let author_str = Option.value ~default:"" (json_get_string "author" json) in
  let authors =
    Astring.String.cuts ~empty:false ~sep:" and " author_str
    |> List.map Bibtex.list_value
    |> List.map (fun v -> List.rev v |> String.concat " ")
    |> List.map J.string
  in
  let keywords =
    json_get_string "keywords" json
    |> Option.map (fun k -> Astring.String.cuts ~empty:false ~sep:", " k |> List.map J.string)
    |> Option.value ~default:[]
  in
  let json' = json_update "author" (Some (J.list authors)) json in
  match keywords with
  | [] -> json'
  | _ -> json_update "keywords" (Some (J.list keywords)) json'

let add_bibtex ~slug json =
  let add_if_present k m =
    Option.fold ~none:m ~some:(fun v -> Bibtex.SM.add k v m) (json_get_string k json)
  in
  let add_authors m =
    match json_find "author" json with
    | Some (Jsont.Array (items, _)) ->
        let authors =
          List.filter_map
            (fun item ->
              match item with Jsont.String (s, _) -> Some s | _ -> None)
            items
          |> String.concat " and "
        in
        Bibtex.SM.add "author" authors m
    | _ -> m
  in
  let cite_key = Astring.String.map (function '-' -> '_' | x -> x) slug in
  let fields = Bibtex.SM.empty in
  let type' =
    json_get_string "bibtype" json
    |> Option.map String.lowercase_ascii
    |> Option.value ~default:"misc"
  in
  let fields =
    add_authors fields |> add_if_present "title" |> add_if_present "doi"
    |> add_if_present "month" |> add_if_present "year" |> add_if_present "url"
  in
  let fields =
    match type' with
    | "article" ->
        add_if_present "journal" fields
        |> add_if_present "volume"
        |> add_if_present "number"
        |> add_if_present "pages"
    | "inproceedings" | "incollection" ->
        add_if_present "booktitle" fields
        |> add_if_present "editor"
        |> add_if_present "address"
        |> add_if_present "series"
        |> add_if_present "number"
        |> add_if_present "volume"
        |> add_if_present "organization"
        |> add_if_present "publisher"
        |> add_if_present "pages"
    | "book" ->
        add_if_present "editor" fields
        |> add_if_present "publisher"
        |> add_if_present "volume"
        |> add_if_present "pages"
    | "misc" -> add_if_present "howpublished" fields
    | "techreport" ->
        add_if_present "institution" fields
        |> add_if_present "number"
        |> add_if_present "address"
    | b ->
        Log.warn (fun m -> m "Unknown bibtype: %s" b);
        fields
  in
  let bib = Bibtex.v ~type' ~cite_key ~fields () |> Fmt.str "%a" Bibtex.pp in
  json_update "bib" (Some (J.string bib)) json

let json_of_doi t ~slug doi =
  let bib = bib_of_doi t doi in
  let json = fields_of_bib bib in
  split_authors json |> add_bibtex ~slug
