(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type karakeep_data = {
  remote_url : string;
  id : string;
  tags : string list;
  metadata : (string * string) list;
}

type bushel_data = {
  slugs : string list;
  tags : string list;
}

type t = {
  url : string;
  date : Ptime.date;
  description : string;
  karakeep : karakeep_data option;
  bushel : bushel_data option;
}

type ts = t list

let url { url; _ } = url
let date { date; _ } = date
let description { description; _ } = description
let datetime v = Bushel_types.ptime_of_date_exn (date v)

let compare a b = Ptime.compare (datetime b) (datetime a)

let string_field ?default key fields =
  match List.assoc_opt key fields, default with
  | Some (`String value), _ -> value
  | _, Some value -> value
  | _ -> failwith ("link: missing or invalid " ^ key)

let strings_field key fields =
  match List.assoc_opt key fields with
  | Some (`A values) ->
    List.filter_map (function `String value -> Some value | _ -> None) values
  | _ -> []

let date_field fields =
  let value = string_field "date" fields in
  match Bushel_types.date_of_string ~kind:"date" value with
  | Ok date -> date
  | Error _ -> (
    match Ptime.of_rfc3339 value with
    | Ok (time, _, _) -> Ptime.to_date time
    | Error _ -> failwith "link: missing or invalid date")

let t_of_yaml = function
  | `O fields ->
    let url = string_field "url" fields in
    let date = date_field fields in
    let description = string_field ~default:"" "description" fields in
    let karakeep =
      match List.assoc_opt "karakeep" fields with
      | Some (`O k_fields) ->
        let remote_url = string_field "remote_url" k_fields in
        let id = string_field "id" k_fields in
        let tags = strings_field "tags" k_fields in
        let metadata =
          match List.assoc_opt "metadata" k_fields with
          | Some (`O meta_fields) ->
            List.filter_map (fun (k, v) ->
              match v with `String value -> Some (k, value) | _ -> None
            ) meta_fields
          | _ -> []
        in
        Some { remote_url; id; tags; metadata }
      | _ -> None
    in
    let bushel =
      match List.assoc_opt "bushel" fields with
      | Some (`O b_fields) ->
        let slugs = strings_field "slugs" b_fields in
        let tags = strings_field "tags" b_fields in
        Some { slugs; tags }
      | _ -> None
    in
    { url; date; description; karakeep; bushel }
  | _ -> failwith "link: invalid yaml"

let to_yaml t =
  let (year, month, day) = t.date in
  let date_str = Printf.sprintf "%04d-%02d-%02d" year month day in

  let base_fields = [
    ("url", `String t.url);
    ("date", `String date_str);
  ] @
  (if t.description = "" then [] else [("description", `String t.description)])
  in

  let karakeep_fields =
    match t.karakeep with
    | Some { remote_url; id; tags; metadata } ->
      let karakeep_obj = [
        ("remote_url", `String remote_url);
        ("id", `String id);
      ] in
      let karakeep_obj =
        if tags = [] then karakeep_obj
        else karakeep_obj @ [("tags", `A (List.map (fun t -> `String t) tags))]
      in
      let karakeep_obj =
        if metadata = [] then karakeep_obj
        else karakeep_obj @ [("metadata", `O (List.map (fun (k, v) -> (k, `String v)) metadata))]
      in
      [("karakeep", `O karakeep_obj)]
    | None -> []
  in

  let bushel_fields =
    match t.bushel with
    | Some { slugs; tags } ->
      let bushel_obj =
        (if slugs = [] then []
         else [("slugs", `A (List.map (fun s -> `String s) slugs))])
        @
        (if tags = [] then []
         else [("tags", `A (List.map (fun t -> `String t) tags))])
      in
      if bushel_obj = [] then [] else [("bushel", `O bushel_obj)]
    | None -> []
  in

  `O (base_fields @ karakeep_fields @ bushel_fields)

let load_links_file path =
  try
    let yaml_str = In_channel.(with_open_bin path input_all) in
    match Yamlrw.of_string yaml_str with
    | `A links -> List.map t_of_yaml links
    | _ -> []
  with _ -> []

let save_links_file path links =
  let yaml = `A (List.map to_yaml links) in
  let yaml_str = Yamlrw.to_string yaml in
  Out_channel.with_open_bin path (fun oc -> output_string oc yaml_str)

let academic_patterns = [
  ("arxiv.org", ["/abs/"; "/pdf/"]);
  ("dl.acm.org", ["/doi/10."]);
  ("linkinghub.elsevier.com", []);
  ("sciencedirect.com", ["/science/article"]);
  ("ieeexplore.ieee.org", []);
  ("academic.oup.com", []);
  ("nature.com", ["/articles/"]);
  ("journals.sagepub.com", []);
  ("garfield.library.upenn.edu", []);
  ("link.springer.com", []);
  ("tandfonline.com", ["/doi/"]);
  ("cambridge.org", ["/core/journals/"]);
  ("science.org", ["/doi/"]);
  ("royalsocietypublishing.org", []);
  ("pnas.org", ["/doi/"]);
  ("onlinelibrary.wiley.com", ["/doi/"]);
  ("zenodo.org", ["/record"; "/records"]);
  ("frontiersin.org", ["/articles/"]);
  ("biorxiv.org", ["/content/"]);
  ("medrxiv.org", ["/content/"]);
  ("journals.plos.org", ["/plosone/article"]);
  ("cell.com", []);
  ("elifesciences.org", ["/articles/"]);
  ("peerj.com", ["/articles/"]);
  ("mdpi.com", []);
]

let is_infix ~affix s =
  let n = String.length affix and m = String.length s in
  let rec matches i j = j = n || (s.[i + j] = affix.[j] && matches i (j + 1)) in
  let rec scan i = i + n <= m && (matches i 0 || scan (i + 1)) in
  scan 0

let is_academic_url url =
  match Uriz.of_string url with
  | Null -> false
  | This uri -> (
    match Uriz.host uri with
    | Null -> false
    | This host ->
      let host =
        if String.starts_with ~prefix:"www." host
        then String.sub host 4 (String.length host - 4)
        else host
      in
      let path = Uriz.path uri in
      List.exists (fun (domain, prefixes) ->
        let domain_match =
          String.equal host domain
          || String.ends_with ~suffix:("." ^ domain) host
        in
        domain_match && (
          prefixes = [] ||
          List.exists (fun prefix -> String.starts_with ~prefix path) prefixes
        )
      ) academic_patterns)

let is_doi_url url = is_infix ~affix:"doi.org/" url

let is_paper_url url =
  is_doi_url url || is_academic_url url

let merge_links ?(prefer_new_date=false) existing new_links =
  let links_by_url = Hashtbl.create (List.length existing) in

  List.iter (fun link -> Hashtbl.replace links_by_url link.url link) existing;

  List.iter (fun new_link ->
    match Hashtbl.find_opt links_by_url new_link.url with
    | None ->
      Hashtbl.add links_by_url new_link.url new_link
    | Some old_link ->
      let description =
        if new_link.description <> "" then new_link.description
        else old_link.description
      in
      let karakeep =
        match new_link.karakeep, old_link.karakeep with
        | Some new_k, Some old_k when new_k.remote_url = old_k.remote_url ->
          let merged_metadata =
            let meta_tbl = Hashtbl.create (List.length old_k.metadata) in
            List.iter (fun (k, v) -> Hashtbl.replace meta_tbl k v) old_k.metadata;
            List.iter (fun (k, v) -> Hashtbl.replace meta_tbl k v) new_k.metadata;
            Hashtbl.fold (fun k v acc -> (k, v) :: acc) meta_tbl []
            |> List.sort (fun (a, _) (b, _) -> String.compare a b)
          in
          let merged_tags = List.sort_uniq String.compare (old_k.tags @ new_k.tags) in
          Some { new_k with metadata = merged_metadata; tags = merged_tags }
        | Some new_k, _ -> Some new_k
        | None, old_k -> old_k
      in
      let bushel =
        match new_link.bushel, old_link.bushel with
        | Some new_b, Some old_b ->
          let merged_slugs = List.sort_uniq String.compare (old_b.slugs @ new_b.slugs) in
          let merged_tags = List.sort_uniq String.compare (old_b.tags @ new_b.tags) in
          Some { slugs = merged_slugs; tags = merged_tags }
        | Some new_b, _ -> Some new_b
        | None, old_b -> old_b
      in
      let date =
        if prefer_new_date then new_link.date
        else if compare new_link old_link > 0 then new_link.date
        else old_link.date
      in
      let merged_link = { url = new_link.url; date; description; karakeep; bushel } in
      Hashtbl.replace links_by_url new_link.url merged_link
  ) new_links;

  Hashtbl.to_seq_values links_by_url
  |> List.of_seq
  |> List.sort compare
