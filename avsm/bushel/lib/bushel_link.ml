(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** External link tracking for Bushel *)

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

(** {1 Accessors} *)

let url { url; _ } = url
let date { date; _ } = date
let description { description; _ } = description
let datetime v = Bushel_types.ptime_of_date_exn (date v)

(** {1 Comparison} *)

let compare a b = Ptime.compare (datetime b) (datetime a)

(** {1 YAML Parsing} *)

let t_of_yaml = function
  | `O fields ->
    let url =
      match List.assoc_opt "url" fields with
      | Some (`String v) -> v
      | _ -> failwith "link: missing or invalid url"
    in
    let date =
      match List.assoc_opt "date" fields with
      | Some (`String v) ->
        (try
           match String.split_on_char '-' v with
           | [y; m; d] -> (int_of_string y, int_of_string m, int_of_string d)
           | _ ->
             v |> Ptime.of_rfc3339 |> Result.get_ok |> fun (a, _, _) -> Ptime.to_date a
         with _ ->
           v |> Ptime.of_rfc3339 |> Result.get_ok |> fun (a, _, _) -> Ptime.to_date a)
      | _ -> failwith "link: missing or invalid date"
    in
    let description =
      match List.assoc_opt "description" fields with
      | Some (`String v) -> v
      | _ -> ""
    in
    let karakeep =
      match List.assoc_opt "karakeep" fields with
      | Some (`O k_fields) ->
        let remote_url =
          match List.assoc_opt "remote_url" k_fields with
          | Some (`String v) -> v
          | _ -> failwith "link: invalid karakeep.remote_url"
        in
        let id =
          match List.assoc_opt "id" k_fields with
          | Some (`String v) -> v
          | _ -> failwith "link: invalid karakeep.id"
        in
        let tags =
          match List.assoc_opt "tags" k_fields with
          | Some (`A tag_list) ->
            List.filter_map (function `String t -> Some t | _ -> None) tag_list
          | _ -> []
        in
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
        let slugs =
          match List.assoc_opt "slugs" b_fields with
          | Some (`A slug_list) ->
            List.filter_map (function `String s -> Some s | _ -> None) slug_list
          | _ -> []
        in
        let tags =
          match List.assoc_opt "tags" b_fields with
          | Some (`A tag_list) ->
            List.filter_map (function `String t -> Some t | _ -> None) tag_list
          | _ -> []
        in
        Some { slugs; tags }
      | _ -> None
    in
    { url; date; description; karakeep; bushel }
  | _ -> failwith "link: invalid yaml"

(** {1 YAML Serialization} *)

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
        else ("tags", `A (List.map (fun t -> `String t) tags)) :: karakeep_obj
      in
      let karakeep_obj =
        if metadata = [] then karakeep_obj
        else ("metadata", `O (List.map (fun (k, v) -> (k, `String v)) metadata)) :: karakeep_obj
      in
      [("karakeep", `O karakeep_obj)]
    | None -> []
  in

  let bushel_fields =
    match t.bushel with
    | Some { slugs; tags } ->
      let bushel_obj = [] in
      let bushel_obj =
        if slugs = [] then bushel_obj
        else ("slugs", `A (List.map (fun s -> `String s) slugs)) :: bushel_obj
      in
      let bushel_obj =
        if tags = [] then bushel_obj
        else ("tags", `A (List.map (fun t -> `String t) tags)) :: bushel_obj
      in
      if bushel_obj = [] then [] else [("bushel", `O bushel_obj)]
    | None -> []
  in

  `O (base_fields @ karakeep_fields @ bushel_fields)

(** {1 File Operations} *)

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
  let oc = open_out path in
  output_string oc yaml_str;
  close_out oc

(** {1 Merging} *)

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
