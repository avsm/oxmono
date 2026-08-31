(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type forge =
  | Github
  | Tangled

type source =
  | Forge
  | Registry of string

type release = {
  source : source;
  version : string;
  tag : string option;
  date : Ptime.date;
  name : string option;
  url : string option;
}

type t = {
  repo : string;
  forge : forge;
  project : string option;
  synced_at : Ptime.date option;
  releases : release list;
}

type ts = t list

let repo { repo; _ } = repo
let forge { forge; _ } = forge
let project { project; _ } = project
let releases { releases; _ } = releases

let forge_to_string = function Github -> "github" | Tangled -> "tangled"

let forge_of_string = function
  | "github" -> Some Github
  | "tangled" -> Some Tangled
  | _ -> None

let source_to_string = function Forge -> "forge" | Registry r -> r
let source_of_string = function "forge" -> Forge | r -> Registry r
let is_own r = r.source = Forge

let compare_release a b =
  match
    Ptime.compare
      (Bushel_types.ptime_of_date_exn b.date)
      (Bushel_types.ptime_of_date_exn a.date)
  with
  | 0 -> String.compare a.version b.version
  | c -> c

let latest t = match t.releases with [] -> None | r :: _ -> Some r

let compare a b =
  match (latest a, latest b) with
  | Some ra, Some rb -> (
    match
      Ptime.compare
        (Bushel_types.ptime_of_date_exn rb.date)
        (Bushel_types.ptime_of_date_exn ra.date)
    with
    | 0 -> String.compare a.repo b.repo
    | c -> c)
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> String.compare a.repo b.repo

(* The same shape the other yaml-backed files parse with: a lookup over the
   association list, failing loudly on a field that has to be there. *)
let string_field ?default key fields =
  match (List.assoc_opt key fields, default) with
  | Some (`String value), _ -> value
  | _, Some value -> value
  | _ -> failwith ("release: missing or invalid " ^ key)

let string_opt_field key fields =
  match List.assoc_opt key fields with
  | Some (`String "") -> None
  | Some (`String value) -> Some value
  | _ -> None

(* A version is a string, but yaml reads a bare 1.2 as a float and 1 as an
   int, so a file written by hand does not have to quote them. *)
let version_field key fields =
  match List.assoc_opt key fields with
  | Some (`String v) -> v
  | Some (`Float f) ->
    if Float.is_integer f then Printf.sprintf "%.0f" f else Printf.sprintf "%g" f
  | _ -> failwith ("release: missing or invalid " ^ key)

let date_of_value ~what value =
  match value with
  | `String s -> (
    match Bushel_types.date_of_string ~kind:"date" s with
    | Ok date -> date
    | Error _ -> (
      match Ptime.of_rfc3339 s with
      | Ok (time, _, _) -> Ptime.to_date time
      | Error _ -> failwith ("release: invalid " ^ what)))
  | _ -> failwith ("release: missing or invalid " ^ what)

let date_field key fields =
  match List.assoc_opt key fields with
  | Some v -> date_of_value ~what:key v
  | None -> failwith ("release: missing or invalid " ^ key)

let date_opt_field key fields =
  match List.assoc_opt key fields with
  | Some v -> ( try Some (date_of_value ~what:key v) with Failure _ -> None)
  | None -> None

let release_of_yaml = function
  | `O fields ->
    {
      source = source_of_string (string_field ~default:"forge" "source" fields);
      version = version_field "version" fields;
      tag = string_opt_field "tag" fields;
      date = date_field "date" fields;
      name = string_opt_field "name" fields;
      url = string_opt_field "url" fields;
    }
  | _ -> failwith "release: invalid yaml"

let of_yaml = function
  | `O fields ->
    let repo = string_field "repo" fields in
    let forge =
      let s = string_field ~default:"github" "forge" fields in
      match forge_of_string s with
      | Some f -> f
      | None -> failwith ("release: unknown forge " ^ s)
    in
    let releases =
      match List.assoc_opt "releases" fields with
      | Some (`A values) -> List.map release_of_yaml values
      | _ -> []
    in
    {
      repo;
      forge;
      project = string_opt_field "project" fields;
      synced_at = date_opt_field "synced_at" fields;
      releases = List.sort compare_release releases;
    }
  | _ -> failwith "release: invalid yaml"

let date_to_string (year, month, day) =
  Printf.sprintf "%04d-%02d-%02d" year month day

let opt_field key = function None -> [] | Some v -> [ (key, `String v) ]

let release_to_yaml r =
  `O
    ((match r.source with
     | Forge -> []
     | Registry name -> [ ("source", `String name) ])
    (* Written as a string. yamlrw quotes the ones that would otherwise read
       back as numbers, so 4.10 survives rather than becoming 4.1. *)
    @ [ ("version", `String r.version) ]
    @ opt_field "tag" r.tag
    @ [ ("date", `String (date_to_string r.date)) ]
    @ opt_field "name" r.name
    @ opt_field "url" r.url)

let to_yaml t =
  `O
    ([ ("repo", `String t.repo); ("forge", `String (forge_to_string t.forge)) ]
    @ opt_field "project" t.project
    @ (match t.synced_at with
      | None -> []
      | Some d -> [ ("synced_at", `String (date_to_string d)) ])
    @ [
        ( "releases",
          `A (List.map release_to_yaml (List.sort compare_release t.releases))
        );
      ])

(* A missing file is an empty list, but a malformed one is an error rather
   than an empty list. The sync merges onto what it loads and writes the
   result back, so swallowing a parse failure here would replace a good file
   with nothing. *)
let load_file path =
  if not (Sys.file_exists path) then []
  else
    let s = In_channel.(with_open_bin path input_all) in
    match Yamlrw.of_string s with
    | `A values -> List.map of_yaml values
    | `Null -> []
    | _ -> failwith "releases: expected a list at the top level"

let save_file path ts =
  let yaml = `A (List.map to_yaml (List.sort compare ts)) in
  let s = Yamlrw.to_string yaml in
  Out_channel.with_open_bin path (fun oc -> output_string oc s)

let merge existing incoming =
  let replaced = List.map (fun t -> (t.repo, t)) incoming in
  let kept =
    List.filter (fun t -> not (List.mem_assoc t.repo replaced)) existing
  in
  List.sort compare (kept @ incoming)
