(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type forge =
  | Github
  | Tangled

type registry = {
  name : string;
  package : string;
  url : string;
}

type release = {
  version : string;
  tag : string option;
  date : Ptime.date;
  summary : string;
  url : string;
  registries : registry list;
}

type t = {
  repo : string;
  forge : forge;
  project : string option;
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

(* Unreserved characters stay and the rest are percent-encoded, so a scoped
   npm name is a single path segment. *)
let encode_segment s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' ->
        Buffer.add_char b c
      | c -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let metadata_url reg r =
  Printf.sprintf
    "https://packages.ecosyste.ms/registries/%s/packages/%s/versions/%s"
    (encode_segment reg.name) (encode_segment reg.package)
    (encode_segment r.version)

let add_registries r regs =
  let fresh =
    List.filter
      (fun n -> not (List.exists (fun h -> h.name = n.name) r.registries))
      regs
  in
  { r with registries = r.registries @ fresh }

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
    if Float.is_integer f then Printf.sprintf "%.0f" f
    else Printf.sprintf "%g" f
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

let registry_of_yaml = function
  | `O fields ->
    {
      name = string_field "name" fields;
      package = string_field "package" fields;
      url = string_field "url" fields;
    }
  | _ -> failwith "release: invalid registry"

let release_of_yaml = function
  | `O fields ->
    {
      version = version_field "version" fields;
      tag = string_opt_field "tag" fields;
      date = date_field "date" fields;
      summary = string_field "summary" fields;
      url = string_field "url" fields;
      registries =
        (match List.assoc_opt "registries" fields with
        | Some (`A values) -> List.map registry_of_yaml values
        | _ -> []);
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
      releases = List.sort compare_release releases;
    }
  | _ -> failwith "release: invalid yaml"

let date_to_string (year, month, day) =
  Printf.sprintf "%04d-%02d-%02d" year month day

let opt_field key = function None -> [] | Some v -> [ (key, `String v) ]

let registry_to_yaml r =
  `O
    [
      ("name", `String r.name);
      ("package", `String r.package);
      ("url", `String r.url);
    ]

let release_to_yaml r =
  `O
    (* Written as a string. yamlrw quotes the ones that would otherwise read
       back as numbers, so 4.10 survives rather than becoming 4.1. *)
    ([ ("version", `String r.version) ]
    @ opt_field "tag" r.tag
    @ [
        ("date", `String (date_to_string r.date));
        ("summary", `String r.summary);
        ("url", `String r.url);
      ]
    @
    match r.registries with
    | [] -> []
    | l -> [ ("registries", `A (List.map registry_to_yaml l)) ])

let to_yaml t =
  `O
    ([ ("repo", `String t.repo); ("forge", `String (forge_to_string t.forge)) ]
    @ opt_field "project" t.project
    @ [
        ( "releases",
          `A (List.map release_to_yaml (List.sort compare_release t.releases))
        );
      ])

(* A missing file is an empty list, but a malformed one is an error rather
   than an empty list. The commands merge onto what they load and write the
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
    | exception _ -> failwith "releases: not valid yaml"

let save_file path ts =
  let yaml = `A (List.map to_yaml (List.sort compare ts)) in
  let s = Yamlrw.to_string yaml in
  Out_channel.with_open_bin path (fun oc -> output_string oc s)

let union_releases existing incoming =
  let kept =
    List.filter
      (fun e -> not (List.exists (fun i -> i.version = e.version) incoming))
      existing
  in
  List.sort compare_release (kept @ incoming)

let merge existing incoming =
  let combine (t : t) =
    match List.find_opt (fun e -> e.repo = t.repo) existing with
    | None -> { t with releases = List.sort compare_release t.releases }
    | Some e ->
      {
        t with
        project = (match t.project with None -> e.project | p -> p);
        releases = union_releases e.releases t.releases;
      }
  in
  let incoming' = List.map combine incoming in
  let kept =
    List.filter
      (fun e -> not (List.exists (fun t -> t.repo = e.repo) incoming))
      existing
  in
  List.sort compare (kept @ incoming')
