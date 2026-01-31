(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Opam repository traversal and package lookup *)

module Log = (val Logs.src_log (Logs.Src.create "oxmono.opam_repo") : Logs.LOG)
module FP = OpamParserTypes.FullPos

type url_info = {
  src : string;
  checksum : string option;
}

let extract_string_value (v : FP.value) =
  match v.pelem with
  | FP.String s -> Some s
  | _ -> None

let extract_url_from_items (items : FP.opamfile_item list) : url_info option =
  let src = ref None in
  let checksum = ref None in
  List.iter (fun (item : FP.opamfile_item) ->
    match item.pelem with
    | FP.Variable (name, value) ->
      (match name.pelem with
      | "src" -> src := extract_string_value value
      | "checksum" ->
        (match value.pelem with
        | FP.String s -> checksum := Some s
        | FP.List { pelem = checksums; _ } ->
          (* Take the first checksum (usually sha256) *)
          (match List.filter_map extract_string_value checksums with
          | first :: _ -> checksum := Some first
          | [] -> ())
        | _ -> ())
      | _ -> ())
    | _ -> ()
  ) items;
  match !src with
  | Some s -> Some { src = s; checksum = !checksum }
  | None -> None

let extract_url (opamfile : FP.opamfile) : url_info option =
  List.find_map (fun (item : FP.opamfile_item) ->
    match item.pelem with
    | FP.Section { section_kind; section_items; _ } ->
      if section_kind.pelem = "url" then
        extract_url_from_items section_items.pelem
      else
        None
    | _ -> None
  ) opamfile.file_contents

let parse_opam_file content filename =
  try
    Ok (OpamParser.FullPos.string content filename)
  with e ->
    Error (Printf.sprintf "Failed to parse %s: %s" filename (Printexc.to_string e))

(** Compare version strings. Returns negative if v1 < v2, 0 if equal, positive if v1 > v2 *)
let compare_versions v1 v2 =
  (* Simple version comparison - split by dots and compare numerically *)
  let split_version v =
    String.split_on_char '.' v
    |> List.map (fun s ->
      try int_of_string (String.trim s)
      with _ -> 0)
  in
  let rec compare_lists l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x1 :: xs1, x2 :: xs2 ->
      let c = compare x1 x2 in
      if c <> 0 then c else compare_lists xs1 xs2
  in
  compare_lists (split_version v1) (split_version v2)

let list_versions ~fs repo_path package_name =
  let packages_dir = Eio.Path.(fs / repo_path / "packages" / package_name) in
  if not (Eio.Path.is_directory packages_dir) then
    []
  else
    Eio.Path.read_dir packages_dir
    |> List.filter_map (fun entry ->
      (* Entry format: package_name.version *)
      let prefix = package_name ^ "." in
      if String.starts_with ~prefix entry then
        Some (String.sub entry (String.length prefix) (String.length entry - String.length prefix))
      else
        None)
    |> List.sort (fun v1 v2 -> compare_versions v2 v1)  (* Sort descending *)

let find_latest_version ~fs repo_path package_name =
  match list_versions ~fs repo_path package_name with
  | [] -> None
  | latest :: _ -> Some latest

let read_opam_file ~fs repo_path package_name version =
  let dir_name = package_name ^ "." ^ version in
  let opam_path = Eio.Path.(fs / repo_path / "packages" / package_name / dir_name / "opam") in
  if not (Eio.Path.is_file opam_path) then
    Error (Printf.sprintf "opam file not found: packages/%s/%s/opam" package_name dir_name)
  else
    let content = Eio.Path.load opam_path in
    parse_opam_file content (Eio.Path.native_exn opam_path)

let lookup_package ~fs repo_paths package_name =
  let rec try_repos = function
    | [] -> Error (Printf.sprintf "Package %s not found in any repository" package_name)
    | repo_path :: rest ->
      Log.debug (fun m -> m "Looking for %s in %s" package_name repo_path);
      match find_latest_version ~fs repo_path package_name with
      | None -> try_repos rest
      | Some version ->
        Log.info (fun m -> m "Found %s.%s in %s" package_name version repo_path);
        match read_opam_file ~fs repo_path package_name version with
        | Error e -> Error e
        | Ok opamfile ->
          match extract_url opamfile with
          | None -> Error (Printf.sprintf "No url section in %s.%s" package_name version)
          | Some url_info ->
            let source =
              if Source.is_git_url url_info.src then
                Source.git ~url:url_info.src ~commit:(Option.value ~default:"HEAD" url_info.checksum)
              else
                let checksum = Option.value ~default:"" url_info.checksum in
                Source.archive ~url:url_info.src ~checksum
            in
            Ok (version, source)
  in
  try_repos repo_paths
