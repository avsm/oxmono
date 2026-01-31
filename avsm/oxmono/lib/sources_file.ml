(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML parsing and writing for sources.yaml *)

let sources_filename = "sources.yaml"

let parse_source name (yaml : Yamlrw.value) : (Source.source, string) result =
  match yaml with
  | `O fields ->
    (match List.assoc_opt "git" fields with
    | Some (`O git_fields) ->
      let url = match List.assoc_opt "url" git_fields with
        | Some (`String s) -> Some s
        | _ -> None
      in
      let commit = match List.assoc_opt "commit" git_fields with
        | Some (`String s) -> Some s
        | _ -> None
      in
      (match url, commit with
      | Some url, Some commit -> Ok (Source.Git { url; commit })
      | _ -> Error (Printf.sprintf "Package %s: git source missing url or commit" name))
    | _ ->
      match List.assoc_opt "archive" fields with
      | Some (`O archive_fields) ->
        let url = match List.assoc_opt "url" archive_fields with
          | Some (`String s) -> Some s
          | _ -> None
        in
        let checksum = match List.assoc_opt "checksum" archive_fields with
          | Some (`String s) -> Some s
          | _ -> None
        in
        (match url, checksum with
        | Some url, Some checksum -> Ok (Source.Archive { url; checksum })
        | _ -> Error (Printf.sprintf "Package %s: archive source missing url or checksum" name))
      | _ -> Error (Printf.sprintf "Package %s: unknown source type" name))
  | _ -> Error (Printf.sprintf "Package %s: expected object" name)

let parse (yaml : Yamlrw.value) : (Source.t, string) result =
  match yaml with
  | `O fields ->
    (match List.assoc_opt "packages" fields with
    | Some (`O pkg_fields) ->
      let results = List.map (fun (name, v) ->
        match parse_source name v with
        | Ok source -> Ok (name, source)
        | Error e -> Error e
      ) pkg_fields in
      let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) results in
      if errors <> [] then
        Error (String.concat "\n" errors)
      else
        Ok { Source.packages = List.filter_map (function Ok p -> Some p | Error _ -> None) results }
    | Some _ -> Error "packages field must be an object"
    | None -> Ok Source.empty)
  | `Null -> Ok Source.empty
  | _ -> Error "Expected object at root"

let to_yaml (sources : Source.t) : Yamlrw.value =
  let source_to_yaml = function
    | Source.Git { url; commit } ->
      `O [("git", `O [("url", `String url); ("commit", `String commit)])]
    | Source.Archive { url; checksum } ->
      `O [("archive", `O [("url", `String url); ("checksum", `String checksum)])]
  in
  let packages = List.map (fun (name, source) ->
    (name, source_to_yaml source)
  ) sources.packages in
  `O [("packages", `O packages)]

let load ~fs path =
  let full_path = Eio.Path.(fs / path) in
  if not (Eio.Path.is_file full_path) then
    Ok Source.empty
  else
    let content = Eio.Path.load full_path in
    try
      let yaml = Yamlrw.of_string content in
      parse yaml
    with Yamlrw.Yamlrw_error e ->
      Error ("YAML parse error: " ^ Yamlrw.Error.to_string e)

let save ~fs path sources =
  let yaml = to_yaml sources in
  let content = Yamlrw.to_string yaml in
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(fs / path) content
