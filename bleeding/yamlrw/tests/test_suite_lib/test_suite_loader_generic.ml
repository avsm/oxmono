(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Generic test suite loader - parameterized by file I/O operations *)

type test_case = {
  id : string;
  name : string;
  yaml : string;
  tree : string option;
  json : string option;
  fail : bool;
}
(** Test case representation *)

(** Module type for file I/O operations *)
module type FILE_IO = sig
  type ctx
  (** Context type for file operations (unit for sync, ~fs for Eio) *)

  val read_file : ctx -> string -> string option
  (** Read a file, returning None if it doesn't exist or can't be read *)

  val file_exists : ctx -> string -> bool
  (** Check if a path exists and is a regular file *)

  val is_directory : ctx -> string -> bool
  (** Check if a path exists and is a directory *)

  val read_dir : ctx -> string -> string list
  (** List directory entries *)
end

(** Create a test loader from file I/O operations *)
module Make (IO : FILE_IO) = struct
  type test_case = {
    id : string;
    name : string;
    yaml : string;
    tree : string option;
    json : string option;
    fail : bool;
  }

  let read_file_required ctx path =
    match IO.read_file ctx path with Some s -> s | None -> ""

  (** Load a single test from a directory *)
  let load_test_dir ctx base_id dir_path =
    let name_file = Filename.concat dir_path "===" in
    let yaml_file = Filename.concat dir_path "in.yaml" in
    let tree_file = Filename.concat dir_path "test.event" in
    let json_file = Filename.concat dir_path "in.json" in
    let error_file = Filename.concat dir_path "error" in

    (* Must have in.yaml to be a valid test *)
    if not (IO.file_exists ctx yaml_file) then None
    else
      let name =
        match IO.read_file ctx name_file with
        | Some s -> String.trim s
        | None -> base_id
      in
      let yaml = read_file_required ctx yaml_file in
      let tree = IO.read_file ctx tree_file in
      let json = IO.read_file ctx json_file in
      let fail = IO.file_exists ctx error_file in
      Some { id = base_id; name; yaml; tree; json; fail }

  (** Load tests from a test ID directory (may have subdirectories for variants)
  *)
  let load_test_id ctx test_suite_path test_id =
    let dir_path = Filename.concat test_suite_path test_id in
    if not (IO.is_directory ctx dir_path) then []
    else
      let entries = IO.read_dir ctx dir_path in
      (* Check if this directory has variant subdirectories (00, 01, etc.) *)
      let has_variants =
        List.exists
          (fun e ->
            let subdir = Filename.concat dir_path e in
            IO.is_directory ctx subdir
            && String.length e >= 2
            && e.[0] >= '0'
            && e.[0] <= '9')
          entries
      in

      if has_variants then
        (* Load each variant subdirectory *)
        let variants =
          entries
          |> List.filter (fun e ->
              let subdir = Filename.concat dir_path e in
              IO.is_directory ctx subdir
              && String.length e >= 2
              && e.[0] >= '0'
              && e.[0] <= '9')
          |> List.sort String.compare
        in
        List.filter_map
          (fun variant ->
            let variant_path = Filename.concat dir_path variant in
            let variant_id = Printf.sprintf "%s:%s" test_id variant in
            load_test_dir ctx variant_id variant_path)
          variants
      else
        (* Single test in this directory *)
        match load_test_dir ctx test_id dir_path with
        | Some t -> [ t ]
        | None -> []

  (** Load all tests from a test suite directory *)
  let load_directory ctx test_suite_path =
    if not (IO.is_directory ctx test_suite_path) then []
    else
      let entries = IO.read_dir ctx test_suite_path in
      let test_ids =
        entries
        |> List.filter (fun e ->
            IO.is_directory ctx (Filename.concat test_suite_path e)
            && String.length e >= 4
            && e.[0] >= '0'
            && e.[0] <= 'Z')
        |> List.sort String.compare
      in
      List.concat_map (load_test_id ctx test_suite_path) test_ids
end
