(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Archive fetch, extract, and checksum verification *)

module Log = (val Logs.src_log (Logs.Src.create "oxmono.archive") : Logs.LOG)

let download ~env ~url ~target =
  let target_path = Eio.Path.native_exn target in
  Log.info (fun m -> m "Downloading %s to %s" url target_path);
  Process.run ~env ["curl"; "-fsSL"; "-o"; target_path; url]

let verify_checksum ~env ~file ~expected =
  let file_path = Eio.Path.native_exn file in
  (* Parse checksum format: "sha256=abc123..." or just "abc123..." *)
  let algo, expected_hash =
    match String.split_on_char '=' expected with
    | [algo; hash] -> (algo, hash)
    | _ -> ("sha256", expected)
  in
  Log.debug (fun m -> m "Verifying %s checksum of %s" algo file_path);
  let cmd = match algo with
    | "sha256" -> ["shasum"; "-a"; "256"; file_path]
    | "sha512" -> ["shasum"; "-a"; "512"; file_path]
    | "md5" -> ["md5"; "-q"; file_path]
    | _ ->
      Log.warn (fun m -> m "Unknown checksum algorithm: %s, skipping verification" algo);
      []
  in
  if cmd = [] then Ok ()
  else
    match Process.run_with_output ~env cmd with
    | Error e -> Error e
    | Ok output ->
      let computed = String.split_on_char ' ' (String.trim output) |> List.hd in
      if computed = expected_hash then begin
        Log.debug (fun m -> m "Checksum verified");
        Ok ()
      end else begin
        Log.err (fun m -> m "Checksum mismatch: expected %s, got %s" expected_hash computed);
        Error (`Exit_code 1)
      end

let extract ~env ~archive ~target =
  let archive_path = Eio.Path.native_exn archive in
  let target_path = Eio.Path.native_exn target in
  Log.info (fun m -> m "Extracting %s to %s" archive_path target_path);
  (* Determine archive type from extension *)
  let ext = Filename.extension archive_path in
  let cmd = match ext with
    | ".tbz" | ".bz2" ->
      ["tar"; "-xjf"; archive_path; "-C"; target_path; "--strip-components=1"]
    | ".tgz" | ".gz" ->
      ["tar"; "-xzf"; archive_path; "-C"; target_path; "--strip-components=1"]
    | ".zip" ->
      ["unzip"; "-q"; "-d"; target_path; archive_path]
    | ".tar" ->
      ["tar"; "-xf"; archive_path; "-C"; target_path; "--strip-components=1"]
    | _ ->
      Log.warn (fun m -> m "Unknown archive type: %s, trying tar" ext);
      ["tar"; "-xf"; archive_path; "-C"; target_path; "--strip-components=1"]
  in
  Process.run ~env cmd

let fetch_and_extract ~env ~url ~checksum ~target =
  let temp_dir = Filename.get_temp_dir_name () in
  let archive_name = Filename.basename url in
  let archive_path = Eio.Path.(Eio.Stdenv.fs env / temp_dir / archive_name) in
  (* Download *)
  match download ~env ~url ~target:archive_path with
  | Error e -> Error e
  | Ok () ->
    (* Verify checksum if provided *)
    let checksum_ok =
      if checksum = "" then Ok ()
      else verify_checksum ~env ~file:archive_path ~expected:checksum
    in
    match checksum_ok with
    | Error e -> Error e
    | Ok () ->
      (* Create target directory if needed *)
      if not (Eio.Path.is_directory target) then begin
        let target_path = Eio.Path.native_exn target in
        match Process.run ~env ["mkdir"; "-p"; target_path] with
        | Error e -> Error e
        | Ok () -> extract ~env ~archive:archive_path ~target
      end else
        extract ~env ~archive:archive_path ~target
