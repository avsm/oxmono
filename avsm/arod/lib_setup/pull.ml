(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Manifest

let exists path =
  match Unix.lstat path with _ -> true
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false

let rec mkdir path =
  if not (Sys.file_exists path) then begin
    mkdir (Filename.dirname path);
    Unix.mkdir path 0o700
  end

let write_new path contents =
  if exists path then failwith ("Refusing to overwrite " ^ path);
  mkdir (Filename.dirname path);
  let tmp, oc = Filename.open_temp_file ~perms:0o600
      ~temp_dir:(Filename.dirname path) ".arod-pull-" ".tmp" in
  Fun.protect ~finally:(fun () -> close_out_noerr oc;
    if exists tmp then Unix.unlink tmp) (fun () ->
      output_string oc contents;
      flush oc;
      Unix.fsync (Unix.descr_of_out_channel oc);
      close_out oc;
      (* Linking publishes atomically and cannot replace an existing file. *)
      Unix.link tmp path)

let sortal_path fs =
  Xdge.create ~create_dirs:false fs "sortal" |> Xdge.data_dir
  |> Eio.Path.native_exn

let repos t = [
  "data", t.bushel.data_dir, t.bushel.sync;
  "images", t.bushel.images_dir, t.bushel.images_sync;
  "sortal", t.sortal_dir, t.sortal ]

let check_repo path (cfg : Gitops.Sync.Config.t) =
  ignore (Process.capture ["git"; "check-ref-format";
    "refs/heads/" ^ cfg.branch]);
  if exists path then begin
    if not (exists (Filename.concat path ".git")) then
      failwith ("Not a Git checkout: " ^ path);
    let actual = Process.git path ["remote"; "get-url"; "origin"] in
    if actual <> cfg.remote then failwith ("Origin differs: " ^ path);
    if Process.git path ["symbolic-ref"; "--short"; "HEAD"] <> cfg.branch
    then failwith ("Branch differs: " ^ path);
    if Process.git path ["status"; "--porcelain"] <> "" then
      failwith ("Local changes need committing or stashing: " ^ path)
  end

let restore_repo ?(non_interactive = false) path (cfg : Gitops.Sync.Config.t) =
  check_repo path cfg;
  if not (exists path) then begin
    mkdir (Filename.dirname path);
    (* A failed clone must not leave a checkout that looks restored. *)
    let tmp = path ^ ".arod-clone" in
    if exists tmp then
      failwith ("Inspect/remove interrupted clone before retrying: " ^ tmp);
    Process.run ~non_interactive ["git"; "clone"; "--branch"; cfg.branch; "--";
      cfg.remote; tmp];
    if exists path then failwith ("Destination appeared during clone: " ^ path);
    Unix.rename tmp path
  end else begin
    Process.run ~non_interactive ["git"; "-C"; path; "fetch"; "origin";
      "+refs/heads/" ^ cfg.branch ^ ":refs/remotes/origin/" ^ cfg.branch];
    Process.run ["git"; "-C"; path; "merge"; "--ff-only";
      "refs/remotes/origin/" ^ cfg.branch]
  end

let check_mapping ~fs t =
  let path = sortal_path fs in
  if path <> t.sortal_dir && exists path then begin
    let target = try Unix.realpath path with _ ->
      if (Unix.lstat path).st_kind = Unix.S_LNK then
        let p = Unix.readlink path in
        if Filename.is_relative p then Filename.concat (Filename.dirname path) p
        else p
      else path in
    let expected = try Unix.realpath t.sortal_dir with _ -> t.sortal_dir in
    if target <> expected then
      failwith ("Existing Sortal data path differs: " ^ path)
  end

let map_sortal ~fs t =
  check_mapping ~fs t;
  let path = sortal_path fs in
  if path <> t.sortal_dir && not (exists path) then begin
    mkdir (Filename.dirname path);
    Unix.symlink t.sortal_dir path
  end

let configurations ?config t =
  let arod_path = Option.value ~default:(Arod.Config.config_file ()) config in
  let arod = Tomlt_bytesrw.encode_string Arod.Config.config_codec t.arod in
  let bushel =
    Tomlt_bytesrw.encode_string Bushel_config.config_codec t.bushel in
  let sortal = "[sync]\n" ^
      Tomlt_bytesrw.encode_string Gitops.Sync.Config.codec t.sortal in
  [arod_path, arod; Bushel_config.config_file (), bushel;
   Sortal_config.config_file (), sortal]

let check_configs ?config t =
  let arod_path = Option.value ~default:(Arod.Config.config_file ()) config in
  if Sys.file_exists arod_path then begin
    let a = try Arod.Config.of_file arod_path with _ ->
      failwith "Cannot decode existing arod configuration" in
    let a = { a with server = { a.server with stats_password = None } } in
    if a <> t.arod then failwith "Existing arod config differs from manifest"
  end;
  if Sys.file_exists (Bushel_config.config_file ()) then
    (match Bushel_config.load () with
     | Ok b when { b with sync = t.bushel.sync;
         images_sync = t.bushel.images_sync } = t.bushel -> ()
     | _ -> failwith "Existing Bushel config differs from manifest")

let report_scan t =
  List.iter (fun (name, path, _) ->
    Printf.printf "%s: %s\n" name (portable path);
    if exists (Filename.concat path ".git") then begin
      let count args =
        let files = Process.git path args in
        if files = "" then 0 else List.length (String.split_on_char '\n' files)
      in
      Printf.printf "  %d untracked, %d ignored files need separate review\n"
        (count ["ls-files"; "--others"; "--exclude-standard"])
        (count ["ls-files"; "--others"; "--ignored"; "--exclude-standard"])
    end) (repos t);
  List.iter (fun s ->
    Printf.printf "%s: profile %s, %s, %s\n"
      s.kind s.profile s.account s.endpoint) t.services;
  Printf.printf "PDFs: %s (%s)\n" (portable t.bushel.paper_pdfs_dir)
    (if t.pdf_source = None then "no restoration source"
     else "source configured");
  Printf.printf "Only typed public settings are exported. Local secrets, extra \
    config fields, untracked files and access logs are excluded.\n"

let plan ~fs ?config t =
  List.iter (fun (path, _) -> Printf.printf "Config: %s (%s)\n" path
      (if exists path then "preserve/check" else "create"))
    (configurations ?config t);
  List.iter (fun (name, path, cfg) ->
    Printf.printf "%s: %s %s from %s [%s]\n" name
      (if exists path then "fast-forward" else "clone")
      path cfg.Gitops.Sync.Config.remote cfg.branch) (repos t);
  Printf.printf "Sortal mapping: %s -> %s\n" (sortal_path fs) t.sortal_dir;
  List.iter (fun s -> Printf.printf "Auth: %s / %s (%s)\n"
    s.kind s.profile (if Sys.file_exists (session_path s)
      then "verify saved session" else "login needed")) t.services;
  Printf.printf "PDFs: %s\n" (Option.value
      ~default:"no source; existing PDFs will be checked" t.pdf_source);
  Printf.printf "Process: thumbs, faces, videos, srcsetter, dois; index\n";
  Printf.printf "Zotero translation endpoint: %s\n"
    t.bushel.zotero_translation_server

let pdfs_present path =
  Sys.file_exists path && Sys.is_directory path &&
  Array.exists (fun file -> Filename.check_suffix file ".pdf")
    (Sys.readdir path)

let run ~sw ~env ?config ~dry_run ~non_interactive ~skip_auth
    ~skip_process t =
  validate t;
  plan ~fs:env#fs ?config t;
  check_mapping ~fs:env#fs t;
  check_configs ?config t;
  List.iter (fun (_, path, cfg) -> check_repo path cfg) (repos t);
  if dry_run then 0 else begin
    let failures = ref [] in
    let step name f =
      Printf.printf "[%s] starting\n%!" name;
      try f (); Printf.printf "[%s] ready\n%!" name; true
      with
      | Failure message ->
          failures := name :: !failures;
          Printf.eprintf "[%s] blocked: %s\n%!" name message; false
      | _ ->
          failures := name :: !failures;
          (* Network and authentication exceptions can contain response data. *)
          Printf.eprintf "[%s] failed; inspect with the owning CLI\n%!" name;
          false
    in
    List.iter (fun (path, text) ->
      if not (exists path) then write_new path text) (configurations ?config t);
    let repo_results = List.map (fun (name, path, cfg) ->
      step ("repo:" ^ name) (fun () ->
        restore_repo ~non_interactive path cfg)) (repos t) in
    let repos_ok = List.for_all Fun.id repo_results in
    let mapping_ok = repos_ok && step "sortal-mapping" (fun () ->
      map_sortal ~fs:env#fs t) in
    if not skip_auth then List.iter (fun s ->
      ignore (step ("auth:" ^ s.kind) (fun () ->
        (* Suppress all auth exception payloads, including Failure. *)
        try Auth.ensure ~sw ~env ~non_interactive s with _ ->
          failwith "Login/verification incomplete. Check access or retry")))
      t.services;
    let pdf_ok = step "pdfs" (fun () ->
      Option.iter (fun source ->
        mkdir t.bushel.paper_pdfs_dir;
        Process.run (["rsync"; "-a"; "--ignore-existing"] @
          (if non_interactive then ["-e"; "ssh -oBatchMode=yes"] else []) @ [
          "--include=*/"; "--include=*.pdf"; "--exclude=*"; "--";
          expand source ^ "/"; t.bushel.paper_pdfs_dir ^ "/"])) t.pdf_source;
      if not (pdfs_present t.bushel.paper_pdfs_dir) then
        failwith "No PDFs. Set pdf_source in pull.toml or restore PDFs") in
    let processed = if skip_process then false else
      mapping_ok && pdf_ok && step "process" (fun () ->
        let entries = Bushel_eio.Bushel_loader.load env#fs t.bushel.data_dir in
        let steps = Bushel_sync.[Thumbs; Faces; Videos; Srcsetter; Dois] in
        let results = Bushel_sync.run ~dry_run:false ~retry_errors:true
          ~sw ~env ~data_dir:t.bushel.data_dir ~config:t.bushel
          ~steps ~entries in
        List.iter (fun r ->
          Printf.printf "  %s: %s\n"
            (Bushel_sync.string_of_step r.Bushel_sync.step)
            (if r.success then "ready" else "failed")) results;
        if List.exists (fun r -> not r.Bushel_sync.success) results then
          failwith "Processing incomplete") in
    let ready = mapping_ok && pdf_ok && (processed || skip_process) &&
      step "website" (fun () ->
        let ctx = Arod.Ctx.create ~config:t.arod env#fs in
        ignore (Arod.Ctx.author_exn ctx);
        if Arod.Ctx.all_entries ctx = [] then failwith "No content loaded";
        if Arod.Ctx.images ctx = [] then failwith "No processed image index";
        let xdg = Xdge.create env#fs "arod" in
        let search = Arod_search.create ~sw
            Eio.Path.(Xdge.cache_dir xdg / "search.db") in
        Arod_search.rebuild search ctx) in
    Printf.printf "Serving: %s. Authentication: %s.\n%!"
      (if ready then "ready" else "incomplete")
      (if skip_auth then "not checked" else
       if List.exists (String.starts_with ~prefix:"auth:") !failures
       then "incomplete" else "ready");
    if ready && !failures = [] then 0 else 1
  end
