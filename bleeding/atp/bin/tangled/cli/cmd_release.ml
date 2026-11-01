(* SPDX-License-Identifier: ISC *)
(* Release artifacts: files attached to a tag's commit. *)
open Cmdliner
module Api = Tangled.Api
module Artifact = Api.Lex.Repo.Artifact
module Tags = Api.Lex.Git.ListTags

let tag = Common.positional 0 "TAG" "Tag, or a full commit ID."

let all_tags api r =
  let rec pages cursor acc =
    let out = Api.tags api r ~limit:100 ?cursor () in
    let acc = acc @ List.map (Api.decode Tags.tag_info_jsont) out.tags in
    match out.cursor with
    | Some c when c <> "" && Some c <> cursor -> pages (Some c) acc
    | _ -> acc
  in
  pages None []

let is_oid s =
  (String.length s = 40 || String.length s = 64)
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) s

(* [oid_of_tag api r tag] is the commit a release for [tag] attaches to. *)
let oid_of_tag api r tag =
  if is_oid tag then tag
  else
    match
      List.find_opt (fun (t : Tags.tag_info) -> t.name = tag) (all_tags api r)
    with
    | Some t -> t.oid
    | None ->
        failwith ("No tag " ^ tag ^ ". Push it first: git push origin " ^ tag)

let oid (a : Artifact.main) = Api.oid_of a.subject

let size n =
  let n = Int64.to_float n in
  if n < 1024. then Printf.sprintf "%.0f B" n
  else if n < 1048576. then Printf.sprintf "%.1f KiB" (n /. 1024.)
  else Printf.sprintf "%.1f MiB" (n /. 1048576.)

let list_cmd =
  let tag_opt = Common.optional [ "tag" ] "TAG" "Only the artifacts of TAG." in
  let action repo knot only json =
    Common.run (fun env ->
        let artifacts, names =
          Common.with_api ~busy:"Reading releases" env (fun api ->
              let r = Common.resolve api ?knot repo in
              let artifacts = Api.list_artifacts api r in
              let tags = all_tags api r in
              let name_of oid =
                match
                  List.find_opt (fun (t : Tags.tag_info) -> t.oid = oid) tags
                with
                | Some t -> t.name
                | None -> Ui.short_sha oid
              in
              let only = Option.map (oid_of_tag api r) only in
              ( List.filter
                  (fun (_, a) ->
                    match only with None -> true | Some o -> oid a = o)
                  artifacts,
                name_of ))
        in
        if json then
          Common.json
            (Jsont.Json.list
               (List.map
                  (fun (rkey, a) ->
                    Common.object_
                      [
                        ("id", Common.string rkey);
                        ("tag", Common.string (names (oid a)));
                        ("record", Api.encode Artifact.main_jsont a);
                      ])
                  artifacts))
        else if Ui.tty () then
          Ui.table
            ~heading:(Ui.heading "Artifacts" (List.length artifacts))
            [
              Ui.column "Tag";
              Ui.column "Name";
              Ui.column ~align:`Right "Size";
              Ui.column "Uploaded";
            ]
            (List.map
               (fun (_, (a : Artifact.main)) ->
                 [
                   Ui.accent (names (oid a));
                   Ui.bold a.name;
                   Ui.muted (size a.artifact.size);
                   Ui.muted a.created_at;
                 ])
               artifacts)
        else
          List.iter
            (fun (_, (a : Artifact.main)) ->
              Fmt.pr "%s\t%s\t%Ld@." (names (oid a)) a.name a.artifact.size)
            artifacts)
  in
  Common.cmd
    (Cmd.info "list" ~doc:"List release artifacts by tag.")
    Term.(
      term_result
        (const action $ Common.repo $ Common.knot $ tag_opt $ Common.json_flag))

let content_type name =
  let ends suffix = Filename.check_suffix name suffix in
  if ends ".tar.gz" || ends ".tgz" then "application/gzip"
  else if ends ".tbz" || ends ".tar.bz2" then "application/x-bzip2"
  else if ends ".tar.xz" || ends ".txz" then "application/x-xz"
  else if ends ".zip" then "application/zip"
  else if ends ".tar" then "application/x-tar"
  else if ends ".txt" || ends ".md" then "text/plain"
  else if ends ".json" then "application/json"
  else "application/octet-stream"

let upload_cmd =
  let files =
    Arg.(
      non_empty & pos_right 0 file []
      & info [] ~docv:"FILE" ~doc:"Files to attach.")
  in
  let action repo knot tag files =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let r, oid =
              Ui.busy ("Finding " ^ tag) (fun () ->
                  let r = Common.resolve api ?knot repo in
                  (r, oid_of_tag api r tag))
            in
            List.iter
              (fun path ->
                let name = Filename.basename path in
                let data = In_channel.with_open_bin path In_channel.input_all in
                Ui.busy ("Uploading " ^ name) (fun () ->
                    ignore
                      (Api.upload_artifact api r ~oid ~name
                         ~content_type:(content_type name) data));
                Ui.ok "Attached %s to %s" name tag)
              files))
  in
  Common.cmd
    (Cmd.info "upload" ~doc:"Attach files to a tag, each at most 50 MiB.")
    Term.(term_result (const action $ Common.repo $ Common.knot $ tag $ files))

let select api r tag name =
  let o = oid_of_tag api r tag in
  let matches =
    List.filter
      (fun (_, (a : Artifact.main)) ->
        oid a = o && match name with None -> true | Some n -> a.name = n)
      (Api.list_artifacts api r)
  in
  if matches = [] then
    failwith
      (match name with
      | Some n -> Printf.sprintf "%s has no artifact %s" tag n
      | None -> tag ^ " has no artifacts")
  else matches

let download_cmd =
  let name =
    Arg.(
      value
      & pos 1 (some string) None
      & info [] ~docv:"NAME"
          ~doc:"Artifact to download (default: all of them).")
  in
  let dir =
    Arg.(
      value & opt string "."
      & info [ "dir"; "D" ] ~docv:"DIR" ~doc:"Directory to write into.")
  in
  let action repo knot tag name dir =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let r, artifacts =
              Ui.busy ("Finding " ^ tag) (fun () ->
                  let r = Common.resolve api ?knot repo in
                  (r, select api r tag name))
            in
            List.iter
              (fun (_, (a : Artifact.main)) ->
                if Filename.basename a.name <> a.name || a.name = ".." then
                  failwith ("Refusing an artifact named " ^ a.name);
                let path = Filename.concat dir a.name in
                let data =
                  Ui.busy ("Downloading " ^ a.name) (fun () ->
                      Api.download_artifact api r a)
                in
                Out_channel.with_open_bin path (fun oc ->
                    Out_channel.output_string oc data);
                Ui.ok "Wrote %s" path)
              artifacts))
  in
  Common.cmd
    (Cmd.info "download" ~doc:"Download a tag's artifacts.")
    Term.(
      term_result (const action $ Common.repo $ Common.knot $ tag $ name $ dir))

let delete_cmd =
  let name = Common.positional 1 "NAME" "Artifact to delete." in
  let action repo knot tag name =
    Common.run (fun env ->
        Common.with_api ~busy:("Deleting " ^ name) env (fun api ->
            let r = Common.resolve api ?knot repo in
            List.iter
              (fun (rkey, _) -> Api.delete_artifact api r ~rkey)
              (select api r tag (Some name)));
        Ui.ok "Deleted %s from %s" name tag)
  in
  Common.cmd
    (Cmd.info "delete" ~doc:"Delete an artifact from a tag.")
    Term.(term_result (const action $ Common.repo $ Common.knot $ tag $ name))

let cmd =
  Cmd.group
    (Cmd.info "release" ~doc:"Attach files to tags, and fetch them.")
    [ list_cmd; upload_cmd; download_cmd; delete_cmd ]
