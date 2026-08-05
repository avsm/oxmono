(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Type alias for convenience *)
module Repo = Atp_lexicon_tangled.Sh.Tangled.Repo

(* Pretty printer for generated repo type *)
let pp_repo ppf (r : Repo.main) =
  Fmt.pf ppf "@[<v>Name: %s@,Knot: %s%a@,Created: %s%a@]" r.name r.knot
    Fmt.(option (fmt "@,Description: %s"))
    r.description r.created_at
    Fmt.(option (fmt "@,Spindle: %s"))
    r.spindle

let app_name = "tangled"

(* Helper to load session and create API *)
let with_api env f =
  Eio.Switch.run @@ fun sw ->
  let fs = env#fs in
  match Xrpc_auth.Session.load fs ~app_name () with
  | None ->
      Fmt.epr "Not logged in. Use 'tangled auth login' first.@.";
      exit 1
  | Some session ->
      let api = Tangled.Api.create ~sw ~env ~app_name ~pds:session.pds () in
      Tangled.Api.resume api ~session;
      f api

(* Repo list command *)

let user_arg =
  let doc = "User handle or DID to list repos for (default: logged-in user)." in
  Arg.(value & opt (some string) None & info [ "user"; "u" ] ~docv:"USER" ~doc)

let list_action ~user env =
  with_api env @@ fun api ->
  let did =
    match user with
    | Some u ->
        if String.starts_with ~prefix:"did:" u then u
        else Tangled.Api.resolve_handle api u
    | None -> Tangled.Api.get_did api
  in
  let repos = Tangled.Api.list_repos api ~did () in
  if repos = [] then Fmt.pr "No repositories found.@."
  else List.iter (fun (_rkey, repo) -> Fmt.pr "%a@.@." pp_repo repo) repos

let list_cmd =
  let doc = "List repositories." in
  let info = Cmd.info "list" ~doc ~sdocs:Manpage.s_common_options in
  let list' user = Eio_main.run @@ fun env -> list_action ~user env in
  Cmd.v info Term.(const list' $ user_arg)

(* Repo create command *)

let name_arg =
  let doc = "Repository name." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)

let knot_arg =
  let doc = "Knot server hostname." in
  Arg.(
    value
    & opt string "knot.tangled.sh"
    & info [ "knot"; "k" ] ~docv:"KNOT" ~doc)

let description_arg =
  let doc = "Repository description." in
  Arg.(
    value
    & opt (some string) None
    & info [ "description"; "d" ] ~docv:"DESC" ~doc)

let default_branch_arg =
  let doc = "Default branch name." in
  Arg.(
    value & opt (some string) None & info [ "branch"; "b" ] ~docv:"BRANCH" ~doc)

let create_action ~name ~knot ~description ~default_branch env =
  with_api env @@ fun api ->
  let rkey =
    Tangled.Api.create_repo api ~name ~knot ?description ?default_branch ()
  in
  let did = Tangled.Api.get_did api in
  Fmt.pr "Created repository: %s/%s@." did name;
  Fmt.pr "Clone URL: %s@." (Tangled.Api.git_url api ~knot ~did ~name);
  Fmt.pr "AT URI: at://%s/sh.tangled.repo/%s@." did rkey

let create_cmd =
  let doc = "Create a new repository." in
  let info = Cmd.info "create" ~doc in
  let create' name knot description default_branch =
    Eio_main.run @@ fun env ->
    create_action ~name ~knot ~description ~default_branch env
  in
  Cmd.v info
    Term.(
      const create' $ name_arg $ knot_arg $ description_arg $ default_branch_arg)

(* Repo clone command *)

let repo_arg =
  let doc = "Repository identifier (user/repo or AT URI)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"REPO" ~doc)

let dir_arg =
  let doc = "Target directory." in
  Arg.(value & pos 1 (some string) None & info [] ~docv:"DIR" ~doc)

let clone_action ~repo ~dir env =
  with_api env @@ fun api ->
  Tangled.Api.clone api ~repo ?dir ();
  let target = Option.value ~default:repo dir in
  Fmt.pr "Cloned %s to %s@." repo target

let clone_cmd =
  let doc = "Clone a repository." in
  let info = Cmd.info "clone" ~doc in
  let clone' repo dir =
    Eio_main.run @@ fun env -> clone_action ~repo ~dir env
  in
  Cmd.v info Term.(const clone' $ repo_arg $ dir_arg)

(* Repo info command *)

let info_action ~repo env =
  with_api env @@ fun api ->
  (* Parse repo identifier *)
  let did, name, knot =
    if String.starts_with ~prefix:"at://" repo then
      match Tangled.Types.parse_at_uri repo with
      | Some uri -> (
          let repos = Tangled.Api.list_repos api ~did:uri.did () in
          match List.find_opt (fun (rkey, _) -> rkey = uri.rkey) repos with
          | Some (_, (r : Repo.main)) -> (uri.did, r.name, r.knot)
          | None -> failwith ("Repository not found: " ^ repo))
      | None -> failwith ("Invalid AT URI: " ^ repo)
    else
      match String.split_on_char '/' repo with
      | [ user; name ] -> (
          let did =
            if String.starts_with ~prefix:"did:" user then user
            else Tangled.Api.resolve_handle api user
          in
          let repos = Tangled.Api.list_repos api ~did () in
          match
            List.find_opt (fun (_, (r : Repo.main)) -> r.name = name) repos
          with
          | Some (_, (r : Repo.main)) -> (did, name, r.knot)
          | None -> failwith ("Repository not found: " ^ repo))
      | _ -> failwith ("Invalid repo format: " ^ repo)
  in

  (* Get repo info from knot *)
  let info = Tangled.Api.get_repo_info api ~knot ~did ~name in
  Fmt.pr "@[<v>Repository: %s/%s@,Knot: %s@,%a@]@." did name knot
    Tangled.Types.pp_repo_info info

let info_cmd =
  let doc = "Show repository information." in
  let cmd_info = Cmd.info "info" ~doc in
  let info' repo = Eio_main.run @@ fun env -> info_action ~repo env in
  Cmd.v cmd_info Term.(const info' $ repo_arg)

(* Repo delete command *)

let delete_name_arg =
  let doc = "Repository name to delete." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)

let force_arg =
  let doc = "Skip confirmation prompt." in
  Arg.(value & flag & info [ "force"; "f" ] ~doc)

let delete_action ~name ~knot ~force env =
  with_api env @@ fun api ->
  if not force then begin
    Fmt.pr "Delete repository '%s' from knot '%s'? [y/N] @?" name knot;
    let response = read_line () in
    if not (String.lowercase_ascii response = "y" || response = "yes") then begin
      Fmt.pr "Aborted.@.";
      exit 0
    end
  end;

  Tangled.Api.delete_repo api ~name ~knot;
  Fmt.pr "Deleted repository: %s@." name

let delete_cmd =
  let doc = "Delete a repository." in
  let info = Cmd.info "delete" ~doc in
  let delete' name knot force =
    Eio_main.run @@ fun env -> delete_action ~name ~knot ~force env
  in
  Cmd.v info Term.(const delete' $ delete_name_arg $ knot_arg $ force_arg)

(* Repo command group *)

let cmd =
  let doc = "Repository commands." in
  let info = Cmd.info "repo" ~doc in
  Cmd.group info [ list_cmd; create_cmd; clone_cmd; info_cmd; delete_cmd ]
