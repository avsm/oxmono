(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

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

(* Type aliases for convenience *)
module Pipeline = Atp_lexicon_tangled.Sh.Tangled.Pipeline

(* Pretty printers *)

let pp_trigger_kind ppf (tm : Pipeline.trigger_metadata) =
  match tm.kind with
  | "push" -> (
      match tm.push with
      | Some p -> Fmt.pf ppf "push (%s)" p.ref_
      | None -> Fmt.pf ppf "push")
  | "pull_request" -> (
      match tm.pull_request with
      | Some pr -> Fmt.pf ppf "PR (%s → %s)" pr.source_branch pr.target_branch
      | None -> Fmt.pf ppf "pull_request")
  | "manual" -> Fmt.pf ppf "manual"
  | k -> Fmt.pf ppf "%s" k

let pp_status ppf status =
  let style =
    match status with
    | "success" -> `Green
    | "failed" -> `Red
    | "running" -> `Yellow
    | "pending" -> `Blue
    | "cancelled" | "timeout" -> `Magenta
    | _ -> `None
  in
  Fmt.pf ppf "%a" Fmt.(styled style string) status

let pp_pipeline_summary ppf (rkey, (p : Pipeline.main)) =
  Fmt.pf ppf "@[<v>%s@,  repo: %s/%s@,  trigger: %a@,  workflows: %d@]" rkey
    p.trigger_metadata.repo.did p.trigger_metadata.repo.repo pp_trigger_kind
    p.trigger_metadata (List.length p.workflows)

let pp_pipeline_detail ppf (rkey, (p : Pipeline.main)) =
  let tm = p.trigger_metadata in
  Fmt.pf ppf "@[<v>Pipeline: %s@,@," rkey;
  Fmt.pf ppf "Trigger: %a@," pp_trigger_kind tm;
  Fmt.pf ppf "Repository: %s/%s@," tm.repo.did tm.repo.repo;
  Fmt.pf ppf "Knot: %s@," tm.repo.knot;
  Fmt.pf ppf "Default Branch: %s@,@," tm.repo.default_branch;
  (* Trigger-specific details *)
  (match tm.push with
  | Some push ->
      Fmt.pf ppf "Push Details:@,";
      Fmt.pf ppf "  ref: %s@," push.ref_;
      Fmt.pf ppf "  new: %s@," push.new_sha;
      Fmt.pf ppf "  old: %s@,@," push.old_sha
  | None -> ());
  (match tm.pull_request with
  | Some pr ->
      Fmt.pf ppf "Pull Request Details:@,";
      Fmt.pf ppf "  source: %s @ %s@," pr.source_branch pr.source_sha;
      Fmt.pf ppf "  target: %s@," pr.target_branch;
      Fmt.pf ppf "  action: %s@,@," pr.action
  | None -> ());
  (match tm.manual with
  | Some m ->
      Fmt.pf ppf "Manual Trigger:@,";
      (match m.inputs with
      | Some inputs ->
          List.iter
            (fun (inp : Pipeline.pair) ->
              Fmt.pf ppf "  %s: %s@," inp.key inp.value)
            inputs
      | None -> Fmt.pf ppf "  (no inputs)@,");
      Fmt.pf ppf "@,"
  | None -> ());
  (* Workflows *)
  Fmt.pf ppf "Workflows:@,";
  List.iter
    (fun (w : Pipeline.workflow) ->
      Fmt.pf ppf "  - %s (engine: %s)@," w.name w.engine)
    p.workflows;
  Fmt.pf ppf "@]"

let pp_status_entry ppf (s : Pipeline.Status.main) =
  Fmt.pf ppf "@[<h>%s: %a%a@]" s.workflow pp_status s.status
    (Fmt.option (fun ppf e -> Fmt.pf ppf " - %s" e))
    s.error

(* Pipeline list command *)

let user_arg =
  let doc =
    "User handle or DID to list pipelines for (default: logged-in user)."
  in
  Arg.(value & opt (some string) None & info [ "user"; "u" ] ~docv:"USER" ~doc)

let repo_arg =
  let doc = "Filter by repository name (required to look up spindle)." in
  Arg.(
    required & opt (some string) None & info [ "repo"; "r" ] ~docv:"REPO" ~doc)

let spindle_arg =
  let doc = "Spindle hostname (default: look up from repo record)." in
  Arg.(
    value
    & opt (some string) None
    & info [ "spindle"; "s" ] ~docv:"SPINDLE" ~doc)

let limit_arg =
  let doc = "Maximum number of pipelines to show." in
  Arg.(value & opt int 10 & info [ "limit"; "n" ] ~docv:"N" ~doc)

let list_action ~user ~repo ~spindle ~limit env =
  with_api env @@ fun api ->
  let did =
    match user with
    | Some u ->
        if String.starts_with ~prefix:"did:" u then u
        else Tangled.Api.resolve_handle api u
    | None -> Tangled.Api.get_did api
  in
  (* Get spindle from argument or look up from repo *)
  let spindle =
    match spindle with
    | Some s -> s
    | None -> (
        match Tangled.Api.get_spindle_for_repo api ~did ~repo_name:repo with
        | Some (s, _) -> s
        | None ->
            Fmt.epr "Repository %s has no spindle configured for CI.@." repo;
            exit 1)
  in
  let pipelines =
    Tangled.Api.list_pipelines_for_repo api ~spindle ~did ~repo_name:repo ()
  in
  let pipelines =
    if List.length pipelines > limit then
      List.filteri (fun i _ -> i < limit) pipelines
    else pipelines
  in
  if pipelines = [] then
    Fmt.pr "No pipelines found for %s (spindle: %s).@." repo spindle
  else begin
    Fmt.pr "@[<v>";
    List.iter (fun p -> Fmt.pr "%a@,@," pp_pipeline_summary p) pipelines;
    Fmt.pr "@]"
  end

let list_cmd =
  let doc = "List CI pipelines for a repository." in
  let info = Cmd.info "list" ~doc ~sdocs:Manpage.s_common_options in
  let list' user repo spindle limit =
    Eio_main.run @@ fun env -> list_action ~user ~repo ~spindle ~limit env
  in
  Cmd.v info Term.(const list' $ user_arg $ repo_arg $ spindle_arg $ limit_arg)

(* Pipeline show command *)

let rkey_arg =
  let doc = "Pipeline rkey (record key)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"RKEY" ~doc)

let show_action ~rkey ~user ~repo ~spindle env =
  with_api env @@ fun api ->
  let did =
    match user with
    | Some u ->
        if String.starts_with ~prefix:"did:" u then u
        else Tangled.Api.resolve_handle api u
    | None -> Tangled.Api.get_did api
  in
  let spindle =
    match spindle with
    | Some s -> s
    | None -> (
        match Tangled.Api.get_spindle_for_repo api ~did ~repo_name:repo with
        | Some (s, _) -> s
        | None ->
            Fmt.epr "Repository %s has no spindle configured for CI.@." repo;
            exit 1)
  in
  let pipelines =
    Tangled.Api.list_pipelines_for_repo api ~spindle ~did ~repo_name:repo ()
  in
  match List.find_opt (fun (k, _) -> k = rkey) pipelines with
  | Some p -> Fmt.pr "%a@." pp_pipeline_detail p
  | None ->
      Fmt.epr "Pipeline not found: %s@." rkey;
      exit 1

let show_cmd =
  let doc = "Show pipeline details." in
  let info = Cmd.info "show" ~doc in
  let show' rkey user repo spindle =
    Eio_main.run @@ fun env -> show_action ~rkey ~user ~repo ~spindle env
  in
  Cmd.v info Term.(const show' $ rkey_arg $ user_arg $ repo_arg $ spindle_arg)

(* Pipeline status command *)

let status_action ~rkey ~user ~repo ~spindle env =
  with_api env @@ fun api ->
  let did =
    match user with
    | Some u ->
        if String.starts_with ~prefix:"did:" u then u
        else Tangled.Api.resolve_handle api u
    | None -> Tangled.Api.get_did api
  in
  let spindle =
    match spindle with
    | Some s -> s
    | None -> (
        match Tangled.Api.get_spindle_for_repo api ~did ~repo_name:repo with
        | Some (s, _) -> s
        | None ->
            Fmt.epr "Repository %s has no spindle configured for CI.@." repo;
            exit 1)
  in
  (* First get the pipeline to show context *)
  let pipelines =
    Tangled.Api.list_pipelines_for_repo api ~spindle ~did ~repo_name:repo ()
  in
  (match List.find_opt (fun (k, _) -> k = rkey) pipelines with
  | Some (_, p) ->
      Fmt.pr "Pipeline: %s@," rkey;
      Fmt.pr "Trigger: %a@," pp_trigger_kind p.trigger_metadata;
      Fmt.pr "Repo: %s/%s@,@," p.trigger_metadata.repo.did
        p.trigger_metadata.repo.repo
  | None ->
      Fmt.epr "Pipeline not found: %s@." rkey;
      exit 1);
  (* Get status summary *)
  let summary =
    Tangled.Api.get_pipeline_summary api ~spindle ~pipeline_rkey:rkey ()
  in
  if summary = [] then Fmt.pr "No status updates found.@."
  else begin
    Fmt.pr "Workflow Status:@,";
    List.iter (fun (_, s) -> Fmt.pr "  %a@," pp_status_entry s) summary
  end

let status_cmd =
  let doc = "Show pipeline status." in
  let info = Cmd.info "status" ~doc in
  let status' rkey user repo spindle =
    Eio_main.run @@ fun env -> status_action ~rkey ~user ~repo ~spindle env
  in
  Cmd.v info Term.(const status' $ rkey_arg $ user_arg $ repo_arg $ spindle_arg)

(* Pipeline command group *)

let cmd =
  let doc = "CI pipeline commands." in
  let info = Cmd.info "pipeline" ~doc in
  Cmd.group info [ list_cmd; show_cmd; status_cmd ]
