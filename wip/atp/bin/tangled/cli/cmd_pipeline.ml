(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Ci = Atp_lexicon_tangled.Sh.Tangled.Ci

let spindle =
  Common.optional [ "spindle"; "s" ] "SPINDLE"
    "Spindle hostname or HTTP(S) origin."

let repo =
  Common.optional [ "repo"; "r" ] "REPO"
    "Repository DID, owner/name, or record URI."

let pipeline =
  Common.positional 0 "PIPELINE" "Spindle-local pipeline ID or pipeline AT-URI."

let sha =
  Arg.(
    required
    & opt (some string) None
    & info [ "sha" ] ~docv:"SHA" ~doc:"Exact commit SHA.")

let source_repo =
  Common.optional [ "source-repo" ] "DID" "Fork repository DID to check out."

let ref_ =
  Common.optional [ "ref" ] "REF"
    "Original Git reference for display and workflow metadata."

let inputs =
  Common.strings [ "input" ] "KEY=VALUE" "Workflow input (repeatable)."

let id value =
  if String.starts_with ~prefix:"at://" value then Api.rkey_of_uri value
  else value

let target api ~repo ~user ~spindle =
  match (repo, spindle) with
  | Some repo, Some spindle
    when String.starts_with ~prefix:"did:" repo
         && not (String.contains repo '/') ->
      (repo, spindle)
  | Some name, spindle ->
      let name =
        match user with
        | Some user when not (String.contains name '/') -> user ^ "/" ^ name
        | _ -> name
      in
      let repository = Api.resolve_repo api name in
      let spindle =
        match (spindle, repository.record.spindle) with
        | Some s, _ | None, Some s -> s
        | None, None ->
            invalid_arg "Repository has no spindle. Supply --spindle."
      in
      (Api.repo_did repository, spindle)
  | None, _ -> invalid_arg "Supply --repo"

let host api ~repo ~user ~spindle =
  match spindle with
  | Some spindle -> spindle
  | None -> snd (target api ~repo ~user ~spindle)

let pp_pipeline ppf (p : Ci.Pipeline.main) =
  Fmt.pf ppf "%s  %s%a@." p.id p.commit Fmt.(option (fmt "  %s")) p.created_at;
  List.iter
    (fun (w : Ci.Pipeline.workflow) ->
      Fmt.pf ppf "  %-24s %s%a@." w.name w.status
        Fmt.(option (fmt "  %s"))
        w.error)
    p.workflows

let show name =
  let action pipeline repo user spindle json =
    Common.run (fun env ->
        Common.with_api ~authenticated:(spindle = None) env (fun api ->
            let spindle = host api ~repo ~user ~spindle in
            let value = Api.get_pipeline api ~spindle ~pipeline:(id pipeline) in
            if json then Common.print Ci.Pipeline.main_jsont value
            else Fmt.pr "%a" pp_pipeline value))
  in
  Cmd.v
    (Cmd.info name ~doc:"Show a pipeline and its workflow statuses.")
    Term.(
      term_result
        (const action $ pipeline $ repo $ Common.user $ spindle
       $ Common.json_flag))

let list_cmd =
  let commits =
    Common.strings [ "commit" ] "SHA" "Filter by commit (repeatable)."
  in
  let kinds =
    Common.strings [ "kind" ] "KIND"
      "Filter by push, pull_request or manual (repeatable)."
  in
  let action repo user spindle limit cursor commits kinds json =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repo, spindle = target api ~repo ~user ~spindle in
            let page =
              Api.query_pipelines api ~spindle ~repo ~limit ?cursor ~commits
                ~kinds ()
            in
            if json then Common.print Ci.QueryPipelines.output_jsont page
            else begin
              List.iter (fun p -> Fmt.pr "%a" pp_pipeline p) page.pipelines;
              Option.iter (fun c -> Fmt.pr "Next cursor: %s@." c) page.cursor
            end))
  in
  Cmd.v
    (Cmd.info "list"
       ~doc:"Query pipelines with server-side filters and pagination.")
    Term.(
      term_result
        (const action $ repo $ Common.user $ spindle $ Common.limit
       $ Common.cursor $ commits $ kinds $ Common.json_flag))

let trigger_cmd =
  let action repo user spindle audience sha ref_ source_repo inputs workflows =
    Common.run (fun env ->
        let inputs =
          List.map
            (fun input ->
              let key, value = Common.pair input in
              { Ci.Trigger.key; value })
            inputs
          |> Common.nonempty
        in
        let trigger : Ci.Trigger.manual = { sha; ref_; source_repo; inputs } in
        let trigger = Api.encode Ci.Trigger.manual_jsont trigger in
        Tangled.Schema.validate ~nsid:"sh.tangled.ci.trigger"
          (Tangled.Schema.field "manual"
             (Tangled.Schema.field "defs"
                (Tangled.Schema.document "sh.tangled.ci.trigger")))
          trigger;
        Common.with_api env (fun api ->
            let repo, spindle = target api ~repo ~user ~spindle in
            Common.print Ci.TriggerPipeline.output_jsont
              (Api.trigger_pipeline api ~spindle ?audience ~repo ~trigger
                 ?workflows:(Common.nonempty workflows)
                 ())))
  in
  Cmd.v
    (Cmd.info "trigger" ~doc:"Run workflows at an explicit commit.")
    Term.(
      term_result
        (const action $ repo $ Common.user $ spindle $ Common.audience $ sha
       $ ref_ $ source_repo $ inputs $ Common.workflows))

let retry_cmd =
  let action pipeline repo user spindle audience workflows =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let spindle = host api ~repo ~user ~spindle in
            let previous =
              Api.get_pipeline api ~spindle ~pipeline:(id pipeline)
            in
            let repo =
              match previous.repo with
              | Some repo -> repo
              | None -> fst (target api ~repo ~user ~spindle:(Some spindle))
            in
            let trigger =
              match Tangled.Schema.member "$type" previous.trigger with
              | Some type_
                when Tangled.Schema.text type_ = "sh.tangled.ci.trigger#manual"
                     || Tangled.Schema.text type_
                        = "sh.tangled.ci.trigger#pullRequest" ->
                  previous.trigger
              | _ ->
                  Api.encode Ci.Trigger.manual_jsont
                    {
                      sha = previous.commit;
                      ref_ = None;
                      source_repo = previous.source_repo;
                      inputs = None;
                    }
            in
            let workflows =
              if workflows = [] then
                List.map
                  (fun (w : Ci.Pipeline.workflow) -> w.name)
                  previous.workflows
              else workflows
            in
            Common.print Ci.TriggerPipeline.output_jsont
              (Api.trigger_pipeline api ~spindle ?audience ~repo ~trigger
                 ~workflows ())))
  in
  Cmd.v
    (Cmd.info "retry"
       ~doc:
         "Run a new pipeline at the original commit, retaining workflow \
          selection.")
    Term.(
      term_result
        (const action $ pipeline $ repo $ Common.user $ spindle
       $ Common.audience $ Common.workflows))

let cancel_cmd =
  let action pipeline repo user spindle audience workflows =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repo, spindle = target api ~repo ~user ~spindle in
            Api.cancel_pipeline api ~spindle ?audience ~repo
              ~pipeline:(id pipeline)
              ?workflows:(Common.nonempty workflows)
              ()))
  in
  Cmd.v
    (Cmd.info "cancel" ~doc:"Cancel a pipeline or selected workflows.")
    Term.(
      term_result
        (const action $ pipeline $ repo $ Common.user $ spindle
       $ Common.audience $ Common.workflows))

let logs_cmd =
  let action pipeline repo user spindle workflows json =
    Common.run (fun env ->
        Common.with_api ~authenticated:(spindle = None) env (fun api ->
            let spindle = host api ~repo ~user ~spindle in
            let params =
              [ ("pipeline", id pipeline) ]
              @ List.map (fun w -> ("workflows", w)) workflows
            in
            Stream.subscribe env ~service:spindle
              ~nsid:"sh.tangled.ci.subscribePipelineLogs" ~params
              (fun type_ value ->
                if json then
                  Common.json
                    (Common.object_
                       [ ("type", Common.string type_); ("body", value) ])
                else if type_ = "#data" then begin
                  let channel =
                    match Tangled.Schema.member "stream" value with
                    | Some stream when Tangled.Schema.text stream = "stderr" ->
                        stderr
                    | _ -> stdout
                  in
                  output_string channel
                    (Tangled.Schema.text (Tangled.Schema.field "content" value));
                  flush channel
                end)))
  in
  Cmd.v
    (Cmd.info "logs"
       ~doc:"Stream workflow logs, preserving partial lines and stderr.")
    Term.(
      term_result
        (const action $ pipeline $ repo $ Common.user $ spindle
       $ Common.workflows $ Common.json_flag))

let definition_cmd =
  let action repo user spindle sha source_repo =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repo, spindle = target api ~repo ~user ~spindle in
            let params =
              [ ("repo", repo); ("sha", sha) ]
              @ Option.to_list
                  (Option.map (fun repo -> ("sourceRepo", repo)) source_repo)
            in
            Common.print Ci.DescribeWorkflowDefinition.output_jsont
              (Xrpc.Client.query
                 (Api.public_client api ~service:spindle)
                 ~nsid:"sh.tangled.ci.describeWorkflowDefinition" ~params
                 ~decoder:Ci.DescribeWorkflowDefinition.output_jsont)))
  in
  Cmd.v
    (Cmd.info "definition"
       ~doc:"Inspect the workflow definition fingerprint at a commit.")
    Term.(
      term_result
        (const action $ repo $ Common.user $ spindle $ sha $ source_repo))

let cmd =
  Cmd.group
    (Cmd.info "pipeline" ~doc:"Query and control CI on a spindle.")
    [
      list_cmd;
      show "show";
      show "status";
      trigger_cmd;
      retry_cmd;
      cancel_cmd;
      logs_cmd;
      definition_cmd;
    ]
