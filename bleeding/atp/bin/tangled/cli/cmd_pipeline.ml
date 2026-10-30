(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Ci = Api.Lex.Ci

let spindle =
  Common.optional [ "spindle"; "s" ] "SPINDLE"
    "Spindle hostname or DID (default: the repository's spindle)."

let repo =
  Arg.(
    required
    & opt (some string) None
    & info [ "repo"; "r" ] ~docv:"REPO"
        ~doc:"Repository DID, owner/name, or declaration URI.")

let pipeline = Common.positional 0 "PIPELINE" "Spindle-local pipeline ID."

let commit =
  Arg.(
    required
    & opt (some string) None
    & info [ "commit"; "sha" ] ~docv:"SHA" ~doc:"Exact commit ID.")

let ref_ =
  Common.optional [ "ref" ] "REF" "Git reference the commit was resolved from."

let inputs =
  Common.strings [ "input" ] "KEY=VALUE" "Workflow input (repeatable)."

(* The repository DID and spindle origin for [repo]. A repository DID with an
   explicit spindle needs no knot or PDS lookup. *)
let target api ~repo ~spindle =
  if
    String.starts_with ~prefix:"did:" repo
    && (not (String.contains repo '/'))
    && spindle <> None
  then (repo, Api.spindle_url api (Option.get spindle))
  else
    let repository = Api.resolve_repo api repo in
    (repository.did, Api.spindle_of api ?spindle repository)

let trigger_name trigger =
  match Tangled.Schema.member "$type" trigger with
  | Some t -> Tangled.Schema.text t
  | None -> "unknown"

let pp_pipeline ppf (p : Ci.Defs.pipeline) =
  Fmt.pf ppf "%s  %s  %s%a@." p.id p.commit (trigger_name p.trigger)
    Fmt.(option (fmt "  %s"))
    p.created_at;
  List.iter
    (fun (w : Ci.Defs.workflow_run) ->
      Fmt.pf ppf "  %-24s %s%a@."
        (Option.value ~default:w.id w.name)
        w.status
        Fmt.(option (fmt "  %s"))
        w.error)
    (Api.workflow_runs p)

let show name =
  let action pipeline repo spindle json =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repo, spindle = target api ~repo ~spindle in
            let value = Api.get_pipeline api ~spindle ~repo ~pipeline in
            if json then Common.print Ci.Defs.pipeline_jsont value
            else Fmt.pr "%a" pp_pipeline value))
  in
  Cmd.v
    (Cmd.info name ~doc:"Show a pipeline and its workflow statuses.")
    Term.(
      term_result (const action $ pipeline $ repo $ spindle $ Common.json_flag))

let list_cmd =
  let commits =
    Common.strings [ "commit" ] "SHA" "Filter by commit (repeatable)."
  in
  let triggers =
    Common.strings [ "trigger" ] "NSID"
      "Filter by trigger, such as org.tangled.event.push (repeatable)."
  in
  let action repo spindle limit cursor commits triggers json =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repo, spindle = target api ~repo ~spindle in
            let page =
              Api.query_pipelines api ~spindle ~repo ~limit ?cursor ~commits
                ~triggers ()
            in
            if json then Common.print Ci.QueryPipelines.output_jsont page
            else begin
              List.iter
                (fun p ->
                  Fmt.pr "%a" pp_pipeline (Api.decode Ci.Defs.pipeline_jsont p))
                page.pipelines;
              Option.iter (fun c -> Fmt.pr "Next cursor: %s@." c) page.cursor
            end))
  in
  Cmd.v
    (Cmd.info "list" ~doc:"Query pipelines with filters and pagination.")
    Term.(
      term_result
        (const action $ repo $ spindle $ Common.limit $ Common.cursor $ commits
       $ triggers $ Common.json_flag))

let print_ci (ci : Api.Lex.Event.NotifyEvent.ci_output) =
  List.iter print_endline (Option.value ~default:[] ci.pipelines);
  Option.iter prerr_endline ci.message

let trigger_cmd =
  let action repo spindle audience commit ref_ inputs workflows =
    Common.run (fun env ->
        let inputs = List.map Common.pair inputs in
        Common.with_api env (fun api ->
            let repo, spindle = target api ~repo ~spindle in
            print_ci
              (Api.trigger_pipeline api ~spindle ?audience ~repo ~commit ?ref_
                 ?workflows:(Common.nonempty workflows)
                 ~inputs ())))
  in
  Cmd.v
    (Cmd.info "trigger"
       ~doc:"Run workflows at an explicit commit. Prints the pipeline IDs.")
    Term.(
      term_result
        (const action $ repo $ spindle $ Common.audience $ commit $ ref_
       $ inputs $ Common.workflows))

let retry_cmd =
  let action pipeline repo spindle audience workflows =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repo, spindle = target api ~repo ~spindle in
            print_endline
              (Api.retry_pipeline api ~spindle ?audience ~repo ~pipeline
                 ?workflows:(Common.nonempty workflows)
                 ())))
  in
  Cmd.v
    (Cmd.info "retry" ~doc:"Run a pipeline again as a new pipeline.")
    Term.(
      term_result
        (const action $ pipeline $ repo $ spindle $ Common.audience
       $ Common.workflows))

let cancel_cmd =
  let action pipeline repo spindle audience workflows =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repo, spindle = target api ~repo ~spindle in
            Api.cancel_pipeline api ~spindle ?audience ~repo ~pipeline
              ?workflows:(Common.nonempty workflows)
              ()))
  in
  Cmd.v
    (Cmd.info "cancel" ~doc:"Cancel a pipeline or selected workflows.")
    Term.(
      term_result
        (const action $ pipeline $ repo $ spindle $ Common.audience
       $ Common.workflows))

let approve_cmd =
  let action pipeline repo spindle audience =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repo, spindle = target api ~repo ~spindle in
            Api.approve_pipeline api ~spindle ?audience ~repo ~pipeline ()))
  in
  Cmd.v
    (Cmd.info "approve" ~doc:"Approve a pipeline blocked for review.")
    Term.(
      term_result (const action $ pipeline $ repo $ spindle $ Common.audience))

let logs_cmd =
  let action pipeline repo spindle workflows json =
    Common.run (fun env ->
        let repo, spindle =
          Common.with_api env (fun api -> target api ~repo ~spindle)
        in
        let params =
          [ ("repo", repo); ("pipeline", pipeline) ]
          @ List.map (fun w -> ("workflows", w)) workflows
        in
        Stream.subscribe env ~service:spindle
          ~nsid:"org.tangled.ci.subscribePipelineLogs" ~params
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
            end))
  in
  Cmd.v
    (Cmd.info "logs"
       ~doc:"Stream workflow logs, preserving partial lines and stderr.")
    Term.(
      term_result
        (const action $ pipeline $ repo $ spindle $ Common.workflows
       $ Common.json_flag))

let definitions_cmd =
  let action repo spindle ref_ =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repo, spindle = target api ~repo ~spindle in
            Common.print Ci.ListWorkflowDefinitions.output_jsont
              (Api.workflow_definitions api ~spindle ~repo ?ref_ ())))
  in
  Cmd.v
    (Cmd.info "definitions" ~doc:"List the workflows a spindle would run.")
    Term.(term_result (const action $ repo $ spindle $ ref_))

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
      approve_cmd;
      logs_cmd;
      definitions_cmd;
    ]
