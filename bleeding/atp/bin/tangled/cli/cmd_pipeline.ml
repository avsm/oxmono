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

let short_trigger p =
  let t = trigger_name p in
  match String.rindex_opt t '.' with
  | Some i -> String.sub t (i + 1) (String.length t - i - 1)
  | None -> t

let run_name (w : Ci.Defs.workflow_run) = Option.value ~default:w.id w.name

let summary (p : Ci.Defs.pipeline) =
  Ui.V.concat ~sep:(Ui.text "  ")
    (List.map
       (fun (w : Ci.Defs.workflow_run) ->
         Ui.V.concat
           [ Ui.status_mark w.status; Ui.text " "; Ui.text (run_name w) ])
       (Api.workflow_runs p))

let show_pipeline (p : Ci.Defs.pipeline) =
  let runs = Api.workflow_runs p in
  Ui.panel
    (Ui.title ("Pipeline " ^ p.id))
    (Ui.V.vcat ~gap:1
       [
         Ui.fields
           [
             ("Commit", Some (Ui.accent p.commit));
             ("Trigger", Some (Ui.text (trigger_name p.trigger)));
             ("Created", Option.map Ui.muted p.created_at);
             ("Repository", Some (Ui.muted p.repo));
             ("Source", Option.map Ui.muted p.source_repo);
           ];
         Console.Table.view
           [
             Ui.column "";
             Ui.column "Workflow";
             Ui.column "Status";
             Ui.column "Started";
             Ui.column "Error";
           ]
           (List.map
              (fun (w : Ci.Defs.workflow_run) ->
                [
                  Ui.status_mark w.status;
                  Ui.bold (run_name w);
                  Ui.state w.status;
                  Ui.muted (Option.value ~default:"" w.started_at);
                  Ui.V.role `Failed
                    (Ui.text (Option.value ~default:"" w.error));
                ])
              runs);
       ])

let show name =
  let action pipeline repo spindle json =
    Common.run (fun env ->
        let value =
          Common.with_api ~busy:("Reading pipeline " ^ pipeline) env (fun api ->
              let repo, spindle = target api ~repo ~spindle in
              Api.get_pipeline api ~spindle ~repo ~pipeline)
        in
        if json then Common.print Ci.Defs.pipeline_jsont value
        else if Ui.tty () then show_pipeline value
        else Fmt.pr "%a" pp_pipeline value)
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
        let page =
          Common.with_api ~busy:"Querying pipelines" env (fun api ->
              let repo, spindle = target api ~repo ~spindle in
              Api.query_pipelines api ~spindle ~repo ~limit ?cursor ~commits
                ~triggers ())
        in
        if json then Common.print Ci.QueryPipelines.output_jsont page
        else if Ui.tty () then begin
          let pipelines =
            List.map (Api.decode Ci.Defs.pipeline_jsont) page.pipelines
          in
          Ui.table
            ~heading:(Ui.heading "Pipelines" (List.length pipelines))
            [
              Ui.column "ID";
              Ui.column "Commit";
              Ui.column "Trigger";
              Ui.column "Created";
              Ui.column "Workflows";
            ]
            (List.map
               (fun (p : Ci.Defs.pipeline) ->
                 [
                   Ui.accent p.id;
                   Ui.text (Ui.short_sha p.commit);
                   Ui.text (short_trigger p.trigger);
                   Ui.muted (Option.value ~default:"" p.created_at);
                   summary p;
                 ])
               pipelines);
          Option.iter
            (fun c ->
              Ui.print
                (Ui.V.concat
                   [ Ui.muted "More with "; Ui.bold ("--cursor " ^ c) ]))
            page.cursor
        end
        else begin
          List.iter
            (fun p ->
              Fmt.pr "%a" pp_pipeline (Api.decode Ci.Defs.pipeline_jsont p))
            page.pipelines;
          Option.iter (fun c -> Fmt.pr "Next cursor: %s@." c) page.cursor
        end)
  in
  Cmd.v
    (Cmd.info "list" ~doc:"Query pipelines with filters and pagination.")
    Term.(
      term_result
        (const action $ repo $ spindle $ Common.limit $ Common.cursor $ commits
       $ triggers $ Common.json_flag))

let print_ci (ci : Api.Lex.Event.NotifyEvent.ci_output) =
  let ids = Option.value ~default:[] ci.pipelines in
  if Ui.tty () then List.iter (fun id -> Ui.ok "Started pipeline %s" id) ids
  else List.iter print_endline ids;
  Option.iter prerr_endline ci.message

let trigger_cmd =
  let action repo spindle audience commit ref_ inputs workflows =
    Common.run (fun env ->
        let inputs = List.map Common.pair inputs in
        print_ci
          (Common.with_api ~busy:"Triggering workflows" env (fun api ->
               let repo, spindle = target api ~repo ~spindle in
               Api.trigger_pipeline api ~spindle ?audience ~repo ~commit ?ref_
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
        let id =
          Common.with_api ~busy:("Retrying " ^ pipeline) env (fun api ->
              let repo, spindle = target api ~repo ~spindle in
              Api.retry_pipeline api ~spindle ?audience ~repo ~pipeline
                ?workflows:(Common.nonempty workflows)
                ())
        in
        if Ui.tty () then Ui.ok "Started pipeline %s" id else print_endline id)
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
        Common.with_api ~busy:("Cancelling " ^ pipeline) env (fun api ->
            let repo, spindle = target api ~repo ~spindle in
            Api.cancel_pipeline api ~spindle ?audience ~repo ~pipeline
              ?workflows:(Common.nonempty workflows)
              ());
        Ui.ok "Cancelled pipeline %s" pipeline)
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
        Common.with_api ~busy:("Approving " ^ pipeline) env (fun api ->
            let repo, spindle = target api ~repo ~spindle in
            Api.approve_pipeline api ~spindle ?audience ~repo ~pipeline ());
        Ui.ok "Approved pipeline %s" pipeline)
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
              let content =
                Tangled.Schema.text (Tangled.Schema.field "content" value)
              in
              (* A log is the build's own output. On a terminal, keep its
                 colours but drop the escapes that would move the cursor or
                 retitle the window. *)
              output_string channel
                (if Console_tty.is_terminal (Unix.descr_of_out_channel channel)
                 then Console_text.strip ~keep_styles:true content
                 else content);
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
  let action repo spindle ref_ json =
    Common.run (fun env ->
        let defs =
          Common.with_api ~busy:"Reading workflow definitions" env (fun api ->
              let repo, spindle = target api ~repo ~spindle in
              Api.workflow_definitions api ~spindle ~repo ?ref_ ())
        in
        if json || not (Ui.tty ()) then
          Common.print Ci.ListWorkflowDefinitions.output_jsont defs
        else
          let workflows =
            List.map (Api.decode Ci.Defs.workflow_jsont) defs.workflows
          in
          Ui.table
            ~heading:(Ui.heading "Workflows" (List.length workflows))
            [ Ui.column "Name"; Ui.column "Triggers"; Ui.column "Hash" ]
            (List.map
               (fun (w : Ci.Defs.workflow) ->
                 [
                   Ui.bold (Option.value ~default:w.id w.name);
                   Ui.text (String.concat ", " w.triggers);
                   Ui.muted (Option.fold ~none:"" ~some:Ui.short_sha w.hash);
                 ])
               workflows))
  in
  Cmd.v
    (Cmd.info "definitions"
       ~doc:"List the workflows a spindle would run. Pipes get JSON.")
    Term.(term_result (const action $ repo $ spindle $ ref_ $ Common.json_flag))

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
