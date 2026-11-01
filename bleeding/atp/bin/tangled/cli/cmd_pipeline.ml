(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Ci = Api.Lex.Ci

let spindle = Common.spindle
let repo = Common.repo

let pipeline =
  Arg.(
    value
    & pos 0 (some string) None
    & info [] ~docv:"PIPELINE"
        ~doc:
          "The pipeline's ID, as $(b,list) shows it. Without it, the latest \
           pipeline for the checked-out commit, or else the latest of all.")

let commit =
  Common.optional [ "commit"; "sha" ] "SHA"
    "Run on commit SHA instead of the checked-out commit."

let ref_ =
  Common.optional [ "ref" ] "REF"
    "Run as if pushed to REF, such as $(i,refs/heads/main), instead of the \
     current branch. Workflows may run differently on different branches."

let inputs =
  Common.strings [ "input" ] "KEY=VALUE"
    "Give the workflows the input KEY with VALUE. Repeat the option for more."

let target api ~repo ~spindle = Common.spindle_target api ~repo ~spindle

(* [latest api ~repo ~spindle ~inferred] is the ID of the newest pipeline. In
   a clone of the repository it prefers one for the checked-out commit. *)
let latest api ~repo ~spindle ~inferred =
  let newest commits =
    match
      (Api.query_pipelines api ~spindle ~repo ~commits ~limit:1 ()).pipelines
    with
    | p :: _ -> Some (Api.decode Ci.Defs.pipeline_jsont p).id
    | [] -> None
  in
  let head = if inferred then Local.head () else None in
  match Option.bind head (fun h -> newest [ h ]) with
  | Some id -> id
  | None -> (
      match newest [] with
      | Some id ->
          Option.iter
            (fun h ->
              Logs.warn (fun m ->
                  m "No pipeline ran on %s. Using the latest, %s."
                    (Ui.short_sha h) id))
            head;
          id
      | None -> failwith "The repository has no pipelines yet")

(* [resolve api ~repo ~spindle pipeline] is the repository DID, spindle and
   pipeline a command acts on. *)
let resolve api ~repo ~spindle pipeline =
  let did, spindle = target api ~repo ~spindle in
  let id =
    match pipeline with
    | Some id -> id
    | None -> latest api ~repo:did ~spindle ~inferred:(repo = None)
  in
  (did, spindle, id)

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

let show_cmd =
  let action pipeline repo spindle json =
    Common.run (fun env ->
        let value =
          Common.with_api ~busy:"Reading pipeline" env (fun api ->
              let repo, spindle, pipeline =
                resolve api ~repo ~spindle pipeline
              in
              Api.get_pipeline api ~spindle ~repo ~pipeline)
        in
        if json then Common.print Ci.Defs.pipeline_jsont value
        else if Ui.tty () then show_pipeline value
        else Fmt.pr "%a" pp_pipeline value)
  in
  Common.cmd
    (Cmd.info "show" ~doc:"Show a pipeline and how each of its workflows went.")
    Term.(
      term_result (const action $ pipeline $ repo $ spindle $ Common.json_flag))

let list_cmd =
  let commits =
    Common.strings [ "commit" ] "SHA"
      "Show only pipelines for commit SHA. Repeat the option for more."
  in
  let triggers =
    Common.strings [ "trigger" ] "EVENT"
      "Show only pipelines started by EVENT, such as \
       $(b,org.tangled.event.push) for pushes or \
       $(b,org.tangled.event.pullRequest) for pull requests. Repeat the option \
       for more."
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
  Common.cmd
    (Cmd.info "list" ~doc:"List the repository's pipelines, newest first.")
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
        let commit =
          match commit with
          | Some c -> c
          | None -> (
              match Local.head () with
              | Some h -> h
              | None -> failwith "Name the commit to run with --commit.")
        in
        let ref_ =
          match ref_ with
          | Some _ -> ref_
          | None -> Option.map (fun b -> "refs/heads/" ^ b) (Local.branch ())
        in
        print_ci
          (Common.with_api ~busy:"Triggering workflows" env (fun api ->
               let repo, spindle = target api ~repo ~spindle in
               Api.trigger_pipeline api ~spindle ?audience ~repo ~commit ?ref_
                 ?workflows:(Common.nonempty workflows)
                 ~inputs ())))
  in
  Common.cmd
    (Cmd.info "trigger" ~doc:"Start a pipeline by hand."
       ~man:
         (Common.man
            [
              "Runs the repository's workflows on the checked-out commit, \
               which must be pushed, and prints the new pipeline's ID. Use it \
               for workflows that run only when asked, or to run again without \
               pushing. It needs push access.";
            ]))
    Term.(
      term_result
        (const action $ repo $ spindle $ Common.audience $ commit $ ref_
       $ inputs $ Common.workflows))

let retry_cmd =
  let action pipeline repo spindle audience workflows =
    Common.run (fun env ->
        let id =
          Common.with_api ~busy:"Retrying" env (fun api ->
              let repo, spindle, pipeline =
                resolve api ~repo ~spindle pipeline
              in
              Api.retry_pipeline api ~spindle ?audience ~repo ~pipeline
                ?workflows:(Common.nonempty workflows)
                ())
        in
        if Ui.tty () then Ui.ok "Started pipeline %s" id else print_endline id)
  in
  Common.cmd
    (Cmd.info "retry"
       ~doc:"Run a pipeline's workflows again, as a new pipeline.")
    Term.(
      term_result
        (const action $ pipeline $ repo $ spindle $ Common.audience
       $ Common.workflows))

let cancel_cmd =
  let action pipeline repo spindle audience workflows =
    Common.run (fun env ->
        let pipeline =
          Common.with_api ~busy:"Cancelling" env (fun api ->
              let repo, spindle, pipeline =
                resolve api ~repo ~spindle pipeline
              in
              Api.cancel_pipeline api ~spindle ?audience ~repo ~pipeline
                ?workflows:(Common.nonempty workflows)
                ();
              pipeline)
        in
        Ui.ok "Cancelled pipeline %s" pipeline)
  in
  Common.cmd
    (Cmd.info "cancel"
       ~doc:"Stop a running pipeline, or some of its workflows.")
    Term.(
      term_result
        (const action $ pipeline $ repo $ spindle $ Common.audience
       $ Common.workflows))

let approve_cmd =
  let action pipeline repo spindle audience =
    Common.run (fun env ->
        let pipeline =
          Common.with_api ~busy:"Approving" env (fun api ->
              let repo, spindle, pipeline =
                resolve api ~repo ~spindle pipeline
              in
              Api.approve_pipeline api ~spindle ?audience ~repo ~pipeline ();
              pipeline)
        in
        Ui.ok "Approved pipeline %s" pipeline)
  in
  Common.cmd
    (Cmd.info "approve" ~doc:"Let a waiting pipeline run."
       ~man:
         (Common.man
            [
              "A pull request from someone without push access does not run CI \
               until someone with push access has read its changes and \
               approves it, so that untrusted code does not run unchecked.";
            ]))
    Term.(
      term_result (const action $ pipeline $ repo $ spindle $ Common.audience))

let logs_cmd =
  let action pipeline repo spindle workflows json =
    Common.run (fun env ->
        let repo, spindle, pipeline =
          Common.with_api env (fun api -> resolve api ~repo ~spindle pipeline)
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
  Common.cmd
    (Cmd.info "logs"
       ~doc:"Show a pipeline's output, following it while it runs.")
    Term.(
      term_result
        (const action $ pipeline $ repo $ spindle $ Common.workflows
       $ Common.json_flag))

let source_text source =
  match Tangled.Schema.member "$type" source with
  | Some t -> (
      let field name =
        Option.map Tangled.Schema.text (Tangled.Schema.member name source)
      in
      match Tangled.Schema.text t with
      | "org.tangled.ci.defs#externalSource" -> field "link"
      | _ -> field "path")
  | None -> None

let workflows_ref =
  Common.optional [ "ref" ] "REF"
    "Read the workflows of REF, a branch, tag or commit, instead of the \
     default branch."

let workflows_cmd =
  let name =
    Arg.(
      value
      & pos 0 (some string) None
      & info [] ~docv:"NAME"
          ~doc:"Show the workflow NAME in full instead of listing them.")
  in
  let action repo spindle ref_ name json =
    Common.run (fun env ->
        match name with
        | Some workflow ->
            let w =
              Common.with_api ~busy:("Reading " ^ workflow) env (fun api ->
                  let repo, spindle = target api ~repo ~spindle in
                  Api.workflow_definition api ~spindle ~repo workflow)
            in
            if json || not (Ui.tty ()) then
              Common.print Ci.Defs.workflow_jsont w
            else
              Ui.panel
                (Ui.title (Option.value ~default:w.id w.name))
                (Ui.fields
                   [
                     ("ID", Some (Ui.muted w.id));
                     ( "Triggers",
                       Some (Ui.text (String.concat ", " w.triggers)) );
                     ("Source", Option.map Ui.link (source_text w.source));
                     ("Hash", Option.map Ui.muted w.hash);
                   ])
        | None ->
            let defs =
              Common.with_api ~busy:"Reading workflows" env (fun api ->
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
                       Ui.muted
                         (Option.fold ~none:"" ~some:Ui.short_sha w.hash);
                     ])
                   workflows))
  in
  Common.cmd
    (Cmd.info "workflows"
       ~doc:"List the workflows a push would run, or show one.")
    Term.(
      term_result
        (const action $ repo $ spindle $ workflows_ref $ name
       $ Common.json_flag))

let secret_list =
  let action repo spindle audience json =
    Common.run (fun env ->
        let secrets =
          Common.with_api ~busy:"Reading secrets" env (fun api ->
              let repo, spindle = target api ~repo ~spindle in
              Api.secrets api ~spindle ?audience ~repo ())
        in
        let module S = Api.Lex.Secret.ListSecrets in
        if json then Common.print (Jsont.list S.secret_jsont) secrets
        else if Ui.tty () then
          Ui.table
            ~heading:(Ui.heading "Secrets" (List.length secrets))
            [ Ui.column "Key"; Ui.column "Added"; Ui.column "By" ]
            (List.map
               (fun (s : S.secret) ->
                 [
                   Ui.bold s.key; Ui.muted s.created_at; Ui.muted s.created_by;
                 ])
               secrets)
        else List.iter (fun (s : S.secret) -> print_endline s.key) secrets)
  in
  Common.cmd
    (Cmd.info "list"
       ~doc:"List the names of the secrets, but not their values.")
    Term.(
      term_result
        (const action $ repo $ spindle $ Common.audience $ Common.json_flag))

let secret_key =
  Common.positional 0 "KEY" "The secret's name, as workflows read it."

let secret_set =
  let value =
    Common.optional [ "value" ] "VALUE"
      "The secret's value. Without this option, it is read from standard \
       input, which keeps it out of your shell's history."
  in
  let action repo spindle audience key value =
    Common.run (fun env ->
        let value =
          match value with
          | Some v -> v
          | None ->
              if Unix.isatty Unix.stdin then
                Fmt.epr "Value for %s, then Ctrl-D:@." key;
              let v = In_channel.input_all stdin in
              (* A value typed or echoed ends in a newline that is not part
                 of it. *)
              if String.ends_with ~suffix:"\n" v then
                String.sub v 0 (String.length v - 1)
              else v
        in
        if value = "" then failwith "A secret cannot be empty";
        Common.with_api ~busy:("Setting " ^ key) env (fun api ->
            let repo, spindle = target api ~repo ~spindle in
            Api.add_secret api ~spindle ?audience ~repo ~key value);
        Ui.ok "Set secret %s" key)
  in
  Common.cmd
    (Cmd.info "set" ~doc:"Add a secret.")
    Term.(
      term_result
        (const action $ repo $ spindle $ Common.audience $ secret_key $ value))

let secret_remove =
  let action repo spindle audience key =
    Common.run (fun env ->
        Common.with_api ~busy:("Removing " ^ key) env (fun api ->
            let repo, spindle = target api ~repo ~spindle in
            Api.remove_secret api ~spindle ?audience ~repo key);
        Ui.ok "Removed secret %s" key)
  in
  Common.cmd
    (Cmd.info "remove" ~doc:"Remove a secret.")
    Term.(
      term_result
        (const action $ repo $ spindle $ Common.audience $ secret_key))

let secret_group =
  Cmd.group
    (Cmd.info "secret"
       ~doc:"Manage the secrets a repository's workflows can read."
       ~man:
         (Common.man
            [
              "A secret, such as a token for publishing a release, is given to \
               the workflows as an environment variable and hidden from their \
               logs. Once set, a secret's value cannot be read back. Managing \
               secrets needs push access.";
            ]))
    [ secret_list; secret_set; secret_remove ]

let cmd =
  Cmd.group
    (Cmd.info "pipeline" ~doc:"Follow and control a repository's CI."
       ~man:
         (Common.man
            [
              "A repository's CI runs on the spindle it names. The workflows \
               are YAML files in the repository's $(i,.tangled/workflows) \
               directory, and each says when it runs, such as on a push to a \
               branch or a pull request. Each time, the spindle starts a \
               pipeline: one run of those workflows on one commit.";
              "Commands that take a pipeline ID use, without one, the latest \
               pipeline for the commit checked out in the current directory.";
            ]))
    [
      list_cmd;
      show_cmd;
      logs_cmd;
      trigger_cmd;
      retry_cmd;
      cancel_cmd;
      approve_cmd;
      workflows_cmd;
      secret_group;
    ]
