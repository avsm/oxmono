module M = Siftbot_model
open Cmdliner

let feedback_provider ~sw (p : Siftbot.Profile.t) =
  match (p.embedding_url, p.embedding_model) with
  | None, None -> None
  | Some url, Some model when p.embedding_dimensions > 0 ->
      Some
        (Siftbot.Embedding.http ~sw
           ~fetch:(Fetch_curl.v ~sw ~timeout:(Duration.of_sec 60) ())
           ~url
           ?api_key:(Sys.getenv_opt "SIFTBOT_EMBEDDING_API_KEY")
           ~model ~dimensions:p.embedding_dimensions ())
  | _ ->
      invalid_arg
        "Configure embeddingUrl, embeddingModel and embeddingDimensions \
         together"

let profile =
  Arg.(value & opt string "personal" & info [ "profile" ] ~docv:"NAME")

let demo = Arg.(value & flag & info [ "demo" ] ~doc:"Use only synthetic mail.")
let query = Arg.(value & opt string "" & info [ "query" ] ~docv:"FTS")

let explain =
  Arg.(value & opt string "Reviewed in CLI" & info [ "explanation" ])

let with_db profile demo f =
  let name = if demo then "synthetic-demo" else profile in
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          let db =
            Siftbot.Store.open_db ~sw
              Eio.Path.(Eio.Stdenv.fs env / Siftbot.Profile.data_path name)
          in
          if demo then Siftbot.Demo.seed db;
          f env sw db))

let protect f =
  try
    f ();
    `Ok ()
  with
  | Sys.Break ->
      `Error
        ( false,
          "Interrupted. Completed work is saved; rerun the command to resume."
        )
  | Siftbot.Store.Conflict ->
      `Error (false, "Revision conflict. Refresh and retry.")
  | Invalid_argument s | Failure s | Sys_error s -> `Error (false, s)
  | Jsont.Error _ ->
      `Error
        ( false,
          "Invalid structured data. Check the profile or provider response." )
  | Eio.Io (Openrouter.E _, _) ->
      `Error (false, "Model provider request failed.")
  | Jmap_eio.Client.Jmap_client_error _ as e ->
      `Error (false, Jmap_eio.Mail.error e)
  | e -> `Error (false, Printexc.to_string e)

let init profile jp =
  protect (fun () ->
      Siftbot.Profile.init ~name:profile ~jmap_profile:jp;
      with_db profile false (fun _ _ _ -> ());
      Printf.printf "Created profile %s.\n" profile)

let inspect profile demo query =
  protect (fun () ->
      with_db profile demo (fun _ _ db ->
          let s = Siftbot.Store.snapshot db ~query in
          Printf.printf "Revision %d. %d messages shown.\n" s.revision
            (List.length s.messages);
          List.iter
            (fun (a : M.area) ->
              Printf.printf "Area %s [%s]: %s\n" a.id a.status
                (M.sanitize a.name))
            s.areas;
          List.iter
            (fun (m : M.message) ->
              Printf.printf "%s %s %s\n" m.date
                (M.sanitize (M.key m.source))
                (M.sanitize m.subject))
            s.messages;
          List.iter (fun s -> Printf.printf "Job %s\n" (M.sanitize s)) s.jobs))

let cutoff =
  Arg.(
    value
    & opt (some string) None
    & info [ "cutoff" ] ~docv:"RFC3339" ~doc:"Training/evaluation boundary.")

let cutoff_value db demo = function
  | Some cutoff -> cutoff
  | None when demo -> "2026-07-01T00:00:00Z"
  | None -> Siftbot.Import.evaluation_cutoff db

let discover profile demo cutoff =
  protect (fun () ->
      with_db profile demo (fun _ _ db ->
          let cutoff = cutoff_value db demo cutoff in
          ignore
            (match Ptime.of_rfc3339 cutoff with
            | Ok _ -> ()
            | Error _ -> invalid_arg "Use an RFC3339 cutoff");
          Siftbot.Discovery.run db ~before:cutoff;
          print_endline
            "Stored baseline proposals. Approve definitions before \
             classification."))

let evaluate profile demo cutoff =
  protect (fun () ->
      with_db profile demo (fun _ _ db ->
          let cutoff = cutoff_value db demo cutoff in
          let id, reports = Siftbot.Evaluation.freeze_baseline db ~cutoff in
          Printf.printf "Frozen baseline evaluation %s\n" id;
          List.iter print_endline reports))

let learn profile demo since watch max_steps no_progress jobs =
  protect (fun () ->
      let interval = Siftbot.Interval.parse since in
      Option.iter
        (fun n -> if n < 1 then invalid_arg "--max-steps must be positive")
        max_steps;
      with_db profile demo (fun env sw db ->
          if demo then print_endline "Synthetic corpus ready."
          else
            let p = Siftbot.Profile.load profile in
            let now =
              Eio.Time.now (Eio.Stdenv.clock env)
              |> Ptime.of_float_s |> Option.get
            in
            let start = Siftbot.Interval.since ~now interval in
            Printf.printf "Connecting to %s...\n%!" profile;
            let remote =
              Siftbot.Profile.connect ~sw ~fetch:(Fetch_curl.v ~sw ()) env p
            in
            let timestamp = Ptime.to_rfc3339 ~tz_offset_s:0 in
            Siftbot.Import.start db remote ~since:(timestamp start)
              ~until:(timestamp now);
            let first, last = Option.get (Siftbot.Import.interval db remote) in
            Printf.printf "Syncing %s: %s to %s.\n%!" profile
              (String.sub first 0 10) (String.sub last 0 10);
            let clock = Eio.Stdenv.clock env in
            let sync () =
              Cli_progress.sync ~clock:(Eio.Stdenv.mono_clock env)
                ~enabled:(not no_progress) (fun ~on_progress ->
                  Siftbot.Import.sync ?max_steps ~on_progress
                    ~check:Eio.Fiber.check db remote)
            in
            let rec run () =
              match sync () with
              | Siftbot.Import.Paused ->
                  Printf.printf
                    "Paused at your step limit. Resume with: siftbot sync \
                     --profile %s\n\
                     %!"
                    profile
              | Complete ->
                  Printf.printf
                    "Sync complete. %d emails available locally.\n%!"
                    (Siftbot.Store.message_count db);
                  if watch then (
                    (match p.chat_model with
                    | None -> ()
                    | Some _ ->
                        let model, complete =
                          Siftbot.Assistant.completion
                            ~json_schema:(Siftbot.Classifier.schema db)
                            ~fetch:(Fetch_curl.v ~sw ()) p
                        in
                        let count, _ =
                          Cli_progress.classify
                            ~clock:(Eio.Stdenv.mono_clock env)
                            ~enabled:(not no_progress)
                            (fun ~on_progress ~on_classified ->
                              Siftbot.Classifier.run ~on_progress ~on_classified
                                ~jobs
                                ?feedback_provider:(feedback_provider ~sw p) db
                                ~complete ~model ~max_messages:p.max_steps)
                        in
                        Printf.printf
                          "Stored %d classification suggestions.\n%!" count);
                    Cli_progress.interruptible
                      ~clock:(Eio.Stdenv.mono_clock env) (fun () ->
                        Eio.Time.sleep clock 30.);
                    run ())
                  else
                    Printf.printf
                      "Browse and search: siftbot review --profile %s\n%!"
                      profile
            in
            run ()))

let embedding profile demo query =
  protect (fun () ->
      if demo then invalid_arg "Demo mode never contacts an embedding provider";
      with_db profile false (fun _ sw db ->
          let p = Siftbot.Profile.load profile in
          let url, model =
            match (p.embedding_url, p.embedding_model) with
            | Some u, Some m when p.embedding_dimensions > 0 -> (u, m)
            | _ ->
                invalid_arg
                  "Configure embeddingUrl, embeddingModel and \
                   embeddingDimensions"
          in
          let provider =
            Siftbot.Embedding.http ~sw
              ~fetch:(Fetch_curl.v ~sw ~timeout:(Duration.of_sec 60) ())
              ~url
              ?api_key:(Sys.getenv_opt "SIFTBOT_EMBEDDING_API_KEY")
              ~model ~dimensions:p.embedding_dimensions ()
          in
          if query = "" then
            Printf.printf "Indexed %d source passages.\n"
              (Siftbot.Embedding.index db provider ~check:Eio.Fiber.check
                 ~max_batches:p.max_steps)
          else
            Siftbot.Embedding.search db provider ~check:Eio.Fiber.check query
            |> List.iter (fun (m, distance) ->
                   Printf.printf "%.4f %s %s\n" distance
                     (M.sanitize (M.key m.M.source))
                     (M.sanitize m.subject))))

let classify profile demo all no_progress jobs refresh_feedback =
  protect (fun () ->
      if demo then invalid_arg "Demo mode does not use model credentials";
      with_db profile false (fun env sw db ->
          let p = Siftbot.Profile.load profile in
          let model, complete =
            Siftbot.Assistant.completion
              ~json_schema:(Siftbot.Classifier.schema db)
              ~fetch:(Fetch_curl.v ~sw ()) p
          in
          let count, remaining =
            Cli_progress.classify ~clock:(Eio.Stdenv.mono_clock env)
              ~enabled:(not no_progress) (fun ~on_progress ~on_classified ->
                Siftbot.Classifier.run ~on_progress ~on_classified ~jobs
                  ?feedback_provider:(feedback_provider ~sw p) ~refresh_feedback
                  db ~complete ~model
                  ?max_messages:(if all then None else Some p.max_steps))
          in
          Printf.printf
            "Stored suggestions for %d messages. %d still need classification.\n\
             %!"
            count remaining))

let capture profile demo service account email_id title deadline area root
    confirm =
  protect (fun () ->
      if demo then invalid_arg "Demo mode does not write to Dooit";
      with_db profile false (fun _ _ db ->
          let source = M.{ service; account; id = email_id } in
          let m =
            match Siftbot.Store.get db source with
            | Some m -> m
            | None -> invalid_arg "Source is not in this profile"
          in
          let preview = Siftbot.Task.preview m ~title ?deadline ?area () in
          match confirm with
          | None ->
              Printf.printf
                "Task preview\n\
                 Title: %s\n\
                 Deadline: %s\n\
                 Area: %s\n\
                 Source: %s\n\
                 Dooit: %s\n\
                 Confirm this preview with --confirm-revision %d.\n"
                (M.sanitize title)
                (Option.value ~default:"none" deadline)
                (Option.value ~default:"none" area)
                (M.sanitize (M.key source))
                (M.sanitize root)
                (Siftbot.Store.revision db)
          | Some revision ->
              Siftbot.Task.capture db ~revision ~dooit_root:root preview
              |> List.iter (Printf.printf "Dooit task %s\n")))

let overview profile demo area maintain =
  protect (fun () ->
      with_db profile demo (fun _ sw db ->
          if maintain then (
            if demo then invalid_arg "Demo mode never contacts a model";
            let p = Siftbot.Profile.load profile in
            let model, complete =
              Siftbot.Assistant.completion ~fetch:(Fetch_curl.v ~sw ()) p
            in
            ignore
              (Siftbot.Overview.maintain db ~area_id:area ~model ~complete
                 ~max_merges:10));
          print_endline (M.sanitize (Siftbot.Overview.area db ~area_id:area))))

let approve profile demo area explanation =
  protect (fun () ->
      with_db profile demo (fun _ _ db ->
          Siftbot.Store.edit db
            ~revision:(Siftbot.Store.revision db)
            (M.Approve area) ~explanation))

let undo profile demo explanation =
  protect (fun () ->
      with_db profile demo (fun _ _ db ->
          Siftbot.Store.edit db
            ~revision:(Siftbot.Store.revision db)
            M.Undo ~explanation))

let review profile demo setup =
  protect (fun () ->
      if not (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout) then
        invalid_arg
          "Setup and review need an interactive terminal. Use inspect for \
           plain output.";
      let dir = Filename.dirname Sys.executable_name in
      let name =
        if Filename.check_suffix Sys.executable_name ".exe" then
          "siftbot_review.exe"
        else "siftbot-review"
      in
      let path = Filename.concat dir name in
      let args =
        [ path; "--profile"; profile ]
        @ (if demo then [ "--demo" ] else [])
        @ if setup then [ "--setup" ] else []
      in
      Unix.execv path (Array.of_list args))

let command name doc term = Cmd.v (Cmd.info name ~doc) Term.(ret term)
let required_opt name = Arg.(required & opt (some string) None & info [ name ])

let jobs =
  let parse s =
    match int_of_string_opt s with
    | Some n when n >= 1 && n <= 32 -> Ok n
    | _ -> Error (`Msg "jobs must be an integer from 1 to 32")
  in
  Arg.(
    value
    & opt (conv (parse, Format.pp_print_int)) 4
    & info [ "j"; "jobs" ] ~docv:"N"
        ~doc:"Classify up to N emails concurrently (1 to 32).")

let sync_term =
  Term.(
    const (fun p d s limit no_progress -> learn p d s false limit no_progress 1)
    $ profile $ demo
    $ Arg.(
        value & opt string "1y"
        & info [ "since" ] ~docv:"INTERVAL"
            ~doc:
              "Initial interval: Nd, Nw, Nm, Ny or RFC3339. Resumes the stored \
               interval.")
    $ Arg.(
        value
        & opt (some int) None
        & info [ "max-steps" ] ~docv:"N"
            ~doc:"Stop after N sync steps. By default, run until caught up.")
    $ Arg.(
        value & flag
        & info [ "no-progress" ]
            ~doc:"Use plain status lines instead of an animated progress bar."))

let commands =
  [
    command "overview" "Read or update an evidence-linked area overview"
      Term.(
        const overview $ profile $ demo $ required_opt "area"
        $ Arg.(value & flag & info [ "maintain" ]));
    command "capture" "Preview a task, then explicitly capture it in Dooit"
      Term.(
        const capture $ profile $ demo $ required_opt "service"
        $ required_opt "account" $ required_opt "email-id"
        $ required_opt "title"
        $ Arg.(value & opt (some string) None & info [ "deadline" ])
        $ Arg.(value & opt (some string) None & info [ "area" ])
        $ required_opt "dooit-root"
        $ Arg.(value & opt (some int) None & info [ "confirm-revision" ]));
    command "classify" "Classify changed inputs with the configured model"
      Term.(
        const classify $ profile $ demo
        $ Arg.(
            value & flag
            & info [ "all" ]
                ~doc:
                  "Classify all pending local emails. Skip current results and \
                   resume saved progress. Overrides the profile batch limit.")
        $ Arg.(
            value & flag
            & info [ "no-progress" ]
                ~doc:
                  "Use plain status lines instead of an animated progress bar.")
        $ jobs
        $ Arg.(
            value & flag
            & info [ "refresh-feedback" ]
                ~doc:
                  "Revisit completed mail and reclassify only when the \
                   relevant feedback has changed."));
    command "embed" "Index source passages or query their vector index"
      Term.(const embedding $ profile $ demo $ query);
    command "init" "Create a local profile"
      Term.(
        const init $ profile
        $ Arg.(value & opt string "personal" & info [ "jmap-profile" ]));
    command "inspect" "Inspect local corpus, proposals and jobs"
      Term.(const inspect $ profile $ demo $ query);
    command "discover" "Propose areas with the reproducible lexical baseline"
      Term.(const discover $ profile $ demo $ cutoff);
    command "evaluate" "Report baseline diagnostics on reviewed examples"
      Term.(const evaluate $ profile $ demo $ cutoff);
    command "sync" "Sync the requested mail interval to completion" sync_term;
    command "learn" "Alias for sync" sync_term;
    command "watch" "Keep syncing new mail and configured classifications"
      Term.(
        const (fun p d j -> learn p d "1y" true None false j)
        $ profile $ demo $ jobs);
    command "approve" "Approve a proposed area"
      Term.(
        const approve $ profile $ demo
        $ Arg.(required & pos 0 (some string) None & info [] ~docv:"AREA")
        $ explain);
    command "undo" "Reverse the latest local review decision"
      Term.(const undo $ profile $ demo $ explain);
    command "setup" "Configure JMAP profiles in the Bonsai terminal form"
      Term.(const (fun profile -> review profile false true) $ profile);
    command "review" "Open the Bonsai terminal workspace"
      Term.(
        const review $ profile $ demo
        $ Arg.(value & flag & info [ "setup" ] ~doc:"Open connection profiles"));
  ]

let () = exit (Cmd.eval (Cmd.group (Cmd.info "siftbot") commands))
