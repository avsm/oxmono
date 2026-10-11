module M = Siftbot_model
open Cmdliner

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

let learn profile demo since watch =
  protect (fun () ->
      if since <> "1y" then
        invalid_arg "The supported relative interval is --since 1y";
      with_db profile demo (fun env sw db ->
          if demo then print_endline "Synthetic corpus ready."
          else
            let p = Siftbot.Profile.load profile in
            let remote = Siftbot.Profile.connect ~sw env p in
            let now = Unix.gettimeofday () in
            let timestamp seconds =
              Ptime.of_float_s seconds |> Option.get
              |> Ptime.to_rfc3339 ~tz_offset_s:0
            in
            Siftbot.Import.start db remote
              ~since:(timestamp (now -. (365. *. 86400.)))
              ~until:(timestamp now);
            let first, last = Option.get (Siftbot.Import.interval db remote) in
            Printf.printf
              "Account %s; interval %s to %s; at most %d sync steps and %d \
               classifications; body limit %d bytes; chat %s.\n\
               %!"
              (M.sanitize remote.account)
              first last p.max_steps p.max_steps p.body_limit
              (Option.value ~default:"none" p.chat_model);
            let remaining = ref p.max_steps in
            let classifier =
              if watch && p.chat_model <> None then
                Some (Siftbot.Assistant.completion ~sw p)
              else None
            in
            let rec loop n =
              if n = 0 then
                print_endline "Work limit reached. Re-run to resume."
              else
                let done_ = Siftbot.Import.step db remote in
                (match classifier with
                | Some (model, complete) when done_ && !remaining > 0 ->
                    let count =
                      Siftbot.Classifier.run db ~complete ~model
                        ~max_messages:(min 10 !remaining)
                    in
                    remaining := !remaining - count;
                    Printf.printf "Stored %d changed classifications.\n%!" count
                | _ -> ());
                if done_ && not watch then
                  print_endline "Import and catch-up complete."
                else (
                  if done_ then (
                    print_endline "Change checkpoint committed.";
                    Eio.Time.sleep (Eio.Stdenv.clock env) 30.);
                  loop (n - 1))
            in
            loop p.max_steps))

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

let classify profile demo =
  protect (fun () ->
      if demo then invalid_arg "Demo mode does not use model credentials";
      with_db profile false (fun _ sw db ->
          let p = Siftbot.Profile.load profile in
          let model, complete = Siftbot.Assistant.completion ~sw p in
          Printf.printf "Stored suggestions for %d messages.\n"
            (Siftbot.Classifier.run db ~complete ~model
               ~max_messages:p.max_steps)))

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
            let model, complete = Siftbot.Assistant.completion ~sw p in
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
      Term.(const classify $ profile $ demo);
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
    command "learn" "Import a fixed year and catch up read-only"
      Term.(
        const (fun p d s -> learn p d s false)
        $ profile $ demo
        $ Arg.(value & opt string "1y" & info [ "since" ]));
    command "watch" "Follow changes with durable bounded work"
      Term.(const (fun p d -> learn p d "1y" true) $ profile $ demo);
    command "approve" "Approve a proposed area"
      Term.(
        const approve $ profile $ demo
        $ Arg.(required & pos 0 (some string) None & info [] ~docv:"AREA")
        $ explain);
    command "undo" "Reverse the latest local review decision"
      Term.(const undo $ profile $ demo $ explain);
    command "review" "Open the Bonsai terminal workspace"
      Term.(
        const review $ profile $ demo
        $ Arg.(value & flag & info [ "setup" ] ~doc:"Open connection profiles"));
  ]

let () = exit (Cmd.eval (Cmd.group (Cmd.info "siftbot") commands))
