(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module W = Tangled.Watch

let repos_arg =
  Arg.(
    value & pos_all string []
    & info [] ~docv:"REPO"
        ~doc:
          "Watch REPO, as $(i,owner/name) or a name of your own. Without any, \
           every repository of the logged-in account that has a spindle.")

let interval =
  Arg.(
    value & opt float 15.
    & info [ "interval" ] ~docv:"SECONDS"
        ~doc:"Poll each repository's spindle every SECONDS.")

let commit =
  Common.optional [ "commit"; "sha" ] "SHA"
    "Report only pipelines for commits starting with SHA. $(b,HEAD) is the \
     checked-out commit."

let until_finished =
  Arg.(
    value & flag
    & info [ "until-finished" ]
        ~doc:
          "Exit once a reported pipeline finishes, successfully only if it \
           passed. With $(b,--commit), wait for that commit's pipeline to \
           start first.")

let once =
  Arg.(
    value & flag
    & info [ "once" ]
        ~doc:
          "Poll once, report the pipelines running now, and exit, as a \
           snapshot.")

(* A repository is named as people write it, by handle where it has one. *)
let display handles (r : Api.repository) =
  Option.value ~default:r.owner (Hashtbl.find_opt handles r.owner)
  ^ "/" ^ r.name

let time () =
  let t = Ptime_clock.now () in
  Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 t

(* A word is quoted for a shell only when it needs to be. *)
let shell_word s =
  if
    s <> ""
    && String.for_all
         (function
           | 'A' .. 'Z'
           | 'a' .. 'z'
           | '0' .. '9'
           | '_' | '.' | ':' | '+' | '/' | '-' | '@' ->
               true
           | _ -> false)
         s
  then s
  else Filename.quote s

(* What an agent can run next about a pipeline or one of its workflows. *)
let logs_command name (p : W.pipeline) workflow =
  Printf.sprintf "tangled pipeline logs -r %s %s%s" (shell_word name) p.id
    (match workflow with Some w -> " -w " ^ shell_word w | None -> "")

let retry_command name (p : W.pipeline) =
  Printf.sprintf "tangled pipeline retry -r %s %s" (shell_word name) p.id

let approve_command name (p : W.pipeline) =
  Printf.sprintf "tangled pipeline approve -r %s %s" (shell_word name) p.id

let failed (w : W.workflow) =
  match w.status with "failed" | "timeout" | "cancelled" -> true | _ -> false

let tally (p : W.pipeline) =
  let n s =
    List.length (List.filter (fun (w : W.workflow) -> w.status = s) p.workflows)
  in
  ( n "success",
    List.length (List.filter failed p.workflows),
    List.length p.workflows )

let next name (p : W.pipeline) = function
  | `Workflow (w : W.workflow) when failed w ->
      [ logs_command name p (Some w.name) ]
  | `Workflow w when w.status = "blocked" -> [ approve_command name p ]
  | `Finished when W.status p <> "success" ->
      List.filter_map
        (fun (w : W.workflow) ->
          if failed w then Some (logs_command name p (Some w.name)) else None)
        p.workflows
      @ [ retry_command name p ]
  | _ -> []

let json_event ~name ~kind (p : W.pipeline) ?workflow ?previous next =
  let opt k = function Some v -> [ (k, Common.string v) ] | None -> [] in
  let passed, failures, total = tally p in
  Common.object_
    ([
       ("time", Common.string (time ()));
       ("event", Common.string kind);
       ("repo", Common.string name);
       ("repo_did", Common.string p.repo.did);
       ("pipeline", Common.string p.id);
       ("commit", Common.string p.commit);
       ("trigger", Common.string (W.trigger_kind p));
     ]
    @ opt "ref" (W.trigger_ref p)
    @ opt "pull" (W.pull p)
    @ [ ("status", Common.string (W.status p)) ]
    @ (match workflow with
      | Some (w : W.workflow) ->
          [
            ("workflow", Common.string w.name);
            ("workflow_status", Common.string w.status);
          ]
          @ opt "previous" previous @ opt "error" w.error
      | None -> [])
    @ [
        ("passed", Jsont.Json.int passed);
        ("failed", Jsont.Json.int failures);
        ("workflows", Jsont.Json.int total);
        ("url", Common.string (W.web_url p));
        ("next", Jsont.Json.list (List.map Common.string next));
      ])

let print_line line =
  print_endline line;
  flush stdout

let report ~json ~name event =
  let hint commands =
    if not json then List.iter (fun c -> print_line ("  next: " ^ c)) commands
  in
  let emit kind p ?workflow ?previous text next =
    if json then
      print_line
        (match
           Jsont_bytesrw.encode_string Jsont.json
             (json_event ~name ~kind p ?workflow ?previous next)
         with
        | Ok s -> s
        | Error e -> failwith e)
    else (
      print_line (Printf.sprintf "%s %s %s %s" (time ()) name p.W.id text);
      hint next)
  in
  match event with
  | W.Started p ->
      emit "started" p
        (Printf.sprintf "started %s%s %s, %s: %s" (W.trigger_kind p)
           (Option.fold ~none:"" ~some:(fun r -> " " ^ r) (W.trigger_ref p))
           (Ui.short_sha p.commit) (W.status p)
           (String.concat ", "
              (List.map
                 (fun (w : W.workflow) -> w.name ^ " " ^ w.status)
                 p.workflows)))
        []
  | W.Changed (p, changes) ->
      List.iter
        (fun ({ workflow = w; previous } : W.change) ->
          emit "workflow" p ~workflow:w ?previous
            (Printf.sprintf "%s %s%s%s" w.name
               (Option.fold ~none:"added, "
                  ~some:(fun s -> s ^ " -> ")
                  previous)
               w.status
               (Option.fold ~none:"" ~some:(fun e -> ": " ^ e) w.error))
            (next name p (`Workflow w)))
        changes
  | W.Finished p ->
      let passed, _, total = tally p in
      emit "finished" p
        (Printf.sprintf "finished %s, %d of %d passed, %s" (W.status p) passed
           total (W.web_url p))
        (next name p `Finished)

let cmd =
  let action repos interval commit until_finished once json =
    Common.run (fun env ->
        Common.with_api env @@ fun api ->
        let handles = Hashtbl.create 8 in
        let named () =
          match repos with
          | [] ->
              if not (Api.is_logged_in api) then
                failwith
                  "Log in, or name the repositories to watch, as owner/name.";
              let me = Api.get_did api in
              List.filter_map
                (fun (slug, _) ->
                  match Api.resolve_repo api (me ^ "/" ^ slug) with
                  | r when r.manifest.spindle <> None -> Some r
                  | _ -> None
                  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
                  | exception e ->
                      Logs.warn (fun m ->
                          m "Skipping %s: %s" slug (Printexc.to_string e));
                      None)
                (Api.list_declarations api ())
          | specs -> List.map (fun s -> Api.resolve_repo api s) specs
        in
        let watched = ref (named ()) in
        if !watched = [] then
          failwith "There are no repositories with CI to watch.";
        List.iter
          (fun (r : Api.repository) ->
            if not (Hashtbl.mem handles r.owner) then
              Option.iter
                (Hashtbl.replace handles r.owner)
                (Api.handle_of_did api r.owner))
          !watched;
        let commit =
          match commit with
          | Some "HEAD" -> (
              match Local.head () with
              | Some h -> Some h
              | None -> failwith "This directory has no checked-out commit.")
          | c -> c
        in
        let wanted (p : W.pipeline) =
          match commit with
          | Some c -> String.starts_with ~prefix:c p.commit
          | None -> true
        in
        let watcher = W.v ~interval ~clock:env#clock api in
        let on_error (r : Api.repository) e =
          Logs.warn (fun m ->
              m "Could not poll %s: %s" (display handles r)
                (Printexc.to_string e))
        in
        let exception Done of W.pipeline in
        let f event =
          let p =
            match event with
            | W.Started p | W.Changed (p, _) | W.Finished p -> p
          in
          if wanted p then (
            report ~json ~name:(display handles p.repo) event;
            match event with
            | W.Finished p when until_finished -> raise (Done p)
            | _ -> ())
        in
        if once then W.poll watcher ~repos:!watched ~on_error f
        else
          let polls = ref 0 in
          let repos () =
            (* The account's repositories are listed again now and then, to
               pick up new ones. *)
            incr polls;
            (if repos = [] && !polls mod 40 = 0 then
               try watched := named () with
               | Eio.Cancel.Cancelled _ as e -> raise e
               | e ->
                   Logs.warn (fun m ->
                       m "Could not list repositories: %s"
                         (Printexc.to_string e)));
            !watched
          in
          match W.run watcher ~repos ~on_error f with
          | () -> ()
          | exception Done p ->
              if W.status p <> "success" then
                failwith
                  (Printf.sprintf "Pipeline %s finished %s" p.id (W.status p)))
  in
  Common.cmd
    (Cmd.info "watch" ~doc:"Report pipelines as they start, change and finish."
       ~man:
         (Common.man
            [
              "Watches the repositories' spindles and prints a line for each \
               pipeline that starts, each workflow that changes status and \
               each pipeline that finishes. Pipelines that finished before the \
               watch began are not reported.";
              "Lines that need attention are followed by $(i,next:) lines with \
               the commands to run, such as the logs of a failed workflow or \
               approving a waiting pipeline. With $(b,--json), each event is \
               one JSON object per line, with those commands in $(i,next).";
              "Spindles publish no stream of changes, so the watch polls, and \
               a change shows up within $(b,--interval) seconds.";
            ]))
    Term.(
      term_result
        (const action $ repos_arg $ interval $ commit $ until_finished $ once
       $ Common.json_flag))
