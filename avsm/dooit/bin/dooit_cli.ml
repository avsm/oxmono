(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Dooit
open Common
open Cmdliner
module C = Console

let accent = C.Style.(bold + fg (C.Color.rgb 0x4b 0xc9 0xc3))
let muted = C.Style.fg C.Color.bright_black
let styled style value = C.Span.sanitize (C.Span.styled style value)
let opt names doc = Arg.(value & opt (some string) None & info names ~doc)
let flag names doc = Arg.(value & flag & info names ~doc)
let pos n name = Arg.(required & pos n (some string) None & info [] ~docv:name)

let tags =
  Arg.(
    value & opt_all string []
    & info [ "tag" ] ~doc:"Freeform tag; repeat for multiple tags.")

let config_arg =
  opt [ "config" ]
    "TOML configuration path; defaults to the dooit XDG config directory."

let root_arg = opt [ "root" ] "Local note store; overrides the TOML root."
let json_arg = flag [ "json" ] "Print versioned JSON."
let profile_arg = opt [ "jmap-profile" ] "Shared JMAP credential profile."
let account_arg = opt [ "account" ] "JMAP account ID."
let service_arg = opt [ "service" ] "JMAP service identity URL."
let title_arg = opt [ "title" ] "Task title; defaults to the email subject."

let report_arg =
  opt [ "report" ] "New directory for the sync plan and revision snapshots."

let expected_arg =
  opt [ "if-revision" ]
    "Expected SHA-256 revision; use absent for a missing note."

let expected = function
  | Some "absent" -> None
  | Some s -> Some s
  | None -> fail "--if-revision is required"

let output json note =
  let details =
    Option.fold ~none:"" ~some:(fun date -> "  due " ^ date) (Doc.due note)
    ^
    if Doc.tags note = [] then ""
    else "  #" ^ String.concat " #" (Doc.tags note)
  in
  if json then print_endline (json_string ~pretty:true (Doc.public note))
  else if Console_eio.is_tty () then
    let finished = Doc.status note = "done" || Doc.status note = "cancelled" in
    Fmt.pr "%a  %a  %a%a@." C.Span.pp
      (styled muted (Doc.id note))
      C.Span.pp
      (styled
         (if finished then muted else accent)
         ("[" ^ Doc.status note ^ "]"))
      C.Span.pp
      (if finished then styled muted (Doc.title note)
       else C.Span.sanitize (C.Span.text (Doc.title note)))
      C.Span.pp (styled muted details)
  else
    Printf.printf "%s  [%s] %s%s\n" (Doc.id note) (Doc.status note)
      (Doc.title note) details

let outputs json notes =
  if json then
    print_endline
      (json_string ~pretty:true
         (obj
            [
              ("schema", str "dooit.list/v1");
              ("notes", arr (List.map Doc.public notes));
            ]))
  else if Console_eio.is_tty () then (
    Fmt.pr "%a  %a@.@." C.Span.pp (styled accent "Tasks") C.Span.pp
      (styled muted (Printf.sprintf "%d total" (List.length notes)));
    List.iter (output false) notes)
  else if notes = [] then print_endline "No tasks found."
  else List.iter (output false) notes

let diagnostics errors =
  List.iter
    (fun (name, s) -> Printf.eprintf "Needs review: %s: %s\n" name s)
    errors

let guard f =
  try `Ok (f ()) with
  | Error s -> `Error (false, s)
  | Fetch_dav.Http_error e ->
      `Error (false, Printf.sprintf "WebDAV HTTP %d" e.status)
  | Fetch_dav.Protocol_error s -> `Error (false, "WebDAV protocol: " ^ s)
  | Jmap_eio.Client.Jmap_client_error _ -> `Error (false, "JMAP request failed")
  | Yamlrw.Yamlrw_error _ ->
      `Error (false, "invalid YAML; original file retained")
  | Unix.Unix_error (e, _, p) -> `Error (false, Unix.error_message e ^ ": " ^ p)
  | Sys_error s -> `Error (false, s)
  | Eio.Io _ -> `Error (false, "network or filesystem I/O failed")
  | Invalid_argument _ ->
      `Error (false, "invalid argument or refused request scope")

let context environment =
  Term.(
    ret
      (const (fun path root ->
           guard (fun () ->
               Eio_unix.run_in_systhread (fun () ->
                   Config.load ~fs:environment#fs ?path ?root ())))
      $ config_arg $ root_arg))

let command name doc term = Cmd.v (Cmd.info name ~doc) Term.(ret term)

let remote ~sw config ~readonly =
  let fetch =
    Fetch_curl.v ~sw ~timeout:(Duration.of_sec 60)
      ~max_response:(16 * 1024 * 1024)
      ()
  in
  Remote.make ~fetch ~config:(Config.require_webdav config) ~readonly

let sync_output json actions =
  if json then print_endline (json_string ~pretty:true (Sync.summary actions))
  else if actions = [] then print_endline "Already synchronized."
  else
    List.iter
      (fun a ->
        Printf.printf "%-12s %s%s\n" a.Sync.kind a.id
          (if a.detail = "" then "" else ": " ^ a.detail))
      actions

let backend_arg =
  Arg.(
    value
    & opt
        (some
           (enum
              [
                ("git", Config.Git);
                ("webdav", Config.Webdav);
                ("local", Config.Local);
              ]))
        None
    & info [ "backend" ] ~docv:"BACKEND"
        ~doc:"Backend for a new configuration: git (default), webdav or local.")

let result c ~action ~json message =
  if json then
    print_endline
      (json_string ~pretty:true
         (obj
            [
              ("schema", str "dooit.operation/v1");
              ("action", str action);
              ("backend", str (Config.backend_name c.Config.backend));
              ("root", str c.root);
              ("message", str message);
            ]))
  else print_endline message

let open_store c =
  if not (exists (Filename.concat c.Config.root "store.json")) then
    fail "no Dooit store at %s; run dooit init or dooit clone" c.root;
  Store.open_ c.root

let store c = Eio_unix.run_in_systhread (fun () -> open_store c)

let config_output c ~json =
  let git =
    obj [ ("remote", str c.Config.git.remote); ("branch", str c.git.branch) ]
  in
  let webdav =
    match c.webdav with
    | None -> `Null
    | Some w ->
        obj [ ("collection", str w.collection); ("username", str w.username) ]
  in
  if json then
    print_endline
      (json_string ~pretty:true
         (obj
            [
              ("schema", str "dooit.config/v1");
              ("path", str c.path);
              ("root", str c.root);
              ("backend", str (Config.backend_name c.backend));
              ("git", git);
              ("webdav", webdav);
            ]))
  else (
    Printf.printf "Config:  %s\nStore:   %s\nBackend: %s\n" c.path c.root
      (Config.backend_name c.backend);
    match c.backend with
    | Config.Git ->
        Printf.printf "Remote:  %s\nBranch:  %s\n" c.git.remote c.git.branch
    | Config.Webdav ->
        Printf.printf "Remote:  %s\n" (Config.require_webdav c).collection
    | Config.Local ->
        print_endline "Tasks stay local. No remote synchronization.")

let editor store id =
  let original = Store.get store id in
  Store.ensure_state store;
  let path, oc =
    Filename.open_temp_file ~temp_dir:(Store.state_dir store) "edit-" ".md"
  in
  output_string oc original.raw;
  close_out oc;
  let editor = Option.value (Sys.getenv_opt "EDITOR") ~default:"vi" in
  let pid =
    Unix.create_process "/bin/sh"
      [| "sh"; "-c"; editor ^ " " ^ Filename.quote path |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  (match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | _ -> fail "editor failed; draft retained at %s" path);
  try
    let edited = Doc.parse (read path) in
    Doc.immutable original edited;
    let current = Store.get store id in
    let merged =
      get_ok (Merge.run ~base:original ~local:edited ~remote:current)
    in
    let result =
      Client.apply (Client.local store) ~policy:Local_only
        ~check:Eio.Fiber.check ~id
        ~expected:(Some (Doc.revision current))
        ~operation:(new_uuid ()) ~payload:edited.raw
        (fun _ -> merged)
      |> Client.saved
    in
    Unix.unlink path;
    result
  with Error s -> fail "%s; editor draft retained at %s" s path

let commands environment =
  let ctx = context environment in
  let process_mgr = environment#process_mgr in
  let prepare_git c =
    let git = Config.require_git c in
    let local = store c in
    ignore (Git.setup ~process_mgr local git);
    (local, git)
  in
  let direction action run =
    command action
      (if action = "pull" then "Git: fetch and fast-forward clean local tasks."
       else "Git: commit local tasks and push them to the configured remote.")
      Term.(
        const (fun c json ->
            guard (fun () ->
                let local, git = prepare_git c in
                result c ~action ~json (run ~process_mgr local git)))
        $ ctx $ json_arg)
  in
  let init =
    command "init"
      "Create the configuration, task store and selected backend locally."
      Term.(
        const (fun path root backend json ->
            guard (fun () ->
                let defaults =
                  Config.defaults ~fs:environment#fs ?path ?root ()
                in
                let present = exists defaults.path in
                let c =
                  if present then
                    Config.load ~fs:environment#fs ~path:defaults.path ?root ()
                  else
                    Config.defaults ~fs:environment#fs ?path ?root ?backend ()
                in
                Option.iter
                  (fun selected ->
                    if selected <> c.Config.backend then
                      fail "backend is already %s; edit %s to change it"
                        (Config.backend_name c.backend)
                        c.path)
                  backend;
                let local =
                  Eio_unix.run_in_systhread (fun () ->
                      if exists (Filename.concat c.root "store.json") then
                        Store.open_ c.root
                      else (
                        if
                          exists c.root
                          && Array.length (Sys.readdir c.root) <> 0
                        then
                          fail
                            "choose an empty directory or an existing Dooit \
                             store";
                        Store.init c.root))
                in
                (match c.backend with
                | Config.Git -> ignore (Git.setup ~process_mgr local c.git)
                | Config.Webdav | Config.Local -> ());
                if not present then
                  Eio_unix.run_in_systhread (fun () -> Config.save_new c);
                result c ~action:"init" ~json
                  (Printf.sprintf "Dooit ready at %s (%s).%s" local.root
                     (Config.backend_name c.backend)
                     (if c.backend = Config.Local then ""
                      else " Run dooit sync to synchronize."))))
        $ config_arg $ root_arg $ backend_arg $ json_arg)
  in
  let status =
    command "status"
      "Show local setup, task counts and cached Git state without network \
       access."
      Term.(
        const (fun c json ->
            guard (fun () ->
                let local, notes, errors =
                  Eio_unix.run_in_systhread (fun () ->
                      if
                        not
                          (exists (Filename.concat c.Config.root "store.json"))
                      then (None, [], [])
                      else
                        let local = Store.open_ c.root in
                        let notes, errors = Store.scan local in
                        (Some local, notes, errors))
                in
                let git =
                  match (c.backend, local) with
                  | Config.Git, Some local ->
                      Some (Git.status ~process_mgr local c.git)
                  | _ -> None
                in
                let live =
                  List.filter (fun note -> not (Doc.deleted note)) notes
                in
                let number n = `Float (float_of_int n) in
                let counts =
                  ("total", number (List.length live))
                  :: ("deleted", number (List.length notes - List.length live))
                  :: List.map
                       (fun status ->
                         ( status,
                           number
                             (List.length
                                (List.filter
                                   (fun n -> Doc.status n = status)
                                   live)) ))
                       Doc.statuses
                in
                if json then
                  print_endline
                    (json_string ~pretty:true
                       (obj
                          [
                            ("schema", str "dooit.status/v1");
                            ("backend", str (Config.backend_name c.backend));
                            ("root", str c.root);
                            ("config", str c.path);
                            ("initialized", `Bool (Option.is_some local));
                            ( "store_id",
                              match local with
                              | None -> `Null
                              | Some s -> str s.id );
                            ("tasks", obj counts);
                            ( "git",
                              match git with
                              | None -> `Null
                              | Some g ->
                                  obj
                                    [
                                      ("initialized", `Bool g.initialized);
                                      ("branch", opt_string g.branch);
                                      ("dirty", `Bool g.dirty);
                                      ( "ahead",
                                        Option.fold ~none:`Null ~some:number
                                          g.ahead );
                                      ( "behind",
                                        Option.fold ~none:`Null ~some:number
                                          g.behind );
                                      ( "conflicts",
                                        arr (List.map str g.conflicts) );
                                    ] );
                            ( "errors",
                              arr
                                (List.map
                                   (fun (name, message) ->
                                     obj
                                       [
                                         ("file", str name);
                                         ("message", str message);
                                       ])
                                   errors) );
                          ]))
                else (
                  config_output c ~json:false;
                  match local with
                  | None ->
                      print_endline
                        "Store is not initialized. Run dooit init or dooit \
                         clone."
                  | Some _ ->
                      Printf.printf "Tasks:   %d (%d deleted)\n"
                        (List.length live)
                        (List.length notes - List.length live);
                      Option.iter
                        (fun g ->
                          if not g.Git.initialized then
                            print_endline
                              "Git is not initialized. Run dooit init."
                          else (
                            Printf.printf "Git:     %s (%s)\n"
                              (Option.value g.branch ~default:"detached HEAD")
                              (if g.dirty then "local changes" else "clean");
                            (match (g.ahead, g.behind) with
                            | Some ahead, Some behind ->
                                Printf.printf
                                  "History: %d ahead, %d behind (cached; run \
                                   dooit sync to refresh)\n"
                                  ahead behind
                            | _ ->
                                print_endline
                                  "History: not fetched yet. Run dooit sync.");
                            List.iter
                              (Printf.printf "Conflict: %s\n")
                              g.conflicts))
                        git;
                      diagnostics errors);
                if errors <> [] then fail "some task files need review"))
        $ ctx $ json_arg)
  in
  let add =
    command "add" "Create a task locally. Run dooit sync to publish it."
      Term.(
        const (fun c title tags body_file due operation json ->
            guard (fun () ->
                let body = Option.fold ~none:"" ~some:read body_file in
                let store = open_store c in
                let operation = Option.value operation ~default:(new_uuid ()) in
                check_uuid operation;
                let id = uuid5 store.id ("create:" ^ operation) in
                let payload =
                  json_string
                    (obj
                       [
                         ("title", str title);
                         ("body", str body);
                         ("tags", arr (List.map str tags));
                         ("due", opt_string due);
                       ])
                in
                let d =
                  Client.apply (Client.local store) ~policy:Local_only
                    ~check:Eio.Fiber.check ~id ~expected:None ~operation
                    ~payload (fun _ ->
                      Doc.create ~id ~title ~tags ~body ?due ())
                  |> Client.saved
                in
                output json d))
        $ ctx $ pos 0 "TITLE" $ tags
        $ opt [ "body-file" ] "Markdown body file."
        $ opt [ "due" ] "Due date YYYY-MM-DD."
        $ opt [ "operation-id" ]
            "Idempotent task creation UUID for agents and retries."
        $ json_arg)
  in
  let show =
    command "show" "Read a task and its revision."
      Term.(
        const (fun c id json ->
            guard (fun () ->
                let d = Store.get (open_store c) id in
                if json then output true d
                else if Console_eio.is_tty () then (
                  Fmt.pr "%a@.@." C.Panel.pp
                    (C.Panel.lines
                       ~title:(styled accent (Doc.title d))
                       [ styled muted (Doc.id d); styled accent (Doc.status d) ]);
                  print_string d.raw)
                else print_string d.raw))
        $ ctx $ pos 0 "ID" $ json_arg)
  in
  let list_cmd =
    let query =
      Arg.(
        value & pos 0 string ""
        & info [] ~docv:"QUERY" ~doc:"Optional full-text search query.")
    in
    command "list"
      "List tasks, optionally searching their text and filtering metadata."
      Term.(
        const (fun c text tags status source all json ->
            guard (fun () ->
                Option.iter
                  (fun status ->
                    if not (List.mem status Doc.statuses) then
                      fail
                        "invalid status %s; expected open, active, blocked, \
                         done or cancelled"
                        status)
                  status;
                let notes, errors =
                  Eio_unix.run_in_systhread (fun () ->
                      Index.query ~text ~tags ?status ?source
                        ~include_deleted:all (open_store c))
                in
                outputs json notes;
                diagnostics errors;
                if errors <> [] then fail "some task files need review"))
        $ ctx $ query $ tags
        $ opt [ "status" ] "Filter task status."
        $ opt [ "source" ] "Filter link type."
        $ flag [ "include-deleted" ] "Include retained deletions."
        $ json_arg)
  in
  let edit =
    command "edit" "Edit a task using EDITOR with revision checks."
      Term.(
        const (fun c id json ->
            guard (fun () -> output json (editor (open_store c) id)))
        $ ctx $ pos 0 "ID" $ json_arg)
  in
  let patch =
    command "patch"
      "Apply a versioned JSON patch with revision and operation checks."
      Term.(
        const (fun c id revision operation path json ->
            guard (fun () ->
                let expected = expected revision in
                let operation =
                  match operation with
                  | Some s -> s
                  | None -> fail "--operation-id is required"
                in
                let path =
                  match path with
                  | Some s -> s
                  | None -> fail "--json-file is required"
                in
                let payload = read path in
                let patch = Common.json payload in
                let d =
                  Client.apply
                    (Client.local (store c))
                    ~policy:Local_only ~check:Eio.Fiber.check ~id ~expected
                    ~operation ~payload
                    (function
                      | Some n -> Doc.patch n patch
                      | None -> fail "task does not exist")
                  |> Client.saved
                in
                output json d))
        $ ctx $ pos 0 "ID" $ expected_arg
        $ opt [ "operation-id" ] "Idempotent operation UUID."
        $ opt [ "json-file" ] "Patch request file."
        $ json_arg)
  in
  let change name doc f =
    command name doc
      Term.(
        const (fun c id json ->
            guard (fun () ->
                output json
                  (Client.modify
                     (Client.local (store c))
                     ~policy:Local_only id f
                  |> Client.saved)))
        $ ctx $ pos 0 "ID" $ json_arg)
  in
  let done_ =
    change "done" "Mark a task done." (Doc.change "status" (Some (str "done")))
  in
  let delete =
    change "delete"
      "Hide a task while retaining its content for synchronization." (fun d ->
        if Doc.deleted d then d
        else Doc.change "deleted_at" (Some (str (now ()))) d)
  in
  let restore =
    change "restore" "Restore a retained deletion."
      (Doc.change "deleted_at" None)
  in
  let update =
    let repeated name doc =
      Arg.(value & opt_all string [] & info [ name ] ~doc)
    in
    command "update"
      "Change a task's title, status, due date, tags or body in one \
       revision-checked edit."
      Term.(
        const
          (fun
            c
            id
            title
            status
            due
            clear_due
            add_tags
            remove_tags
            body_file
            json
          ->
            guard (fun () ->
                if clear_due && Option.is_some due then
                  fail "use either --due or --clear-due";
                let op name value =
                  obj [ ("op", str name); ("value", value) ]
                in
                let optional name =
                  Option.fold ~none:[] ~some:(fun s -> [ op name (str s) ])
                in
                let operations =
                  optional "set_title" title
                  @ optional "set_status" status
                  @ optional "set_due" due
                  @ (if clear_due then [ op "set_due" `Null ] else [])
                  @ List.map (fun s -> op "add_tag" (str s)) add_tags
                  @ List.map (fun s -> op "remove_tag" (str s)) remove_tags
                  @ Option.fold ~none:[]
                      ~some:(fun path ->
                        [ op "replace_body" (str (read path)) ])
                      body_file
                in
                if operations = [] then
                  fail
                    "specify at least one change; see dooit task update --help";
                let patch =
                  obj
                    [
                      ("schema", str "dooit.patch/v1");
                      ("operations", arr operations);
                    ]
                in
                output json
                  (Client.modify
                     (Client.local (store c))
                     ~policy:Local_only id
                     (fun note -> Doc.patch note patch)
                  |> Client.saved)))
        $ ctx $ pos 0 "ID"
        $ opt [ "title" ] "New task title."
        $ opt [ "status" ]
            "Task status: open, active, blocked, done or cancelled."
        $ opt [ "due" ] "Due date YYYY-MM-DD."
        $ flag [ "clear-due" ] "Remove the due date."
        $ repeated "add-tag" "Add a tag; repeat for multiple tags."
        $ repeated "remove-tag" "Remove a tag; repeat for multiple tags."
        $ opt [ "body-file" ] "Replace the body with this Markdown file."
        $ json_arg)
  in
  let adopt =
    command "import" "Copy a valid task document into this store."
      Term.(
        const (fun c path json ->
            guard (fun () ->
                output json
                  (Client.add
                     (Client.local (store c))
                     ~policy:Local_only
                     (Doc.parse (read path))
                  |> Client.saved)))
        $ ctx $ pos 0 "PATH" $ json_arg)
  in
  let from_email =
    command "capture"
      "Capture an email as a task, or return existing linked tasks."
      Term.(
        const
          (fun
            c
            email
            profile
            account
            service
            title
            tags
            body_file
            offline
            new_task
            json
          ->
            guard (fun () ->
                let target, subject, hints, _ =
                  if offline then
                    let require name a b =
                      match (a, b) with
                      | Some x, _ | None, Some x -> x
                      | _ -> fail "offline capture requires %s" name
                    in
                    let service =
                      require "--service" service c.Config.jmap_service
                    and account = require "--account" account c.jmap_account in
                    ( Link.email ~service ~account ~email,
                      Option.value title ~default:"Email task",
                      obj [],
                      None )
                  else
                    Eio.Switch.run (fun sw ->
                        Email.lookup ~sw environment ~config:c ?profile ?account
                          ?service email)
                in
                let body = Option.fold ~none:"" ~some:read body_file in
                outputs json
                  (Client.capture ~new_task ~tags ~body ~hints
                     ~title:(Option.value title ~default:subject)
                     ~policy:Local_only
                     (Client.local (store c))
                     target)))
        $ ctx $ pos 0 "EMAIL_ID" $ profile_arg $ account_arg $ service_arg
        $ title_arg $ tags
        $ opt [ "body-file" ] "Markdown body or selected email excerpt."
        $ flag [ "offline" ]
            "Capture supplied source identity without accessing JMAP."
        $ flag [ "new" ] "Create an additional task for the same source."
        $ json_arg)
  in
  let for_email =
    command "find" "Find task links to an email without accessing the network."
      Term.(
        const (fun c email account service json ->
            guard (fun () ->
                let require name a b =
                  match (a, b) with
                  | Some x, _ | None, Some x -> x
                  | _ -> fail "specify %s" name
                in
                let target =
                  Link.email
                    ~service:(require "--service" service c.Config.jmap_service)
                    ~account:(require "--account" account c.jmap_account)
                    ~email
                in
                let notes, errors = Store.scan (open_store c) in
                outputs json (List.filter (Link.matches target) notes);
                diagnostics errors))
        $ ctx $ pos 0 "EMAIL_ID" $ account_arg $ service_arg $ json_arg)
  in
  let source_show =
    command "show"
      "Read the linked email's metadata and plain-text body via JMAP."
      Term.(
        const (fun c id profile json ->
            guard (fun () ->
                let note = Store.get (open_store c) id in
                let source =
                  match
                    List.find_opt
                      (fun l -> field "type" l = "jmap-email")
                      (items "links" note.meta)
                  with
                  | Some l -> get "target" l
                  | None -> fail "task has no JMAP source"
                in
                let target, title, hints, body =
                  Eio.Switch.run (fun sw ->
                      Email.lookup ~sw environment ~config:c ?profile
                        ~account:(field "account_id" source)
                        ~service:(field "service" source) ~fetch_body:true
                        (field "email_id" source))
                in
                if json then
                  print_endline
                    (json_string ~pretty:true
                       (obj
                          [
                            ("schema", str "dooit.source/v1");
                            ("target", target);
                            ("hints", hints);
                            ("body", opt_string body);
                          ]))
                else
                  Printf.printf "%s\n%s\n\n%s\n" title
                    (json_string ~pretty:true target)
                    (Option.value body ~default:"")))
        $ ctx $ pos 0 "ID" $ profile_arg $ json_arg)
  in
  let with_client c f =
    Eio.Switch.run (fun sw ->
        let client =
          Client.of_config ~process_mgr ~remote:(remote ~sw c) (store c) c
        in
        f client)
  in
  let show_sync c ~json = function
    | Client.Webdav_synced actions ->
        sync_output json actions;
        if
          List.exists
            (fun a -> a.Sync.kind = "conflict" || a.kind = "invalid")
            actions
        then fail "some tasks need review; see dooit maintenance conflicts"
    | result_ ->
        result c ~action:"sync" ~json
          (String.concat "\n" (Client.sync_lines result_))
  in
  let webdav_sync c ~dry ~report ~json =
    ignore (Config.require_webdav c);
    with_client c (fun client ->
        show_sync c ~json (Client.sync ?report ~dry_run:dry client))
  in
  let sync =
    command "sync"
      "Synchronize with the configured backend: Git commit/pull/push or \
       two-way WebDAV."
      Term.(
        const (fun c json ->
            guard (fun () ->
                with_client c (fun client ->
                    show_sync c ~json (Client.sync ~dry_run:false client))))
        $ ctx $ json_arg)
  in
  let preview =
    command "preview"
      "WebDAV: preview synchronization without applying changes."
      Term.(
        const (fun c report json ->
            guard (fun () ->
                ignore (Config.require_webdav c);
                webdav_sync c ~dry:true ~report ~json))
        $ ctx $ report_arg $ json_arg)
  in
  let clone =
    command "clone"
      "Download an existing store using the configured backend, preserving its \
       identity."
      Term.(
        const (fun path root json ->
            guard (fun () ->
                let defaults =
                  Config.defaults ~fs:environment#fs ?path ?root ()
                in
                let present = exists defaults.path in
                let c =
                  if present then
                    Config.load ~fs:environment#fs ~path:defaults.path ?root ()
                  else defaults
                in
                let local =
                  match c.Config.backend with
                  | Config.Git -> Git.clone ~process_mgr ~root:c.root c.git
                  | Config.Webdav ->
                      Eio.Switch.run (fun sw ->
                          Sync.clone ~root:c.root (remote ~sw c ~readonly:true))
                  | Config.Local ->
                      fail
                        "local backend has no remote to clone; use dooit init"
                in
                if not present then
                  Eio_unix.run_in_systhread (fun () -> Config.save_new c);
                result c ~action:"clone" ~json
                  ("Cloned Dooit store into " ^ local.root)))
        $ config_arg $ root_arg $ json_arg)
  in
  let conflicts =
    command "conflicts"
      "List retained local conflicts and unresolved Git index entries."
      Term.(
        const (fun c json ->
            guard (fun () ->
                let local = store c in
                let entries =
                  match c.Config.backend with
                  | Config.Git ->
                      (Git.status ~process_mgr local c.git).conflicts
                  | Config.Webdav | Config.Local ->
                      Eio_unix.run_in_systhread (fun () ->
                          Store.conflicts local)
                in
                if json then
                  print_endline
                    (json_string ~pretty:true
                       (obj
                          [
                            ("schema", str "dooit.conflicts/v1");
                            ("backend", str (Config.backend_name c.backend));
                            ("conflicts", arr (List.map str entries));
                          ]))
                else if entries = [] then
                  print_endline "No unresolved local conflicts."
                else List.iter print_endline entries))
        $ ctx $ json_arg)
  in
  let recover =
    command "recover"
      "Finish interrupted local writes whose revisions still match."
      Term.(
        const (fun c json ->
            guard (fun () ->
                let recovered =
                  Eio_unix.run_in_systhread (fun () ->
                      Store.recover (open_store c))
                in
                if json then
                  print_endline
                    (json_string ~pretty:true
                       (obj
                          [
                            ("schema", str "dooit.recovery/v1");
                            ( "operations",
                              arr
                                (List.map
                                   (fun (id, status) ->
                                     obj
                                       [
                                         ("id", str id); ("status", str status);
                                       ])
                                   recovered) );
                          ]))
                else if recovered = [] then
                  print_endline "No interrupted writes."
                else
                  List.iter
                    (fun (id, status) -> Printf.printf "%s %s\n" id status)
                    recovered))
        $ ctx $ json_arg)
  in
  let resolve =
    command "resolve"
      "WebDAV: apply a reviewed conflict resolution with revision checks."
      Term.(
        const (fun c id revision file json ->
            guard (fun () ->
                let file =
                  match file with
                  | Some f -> f
                  | None -> fail "--file is required"
                in
                Eio.Switch.run (fun sw ->
                    let a =
                      Sync.resolve (open_store c)
                        (remote ~sw c ~readonly:false)
                        ~id ~expected:(expected revision) ~file
                    in
                    sync_output json [ a ])))
        $ ctx $ pos 0 "ID" $ expected_arg
        $ opt [ "file" ] "Resolved Markdown task."
        $ json_arg)
  in
  let config_path =
    command "path"
      "Print the resolved configuration path without creating directories."
      Term.(
        const (fun path ->
            guard (fun () ->
                print_endline (Config.defaults ~fs:environment#fs ?path ()).path))
        $ config_arg)
  in
  let config_show =
    command "show" "Show effective backend settings with credentials omitted."
      Term.(
        const (fun c json -> guard (fun () -> config_output c ~json))
        $ ctx $ json_arg)
  in
  [
    init;
    clone;
    status;
    sync;
    direction "pull" Git.pull;
    direction "push" Git.push;
    Cmd.group
      (Cmd.info "task"
         ~doc:"Create, find and update local tasks. Run sync to publish edits.")
      [
        add; list_cmd; show; edit; update; done_; delete; restore; adopt; patch;
      ];
    Cmd.group
      (Cmd.info "email"
         ~doc:"Capture email tasks and inspect their JMAP sources.")
      [ from_email; for_email; source_show ];
    Cmd.group
      (Cmd.info "maintenance"
         ~doc:
           "Inspect conflicts, preview WebDAV sync and recover interrupted \
            writes.")
      [ conflicts; resolve; recover; preview ];
    Cmd.group
      (Cmd.info "config"
         ~doc:"Inspect shared configuration. Run init for first-time setup.")
      [ config_path; config_show ];
  ]

let () =
  Console_eio.setup ();
  Eio_main.run (fun environment ->
      let cmd =
        Cmd.group
          (Cmd.info "dooit" ~version:"0.1.0"
             ~doc:"Tasks with one configured synchronization backend."
             ~man:
               [
                 `S Manpage.s_description;
                 `P
                   "Start with $(b,dooit init), add a task with $(b,dooit task \
                    add TITLE), then run $(b,dooit sync). On another machine, \
                    use $(b,dooit clone).";
                 `P
                   "Task edits are local. Sync publishes them using the \
                    configured Git or WebDAV backend. Git is the default. Pull \
                    and push are optional Git-only commands.";
                 `S Manpage.s_examples;
                 `Pre
                   "dooit init\n\
                    dooit task add \"Reply to Alice\" --tag inbox\n\
                    dooit sync\n\
                    dooit task list --status open\n\
                    dooit task update ID --status blocked\n\
                    dooit status";
                 `S Manpage.s_see_also;
                 `P "dooit COMMAND --help, dooit task COMMAND --help";
               ])
          (commands environment)
      in
      exit (Cmd.eval cmd))
