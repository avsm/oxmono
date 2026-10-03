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

let context environment =
  Term.(
    const (fun path root -> Config.load ~fs:environment#fs ?path ?root ())
    $ config_arg $ root_arg)

let output json note =
  if json then print_endline (json_string ~pretty:true (Doc.public note))
  else if Console_eio.is_tty () then
    Fmt.pr "%a  %a  %a%a@."
      C.Span.pp (styled muted (Doc.id note))
      C.Span.pp (styled accent ("[" ^ Doc.status note ^ "]"))
      C.Span.pp (C.Span.sanitize (C.Span.text (Doc.title note)))
      C.Span.pp
        (styled muted
           (if Doc.tags note = [] then ""
            else "  #" ^ String.concat " #" (Doc.tags note)))
  else
    Printf.printf "%s  [%s] %s%s\n" (Doc.id note) (Doc.status note)
      (Doc.title note)
      (if Doc.tags note = [] then ""
       else "  #" ^ String.concat " #" (Doc.tags note))

let outputs json notes =
  if json then
    print_endline
      (json_string ~pretty:true
         (obj
            [
              ("schema", str "dooit.list/v1");
              ("notes", arr (List.map Doc.public notes));
            ]))
  else if Console_eio.is_tty () then begin
    Fmt.pr "%a  %a@.@." C.Span.pp (styled accent "Tasks")
      C.Span.pp (styled muted (Printf.sprintf "%d total" (List.length notes)));
    List.iter (output false) notes
  end else List.iter (output false) notes

let diagnostics errors =
  List.iter
    (fun (name, s) -> Printf.eprintf "Needs review: %s: %s\n" name s)
    errors

let guard f =
  try
    f ();
    `Ok ()
  with
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
  else
    List.iter
      (fun a ->
        Printf.printf "%-12s %s%s\n" a.Sync.kind a.id
          (if a.detail = "" then "" else ": " ^ a.detail))
      actions

let config_example root =
  Printf.sprintf
    {|# Dooit configuration. Paths may use ~/ or be relative to this file.
root = %s

[webdav]
url = "https://dav.example.net/files/"
subdir = "personal/dooit"
username = "your-login"
password_file = "webdav.password"
# Alternatively: password = "your-app-password" (this file must be mode 0600).

# Optional email integration; credentials use the shared JMAP profile store.
#[jmap]
#profile = "personal"
#account = "your-account-id"
#service = "https://mail.example.net/jmap/session"
|}
    (json_string (str root))

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
      Store.apply store ~id
        ~expected:(Some (Doc.revision current))
        ~operation:(new_uuid ()) ~payload:edited.raw
        (fun _ -> merged)
    in
    Unix.unlink path;
    result
  with Error s -> fail "%s; editor draft retained at %s" s path

let commands environment =
  let ctx = context environment in
  let init =
    command "init" "Create a local note store."
      Term.(
        const (fun c ->
            guard (fun () ->
                let s = Store.init c.Config.root in
                Printf.printf "%s\n%s\n" s.root s.id))
        $ ctx)
  in
  let add =
    command "new" "Create a task note offline."
      Term.(
        const (fun c title tags body_file due operation json ->
            guard (fun () ->
                let body = Option.fold ~none:"" ~some:read body_file in
                let store = Store.open_ c.Config.root in
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
                  Store.apply store ~id ~expected:None ~operation ~payload
                    (fun _ -> Doc.create ~id ~title ~tags ~body ?due ())
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
                let d = Store.get (Store.open_ c.Config.root) id in
                if json then output true d
                else if Console_eio.is_tty () then begin
                  Fmt.pr "%a@.@." C.Panel.pp
                    (C.Panel.lines ~title:(styled accent (Doc.title d))
                       [ styled muted (Doc.id d);
                         styled accent (Doc.status d) ]);
                  print_string d.raw
                end else print_string d.raw))
        $ ctx $ pos 0 "ID" $ json_arg)
  in
  let list_cmd name search =
    let text = if search then pos 0 "QUERY" else Term.const "" in
    command name "Search current note files, tags, status and sources."
      Term.(
        const (fun c text tags status source all json ->
            guard (fun () ->
                let notes, errors =
                  Index.query ~text ~tags ?status ?source ~include_deleted:all
                    (Store.open_ c.Config.root)
                in
                outputs json notes;
                diagnostics errors))
        $ ctx $ text $ tags
        $ opt [ "status" ] "Filter status."
        $ opt [ "source" ] "Filter link type."
        $ flag [ "include-deleted" ] "Include retained deletions."
        $ json_arg)
  in
  let edit =
    command "edit" "Edit a task using EDITOR with revision checks."
      Term.(
        const (fun c id json ->
            guard (fun () ->
                output json (editor (Store.open_ c.Config.root) id)))
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
                  Store.apply (Store.open_ c.Config.root)
                    ~id ~expected ~operation ~payload (function
                    | Some n -> Doc.patch n patch
                    | None -> fail "task does not exist")
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
                output json (Store.modify (Store.open_ c.Config.root) id f)))
        $ ctx $ pos 0 "ID" $ json_arg)
  in
  let done_ =
    change "done" "Mark a task done." (Doc.change "status" (Some (str "done")))
  in
  let reopen =
    change "reopen" "Reopen a completed task."
      (Doc.change "status" (Some (str "open")))
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
  let status =
    command "status" "Set a task status."
      Term.(
        const (fun c id status json ->
            guard (fun () ->
                output json
                  (Store.modify
                     (Store.open_ c.Config.root)
                     id
                     (Doc.change "status" (Some (str status))))))
        $ ctx $ pos 0 "ID" $ pos 1 "STATUS" $ json_arg)
  in
  let tag name op =
    command name "Update a freeform task tag."
      Term.(
        const (fun c id value json ->
            guard (fun () ->
                let patch =
                  obj
                    [
                      ("schema", str "dooit.patch/v1");
                      ( "operations",
                        arr [ obj [ ("op", str op); ("value", str value) ] ] );
                    ]
                in
                output json
                  (Store.modify (Store.open_ c.Config.root) id (fun d ->
                       Doc.patch d patch))))
        $ ctx $ pos 0 "ID" $ pos 1 "TAG" $ json_arg)
  in
  let adopt =
    command "adopt" "Copy a valid task document into this store."
      Term.(
        const (fun c path json ->
            guard (fun () ->
                output json
                  (Store.add
                     (Store.open_ c.Config.root)
                     (Doc.parse (read path)))))
        $ ctx $ pos 0 "PATH" $ json_arg)
  in
  let from_email =
    command "from-email"
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
                  (Link.capture ~new_task ~tags ~body ~hints
                     ~title:(Option.value title ~default:subject)
                     (Store.open_ c.root) target)))
        $ ctx $ pos 0 "EMAIL_ID" $ profile_arg $ account_arg $ service_arg
        $ title_arg $ tags
        $ opt [ "body-file" ] "Markdown body or selected email excerpt."
        $ flag [ "offline" ]
            "Capture supplied source identity without accessing JMAP."
        $ flag [ "new" ] "Create an additional task for the same source."
        $ json_arg)
  in
  let for_email =
    command "for-email"
      "Find task links to an email without accessing the network."
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
                let notes, errors = Store.scan (Store.open_ c.root) in
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
                let note = Store.get (Store.open_ c.Config.root) id in
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
  let sync =
    command "sync"
      "Synchronize current notes with the configured WebDAV subdirectory."
      Term.(
        const (fun c dry report json ->
            guard (fun () ->
                Eio.Switch.run (fun sw ->
                    let r = remote ~sw c ~readonly:dry in
                    let actions =
                      Sync.run ?report ~dry_run:dry
                        (Store.open_ c.Config.root)
                        r
                    in
                    sync_output json actions;
                    if
                      List.exists
                        (fun a ->
                          a.Sync.kind = "conflict" || a.kind = "invalid")
                        actions
                    then fail "some notes need review")))
        $ ctx
        $ flag [ "dry-run" ] "Read-only preview; optionally save a report."
        $ report_arg $ json_arg)
  in
  let clone =
    command "clone"
      "Download the configured WebDAV store into an empty local root."
      Term.(
        const (fun c ->
            guard (fun () ->
                Eio.Switch.run (fun sw ->
                    let r = remote ~sw c ~readonly:true in
                    let store = Sync.clone ~root:c.Config.root r in
                    Printf.printf "%s\n" store.root)))
        $ ctx)
  in
  let conflicts =
    command "conflicts" "List notes with retained unresolved sync conflicts."
      Term.(
        const (fun c json ->
            guard (fun () ->
                let ids = Store.conflicts (Store.open_ c.Config.root) in
                if json then
                  print_endline
                    (json_string
                       (obj
                          [
                            ("schema", str "dooit.conflicts/v1");
                            ("ids", arr (List.map str ids));
                          ]))
                else List.iter print_endline ids))
        $ ctx $ json_arg)
  in
  let recover =
    command "recover"
      "Finish interrupted local writes whose revisions still match."
      Term.(
        const (fun c ->
            guard (fun () ->
                List.iter
                  (fun (id, status) -> Printf.printf "%s %s\n" id status)
                  (Store.recover (Store.open_ c.Config.root))))
        $ ctx)
  in
  let resolve =
    command "resolve"
      "Apply a reviewed resolution file with fresh local and remote \
       preconditions."
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
                      Sync.resolve
                        (Store.open_ c.Config.root)
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
                let xdg =
                  Xdge.create ~create_dirs:false environment#fs "dooit"
                in
                print_endline
                  (Option.value path
                     ~default:
                       Eio.Path.(
                         native_exn (Xdge.config_dir xdg / "config.toml")))))
        $ config_arg)
  in
  let config_show =
    command "show" "Show effective settings with credentials omitted."
      Term.(
        const (fun c ->
            guard (fun () ->
                let dav =
                  match c.Config.webdav with
                  | None -> `Null
                  | Some w ->
                      obj
                        [
                          ("collection", str w.collection);
                          ("username", str w.username);
                          ("authentication", str "app-password");
                        ]
                in
                print_endline
                  (json_string ~pretty:true
                     (obj
                        [
                          ("path", str c.path);
                          ("root", str c.root);
                          ("webdav", dav);
                        ]))))
        $ ctx)
  in
  let config_init =
    command "init" "Write a private example TOML configuration if absent."
      Term.(
        const (fun path root ->
            guard (fun () ->
                let xdg =
                  Xdge.create ~create_dirs:false environment#fs "dooit"
                in
                let path =
                  Option.value path
                    ~default:
                      Eio.Path.(
                        native_exn (Xdge.config_dir xdg / "config.toml"))
                in
                let root =
                  Option.value root
                    ~default:(Eio.Path.native_exn (Xdge.data_dir xdg))
                in
                if exists path then fail "config already exists: %s" path;
                mkdir (Filename.dirname path);
                create_file path (config_example root);
                print_endline path))
        $ config_arg $ root_arg)
  in
  [
    init;
    add;
    show;
    list_cmd "list" false;
    list_cmd "search" true;
    edit;
    patch;
    done_;
    reopen;
    delete;
    restore;
    status;
    adopt;
    Cmd.group
      (Cmd.info "tag" ~doc:"Edit freeform tags.")
      [ tag "add" "add_tag"; tag "remove" "remove_tag" ];
    from_email;
    for_email;
    Cmd.group (Cmd.info "source" ~doc:"Resolve linked sources.") [ source_show ];
    sync;
    clone;
    conflicts;
    recover;
    resolve;
    Cmd.group
      (Cmd.info "config" ~doc:"Manage XDG TOML configuration.")
      [ config_path; config_show; config_init ];
  ]

let () =
  Console_eio.setup ();
  Eio_main.run (fun environment ->
      let cmd =
        Cmd.group
          (Cmd.info "dooit" ~version:"0.1.0"
             ~doc:"Editable task notes, WebDAV sync and JMAP email capture.")
          (commands environment)
      in
      exit (Cmd.eval cmd))
