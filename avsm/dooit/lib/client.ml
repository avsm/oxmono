open Common

type write_policy = Local_only | Synchronize
type publication = Local | Pushed

type pending = {
  id : string;
  operation : string;
  revision : string;
  message : string;
}

type outcome = Applied of Doc.t * publication | Pending of pending

type sync_result =
  | Local_status
  | Git_status of Git.status
  | Git_synced of string
  | Webdav_synced of Sync.action list

type t = {
  store : Store.t;
  backend : Config.backend;
  publish :
    check:(unit -> unit) ->
    id:string ->
    expected:string option ->
    operation:string ->
    payload:string ->
    (Doc.t option -> Doc.t) ->
    outcome;
  synchronize : report:string option -> dry_run:bool -> sync_result;
}

let store t = t.store
let backend t = t.backend

let local_apply store ~check ~id ~expected ~operation ~payload change =
  check ();
  let note =
    Eio_unix.run_in_systhread (fun () ->
        Store.apply store ~id ~expected ~operation ~payload change)
  in
  Applied (note, Local)

let local store =
  {
    store;
    backend = Config.Local;
    publish = local_apply store;
    synchronize = (fun ~report:_ ~dry_run:_ -> Local_status);
  }

let git ~process_mgr store config =
  {
    store;
    backend = Config.Git;
    publish =
      (fun ~check ~id ~expected ~operation ~payload change ->
        try
          Applied
            ( Git.apply ~process_mgr ~check store config ~id ~expected
                ~operation ~payload change,
              Pushed )
        with Git.Pending p ->
          Pending
            {
              id = p.id;
              operation = p.operation;
              revision = p.revision;
              message = p.message;
            });
    synchronize =
      (fun ~report ~dry_run ->
        if report <> None then
          fail "report directories are only supported by WebDAV";
        if dry_run then Git_status (Git.status ~process_mgr store config)
        else (
          ignore (Git.setup ~process_mgr store config);
          Git_synced (Git.sync ~process_mgr store config)));
  }

let of_config ~process_mgr ?remote store (config : Config.t) =
  match config.backend with
  | Config.Local -> local store
  | Config.Git -> git ~process_mgr store (Config.require_git config)
  | Config.Webdav ->
      {
        store;
        backend = Config.Webdav;
        publish =
          (fun ~check:_ ~id:_ ~expected:_ ~operation:_ ~payload:_ _ ->
            fail
              "Synchronized task writes are unavailable for the WebDAV \
               backend. Use a local write and dooit sync.");
        synchronize =
          (fun ~report ~dry_run ->
            let remote =
              match remote with
              | Some create -> create ~readonly:dry_run
              | None -> fail "WebDAV synchronization requires a transport"
            in
            Webdav_synced (Sync.run ?report ~dry_run store remote));
      }

exception Authority_failed of exn * Printexc.raw_backtrace

let apply t ~policy ~check ~id ~expected ~operation ~payload change =
  check ();
  let checked () =
    try check () with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn -> raise (Authority_failed (exn, Printexc.get_raw_backtrace ()))
  in
  let outcome =
    try
      match policy with
      | Local_only ->
          local_apply t.store ~check:checked ~id ~expected ~operation ~payload
            change
      | Synchronize ->
          t.publish ~check:checked ~id ~expected ~operation ~payload change
    with Authority_failed (exn, backtrace) ->
      Printexc.raise_with_backtrace exn backtrace
  in
  check ();
  outcome

let saved = function
  | Applied (note, _) -> note
  | Pending p ->
      fail
        "Task %s saved locally at revision %s, but its Git push is not \
         confirmed. Do not create a duplicate. Retry operation %s after \
         resolving synchronization: %s"
        p.id p.revision p.operation p.message

let modify t ~policy id change =
  let note = Eio_unix.run_in_systhread (fun () -> Store.get t.store id) in
  apply t ~policy ~check:Eio.Fiber.check ~id
    ~expected:(Some (Doc.revision note))
    ~operation:(new_uuid ()) ~payload:"interactive edit"
    (function
      | Some current -> change current | None -> fail "note disappeared")

let add t ~policy note =
  apply t ~policy ~check:Eio.Fiber.check ~id:(Doc.id note) ~expected:None
    ~operation:(new_uuid ()) ~payload:note.raw (fun _ -> note)

let capture ?new_task ?tags ?body ?due ?hints t ~policy ~title target =
  match
    Eio_unix.run_in_systhread (fun () ->
        Link.prepare_capture ?new_task ?tags ?body ?due ?hints ~title t.store
          target)
  with
  | `Existing found -> found
  | `Create note ->
      let outcome =
        try add t ~policy note
        with Error _ as exn -> (
          let concurrent =
            Eio_unix.run_in_systhread (fun () ->
                let path = Store.note_path t.store (Doc.id note) in
                if exists path then
                  let current = Store.get t.store (Doc.id note) in
                  if Link.matches target current then Some current else None
                else None)
          in
          match concurrent with
          | Some current when not (Option.value new_task ~default:false) ->
              Applied (current, Local)
          | _ -> raise exn)
      in
      (* Publication failures are outcomes, not exceptions caught as competing
         creations. Never turn a pending push into a successful capture. *)
      [ saved outcome ]

let sync ?report ~dry_run t = t.synchronize ~report ~dry_run

let sync_lines = function
  | Local_status ->
      [
        "Local backend: tasks are saved locally. There is no remote to \
         synchronize.";
      ]
  | Git_synced message -> [ message ]
  | Git_status status ->
      if not status.initialized then
        [ "Git repository not initialized. Run dooit init. No changes made." ]
      else
        let count = function None -> "unknown" | Some n -> string_of_int n in
        [
          "Git local status only. No changes or remote checks.";
          "Branch: " ^ Option.value status.branch ~default:"detached";
          "Dirty notes: " ^ string_of_bool status.dirty;
          "Ahead: " ^ count status.ahead ^ ", behind: " ^ count status.behind;
        ]
        @ List.map (fun path -> "Conflict: " ^ path) status.conflicts
  | Webdav_synced actions ->
      List.map
        (fun (a : Sync.action) ->
          a.kind ^ " " ^ a.id ^ if a.detail = "" then "" else ": " ^ a.detail)
        actions
