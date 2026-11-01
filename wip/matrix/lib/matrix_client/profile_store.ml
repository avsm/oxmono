type t = { dir : Eio.Fs.dir_ty Eio.Path.t }

let src = Logs.Src.create "matrix.profile_store" ~doc:"Matrix profile files"

module Log = (val Logs.src_log src : Logs.LOG)

(* [Profile_store.t] is deliberately a light-weight handle, so the lock is
   shared by all handles made by this process.  The native lock below is still
   needed for handles in other processes. *)
let mutexes : (string, Eio.Mutex.t) Hashtbl.t = Hashtbl.create 7
let mutexes_lock = Stdlib.Mutex.create ()

let mutex_for path =
  Stdlib.Mutex.lock mutexes_lock;
  Fun.protect
    ~finally:(fun () -> Stdlib.Mutex.unlock mutexes_lock)
    (fun () ->
      match Hashtbl.find_opt mutexes path with
      | Some mutex -> mutex
      | None ->
          let mutex = Eio.Mutex.create () in
          Hashtbl.add mutexes path mutex;
          mutex)

let session_file = "session.json"
let device_file = "device.json"
let one_time_keys_file = "one_time_keys.json"
let olm_sessions_file = "olm_sessions.json"
let megolm_inbound_file = "megolm_inbound.json"
let megolm_outbound_file = "megolm_outbound.json"

let validate_profile_name profile =
  if
    String.length profile = 0
    || String.equal profile "." || String.equal profile ".."
    || String.contains profile '/'
    || String.contains profile '\\'
    || String.contains profile '\000'
  then invalid_arg "profile name must be one non-empty relative path component"

let files =
  [
    session_file;
    device_file;
    one_time_keys_file;
    olm_sessions_file;
    megolm_inbound_file;
    megolm_outbound_file;
  ]

let create_in_root data_dir ~profile =
  validate_profile_name profile;
  let dir = Eio.Path.(data_dir / "profiles" / profile) in
  Io_context.with_context "creating Matrix profile directory" (fun () ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir);
  { dir }

let create ~xdg ~profile = create_in_root (Xdge.data_dir xdg) ~profile
let create_at ~root ~profile = create_in_root root ~profile
let dir t = t.dir

let exists t =
  Io_context.with_context "checking Matrix profile session state" (fun () ->
      Eio.Path.is_file Eio.Path.(t.dir / session_file))

let lock_file = ".profile.lock"

let lock_error message =
  Error.Json_error (Printf.sprintf "profile lock: %s" message)

let unsupported_lock_error =
  Error.Policy_denied "profile lock requires a native filesystem path"

let rec open_and_lock ?(blocking = true) path =
  try
    let fd =
      Io_context.with_context "acquiring Matrix profile lock" (fun () ->
          Eio_unix.run_in_systhread ~label:"matrix-profile-lock" (fun () ->
              (* OCaml's [Unix.open_flag] has no [O_NOFOLLOW].  Exclusive create
                 rejects a symlink when the inode is absent; for an existing
                 lock, compare the inode before and after opening and refuse a
                 mismatch. The opened descriptor is never modified before that
                 comparison, which closes the useful part of the replacement
                 race. *)
              let existing =
                try Some (Unix.lstat path)
                with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
                  None
              in
              let flags =
                match existing with
                | None ->
                    [ Unix.O_CREAT; Unix.O_EXCL; Unix.O_RDWR; Unix.O_CLOEXEC ]
                | Some stat when stat.Unix.st_kind = Unix.S_REG ->
                    [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC ]
                | Some _ ->
                    raise
                      (Unix.Unix_error (Unix.ELOOP, "open profile lock", path))
              in
              let fd = Unix.openfile path flags 0o600 in
              try
                let opened = Unix.fstat fd in
                (match existing with
                | Some before
                  when before.Unix.st_dev <> opened.Unix.st_dev
                       || before.Unix.st_ino <> opened.Unix.st_ino
                       || opened.Unix.st_kind <> Unix.S_REG ->
                    raise
                      (Unix.Unix_error (Unix.ELOOP, "open profile lock", path))
                | None when opened.Unix.st_kind <> Unix.S_REG ->
                    raise
                      (Unix.Unix_error (Unix.ELOOP, "open profile lock", path))
                | _ -> ());
                Unix.fchmod fd 0o600;
                Unix.lockf fd (if blocking then Unix.F_LOCK else Unix.F_TLOCK) 0;
                Some fd
              with exn ->
                let bt = Printexc.get_raw_backtrace () in
                (try Unix.close fd
                 with Unix.Unix_error _ ->
                   Log.debug (fun m -> m "could not close failed profile lock"));
                Printexc.raise_with_backtrace exn bt))
    in
    Ok fd
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> open_and_lock ~blocking path
  | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), "lockf", _) when not blocking
    ->
      Ok None
  | Unix.Unix_error (error, function_name, _argument) ->
      Error
        (Error.Network_error
           (Printf.sprintf "profile lock %s: %s" function_name
              (Unix.error_message error)))

let release_lock fd =
  (* Closing the descriptor also releases the advisory lock.  Keep the
     cleanup best-effort so an exception from the protected operation is not
     masked by a platform-specific unlock/close error. *)
  try
    Eio.Cancel.protect (fun () ->
        Eio_unix.run_in_systhread ~label:"matrix-profile-unlock" (fun () ->
            Fun.protect
              ~finally:(fun () ->
                try Unix.close fd
                with Unix.Unix_error _ ->
                  Log.debug (fun m -> m "could not close profile lock"))
              (fun () ->
                try Unix.lockf fd Unix.F_ULOCK 0
                with Unix.Unix_error _ ->
                  Log.debug (fun m -> m "could not unlock profile lock"))))
  with
  | Unix.Unix_error _ -> Log.debug (fun m -> m "profile lock cleanup failed")
  | Eio.Io _ as exn ->
      let contextual =
        Eio.Exn.add_context exn "releasing profile lock during cleanup"
      in
      Log.debug (fun m ->
          m "profile lock cleanup failed: %a" Eio.Exn.pp contextual)

let with_dir_lock dir fn =
  let lock_path = Eio.Path.(dir / lock_file) in
  match Eio.Path.native lock_path with
  | None -> Error unsupported_lock_error
  | Some path ->
      let mutex = mutex_for path in
      (* Waiting for the process-local mutex remains cancellable.  Once it is
         held, cancellation is protected until both the native descriptor and
         the mutex have been released. *)
      Eio.Mutex.lock mutex;
      Eio.Cancel.protect (fun () ->
          Fun.protect
            ~finally:(fun () -> Eio.Mutex.unlock mutex)
            (fun () ->
              match open_and_lock path with
              | Error _ as error -> error
              | Ok None ->
                  Error
                    (lock_error "blocking acquisition returned without a lock")
              | Ok (Some fd) ->
                  Fun.protect
                    ~finally:(fun () -> release_lock fd)
                    (fun () -> Ok (fn ()))))

let with_lock t fn = with_dir_lock t.dir fn

(* Unlike short snapshot writes, network refresh must remain cancellable,
   including while another process holds the lock. Never hold .profile.lock
   during a network exchange. *)
let with_refresh_lock t ~clock fn =
  match Eio.Path.native Eio.Path.(t.dir / ".refresh.lock") with
  | None -> Error unsupported_lock_error
  | Some path ->
      let mutex = mutex_for path in
      Eio.Mutex.lock mutex;
      Fun.protect
        ~finally:(fun () -> Eio.Mutex.unlock mutex)
        (fun () ->
          let rec acquire () =
            let acquired =
              Eio.Cancel.protect (fun () ->
                  match open_and_lock ~blocking:false path with
                  | Ok (Some fd) ->
                      (* Hand ownership to a wrapper that installs cleanup before
                         its first effect in the restored cancellation context. *)
                      `Held
                        (fun () ->
                          Fun.protect ~finally:(fun () -> release_lock fd) fn)
                  | Ok None -> `Busy
                  | Error error -> `Error error)
            in
            match acquired with
            | `Error error -> Error error
            | `Busy ->
                Eio.Time.sleep clock 0.05;
                acquire ()
            | `Held run -> run ()
          in
          acquire ())

(* The counter is process-local; including the PID makes the names unique
   across processes too. The exclusive create is still important: a process
   may inherit the counter after a fork, and another writer may already have
   the same candidate name. *)
let atomic_file_counter = Atomic.make 0

let sync_directory dir =
  match Eio.Path.native dir with
  | None -> Ok ()
  | Some path -> (
      try
        Eio_unix.run_in_systhread ~label:"matrix-profile-directory-sync"
          (fun () ->
            let fd = Unix.openfile path [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
            Fun.protect
              ~finally:(fun () -> Unix.close fd)
              (fun () -> Unix.fsync fd));
        Ok ()
      with Unix.Unix_error (error, _, _) ->
        Error
          (Error.Network_error
             ("syncing profile directory: " ^ Unix.error_message error)))

let atomic_write ~path ~data =
  match Eio.Path.split path with
  | None ->
      Error
        (Error.Policy_denied
           "atomic write target must have a same-directory basename")
  | Some (dir, basename) -> (
      let unlink_temp temp =
        (* Cleanup is best-effort, but protect it from cancellation so an
           interrupted write cannot leave a temporary file behind. *)
        try Eio.Cancel.protect (fun () -> Eio.Path.unlink ~missing_ok:true temp)
        with Eio.Io _ as exn ->
          let contextual =
            Eio.Exn.add_context exn
              "removing profile temporary file during cleanup"
          in
          Log.debug (fun m ->
              m "could not remove profile temporary file: %a" Eio.Exn.pp
                contextual)
      in
      let rec write_temp attempts =
        if attempts = 100 then
          Error
            (Error.Network_error
               "atomic write could not reserve a temporary file after 100 \
                attempts")
        else
          let sequence = Atomic.fetch_and_add atomic_file_counter 1 in
          let temp_name =
            Printf.sprintf "%s.tmp.%d.%d" basename (Unix.getpid ()) sequence
          in
          let temp = Eio.Path.(dir / temp_name) in
          try
            Eio.Path.with_open_out ~create:(`Exclusive 0o600) temp (fun flow ->
                Eio.Path.chmod ~follow:false ~perm:0o600 temp;
                Eio.Flow.copy_string data flow;
                Eio.File.sync flow);
            Ok temp
          with
          | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) ->
              write_temp (attempts + 1)
          | exn -> (
              let bt = Printexc.get_raw_backtrace () in
              unlink_temp temp;
              match exn with
              | Eio.Io _ ->
                  Eio.Exn.reraise_with_context exn bt
                    "writing profile temporary file"
              | _ -> Printexc.raise_with_backtrace exn bt)
      in
      match write_temp 0 with
      | Error _ as error -> error
      | Ok temp ->
          Fun.protect
            ~finally:(fun () -> unlink_temp temp)
            (fun () ->
              Io_context.with_context "committing Matrix profile file"
                (fun () -> Eio.Path.rename temp path);
              sync_directory dir))

let load : type a. t -> name:string -> a Jsont.t -> (a option, Error.t) result =
 fun t ~name codec ->
  let path = Eio.Path.(t.dir / name) in
  Io_context.with_context "loading Matrix profile file" (fun () ->
      if not (Eio.Path.is_file path) then Ok None
      else
        let file =
          Option.value (Eio.Path.native path) ~default:Jsont.Textloc.file_none
        in
        match Jsont_bytesrw.decode_string ~file codec (Eio.Path.load path) with
        | Ok v -> Ok (Some v)
        | Error msg -> Error (Error.Json_error msg))

(* Everything here is secret, so a file is created 0600. Written to a unique
   same-directory temporary file, synced, and renamed into place, so a crash
   mid-write leaves the previous file intact rather than truncated. *)
let save : type a. t -> name:string -> a Jsont.t -> a -> (unit, Error.t) result
    =
 fun t ~name codec value ->
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec value with
  | Error msg -> Error (Error.Json_error msg)
  | Ok data ->
      let path = Eio.Path.(t.dir / name) in
      atomic_write ~path ~data

let load_session t = load t ~name:session_file Session.Session_file.jsont
let save_session t v = save t ~name:session_file Session.Session_file.jsont v

let update_session t fn =
  match
    with_lock t (fun () ->
        match load_session t with
        | Error error -> Error error
        | Ok None -> Error (lock_error "session file is missing")
        | Ok (Some session) -> save_session t (fn session))
  with
  | Ok result -> result
  | Error error -> Error error

let refresh_pending_file = ".refresh_pending.json"

let save_login t ~clock session =
  let ( let* ) = Result.bind in
  with_refresh_lock t ~clock (fun () ->
      let* result =
        with_lock t (fun () ->
            (* Do not retire a possibly consumed token until its replacement is
             durable. A failed login save must leave the old marker intact. *)
            let* () = save_session t session in
            Eio.Path.unlink ~missing_ok:true
              Eio.Path.(t.dir / refresh_pending_file);
            sync_directory t.dir)
      in
      result)

let same_identity (a : Session.Session_file.t) (b : Session.Session_file.t) =
  Uriz.equal a.server.homeserver b.server.homeserver
  && Matrix_proto.Id.User_id.equal a.server.user_id b.server.user_id
  && Matrix_proto.Id.Device_id.equal a.auth.device_id b.auth.device_id
  && a.auth.method_ = b.auth.method_

let same_tokens (a : Session.Auth.t) (b : Session.Auth.t) =
  a.access_token = b.access_token && a.refresh_token = b.refresh_token

let refresh_session_prepared t ~clock ~(expected : Session.Session_file.t)
    ~prepare =
  let ( let* ) = Result.bind in
  let locked fn =
    let* result = with_lock t fn in
    result
  in
  let pending_file = refresh_pending_file in
  let pending_path = Eio.Path.(t.dir / pending_file) in
  let read_current () =
    let* session = load_session t in
    match session with
    | None -> Error Error.No_session
    | Some latest when same_identity latest expected -> Ok latest
    | Some _ ->
        Error (Error.Policy_denied "profile login changed during token refresh")
  in
  with_refresh_lock t ~clock (fun () ->
      let* latest =
        locked (fun () ->
            let* latest = read_current () in
            let* pending =
              match load t ~name:pending_file Session.Session_file.jsont with
              | Ok pending -> Ok pending
              | Error _ ->
                  Error
                    (Error.Policy_denied
                       "token refresh marker is unreadable; log in again")
            in
            let* () =
              match pending with
              | Some pending
                when same_identity pending latest
                     && same_tokens pending.auth latest.auth ->
                  Error
                    (Error.Policy_denied
                       "previous token refresh was interrupted or failed; log \
                        in again")
              | Some _ ->
                  Eio.Path.unlink ~missing_ok:true pending_path;
                  Ok ()
              | None -> Ok ()
            in
            Ok latest)
      in
      if latest.auth.refresh_token = None then Error Error.No_session
      else if not (same_tokens latest.auth expected.auth) then Ok latest
      else
        let* exchange = prepare latest in
        let* () =
          locked (fun () ->
              let* current = read_current () in
              if not (same_tokens current.auth latest.auth) then
                Error
                  (Error.Policy_denied
                     "profile tokens changed before token refresh")
              else save t ~name:pending_file Session.Session_file.jsont current)
        in
        (* A process death or ambiguous network/storage error leaves the marker.
           Reusing a rotating refresh token could invalidate the whole session.
           A later login, or an already committed rotation, supersedes it. *)
        let* (auth : Session.Auth.t) = exchange () in
        let* committed =
          locked (fun () ->
              let* current = read_current () in
              if not (same_tokens current.auth latest.auth) then Ok current
              else if
                auth.device_id <> current.auth.device_id
                || auth.method_ <> current.auth.method_
              then
                Error
                  (Error.Policy_denied "refresh changed the profile identity")
              else
                let updated = { current with auth } in
                let* () = save_session t updated in
                Ok updated)
        in
        Eio.Path.unlink ~missing_ok:true pending_path;
        Ok committed)

let refresh_session t ~clock ~expected ~refresh =
  refresh_session_prepared t ~clock ~expected ~prepare:(fun latest ->
      Ok (fun () -> refresh latest))

let load_device_keys t = load t ~name:device_file Session.Device_keys.jsont
let save_device_keys t v = save t ~name:device_file Session.Device_keys.jsont v

let load_one_time_keys t =
  load t ~name:one_time_keys_file Session.One_time_keys_file.jsont

let save_one_time_keys t v =
  save t ~name:one_time_keys_file Session.One_time_keys_file.jsont v

let load_olm_sessions t =
  load t ~name:olm_sessions_file Session.Olm_sessions_file.jsont

let save_olm_sessions t v =
  save t ~name:olm_sessions_file Session.Olm_sessions_file.jsont v

let load_megolm_inbound t =
  load t ~name:megolm_inbound_file Session.Megolm_inbound_file.jsont

let save_megolm_inbound t v =
  save t ~name:megolm_inbound_file Session.Megolm_inbound_file.jsont v

let load_megolm_outbound t =
  load t ~name:megolm_outbound_file Session.Megolm_outbound_file.jsont

let save_megolm_outbound t v =
  save t ~name:megolm_outbound_file Session.Megolm_outbound_file.jsont v

let clear t =
  Io_context.with_context "clearing Matrix profile state" (fun () ->
      List.iter
        (fun name ->
          let path = Eio.Path.(t.dir / name) in
          if Eio.Path.is_file path then Eio.Path.unlink path)
        files)
