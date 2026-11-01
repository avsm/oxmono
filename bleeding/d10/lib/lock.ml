let log_src = Logs.Src.create "d10.lock"

module Log = (val Logs.src_log log_src : Logs.LOG)

type mode = Shared | Exclusive
type strategy = Block | Block_timeout of float | No_wait
type t = { fd : Eio_unix.Fd.t; path : string; mutable released : bool }

let path t = t.path
let pp ppf t = Fmt.string ppf t.path

exception Lock_unavailable of { path : string; held_by_pid : int option }

(* [F_RLOCK] / [F_TRLOCK] are the shared variants of [Unix.lockf]'s
   exclusive [F_LOCK] / [F_TLOCK]. Both map to the corresponding fcntl
   F_RDLCK / F_WRLCK record locks. *)
let try_acquire_command = function
  | Shared -> Unix.F_TRLOCK
  | Exclusive -> Unix.F_TLOCK

let try_acquire fd ~mode =
  Eio_unix.Fd.use_exn "d10.lock try_acquire" fd @@ fun raw ->
  match Unix.lockf raw (try_acquire_command mode) 0 with
  | () -> true
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) -> false

let try_read_pid path =
  try
    let s = In_channel.with_open_text path In_channel.input_all in
    int_of_string_opt (String.trim s)
  with Sys_error _ | End_of_file -> None

let write_pid_into fd =
  (* Best-effort diagnostic; pid garbles do not affect correctness. *)
  Eio_unix.Fd.use_exn "d10.lock write_pid" fd @@ fun raw ->
  try
    let _ = Unix.lseek raw 0 Unix.SEEK_SET in
    Unix.ftruncate raw 0;
    let s = string_of_int (Unix.getpid ()) ^ "\n" in
    let _ = Unix.write_substring raw s 0 (String.length s) in
    ()
  with Unix.Unix_error _ -> ()

let ensure_parent ~fs path =
  let dir = Filename.dirname path in
  try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir)
  with Eio.Exn.Io _ -> ()

let release t =
  if not t.released then begin
    (* Set the flag first so an exception in the unlock path can't
       leave us partially-released. The actual [F_ULOCK] is best-effort
       — closing the fd via [Eio_unix.Fd.close] drops the lock anyway. *)
    t.released <- true;
    (try
       Eio_unix.Fd.use_exn "d10.lock release" t.fd (fun raw ->
           try ignore (Unix.lockf raw Unix.F_ULOCK 0)
           with Unix.Unix_error _ -> ())
     with Invalid_argument _ -> () (* [t.fd] already closed *));
    try Eio_unix.Fd.close t.fd with Invalid_argument _ -> ()
  end

(* Non-blocking poll with exponential backoff up to ~1s, retrying until
   acquired or the strategy gives up. Sleep is via [Eio.Time.sleep] so a
   surrounding [Eio.Switch] cancellation interrupts the wait promptly. *)
let do_acquire ~clock ~mode ~strategy ~log_interval_s ~on_wait fd path =
  let started = Unix.gettimeofday () in
  let next_log = ref (started +. log_interval_s) in
  let rec poll backoff =
    if try_acquire fd ~mode then ()
    else
      let now = Unix.gettimeofday () in
      let waited = now -. started in
      (match strategy with
      | No_wait ->
          raise (Lock_unavailable { path; held_by_pid = try_read_pid path })
      | Block_timeout limit when waited >= limit ->
          raise (Lock_unavailable { path; held_by_pid = try_read_pid path })
      | _ -> ());
      if now >= !next_log then begin
        on_wait ~waited_s:waited ~path ~pid:(try_read_pid path);
        next_log := now +. log_interval_s
      end;
      Eio.Time.sleep clock (Float.min backoff 1.0);
      poll (backoff *. 1.5)
  in
  poll 0.05

let default_on_wait ~waited_s ~path ~pid =
  let pid_str = match pid with None -> "?" | Some p -> string_of_int p in
  Log.info (fun m ->
      m "waiting on %s (held by pid %s, %0.0fs)" path pid_str waited_s)

(* Switch-scoped primitive. Opens the lockfile via [Unix.openfile] and
   wraps the raw FD in an [Eio_unix.Fd.t] bound to [sw]. The fd is
   closed (which releases the lock as a kernel side-effect) when [sw]
   ends — by normal completion or by cancellation. Callers that want
   to release the lock before the switch ends call {!release}
   explicitly; a subsequent switch teardown then no-ops on the
   already-closed fd.

   Use this when:
   - the lock's lifetime spans multiple function calls;
   - you want to hold several locks side-by-side in the same scope;
   - you're integrating with other Eio resources that already use [~sw].

   For one-shot block-scoped acquisitions, {!with_lock} is more
   convenient. *)
let acquire ?(mode = Exclusive) ?(strategy = Block) ?(log_interval_s = 5.0)
    ?(on_wait = default_on_wait) ~sw ~clock ~fs ~path () =
  ensure_parent ~fs path;
  let raw =
    try Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644
    with Unix.Unix_error (err, _, _) ->
      Log.warn (fun m ->
          m "openfile %s: %s — skipping lock" path (Unix.error_message err));
      raise (Lock_unavailable { path; held_by_pid = None })
  in
  let fd = Eio_unix.Fd.of_unix ~sw ~close_unix:true raw in
  let t = { fd; path; released = false } in
  try
    do_acquire ~clock ~mode ~strategy ~log_interval_s ~on_wait fd path;
    write_pid_into fd;
    t
  with exn ->
    (* Close the fd on failure so [sw] doesn't dangle on a useless
       resource. The lockf F_ULOCK is implicit in the fd close. *)
    (try Eio_unix.Fd.close fd with Invalid_argument _ -> ());
    raise exn

(* Block-scoped convenience: equivalent to opening a fresh switch,
   calling {!acquire}, running [f], and tearing the switch down. *)
let with_lock ?mode ?strategy ?log_interval_s ?on_wait ~clock ~fs ~path f =
  Eio.Switch.run @@ fun sw ->
  let t =
    acquire ?mode ?strategy ?log_interval_s ?on_wait ~sw ~clock ~fs ~path ()
  in
  Fun.protect ~finally:(fun () -> release t) (fun () -> f t)
