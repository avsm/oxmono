let ( / ) = Filename.concat
let log_src = Logs.Src.create "d10ir.registry"

module Log = (val Logs.src_log log_src : Logs.LOG)

type remote = [ `Http_remote of string ]

let archives_dir ~cache_root = cache_root / "d10ir" / "archives"
let archive_path ~cache_root ~sha = archives_dir ~cache_root / (sha ^ ".tar.zst")
let sidecar_path ~cache_root ~sha = archives_dir ~cache_root / (sha ^ ".json")

let trim_trailing_slash s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

let url_of ~remote ~sha =
  let (`Http_remote base) = remote in
  Fmt.str "%s/d10ir-archives/%s.tar.zst" (trim_trailing_slash base) sha

let sidecar_url_of ~remote ~sha =
  let (`Http_remote base) = remote in
  Fmt.str "%s/d10ir-archives/%s.json" (trim_trailing_slash base) sha

(* In-process single-flight tracker keyed by sha. Two fibers asking for
   the same archive used to race on a shared tmp path: the loser would
   [sha256_of_file] a tmp the winner had already renamed away, get [""],
   and emit a spurious "sha mismatch ... got " warning even though the
   bytes at [dst] were correct.

   The first caller for a given sha "claims" it (registers a promise
   and runs the actual fetch); concurrent callers "join" by awaiting
   the claimant's promise and consume the same result. The table entry
   is removed in [Fun.protect ~finally] so an exception in the body
   still wakes joiners (with [false]) instead of leaking a stuck
   promise. *)
module In_flight = struct
  type action = Claim of bool Eio.Promise.u | Join of bool Eio.Promise.t

  let table : (string, bool Eio.Promise.t) Hashtbl.t = Hashtbl.create 16
  let mu = Mutex.create ()

  let acquire sha =
    Mutex.lock mu;
    let r =
      match Hashtbl.find_opt table sha with
      | Some p -> Join p
      | None ->
          let p, u = Eio.Promise.create () in
          Hashtbl.replace table sha p;
          Claim u
    in
    Mutex.unlock mu;
    r

  let release sha u result =
    Mutex.lock mu;
    Hashtbl.remove table sha;
    Mutex.unlock mu;
    Eio.Promise.resolve u result
end

let sha256_of_file path =
  OpamHash.contents (OpamHash.compute ~kind:`SHA256 path)

(* Best-effort sidecar fetch: companion to the [.tar.zst] download. The
   [.json] sidecar records HOW the archive was assembled (upstream
   URL, resolved commit_sha, extra_files, patches, substs) and is what
   a future indexer walks to stitch binary provenance back to source.
   Missing sidecars on the server are tolerated — old registries that
   pre-date this scheme have only tarballs. *)
let do_fetch_sidecar ~fs ~session ~cache_root ~remote ~sha =
  let dst = sidecar_path ~cache_root ~sha in
  if Sys.file_exists dst then ()
  else
    let dst_dir = archives_dir ~cache_root in
    let tmp = Filename.temp_file ~temp_dir:dst_dir (sha ^ ".") ".json.tmp" in
    let tmp_p = Eio.Path.(fs / tmp) in
    let unlink_tmp () = try Eio.Path.unlink tmp_p with Eio.Exn.Io _ -> () in
    let url = sidecar_url_of ~remote ~sha in
    if D10.Sysops.Http.fetch_session session ~url ~dst:tmp_p then
      begin try Eio.Path.rename tmp_p Eio.Path.(fs / dst)
      with Eio.Exn.Io _ -> unlink_tmp ()
      end
    else unlink_tmp ()

let do_fetch ~fs ~session ~cache_root ~remote ~sha ~dst =
  let dst_dir = archives_dir ~cache_root in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dst_dir);
  (* Stage into a sibling tmp file so the rename to [dst] stays on the
     same filesystem and is atomic. [Filename.temp_file] creates via
     [O_EXCL] so two processes (or two retries in the same process)
     can't collide on the staging name. *)
  let tmp = Filename.temp_file ~temp_dir:dst_dir (sha ^ ".") ".tar.zst.tmp" in
  let tmp_p = Eio.Path.(fs / tmp) in
  let unlink_tmp () = try Eio.Path.unlink tmp_p with Eio.Exn.Io _ -> () in
  let url = url_of ~remote ~sha in
  Log.debug (fun m -> m "fetch %s -> %s" url dst);
  if D10.Sysops.Http.fetch_session session ~url ~dst:tmp_p then
    let actual = try sha256_of_file tmp with Failure _ | Sys_error _ -> "" in
    if actual = sha then begin
      (try Eio.Path.rename tmp_p Eio.Path.(fs / dst)
       with Eio.Exn.Io _ -> unlink_tmp ());
      do_fetch_sidecar ~fs ~session ~cache_root ~remote ~sha;
      true
    end
    else begin
      Log.warn (fun m ->
          m "sha mismatch for %s: declared %s, got %s" url sha actual);
      unlink_tmp ();
      false
    end
  else begin
    unlink_tmp ();
    false
  end

(* Retry policy for archive fetches: the registry can return slow / wedged
   connections (the previous HTTP request hit its 600s cap and was
   cancelled), and a fresh attempt with a fresh socket often succeeds.
   Defaults: 3 attempts with 2s / 4s backoff between them. Set
   [OI_ARCHIVE_RETRIES=N] to override; [N=1] effectively disables retries. *)
let default_max_attempts = 3

let max_attempts () =
  match Sys.getenv_opt "OI_ARCHIVE_RETRIES" with
  | Some s -> (
      match int_of_string_opt s with
      | Some n when n >= 1 -> n
      | _ -> default_max_attempts)
  | None -> default_max_attempts

let log_fetch_success ~sha ~attempts attempt =
  if attempt > 1 then
    Log.app (fun m ->
        m "fetch %s succeeded on attempt %d/%d" sha attempt attempts)

let backoff_and_retry ~clock ~sha ~attempts attempt delay loop =
  Log.warn (fun m ->
      m "fetch %s failed (attempt %d/%d); retrying in %.0fs" sha attempt
        attempts delay);
  Eio.Time.sleep clock delay;
  loop (attempt + 1) (delay *. 2.0)

let do_fetch_with_retries ~clock ~fs ~session ~cache_root ~remote ~sha ~dst =
  let attempts = max_attempts () in
  let rec loop attempt delay =
    if do_fetch ~fs ~session ~cache_root ~remote ~sha ~dst then begin
      log_fetch_success ~sha ~attempts attempt;
      true
    end
    else if attempt < attempts then
      backoff_and_retry ~clock ~sha ~attempts attempt delay loop
    else false
  in
  loop 1 2.0

let claim_and_fetch ~clock ~fs ~session ~cache_root ~remote ~sha ~dst u =
  let ok = ref false in
  Fun.protect
    ~finally:(fun () -> In_flight.release sha u !ok)
    (fun () ->
      let r =
        if Sys.file_exists dst then true
        else
          do_fetch_with_retries ~clock ~fs ~session ~cache_root ~remote ~sha
            ~dst
      in
      ok := r;
      r)

(* {!In_flight} dedups concurrent fibres in the same [oi] run sharing a
   session. Cross-process safety against another [oi] hitting the same
   shared cache is provided by {!Oi.Lock.acquire_global} in
   {!Cmd.Harness.bootstrap}, which serialises whole commands. *)
let pull ~clock ~fs ~session ~cache_root ~remote ~sha =
  let dst = archive_path ~cache_root ~sha in
  if Sys.file_exists dst then true
  else
    match In_flight.acquire sha with
    | Join p -> Eio.Promise.await p
    | Claim u ->
        claim_and_fetch ~clock ~fs ~session ~cache_root ~remote ~sha ~dst u

type prefetch_summary = {
  fetched : int;
  cached : int;
  failed : int;
  missing : string list;
}

(* Per-host concurrency for archive pulls. Cloudflare-fronted hosts
   start refusing extra streams / opening new TCP connections once a
   single client crosses ~8 in-flight requests, and once those
   connections wedge they hit [OI_HTTP_TIMEOUT=600s] before failing.
   Two is enough to overlap latency without tripping the threshold;
   override via [OI_HTTP_PARALLEL=N]. *)
let default_max_fibers = 2

let env_max_fibers () =
  match Sys.getenv_opt "OI_HTTP_PARALLEL" with
  | Some s -> (
      match int_of_string_opt s with Some n when n >= 1 -> Some n | _ -> None)
  | None -> None

let prefetch ~clock ~fs ~session ~cache_root ~remote ?max_fibers shas =
  let max_fibers =
    match max_fibers with
    | Some n -> n
    | None ->
        Stdlib.Option.value (env_max_fibers ()) ~default:default_max_fibers
  in
  let fetched = Atomic.make 0 in
  let cached = Atomic.make 0 in
  let failed = Atomic.make 0 in
  let missing = ref [] in
  let mutex = Mutex.create () in
  let do_one sha =
    let dst = archive_path ~cache_root ~sha in
    if Sys.file_exists dst then Atomic.incr cached
    else if pull ~clock ~fs ~session ~cache_root ~remote ~sha then
      Atomic.incr fetched
    else begin
      Atomic.incr failed;
      Mutex.lock mutex;
      missing := sha :: !missing;
      Mutex.unlock mutex
    end
  in
  Eio.Fiber.List.iter ~max_fibers do_one shas;
  {
    fetched = Atomic.get fetched;
    cached = Atomic.get cached;
    failed = Atomic.get failed;
    missing = List.rev !missing;
  }
