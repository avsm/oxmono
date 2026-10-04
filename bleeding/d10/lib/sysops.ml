[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "sysops"

module Log = (val Logs.src_log log_src : Logs.LOG)

type tools = { tar : string }
type pm = [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t
type clk = float Eio.Time.clock_ty Eio.Resource.t

(* Structured error for non-zero subprocess exits.

   Extends [Eio.Exn.err] (rather than defining a fresh [exception])
   because every subprocess this module spawns goes through [Eio.Process]
   anyway, so the failure mode is morally an Eio I/O error — and the
   Eio convention gives us [Eio.Exn.add_context] for layering "while
   running cp -Rfl from X to Y" annotations, and [Eio.Exn.register_pp]
   wires into the same [Printexc] printer that already handles
   [Eio.Io _]. Callers pattern-match on
   [Eio.Io (Cmd_failed _, _)].

   The previous [Fmt.failwith "command exited N: ..."] tunnel through
   bare [Failure] was indistinguishable from any other [Failure] in
   the system, and silently disarmed catch sites that meant to retry
   only on subprocess failure (e.g. the [cp -Rfl] hardlink-pass
   fallback in [link_tree], which narrowed to [Eio.Exn.Io] and so
   never fired). *)
type Eio.Exn.err +=
  | Cmd_failed of {
      argv : string list;
      status : [ `Exited of int | `Signaled of int ];
      output : string;
    }

let () =
  Eio.Exn.register_pp (fun ppf -> function
    | Cmd_failed { argv; status; output } ->
        let how =
          match status with
          | `Exited n -> Fmt.str "command exited %d" n
          | `Signaled n -> Fmt.str "command killed by signal %d" n
        in
        Fmt.pf ppf "%s: %s" how (String.concat " " argv);
        if output <> "" then
          Fmt.pf ppf "@,--- output ---@,%s" (String.trim output);
        true
    | _ -> false)

let cmd_failed ~argv ~status ~output =
  Eio.Exn.create (Cmd_failed { argv; status; output })

type t = {
  proc_mgr : pm;
  clock : clk;
  stdout : Eio.Flow.sink_ty Eio.Resource.t option;
  stderr : Eio.Flow.sink_ty Eio.Resource.t option;
  tools : tools;
  env : string array;
}

(* -- Low-level helpers --------------------------------------------------- *)

let native p = Eio.Path.native_exn p

(* -- Subprocess timeout --------------------------------------------------

   Default 10-minute hard cap on every syscmd subprocess this module
   spawns: git fetch/clone, tar extract, cp, which, etc. Long enough
   for a slow git clone of a multi-GB repository on flaky connectivity,
   short enough to break a wedged process before it tanks an
   [oi build --all]. Override per-process via [OI_CMD_TIMEOUT] (seconds);
   set to [0] to disable.

   Why this exists: bare [Eio.Process.await] has no timeout. A [git
   clone] from a stalled remote will block the calling fiber forever,
   leaving the rest of the build queued and the spawned [git] process
   uncollectable. With this cap a wedged subprocess fails the calling
   fiber with [Failure], the switch unwinds, the child is killed, and
   the rest of the [oi build --all] continues.

   Build-step subprocesses (compile commands invoked by [D10ir.Direct])
   do NOT go through this path — those legitimately take longer for
   large native compiles and have their own bookkeeping. *)
let default_cmd_timeout_s = 600.0

let cmd_timeout_s () =
  match Sys.getenv_opt "OI_CMD_TIMEOUT" with
  | Some v -> (
      match float_of_string_opt v with
      | Some n when n >= 0.0 -> n
      | _ -> default_cmd_timeout_s)
  | None -> default_cmd_timeout_s

let with_timeout t ~cmd f =
  let timeout = cmd_timeout_s () in
  if timeout <= 0.0 then f ()
  else
    try Eio.Time.with_timeout_exn t.clock timeout f
    with Eio.Time.Timeout ->
      let argv = String.concat " " cmd in
      let mins = timeout /. 60.0 in
      Log.warn (fun m ->
          m "subprocess timed out after %.0fs (%.1f min): %s" timeout mins argv);
      Fmt.failwith
        "subprocess timed out after %.0fs (%.1f min) and was cancelled: %s\n\
         A child process exceeded the per-command time limit. The most common \
         cause is a wedged network operation (git fetch/clone from a slow or \
         unreachable remote, an opam source download, a hung HTTPS handshake). \
         Override the cap by exporting OI_CMD_TIMEOUT=N (seconds; [0] \
         disables)."
        timeout mins argv

let handle_run_quiet_result ~cmd ~buf result =
  let output = String.trim (Buffer.contents buf) in
  match result with
  | `Exited 0 -> if output <> "" then Log.debug (fun m -> m "%s" output)
  | `Exited n ->
      if output <> "" then Log.debug (fun m -> m "%s" output);
      raise (cmd_failed ~argv:cmd ~status:(`Exited n) ~output)
  | `Signaled n -> raise (cmd_failed ~argv:cmd ~status:(`Signaled n) ~output)

let run_quiet t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  with_timeout t ~cmd @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let buf = Buffer.create 256 in
  let sink = Eio.Flow.buffer_sink buf in
  let child =
    Eio.Process.spawn ~env:t.env ~sw t.proc_mgr ~stdout:sink ~stderr:sink cmd
  in
  handle_run_quiet_result ~cmd ~buf (Eio.Process.await child)

let run_capture t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  let out =
    with_timeout t ~cmd @@ fun () ->
    String.trim
      (Eio.Process.parse_out ~env:t.env t.proc_mgr Eio.Buf_read.take_all cmd)
  in
  Log.debug (fun m -> m "%s" out);
  out

(* Like [run_capture] but with stderr redirected into a throwaway
   buffer so chatty subprocesses (git's "warning: redirecting to ..."
   notice) don't leak to the parent's stderr. *)
let run_capture_quiet t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  let stderr_buf = Buffer.create 64 in
  let stderr_sink = Eio.Flow.buffer_sink stderr_buf in
  let out =
    with_timeout t ~cmd @@ fun () ->
    String.trim
      (Eio.Process.parse_out ~env:t.env ~stderr:stderr_sink t.proc_mgr
         Eio.Buf_read.take_all cmd)
  in
  Log.debug (fun m -> m "%s" out);
  out

(* Spawn [which NAME] with both stdout and stderr routed into a
   throwaway buffer. Nix's [which] writes "no NAME in PATH" to stderr
   on miss; [Eio.Process.parse_out] only captures stdout, so without
   the buffered stderr sink that line leaks to the CLI's own stderr. *)
let has_cmd t name =
  let cmd = [ "which"; name ] in
  with_timeout t ~cmd @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let buf = Buffer.create 64 in
  let sink = Eio.Flow.buffer_sink buf in
  let child =
    Eio.Process.spawn ~env:t.env ~sw t.proc_mgr ~stdout:sink ~stderr:sink cmd
  in
  match Eio.Process.await child with
  | `Exited 0 -> true
  | `Exited _ | `Signaled _ -> false

(* -- Initialisation ------------------------------------------------------ *)

let resolve_tools t =
  let tar = if has_cmd t "gtar" then "gtar" else "tar" in
  { tar }

(* Apply non-interactive Git settings to child processes only. Changing the
   parent environment races with environment reads on other domains. *)
let child_env () =
  let overrides =
    [
      "GIT_TERMINAL_PROMPT=0";
      "GIT_ASKPASS=/bin/true";
      "SSH_ASKPASS=/bin/true";
      "SSH_ASKPASS_REQUIRE=never";
    ]
  in
  let key s = String.sub s 0 (String.index s '=') in
  let keys = List.map key overrides in
  let inherited =
    Unix.environment () |> Array.to_list
    |> List.filter (fun s -> not (List.mem (key s) keys))
  in
  Array.of_list (overrides @ inherited)

let v ?stdout ?stderr ~proc_mgr ~fs:_ ~net:_ ~clock () =
  let stdout =
    Option.map (fun s -> (s :> Eio.Flow.sink_ty Eio.Resource.t)) stdout
  in
  let stderr =
    Option.map (fun s -> (s :> Eio.Flow.sink_ty Eio.Resource.t)) stderr
  in
  let t_partial =
    {
      proc_mgr :> pm;
      clock :> clk;
      stdout;
      stderr;
      tools = { tar = "tar" };
      env = child_env ();
    }
  in
  let tools = resolve_tools t_partial in
  { t_partial with tools }

let pp ppf _t = Fmt.string ppf "<sysops>"

(* -- File queries -------------------------------------------------------- *)

let file_exists path =
  try
    ignore (Eio.Path.stat ~follow:true path);
    true
  with Eio.Exn.Io _ -> false

(* -- File copying -------------------------------------------------------- *)

let copy_tree t ~src ~dst =
  let src_s = native src and dst_s = native dst in
  try run_quiet t [ "cp"; "-ac"; src_s; dst_s ]
  with Eio.Io (Cmd_failed _, _) ->
    Eio.Path.rmtree ~missing_ok:true dst;
    run_quiet t [ "cp"; "-a"; src_s; dst_s ]

let link_tree t ~src ~dst =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst;
  let src_s = native src and dst_s = native dst in
  (* No [-v]: [run_quiet] throws the output away and one layer-restore
     can produce tens of thousands of lines that then have to be
     drained through the Eio buffer_sink, which is slow at best and
     has hung in the wild when stdout and stderr are merged.

     Try the hardlink pass first ([cp -Rfl]); if [src] and [dst] live on
     different filesystems (typical in containers: BuildKit cache mounts
     are usually overlay/tmpfs, the image's writable layer is overlay too
     but a different one), [cp] fails with [EXDEV] and a non-zero exit.
     Fall back to a real archive-mode copy ([cp -Rfa]) so the build
     prefix still gets a complete tree. Slower for a one-time fill but
     unblocks every cross-mount setup we've seen. *)
  (* [Cmd_failed] is what [run_quiet] raises on a non-zero exit (now
     wrapped in [Eio.Io]); the earlier wildcard [Eio.Exn.Io] catch in
     fact ran against [Failure], not an Eio error, so it never fired
     and the build died on the first cross-filesystem layer restore
     (BuildKit cache mount -> image overlay -> [EXDEV] from
     [cp -Rfl]). *)
  try run_quiet t [ "cp"; "-Rfl"; src_s ^ "/."; dst_s ^ "/" ]
  with Eio.Io (Cmd_failed _, _) ->
    Log.debug (fun m ->
        m "link_tree %s -> %s: hardlink pass failed; falling back to copy" src_s
          dst_s);
    run_quiet t [ "cp"; "-Rfa"; src_s ^ "/."; dst_s ^ "/" ]

(* -- Low-level command execution ----------------------------------------- *)

(* Run [cmd] with stdout/stderr inherited from the parent terminal so
   any progress or error output the subprocess writes is shown to the
   user as it happens. Used for interactive-feeling commands like git
   pull/push where hiding the subprocess output would leave the user
   guessing. Falls back to [run_quiet] when no stdout/stderr resources
   were registered on [t]. *)
let handle_run_inherit_result ~cmd = function
  | `Exited 0 -> ()
  | `Exited n -> raise (cmd_failed ~argv:cmd ~status:(`Exited n) ~output:"")
  | `Signaled n -> raise (cmd_failed ~argv:cmd ~status:(`Signaled n) ~output:"")

let run_inherit_with t ~stdout ~stderr cmd =
  with_timeout t ~cmd @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let child = Eio.Process.spawn ~env:t.env ~sw t.proc_mgr ~stdout ~stderr cmd in
  handle_run_inherit_result ~cmd (Eio.Process.await child)

let run_inherit t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  match (t.stdout, t.stderr) with
  | None, _ | _, None -> run_quiet t cmd
  | Some stdout, Some stderr -> run_inherit_with t ~stdout ~stderr cmd

module Cmd = struct
  let run t cmd = run_quiet t cmd
  let run_out t cmd = run_capture t cmd
  let run_out_quiet t cmd = run_capture_quiet t cmd
  let run_inherit t cmd = run_inherit t cmd
end

(* -- Archive operations -------------------------------------------------- *)

module Tar = struct
  let extract t ~archive ~dst ?(strip = 0) () =
    let cmd =
      [ t.tools.tar; "xf"; native archive; "-C"; native dst ]
      @ if strip > 0 then [ Fmt.str "--strip-components=%d" strip ] else []
    in
    run_quiet t cmd

  let create_zstd t ~src ~dst =
    run_quiet t
      [ t.tools.tar; "--zstd"; "-cf"; native dst; "-C"; native src; "." ]
end

module Http = struct
  (* -- Per-request HTTP timeout --------------------------------------------

     Default 10-minute hard cap on every in-process [Fetch] fetch this
     module performs. Same rationale as {!cmd_timeout_s} for subprocess
     spawns, but for the HTTP path: a wedged TCP socket (e.g. a remote
     that sent FIN but where our [Eio.Flow.single_read] never observes
     EOF, leaving us in [CLOSE_WAIT] until the kernel's keepalive expires
     ~2 hours later) blocks the whole [oi build --all] pipeline.

     Override per-fetch via [OI_HTTP_TIMEOUT] (seconds); set to [0] to
     disable. Distinct from [OI_CMD_TIMEOUT] so callers can tune the two
     paths independently. *)
  let default_http_timeout_s = 600.0

  let http_timeout_s () =
    match Sys.getenv_opt "OI_HTTP_TIMEOUT" with
    | Some v -> (
        match float_of_string_opt v with
        | Some n when n >= 0.0 -> n
        | _ -> default_http_timeout_s)
    | None -> default_http_timeout_s

  (* [Http.fetch] / [Http.fetch_session] return [bool] (true on success,
     false on operational errors). Caller cancellation propagates. On timeout we log at warn and
     return false so callers fall back to whatever cache hierarchy or
     retry policy they have, the same way they would on a 5xx or a
     network error. *)
  let with_http_timeout ~clock ~url f =
    let timeout = http_timeout_s () in
    if timeout <= 0.0 then f ()
    else
      try Eio.Time.with_timeout_exn clock timeout f
      with Eio.Time.Timeout ->
        Log.warn (fun m ->
            m
              "HTTP request timed out after %.0fs (%.1f min); cancelling: %s\n\
               Override the cap by exporting OI_HTTP_TIMEOUT=N (seconds; [0] \
               disables)."
              timeout (timeout /. 60.0) url);
        false

  (* Stream [src] into [sink] while invoking [on_progress] with the
     running byte count. Throttles callbacks to ~20Hz so a fast
     download doesn't spam the renderer (each [Tty.Progress.set] is
     a synchronous ANSI write that's expensive on slow terminals).
     The final tick fires unconditionally so the bar always settles
     at 100% / total. *)
  let copy_with_progress ~clock ?on_progress ?total src sink =
    match on_progress with
    | None -> Eio.Flow.copy src sink
    | Some on_progress ->
        let buf = Cstruct.create 65536 in
        let received = ref 0L in
        let last_tick = ref 0.0 in
        let throttle_s = 0.05 in
        let maybe_tick () =
          let now = Eio.Time.now clock in
          if now -. !last_tick >= throttle_s then (
            last_tick := now;
            on_progress ~received:!received ~total)
        in
        (try
           while true do
             let n = Eio.Flow.single_read src buf in
             Eio.Flow.write sink [ Cstruct.sub buf 0 n ];
             received := Int64.add !received (Int64.of_int n);
             maybe_tick ()
           done
         with End_of_file -> ());
        on_progress ~received:!received ~total

  (* Scope each response to its consumer, including errors and cancellation.
     An explicit identity encoding preserves archive bytes and Content-Length. *)
  let headers = Fetch.Header.[ raw "Accept-Encoding" "identity" ]

  let save_response ?on_progress ~clock ~url ~dst resp =
    let status = Fetch.status resp in
    if status < 200 || status >= 300 then (
      Log.debug (fun m -> m "http %s: status %d" url status);
      false)
    else
      let total = Fetch.header Fetch.Header.content_length resp in
      Eio.Path.with_open_out ~create:(`Or_truncate 0o644) dst (fun out ->
          copy_with_progress ~clock ?on_progress ?total (Fetch.body resp) out);
      true

  let protect ~url f =
    try f () with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn ->
        Log.debug (fun m -> m "http %s: %s" url (Printexc.to_string exn));
        false

  let client ~sw () =
    let http_version =
      match Sys.getenv_opt "OI_FORCE_HTTP1" with
      | Some v when v <> "" && v <> "0" -> `Http1_1
      | _ -> `Auto
    in
    Fetch_curl.v ~sw ~http_version ~max_response:max_int
      ~max_connections_per_host:8 ()

  let fetch ?on_progress t ~url ~dst =
    with_http_timeout ~clock:t.clock ~url @@ fun () ->
    protect ~url @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    Fetch.with_response ~headers (client ~sw ()) `GET url
      (save_response ?on_progress ~clock:t.clock ~url ~dst)

  let head t ~url =
    with_http_timeout ~clock:t.clock ~url @@ fun () ->
    protect ~url @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    Fetch.with_response ~headers (client ~sw ()) `HEAD url (fun resp ->
        let status = Fetch.status resp in
        status >= 200 && status < 300)

  type session = { req : Fetch_curl.t; clock : clk }

  let with_session ~sw (t : t) f = f { req = client ~sw (); clock = t.clock }

  let fetch_session ?on_progress session ~url ~dst =
    with_http_timeout ~clock:session.clock ~url @@ fun () ->
    protect ~url @@ fun () ->
    Fetch.with_response ~headers session.req `GET url
      (save_response ?on_progress ~clock:session.clock ~url ~dst)
end
