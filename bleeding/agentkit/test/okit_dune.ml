(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Dune_rpc.Private
module Session = Okit.Session

(* The passive dune session against a real dune. The property that matters:
   a build result corresponds to the write that preceded it, including the
   diagnostics of code written after the server started.

   A target is a path or an alias written as dune's command line writes it,
   "@check" or "@@check", which the session turns into the dep-spec the server
   reads. A dep-spec written by hand is refused before the request is sent,
   since the server answers a malformed one with a Code_error naming nothing. *)

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

let holds s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

(* A trace that keeps what it was told, so a test can ask whether a step said
   anything about itself. *)
let recorder () =
  let seen = ref [] in
  ( (fun line -> seen := line :: !seen),
    fun sub -> List.exists (fun l -> holds l sub) !seen )

(* The workspace goes under /tmp rather than TMPDIR, because dune sets TMPDIR
   to a path deep inside its own build directory when it runs a test, and the
   server's socket path under it is longer than a unix socket address may be.
*)
let fixture ~fs name =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "okit_test" name in
  let root = Eio.Path.(fs / dir) in
  let save p s =
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(root / p) s
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / "lib");
  save "dune-project" "(lang dune 3.21)\n";
  save "lib/dune" "(library (name fix))\n";
  save "lib/fix.ml" "let x = 1\n";
  (dir, root, save)

(* The dune a test means when it puts a script of its own on PATH ahead of the
   real one. *)
let real_dune () =
  let dirs = String.split_on_char ':' (Sys.getenv "PATH") in
  let exe d =
    Filename.concat (if d = "" then Filename.current_dir_name else d) "dune"
  in
  match List.find_opt (fun d -> Sys.file_exists (exe d)) dirs with
  | Some d -> exe d
  | None -> failwith "the tests need dune on PATH"

(* A dune on PATH whose script is [body], so that what a test wants to know
   about the process okit spawned is recorded by the process itself. It goes in
   a directory of its own rather than in the workspace, because a file named
   dune in a workspace is a dune file. The answer is the directory, for the
   caller to remove. *)
let shim ~body =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "okit_test" "shim" in
  let path = Filename.concat dir "dune" in
  let oc = open_out path in
  Printf.fprintf oc "#!/bin/sh\n%s\n" body;
  close_out oc;
  Unix.chmod path 0o755;
  Unix.putenv "PATH" (dir ^ ":" ^ Sys.getenv "PATH");
  dir

(* A shim that records something and then becomes the real dune. *)
let and_then_dune before =
  Printf.sprintf "%s\nexec %s \"$@\"" before (real_dune ())

(* A stand-in dune server. It answers the handshake, selecting the highest
   version the client offered for each method, and hands every method name it is
   asked for, the handshake's own included, to [f]. A request is answered with
   what [f] returns for it. [cut] drops the last four bytes of one reply and
   hangs up, which is the one thing a real server cannot be asked to do on
   cue. *)
let serve ?(cut = false) flow ~f =
  let chan = Dune_rpc_eio.Chan.create flow in
  let send sexp = Eio.Flow.copy_string (Csexp.to_string sexp) flow in
  let reply id payload =
    let s =
      Csexp.to_string
        (Conv.to_sexp Packet.sexp (Packet.Response (id, Ok payload)))
    in
    if cut then begin
      Eio.Flow.copy_string (String.sub s 0 (String.length s - 4)) flow;
      Eio.Flow.shutdown flow `All
    end
    else Eio.Flow.copy_string s flow
  in
  let rec go () =
    match Dune_rpc_eio.Chan.read chan with
    | None -> ()
    | Some sexp -> (
        match Conv.of_sexp Packet.sexp ~version:Version.latest sexp with
        | Error _ -> ()
        | Ok (Packet.Notification call) ->
            ignore (f (Method.Name.to_string call.method_));
            go ()
        | Ok (Packet.Request (id, call)) -> (
            match Method.Name.to_string call.method_ with
            | "initialize" as m ->
                ignore (f m);
                send
                  (Conv.to_sexp Packet.sexp
                     (Packet.Response
                        ( id,
                          Ok
                            (Initialize.Response.to_response
                               (Initialize.Response.create ())) )));
                go ()
            | "version_menu" as m ->
                ignore (f m);
                let (Menu offered) =
                  Result.get_ok
                    (Version_negotiation.Request.of_call call
                       ~version:Version.latest)
                in
                let picked =
                  List.map (fun (m, vs) -> (m, List.fold_left max 1 vs)) offered
                in
                send
                  (Conv.to_sexp Packet.sexp
                     (Packet.Response
                        ( id,
                          Ok
                            (Version_negotiation.Response.to_response
                               (Version_negotiation.Response.create picked)) )));
                go ()
            | m ->
                reply id (f m);
                go ())
        | Ok (Packet.Response _) -> go ())
  in
  go ()

(* A stand-in for dune that answers the handshake and then dies half way
   through a reply. A truncated message must read as the server going away, so
   that an owned session relaunches one, and not as bytes that were wrong. *)
let dying_server ~sw ~net ~root =
  let path = Eio.Path.native_exn root ^ "/_build/.rpc/dune" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
    Eio.Path.(root / "_build" / ".rpc");
  let listening =
    Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net (`Unix path)
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow, _ = Eio.Net.accept ~sw listening in
      serve ~cut:true flow ~f:(fun _ -> Csexp.List []))

let dies_mid_message env =
  let net = Eio.Stdenv.net env
  and proc = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env
  and fs = Eio.Stdenv.fs env in
  let dir, root, _ = fixture ~fs "cut" in
  (* The switch finishes first, since it is what unlinks the socket. *)
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      dying_server ~sw ~net ~root;
      match Session.start ~sw ~proc ~net ~clock ~root () with
      | Error e ->
          print_endline e;
          check "attaches to a server that already holds the workspace" false
      | Ok t ->
          check "attaches to a server that already holds the workspace" true;
          check "a reply cut short reads as the server going away"
            (match Session.build t ~targets:[ "." ] with
            | Error e -> holds e "went away" && not (holds e "csexp")
            | Ok _ -> false);
          Session.stop t)

(* A dune that starts and never opens a socket, which is what a server too busy
   to listen and a server that has wedged both look like. The session must give
   up saying so, and must not leave the process it started behind: it holds the
   workspace's build lock, and dune refuses to build a workspace another dune
   holds, so every later build in that workspace would fail for a reason
   nothing states. *)
let no_socket env =
  let net = Eio.Stdenv.net env
  and proc = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env
  and fs = Eio.Stdenv.fs env in
  let dir, root, save = fixture ~fs "nosock" in
  let path = Sys.getenv "PATH" in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" path;
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
    (fun () ->
      (* [exec], so that the pid the script records is the process okit
         spawned and not a shell that outlives what it was waiting for. *)
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / "fakebin");
      save "fakebin/dune" "#!/bin/sh\necho $$ > pid\nexec sleep 60\n";
      Unix.chmod (Filename.concat dir "fakebin/dune") 0o755;
      Unix.putenv "PATH" (Filename.concat dir "fakebin" ^ ":" ^ path);
      Eio.Switch.run @@ fun sw ->
      let trace, saw = recorder () in
      match Session.start ~trace ~sw ~proc ~net ~clock ~root () with
      | Ok t ->
          Session.stop t;
          check "a dune that opens no socket is refused" false
      | Error e ->
          check "a dune that opens no socket is refused"
            (holds e "RPC socket" && holds e "30 seconds");
          (* A wait this long must not look like a session that has stopped
             taking steps, so it says how far along it is. This is the one test
             whose wait is certain to run for whole seconds. *)
          check "a wait for a socket traces its progress"
            (saw "waiting for socket");
          let pid =
            int_of_string (String.trim (Eio.Path.load Eio.Path.(root / "pid")))
          in
          check "the dune it started is stopped"
            (match Unix.kill pid 0 with
            | () -> false
            | exception Unix.Unix_error (ESRCH, _, _) -> true
            | exception _ -> false))

(* A server killed outright cannot unlink its socket, so the file outlives the
   process that made it. Connecting there is refused, which is a workspace with
   no server rather than one okit may not touch, and okit must start a server of
   its own. Dune replaces the socket file when it opens one, so nothing here
   removes it. *)
let stale_socket env =
  let net = Eio.Stdenv.net env
  and proc = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env
  and fs = Eio.Stdenv.fs env in
  let dir, root, _ = fixture ~fs "stale" in
  let path = Sys.getenv "PATH" in
  let socket = Filename.concat dir "_build/.rpc/dune" in
  let recorded () =
    int_of_string (String.trim (Eio.Path.load Eio.Path.(root / "pid")))
  in
  let bin = shim ~body:(and_then_dune "echo $$ > pid") in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" path;
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir; bin ])))
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      match Session.start ~sw ~proc ~net ~clock ~root () with
      | Error e ->
          print_endline e;
          check "a server starts in a workspace with no socket" false
      | Ok t ->
          check "a server starts in a workspace with no socket" true;
          let killed = recorded () in
          Unix.kill killed Sys.sigkill;
          Eio.Time.sleep clock 0.5;
          check "the socket outlives the server that was killed"
            (Sys.file_exists socket);
          Unix.unlink (Filename.concat dir "pid");
          let trace, saw = recorder () in
          (match Session.start ~trace ~sw ~proc ~net ~clock ~root () with
          | Error e ->
              print_endline e;
              check "a stale socket reads as a workspace with no server" false
          | Ok t2 ->
              check "a stale socket reads as a workspace with no server" true;
              check "a stale socket traces what it was" (saw "stale socket");
              check "the session started a server of its own"
                (Sys.file_exists (Filename.concat dir "pid")
                && recorded () <> killed);
              check "the server it started answers"
                (match Session.build t2 ~targets:[ "." ] with
                | Ok { ok = true; _ } -> true
                | Ok _ -> false
                | Error e ->
                    print_endline e;
                    false);
              Session.stop t2);
          Session.stop t)

(* The first line of an error names the dune that just failed, and a session
   that has had more than one server must not confuse them. The tail a caller is
   shown spans the whole session, so the line that names a relaunch's failure
   has to come from the child the relaunch spawned rather than from the oldest
   line still in the ring.

   The shim is one dune the first time and another the second: it starts a real
   server, and when that server has been killed and the session starts one in
   its place, it refuses and says something of its own. *)
let relaunch_says_what_its_own_child_said env =
  let net = Eio.Stdenv.net env
  and proc = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env
  and fs = Eio.Stdenv.fs env in
  let dir, root, _ = fixture ~fs "relaunch" in
  let path = Sys.getenv "PATH" in
  let bin =
    shim
      ~body:
        (Printf.sprintf
           "if [ -f count ]; then echo SECONDCHILD; exit 1; fi\n\
            : > count\n\
            echo FIRSTCHILD\n\
            echo $$ > pid\n\
            exec %s \"$@\""
           (real_dune ()))
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" path;
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir; bin ])))
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      match Session.start ~sw ~proc ~net ~clock ~root () with
      | Error e ->
          print_endline e;
          check "a server starts for a session that will lose it" false
      | Ok t ->
          check "a server starts for a session that will lose it" true;
          let killed =
            int_of_string (String.trim (Eio.Path.load Eio.Path.(root / "pid")))
          in
          Unix.kill killed Sys.sigkill;
          Eio.Time.sleep clock 0.5;
          (match Session.build t ~targets:[ "." ] with
          | Ok _ -> check "a relaunch that is refused reports a failure" false
          | Error e ->
              check "a relaunch that is refused reports a failure" true;
              let first =
                match String.index_opt e '\n' with
                | None -> e
                | Some i -> String.sub e 0 i
              in
              check "a relaunch is named by the dune it just spawned"
                (holds first "SECONDCHILD" && not (holds first "FIRSTCHILD"));
              (* The whole session's output is still there behind that line,
                 since what an earlier server said is context a reader wants. *)
              check "the session's own output is still shown in full"
                (holds e "FIRSTCHILD"));
          Session.stop t)

(* Humpty run under dune exec has INSIDE_DUNE, DUNE_BUILD_DIR and DUNE_RPC in
   its environment. A server that inherited them would take its own directory
   for the workspace root and would open its socket somewhere other than the
   path okit watches, so none of the three may reach the child.

   This test leaves the three variables set, since Unix has no way to remove
   one, and so it runs last. *)
let spawn_environment env =
  let net = Eio.Stdenv.net env
  and proc = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env
  and fs = Eio.Stdenv.fs env in
  let dir, root, _ = fixture ~fs "env" in
  let path = Sys.getenv "PATH" in
  let elsewhere = Filename.temp_dir ~temp_dir:"/tmp" "okit_test" "elsewhere" in
  let dropped = [ "INSIDE_DUNE"; "DUNE_BUILD_DIR"; "DUNE_RPC" ] in
  let bin = shim ~body:(and_then_dune "env > env") in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" path;
      ignore
        (Sys.command
           (Filename.quote_command "rm" [ "-rf"; dir; elsewhere; bin ])))
    (fun () ->
      Unix.putenv "INSIDE_DUNE" "1";
      Unix.putenv "DUNE_BUILD_DIR" elsewhere;
      Unix.putenv "DUNE_RPC" (Filename.concat elsewhere "rpc");
      Eio.Switch.run @@ fun sw ->
      match Session.start ~sw ~proc ~net ~clock ~root () with
      | Error e ->
          print_endline e;
          check "a server starts with a build directory set elsewhere" false
      | Ok t ->
          check "a server starts with a build directory set elsewhere" true;
          let lines =
            String.split_on_char '\n' (Eio.Path.load Eio.Path.(root / "env"))
          in
          List.iter
            (fun v ->
              check
                (Printf.sprintf "the server is spawned without %s" v)
                (not
                   (List.exists
                      (fun l -> String.starts_with ~prefix:(v ^ "=") l)
                      lines)))
            dropped;
          Session.stop t)

(* A socket file with nobody behind it, which is what a dune that was killed
   leaves. A socket that is bound and never listened on refuses a connection in
   the same way, and a test can make one on cue. *)
let dead_socket root =
  let path = Eio.Path.native_exn root ^ "/_build/.rpc/dune" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
    Eio.Path.(root / "_build" / ".rpc");
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind fd (Unix.ADDR_UNIX path);
  Unix.close fd;
  path

(* The server the other humpty started: it takes the socket once okit's own dune
   has died, answers the handshake, and then says what it was asked for next.
   The answer is a promise of that method's name, [`Closed] for a session that
   said nothing and went. A daemon, so that a test whose session never connects
   ends rather than waiting on the accept. *)
let winning_server ~sw ~net ~clock ~root =
  let path = Eio.Path.native_exn root ^ "/_build/.rpc/dune" in
  let heard, tell = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Switch.run @@ fun sw ->
      (* Late enough that okit has spawned its dune and seen it die. *)
      Eio.Time.sleep clock 0.3;
      (try Unix.unlink path with Unix.Unix_error _ -> ());
      let listening =
        Eio.Net.listen ~sw ~backlog:5 ~reuse_addr:true net (`Unix path)
      in
      let handshake = [ "initialize"; "version_menu" ] in
      let rec accept_one () =
        let flow, _ = Eio.Net.accept ~sw listening in
        let asked = ref [] in
        serve flow ~f:(fun m ->
            asked := m :: !asked;
            Csexp.List []);
        (* okit asks the socket whether anyone is there before it makes a
           session on it, so the connection that said nothing at all is not the
           one this test is about. *)
        match List.rev !asked with
        | [] -> accept_one ()
        | methods -> (
            match List.filter (fun m -> not (List.mem m handshake)) methods with
            | [] -> Eio.Promise.resolve tell `Closed
            | m :: _ -> Eio.Promise.resolve tell (`Sent m))
      in
      accept_one ();
      `Stop_daemon);
  heard

(* Two humpties that start on the same workspace both spawn a dune, and only one
   of them takes the workspace's build lock. The loser's dune exits at once and
   its session then reaches the winner's server, which it did not start and must
   not stop. Spawning is not owning.

   [stale] says whether a socket file one of them left behind is at the path.
   The race runs the same either way, and the session must not tell the two
   apart: a workspace with no file at all is what two humpties starting together
   meet, and the loser's child dies there just as it does over a stale file. *)
let loses_the_race ~stale env =
  let net = Eio.Stdenv.net env
  and proc = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env
  and fs = Eio.Stdenv.fs env in
  let dir, root, _ = fixture ~fs (if stale then "race" else "clean") in
  let path = Sys.getenv "PATH" in
  let what = if stale then "over a stale socket" else "in a clean workspace" in
  let named s = Printf.sprintf "%s, %s" s what in
  (* The dune of the humpty that lost: it exits without opening anything. *)
  let bin = shim ~body:"exit 1" in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" path;
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir; bin ])))
    (fun () ->
      if stale then ignore (dead_socket root)
      else
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
          Eio.Path.(root / "_build" / ".rpc");
      Eio.Switch.run @@ fun sw ->
      let heard = winning_server ~sw ~net ~clock ~root in
      match Session.start ~sw ~proc ~net ~clock ~root () with
      | Error e ->
          print_endline e;
          check (named "a session reaches the server that won the race") false
      | Ok t ->
          check (named "a session reaches the server that won the race") true;
          Session.stop t;
          check
            (named "a session does not shut down a server it did not start")
            (match Eio.Promise.await heard with
            | `Closed -> true
            | `Sent m ->
                print_endline m;
                false))

(* A dune started in a workspace another dune instance already holds does not
   serve it. It refuses the workspace, naming the instance that holds it, and
   exits without opening a socket, so a session that watched only the socket
   would wait its whole window for one that cannot appear. The wait watches the
   child it spawned as well, and ends when that child does.

   The holding server's socket is removed first, so that the session takes the
   spawning path rather than attaching to the server that is there. *)
let already_running env =
  let net = Eio.Stdenv.net env
  and proc = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env
  and fs = Eio.Stdenv.fs env in
  let dir, root, _ = fixture ~fs "busy" in
  let socket = Filename.concat dir "_build/.rpc/dune" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let log =
        Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644)
          Eio.Path.(root / "holder.log")
      in
      let holder =
        Eio.Process.spawn ~sw proc ~cwd:root ~stdout:log ~stderr:log
          [ "dune"; "build"; "--passive-watch-mode" ]
      in
      let pid = Eio.Process.pid holder in
      (* The holder is this test's to end. A dune left running holds the
         workspace's build lock against everything that comes after it. *)
      Fun.protect
        ~finally:(fun () ->
          Eio.Cancel.protect @@ fun () ->
          (try Eio.Process.signal holder Sys.sigterm
           with Eio.Io _ | Invalid_argument _ -> ());
          (match
             Eio.Time.with_timeout clock 10. (fun () ->
                 Ok (Eio.Process.await holder))
           with
          | Ok _ -> ()
          | Error `Timeout -> (
              try
                Eio.Process.signal holder Sys.sigkill;
                ignore (Eio.Process.await holder)
              with Eio.Io _ | Invalid_argument _ -> ()));
          check "the dune the test started is stopped"
            (match Unix.kill pid 0 with
            | () -> false
            | exception Unix.Unix_error (ESRCH, _, _) -> true
            | exception _ -> false))
        (fun () ->
          let rec appears n =
            if Sys.file_exists socket then true
            else if n <= 0 then false
            else begin
              Eio.Time.sleep clock 0.1;
              appears (n - 1)
            end
          in
          if not (appears 300) then
            check "another dune comes to hold the workspace" false
          else begin
            check "another dune comes to hold the workspace" true;
            Unix.unlink socket;
            let trace, saw = recorder () in
            let began = Eio.Time.now clock in
            match Session.start ~trace ~sw ~proc ~net ~clock ~root () with
            | Ok t ->
                Session.stop t;
                check "a dune that finds the workspace held is refused" false
            | Error e ->
                check "a dune that finds the workspace held is refused"
                  (holds e "forward" || holds e "running");
                (* Two lines, since the grace between them is otherwise two
                   seconds in which the trace says nothing. Only the second
                   carries a colon and the reason. *)
                check "the grace after the child says it has begun"
                  (saw "server exited, checking");
                check "the child that exited is traced as such"
                  (saw "server exited:");
                (* The window is thirty seconds and the grace after the child
                   goes is two. A bound just above the grace says the wait
                   ended with the child, without depending on how fast dune
                   refuses. *)
                check "the wait ends with the child, not with the window"
                  (Eio.Time.now clock -. began < 6.)
          end))

(* A diagnostic reads as dune prints it: a File header from the location, then
   the message, then one line per promotion. *)
let renders text =
  check "a diagnostic names its file" (holds text "File \"");
  check "a diagnostic carries its message" (holds text "Error")

let run env =
  Eio.Switch.run @@ fun sw ->
  let proc = Eio.Stdenv.process_mgr env
  and net = Eio.Stdenv.net env
  and clock = Eio.Stdenv.clock env
  and fs = Eio.Stdenv.fs env in
  let dir, root, save = fixture ~fs "session" in
  (* The callback is the caller's, and this one raises every time. A session
     traces from inside the critical section that serialises its requests, so an
     exception escaping there would disable the mutex and every later call would
     raise [Eio.Mutex.Poisoned] instead of answering. Every check below is
     therefore also a check that a raising callback changes nothing. *)
  let record, saw = recorder () in
  let trace s =
    record s;
    failwith "the caller's trace callback raised"
  in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
    (fun () ->
      match Session.start ~trace ~sw ~proc ~net ~clock ~root () with
      | Error e ->
          print_endline e;
          check "start" false
      | Ok t ->
          check "start" true;
          check "starting a server traces the spawn" (saw "spawning server");
          (* A spawn forks this process, which for a caller holding a model is
             where a start can stall. The child's existence is traced on its
             own, so that a trace stopping at the line above is that fork and
             not a server slow to open its socket. *)
          check "starting a server traces that the child exists"
            (saw "server spawned");
          (* The handshake reads with nothing bounding it, so it must be named
             before it begins and not once it has answered. *)
          check "starting a server traces the handshake" (saw "handshake");
          (match Session.build t ~targets:[ "." ] with
          | Ok { ok = true; diagnostics = [] } -> check "clean build" true
          | _ -> check "clean build" false);
          check "a build traces itself" (saw "build");
          (* Every request a build makes is named before it is sent, or the
             newest line during a block names the request before it. *)
          check "a build traces the diagnostics it asks for" (saw "diagnostics");
          check "an alias target builds"
            (match Session.build t ~targets:[ "@check" ] with
            | Ok { ok = true; _ } -> true
            | _ -> false);
          (* An alias no dune file defines is a build that fails, not a session
             that errors, so the caller reads dune's own answer. *)
          check "an alias nothing defines fails the build"
            (match Session.build t ~targets:[ "@@nosuch" ] with
            | Ok { ok = false; _ } -> true
            | _ -> false);
          (* okit writes the dep-spec itself, so one written by hand is refused
             with the spelling to write. Passing it on would hand the server a
             malformed s-expression, which it answers with a Code_error that
             names nothing. *)
          check "a raw dep-spec is refused, naming the alias spelling"
            (match Session.build t ~targets:[ "(alias check)" ] with
            | Error e -> holds e "dep-spec" && holds e "@check"
            | Ok _ -> false);
          check "an empty alias name is refused, naming the form to write"
            (match Session.build t ~targets:[ "@" ] with
            | Error e -> holds e "empty" && holds e "@check"
            | Ok _ -> false);
          save "lib/fix.ml" "let x : int = \"no\"\n";
          (match Session.build t ~targets:[ "." ] with
          | Ok { ok = false; diagnostics = d :: _ } ->
              check "failing build" true;
              check "diagnostic locates the file"
                (match Diagnostic.loc d with
                | Some l ->
                    let p = Loc.start l in
                    Filename.basename p.pos_fname = "fix.ml" && p.pos_lnum = 1
                | None -> false);
              let text = Okit.Report.to_text d in
              check "diagnostic says why"
                (holds text "string" && holds text "int");
              renders text
          | _ -> check "failing build" false);
          save "lib/fix.ml" "let x = 1\n";
          (match Session.build t ~targets:[ "." ] with
          | Ok { ok = true; diagnostics = [] } -> check "recovers" true
          | _ -> check "recovers" false);
          check "a trace callback that raises does not poison the session"
            (match Session.build t ~targets:[ "." ] with
            | Ok _ -> true
            | Error e ->
                print_endline e;
                false);
          (* Tests go by their own method rather than by the @runtest alias,
             which the build method will not take. The fixture defines no
             tests, so running them all is a build of nothing. *)
          (match Session.runtest t with
          | Ok { ok = true; diagnostics = [] } -> check "runtest" true
          | Ok _ -> check "runtest" false
          | Error e ->
              print_endline e;
              check "runtest" false);
          (* A test whose output differs from what is recorded, which is the
             only way to reach a promotion. The path promoted is relative,
             since that is what the tool built on this session passes and what
             dune resolves against the workspace it serves. *)
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / "t");
          save "t/dune"
            "(executable (name t))\n\
             (rule (with-stdout-to t.out (run ./t.exe)))\n\
             (rule (alias runtest) (action (diff t.expected t.out)))\n";
          save "t/t.ml" "let () = print_endline \"hello\"\n";
          save "t/t.expected" "goodbye\n";
          (match Session.runtest t with
          | Ok { ok = false; diagnostics = d :: _ } ->
              check "a test whose output differs offers a promotion"
                (Diagnostic.promotion d <> []);
              check "a relative path is promoted"
                (match Session.promote t ~path:"t/t.expected" with
                | Ok () -> true
                | Error e ->
                    print_endline e;
                    false);
              check "the promoted test passes"
                (match Session.runtest t with
                | Ok { ok = true; _ } -> true
                | _ -> false)
          | _ -> check "a test whose output differs offers a promotion" false);
          (* The server still holds the workspace's build lock here, which is
             the case a map has to survive: the agent asks what the workspace
             looks like without giving up the session it is building through. *)
          check "the map is taken while the server holds the workspace"
            (match Okit.Project.describe ~proc ~root () with
            | Ok p -> p.components <> []
            | Error e ->
                print_endline e;
                false);
          Session.stop t;
          (* A stopped session reports that it is stopped. It must not raise,
             and an owned server must not be started again to serve the
             call. *)
          check "a stopped session refuses a build"
            (match Session.build t ~targets:[ "." ] with
            | Error e -> holds e "stopped"
            | Ok _ -> false);
          check "a stopped session refuses a promotion"
            (match Session.promote t ~path:"lib/fix.ml" with
            | Error e -> holds e "stopped"
            | Ok () -> false);
          Session.stop t)

(* The map of a workspace whose libraries depend on one another. What a reader
   needs from it is the dependency, the directory and the modules, all named as
   the source tree names them rather than as the build directory does. *)
let describes env =
  let proc = Eio.Stdenv.process_mgr env and fs = Eio.Stdenv.fs env in
  let dir, root, save = fixture ~fs "describe" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
    (fun () ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / "lib2");
      save "lib2/dune" "(library (name fix2) (libraries fix))\n";
      save "lib2/fix2.ml" "let y = Fix.x\n";
      (* A directory that is no workspace is a refusal saying so, rather than a
         map with nothing in it, which would send a reader looking for code
         that is not missing. *)
      let bare = Filename.temp_dir ~temp_dir:"/tmp" "okit_test" "bare" in
      check "a directory that is no workspace is refused"
        (match Okit.Project.describe ~proc ~root:Eio.Path.(fs / bare) () with
        | Ok _ -> false
        | Error e -> holds e bare && holds e "workspace");
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; bare ]));
      let trace, saw = recorder () in
      match Okit.Project.describe ~trace ~proc ~root () with
      | Error e ->
          print_endline e;
          check "describe" false
      | Ok p ->
          check "describe" true;
          check "describe traces the components it found" (saw "component");
          let find n =
            List.find_opt (fun c -> c.Okit.Project.name = n) p.components
          in
          (match find "fix2" with
          | Some c ->
              check "requires resolved" (c.requires = [ "fix" ]);
              check "source dir" (c.source_dir = "lib2");
              check "modules listed"
                (List.exists
                   (fun (m : Okit.Project.module_) -> m.name = "Fix2")
                   c.modules)
          | None -> check "requires resolved" false);
          let txt = Okit.Project.to_text p in
          check "text names both" (holds txt "fix" && holds txt "fix2"))

let () =
  Eio_main.run (fun env ->
      run env;
      describes env;
      dies_mid_message env;
      no_socket env;
      stale_socket env;
      relaunch_says_what_its_own_child_said env;
      loses_the_race ~stale:true env;
      loses_the_race ~stale:false env;
      already_running env;
      spawn_environment env);
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
