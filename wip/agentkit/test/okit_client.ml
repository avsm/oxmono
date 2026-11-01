(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The client half of okitd: a real 'humpty-cpu okitd' for what a working
   session looks like, and a shell script speaking the protocol for the ways
   the far end can fail. What is being tested is that a failure is prompt,
   final and explained: a call that cannot be answered must say so with the
   tail of okitd's standard error rather than wait, and every call after it
   must say the same thing at once. *)

module Client = Okit.Client
module Proto = Okit.Proto

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

let err = function Ok _ -> "" | Error e -> e

(* The binary under test, passed by the dune rule so that the test runs the one
   this build produced. *)
let exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else "humpty-cpu"

(* The workspace goes under /tmp rather than TMPDIR, because dune sets TMPDIR
   to a path deep inside its own build directory when it runs a test, and the
   server's socket path under it is longer than a unix socket address may be. *)
let fixture name =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "okitc_test" name in
  let save p s =
    let oc = open_out (Filename.concat dir p) in
    output_string oc s;
    close_out oc
  in
  Unix.mkdir (Filename.concat dir "lib") 0o755;
  save "dune-project" "(lang dune 3.21)\n";
  save "lib/dune" "(library (name fix))\n";
  save "lib/fix.ml" "let x = 1\n";
  dir

let remove dirs =
  ignore (Sys.command (Filename.quote_command "rm" ("-rf" :: dirs)))

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

(* A dune on PATH recording the pid of the passive server okitd starts, so that
   the test can ask whether stopping the client stopped that server too. It goes
   in a directory of its own, because a file named dune in a workspace is a dune
   file. *)
let shim () =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "okitc_test" "shim" in
  let path = Filename.concat dir "dune" in
  let oc = open_out path in
  Printf.fprintf oc
    "#!/bin/sh\n\
     case \" $* \" in *\" --passive-watch-mode \"*) echo $$ > pid      ;; esac\n\
     exec %s \"$@\"\n"
    (real_dune ());
  close_out oc;
  Unix.chmod path 0o755;
  Unix.putenv "PATH" (dir ^ ":" ^ Sys.getenv "PATH");
  dir

(* The pid a script of this test wrote for itself. *)
let pid dir =
  let ic = open_in (Filename.concat dir "pid") in
  let n = int_of_string (String.trim (input_line ic)) in
  close_in ic;
  n

(* Whether a process is gone, polled: a client kills okitd and okitd asks its
   dune server to shut down without waiting for it to have done so. *)
let rec gone ~clock p n =
  match Unix.kill p 0 with
  | () when n <= 0 -> false
  | () ->
      Eio.Time.sleep clock 0.2;
      gone ~clock p (n - 1)
  | exception Unix.Unix_error (ESRCH, _, _) -> true
  | exception _ -> false

(* An okitd of a few lines of shell, speaking just enough of the protocol to
   fail in one particular way. The argv parameter of Client.start exists for
   this: a fault of the far end cannot be staged with the real okitd, which is
   written not to have one. *)
let hello_line =
  {|{"hello":{"status":"okit: a stand-in","dune":false,"merlin":false}}|}

let standin name body =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "okitc_test" name in
  let path = Filename.concat dir "okitd" in
  let oc = open_out path in
  Printf.fprintf oc "#!/bin/sh\necho $$ > %s/pid\n%s" (Filename.quote dir) body;
  close_out oc;
  Unix.chmod path 0o755;
  (dir, [ "/bin/sh"; path ])

(* A stand-in that greets and then never says another word, which is every way
   of leaving a call unanswered. *)
let silent =
  Printf.sprintf "echo '%s'\nwhile read -r line; do :; done\n" hello_line

let elapsed clock f =
  let t0 = Eio.Time.now clock in
  let r = f () in
  (r, Eio.Time.now clock -. t0)

(* A real okitd over a real dune. One session answers every call here, since
   starting a dune server is the slowest thing okitd does. *)
let serves env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let dir = fixture "ws" in
  let path = Sys.getenv "PATH" in
  let bin = shim () in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" path;
      remove [ dir; bin ])
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let started = ref [] in
      let trace = ref [] in
      match
        Client.start ~sw ~proc ~clock
          ~trace:(fun l -> started := l :: !started)
          ~argv:[ exe; "okitd"; "--dir"; dir ]
      with
      | Error e -> check ("okitd starts: " ^ e) false
      | Ok t ->
          let h = Client.hello t in
          check "the greeting says the dune tools answer" h.Proto.dune;
          check "the greeting carries the note the interface shows"
            (h.Proto.status
            =
            if h.Proto.merlin then "okit: dune tools active, ocamlmerlin found"
            else
              "okit: dune tools active, and no ocamlmerlin to answer for the \
               workspace");
          check "the steps of the start reach the session's trace"
            (!started <> []);
          check "a started session is alive" (Client.alive t);
          let out =
            Client.call t ~timeout:180.
              (Proto.Build { targets = "." })
              ~on_trace:(fun l -> trace := l :: !trace)
          in
          check "a build answers" (out = Ok "build ok");
          check "a build streams its traces to the call" (!trace <> []);
          check "a build names itself on the trace"
            (List.exists (fun l -> holds l "build") !trace);
          let out =
            Client.call t ~timeout:180.
              (Proto.Project { module_ = "" })
              ~on_trace:ignore
          in
          check "a second call is answered on the same session"
            (Result.is_ok out && holds (Result.value out ~default:"") "fix");
          let out =
            Client.call t ~timeout:180.
              (Proto.Bash { command = "echo hello" })
              ~on_trace:ignore
          in
          check "a third call is answered too" (out = Ok "hello\n");
          Client.stop t;
          check "a stopped session is not alive" (not (Client.alive t));
          check "a call after a stop says so"
            (holds (err (Client.call t Proto.Test ~on_trace:ignore)) "stopped");
          check "stopping the client stops the dune server okitd started"
            (gone ~clock (pid dir) 50);
          Client.stop t;
          check "stopping twice does nothing" (not (Client.alive t)))

(* okitd dying with a call in flight. The call must be answered, promptly, with
   what okitd last said, and so must every call after it. *)
let dies env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let dir, argv =
    standin "killed"
      (Printf.sprintf
         "echo '%s'\n\
          while read -r line; do\n\
         \  echo 'the stand-in was killed with a call in flight' >&2\n\
         \  kill -9 $$\n\
          done\n"
         hello_line)
  in
  Fun.protect
    ~finally:(fun () -> remove [ dir ])
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      match Client.start ~sw ~proc ~clock ~trace:ignore ~argv with
      | Error e -> check ("a stand-in starts: " ^ e) false
      | Ok t ->
          let first, took =
            elapsed clock (fun () ->
                Client.call t (Proto.Bash { command = "x" }) ~on_trace:ignore)
          in
          check "a call whose server dies is answered rather than left waiting"
            (Result.is_error first && took < 30.);
          check "the answer says the server went"
            (holds (err first) "okit's server");
          check "the answer carries the tail of its standard error"
            (holds (err first) "killed with a call in flight");
          check "a dead session is not alive" (not (Client.alive t));
          let again, took =
            elapsed clock (fun () ->
                Client.call t (Proto.Bash { command = "x" }) ~on_trace:ignore)
          in
          check "a later call gives the same reason without waiting"
            (again = first && took < 5.);
          check "the pid file names a process that is gone"
            (gone ~clock (pid dir) 25))

(* A line the client cannot read is a fault of the peer, which is this same
   binary. The session ends on it rather than going on to answer the wrong
   question. *)
let garbage env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let dir, argv =
    standin "garbage"
      (Printf.sprintf
         "echo '%s'\nwhile read -r line; do echo 'not a message'; done\n"
         hello_line)
  in
  Fun.protect
    ~finally:(fun () -> remove [ dir ])
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      match Client.start ~sw ~proc ~clock ~trace:ignore ~argv with
      | Error e -> check ("a stand-in starts: " ^ e) false
      | Ok t ->
          let answer =
            Client.call t (Proto.Bash { command = "x" }) ~on_trace:ignore
          in
          check "a line that is not a message ends the call"
            (Result.is_error answer);
          check "and the offending line is quoted"
            (holds (err answer) "not a message");
          check "and the session with it" (not (Client.alive t));
          check "the peer is killed for it" (gone ~clock (pid dir) 25))

(* A bound the caller sets, and a server that never answers. The call must come
   back on time and the server must be gone, since a client that gave up on a
   call it cannot cancel would otherwise leave it running. *)
let times_out env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let dir, argv = standin "silent" silent in
  Fun.protect
    ~finally:(fun () -> remove [ dir ])
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      match Client.start ~sw ~proc ~clock ~trace:ignore ~argv with
      | Error e -> check ("a stand-in starts: " ^ e) false
      | Ok t ->
          let answer, took =
            elapsed clock (fun () ->
                Client.call t ~timeout:1.
                  (Proto.Bash { command = "x" })
                  ~on_trace:ignore)
          in
          check "a call that exceeds its bound is answered at the bound"
            (Result.is_error answer && took >= 1. && took < 10.);
          check "the answer names the operation and the bound"
            (holds (err answer) "bash" && holds (err answer) "1 second");
          check "a timed-out session is not alive" (not (Client.alive t));
          check "and the server that would not answer is killed"
            (gone ~clock (pid dir) 25))

(* A stop while another fiber has a call in flight. The call must be answered
   by the stop rather than left waiting on the okitd being closed, which is the
   freeze this whole design exists to prevent. *)
let stops_a_call env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let dir, argv = standin "racing" silent in
  Fun.protect
    ~finally:(fun () -> remove [ dir ])
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      match Client.start ~sw ~proc ~clock ~trace:ignore ~argv with
      | Error e -> check ("a stand-in starts: " ^ e) false
      | Ok t ->
          let answer = ref (Ok "") and took = ref 0. in
          Eio.Fiber.both
            (fun () ->
              let r, d =
                elapsed clock (fun () ->
                    Client.call t
                      (Proto.Bash { command = "x" })
                      ~on_trace:ignore)
              in
              answer := r;
              took := d)
            (fun () ->
              Eio.Time.sleep clock 0.5;
              Client.stop t);
          check "a stop answers the call it interrupts"
            (Result.is_error !answer && !took < 30.);
          check "and the answer says the session was stopped"
            (holds (err !answer) "stopped");
          check "a stopped session is not alive" (not (Client.alive t)))

(* A fiber that gives up on a call. It must not leave the lock disabled behind
   it, or every later call raises where this interface promises an error. *)
let cancelled env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let dir, argv = standin "cancelled" silent in
  Fun.protect
    ~finally:(fun () -> remove [ dir ])
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      match Client.start ~sw ~proc ~clock ~trace:ignore ~argv with
      | Error e -> check ("a stand-in starts: " ^ e) false
      | Ok t ->
          let outcome =
            Eio.Fiber.first
              (fun () ->
                `Answered
                  (Client.call t
                     (Proto.Bash { command = "x" })
                     ~on_trace:ignore))
              (fun () ->
                Eio.Time.sleep clock 0.5;
                `Gave_up)
          in
          check "a fiber that gives up on a call is not held by it"
            (outcome = `Gave_up);
          check "a cancelled call ends the session" (not (Client.alive t));
          let again, took =
            elapsed clock (fun () ->
                Client.call t (Proto.Bash { command = "x" }) ~on_trace:ignore)
          in
          check "a later call answers rather than raising"
            (Result.is_error again && took < 5.);
          check "and says the call was cancelled"
            (holds (err again) "cancelled");
          check "the server left with the call is killed"
            (gone ~clock (pid dir) 25))

(* Two greetings, which no okitd of this build sends. *)
let greets_twice env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let dir, argv =
    standin "twice"
      (Printf.sprintf "echo '%s'\necho '%s'\nwhile read -r line; do :; done\n"
         hello_line hello_line)
  in
  Fun.protect
    ~finally:(fun () -> remove [ dir ])
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      match Client.start ~sw ~proc ~clock ~trace:ignore ~argv with
      | Error e -> check ("a stand-in starts: " ^ e) false
      | Ok t ->
          let answer =
            Client.call t (Proto.Bash { command = "x" }) ~on_trace:ignore
          in
          check "a second greeting is a fault" (holds (err answer) "twice");
          check "and it ends the session" (not (Client.alive t)))

(* A result for a call nobody made. The client cannot tell which answer belongs
   to which question after that, so the session ends. *)
let answers_another_call env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let dir, argv =
    standin "mismatch"
      (Printf.sprintf
         "echo '%s'\n\
          while read -r line; do echo \
          '{\"result\":{\"id\":99,\"output\":\"x\"}}'; done\n"
         hello_line)
  in
  Fun.protect
    ~finally:(fun () -> remove [ dir ])
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      match Client.start ~sw ~proc ~clock ~trace:ignore ~argv with
      | Error e -> check ("a stand-in starts: " ^ e) false
      | Ok t ->
          let answer =
            Client.call t (Proto.Bash { command = "x" }) ~on_trace:ignore
          in
          check "an answer to a call nobody made is a fault"
            (holds (err answer) "answered call 99");
          check "and it ends the session" (not (Client.alive t)))

(* A server that leaves before it greets. The start fails rather than waiting
   out its window, and says what the server said on the way out. *)
let never_greets env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let dir, argv =
    standin "mute" "echo 'the stand-in refuses to start' >&2\nexit 1\n"
  in
  Fun.protect
    ~finally:(fun () -> remove [ dir ])
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let answer, took =
        elapsed clock (fun () ->
            Client.start ~sw ~proc ~clock ~trace:ignore ~argv)
      in
      match answer with
      | Ok _ -> check "a server that never greets fails the start" false
      | Error e ->
          check "a server that never greets fails the start at once" (took < 30.);
          check "the failure says the greeting never came"
            (holds e "before it greeted");
          check "the failure carries the tail of its standard error"
            (holds e "refuses to start"))

(* A command that cannot be run at all. *)
let never_spawns env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  match
    Client.start ~sw ~proc ~clock ~trace:ignore ~argv:[ "/nonexistent/okitd" ]
  with
  | Ok _ -> check "a command that does not exist fails the start" false
  | Error e ->
      check "a command that does not exist fails the start"
        (holds e "could not be started" && holds e "/nonexistent/okitd")

let () =
  Eio_main.run (fun env ->
      serves env;
      dies env;
      garbage env;
      times_out env;
      stops_a_call env;
      cancelled env;
      greets_twice env;
      answers_another_call env;
      never_greets env;
      never_spawns env);
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
