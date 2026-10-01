(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The client half of numptyd: a real 'numpty-cpu netd' for what a working
   session looks like, and a shell script speaking the protocol for the ways
   the far end can fail. What is being tested is that a failure is prompt,
   final and explained: a call that cannot be answered must say so with the
   tail of numptyd's standard error rather than wait, every call after it must
   say the same thing at once, and the daemon must be able to see that the run
   is over.

   Nothing here reaches the network. The working session is exercised with
   [run], which the netd test drives beside a fetch. *)

module Client = Numpty_net.Client
module Proto = Numpty_net.Proto
module Tool = Ds4.Tool

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
let exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else "numpty-cpu"

let remove dirs =
  ignore (Sys.command (Filename.quote_command "rm" ("-rf" :: dirs)))

(* The pid a script of this test wrote for itself. *)
let pid dir =
  let ic = open_in (Filename.concat dir "pid") in
  let n = int_of_string (String.trim (input_line ic)) in
  close_in ic;
  n

(* Whether a process is gone, polled: a client kills numptyd and does not wait
   for the kernel to have reaped it. *)
let rec gone ~clock p n =
  match Unix.kill p 0 with
  | () when n <= 0 -> false
  | () ->
      Eio.Time.sleep clock 0.2;
      gone ~clock p (n - 1)
  | exception Unix.Unix_error (ESRCH, _, _) -> true
  | exception _ -> false

(* A numptyd of a few lines of shell, speaking just enough of the protocol to
   fail in one particular way. The argv parameter of Client.start exists for
   this: a fault of the far end cannot be staged with the real numptyd, which
   is written not to have one. *)
let hello_line = {|{"hello":{"status":"numpty: a stand-in","curl":false}}|}

let standin name body =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "numptyc_test" name in
  let path = Filename.concat dir "netd" in
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

(* Drive a tool as the agent does, with JSON arguments through the codec. *)
let tool_call tool args =
  Tool.invoke tool { Dsml.name = Tool.name tool; arguments = args; id = None }

(* A real numptyd. One session answers every call here. *)
let serves env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let traces = ref [] in
  match
    Client.start ~sw ~proc ~clock
      ~trace:(fun l -> traces := l :: !traces)
      ~argv:[ exe; "netd" ]
  with
  | Error e -> check ("numptyd starts: " ^ e) false
  | Ok t ->
      check "the greeting says whether a curl answered"
        ((Client.hello t).Proto.curl
        = holds (Client.hello t).Proto.status "curl");
      check "a started session is alive" (Client.alive t);
      check "and is not a fault" (Client.fault t = None);
      let seen = ref [] in
      let out =
        Client.call t
          (Proto.Run { program = "echo"; args = [ "hi" ] })
          ~on_trace:(fun l -> seen := l :: !seen)
      in
      check "a run answers" (out = Ok "echo exited 0\nhi\n");
      check "and streams its trace to the call"
        (List.exists (fun l -> holds l "echo") !seen);
      let out =
        Client.call t
          (Proto.Run { program = "echo"; args = [ "again" ] })
          ~on_trace:ignore
      in
      check "a second call is answered on the same session"
        (out = Ok "echo exited 0\nagain\n");
      (* The tools relay what the client answered, so a caller reads the same
         words whether it asked through a tool or through the protocol. *)
      let answer =
        tool_call
          (Numpty_net.Tools.run ~client:t)
          {|{"program":"echo","args":["through the tool"]}|}
      in
      check "the run tool relays the answer"
        (answer = "echo exited 0\nthrough the tool\n");
      let answer =
        tool_call
          (Numpty_net.Tools.run ~client:t)
          {|{"program":"echo","args":"hi"}|}
      in
      check "and says the form of args a caller got wrong"
        (holds answer "JSON array of strings");
      let answer =
        tool_call
          (Numpty_net.Tools.fetch ~client:t)
          {|{"url":"http://127.0.0.1/","render":"markdown"}|}
      in
      check "the fetch tool names the renders it has"
        (holds answer "\"text\"" && holds answer "\"raw\"");
      Client.stop t;
      check "a stopped session is not alive" (not (Client.alive t));
      (* A stop is not a fault. The daemon ends the run on a fault and not on
         its own shutdown, so the two must not read alike. *)
      check "and a stop is not a fault" (Client.fault t = None);
      check "a call after a stop says so"
        (holds
           (err
              (Client.call t
                 (Proto.Run { program = "echo"; args = [] })
                 ~on_trace:ignore))
           "stopped");
      Client.stop t;
      check "stopping twice does nothing" (not (Client.alive t))

(* numptyd dying with a call in flight. The call must be answered, promptly,
   with what numptyd last said, and so must every call after it. *)
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
                Client.call t
                  (Proto.Head { url = "http://127.0.0.1/" })
                  ~on_trace:ignore)
          in
          check "a call whose server dies is answered rather than left waiting"
            (Result.is_error first && took < 30.);
          check "the answer says the network child went"
            (holds (err first) "numpty's network child");
          check "the answer carries the tail of its standard error"
            (holds (err first) "killed with a call in flight");
          check "a dead session is not alive" (not (Client.alive t));
          (* The daemon reads this to stop the run, take the handover and exit
             nonzero, since there is no respawn. *)
          check "and the daemon can see it as a fault"
            (Client.fault t = Some (err first));
          let again, took =
            elapsed clock (fun () ->
                Client.call t
                  (Proto.Head { url = "http://127.0.0.1/" })
                  ~on_trace:ignore)
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
          let answer, took =
            elapsed clock (fun () ->
                Client.call t
                  (Proto.Head { url = "http://127.0.0.1/" })
                  ~on_trace:ignore)
          in
          check "a line that is not a message ends the call at once"
            (Result.is_error answer && took < 30.);
          check "and the offending line is quoted"
            (holds (err answer) "not a message");
          check "and the session with it" (not (Client.alive t));
          check "and the daemon can see it as a fault"
            (Client.fault t = Some (err answer));
          check "the peer is killed for it" (gone ~clock (pid dir) 25))

(* A server that never answers. The call must come back on time and the server
   must be gone, since a client that gave up on a call it cannot cancel would
   otherwise leave it running. *)
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
                  (Proto.Fetch
                     {
                       url = "http://127.0.0.1/";
                       render = Proto.Text;
                       max_bytes = None;
                     })
                  ~on_trace:ignore)
          in
          check "a call that exceeds its bound is answered at the bound"
            (Result.is_error answer && took >= 1. && took < 10.);
          check "the answer names the operation and the bound"
            (holds (err answer) "fetch" && holds (err answer) "1 second");
          check "a timed-out session is not alive" (not (Client.alive t));
          check "and the daemon can see it as a fault"
            (Client.fault t = Some (err answer));
          check "and the server that would not answer is killed"
            (gone ~clock (pid dir) 25))

(* A stop while another fiber has a call in flight. The call must be answered
   by the stop rather than left waiting on the numptyd being closed, which is
   the freeze this whole design exists to prevent. *)
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
                      (Proto.Head { url = "http://127.0.0.1/" })
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
          check "a stopped session is not alive" (not (Client.alive t));
          check "and a stop is still not a fault" (Client.fault t = None))

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
                     (Proto.Head { url = "http://127.0.0.1/" })
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
                Client.call t
                  (Proto.Head { url = "http://127.0.0.1/" })
                  ~on_trace:ignore)
          in
          check "a later call answers rather than raising"
            (Result.is_error again && took < 5.);
          check "and says the call was cancelled"
            (holds (err again) "cancelled");
          check "the server left with the call is killed"
            (gone ~clock (pid dir) 25))

(* Two greetings, which no numptyd of this build sends. *)
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
            Client.call t
              (Proto.Head { url = "http://127.0.0.1/" })
              ~on_trace:ignore
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
            Client.call t
              (Proto.Head { url = "http://127.0.0.1/" })
              ~on_trace:ignore
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
    Client.start ~sw ~proc ~clock ~trace:ignore ~argv:[ "/nonexistent/netd" ]
  with
  | Ok _ -> check "a command that does not exist fails the start" false
  | Error e ->
      check "a command that does not exist fails the start"
        (holds e "could not be started" && holds e "/nonexistent/netd")

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
