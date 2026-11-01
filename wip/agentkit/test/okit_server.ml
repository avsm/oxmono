(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* okitd, driven as humpty drives it: a real 'humpty-cpu okitd' over pipes,
   against a real dune. What matters here is the shape of an exchange rather
   than the text of an answer, which the tools' own tests pin: a greeting, a
   call whose traces carry that call's id, an operation refused in words when
   the session it needs is absent, and an exit that leaves no dune server
   behind. *)

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

(* The binary under test, passed by the dune rule so that the test runs the one
   this build produced. *)
let exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else "humpty-cpu"

(* The workspace goes under /tmp rather than TMPDIR, because dune sets TMPDIR
   to a path deep inside its own build directory when it runs a test, and the
   server's socket path under it is longer than a unix socket address may be. *)
let fixture ?(dune = true) name =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "okitd_test" name in
  let save p s =
    let oc = open_out (Filename.concat dir p) in
    output_string oc s;
    close_out oc
  in
  if dune then begin
    Unix.mkdir (Filename.concat dir "lib") 0o755;
    save "dune-project" "(lang dune 3.21)\n";
    save "lib/dune" "(library (name fix))\n";
    save "lib/fix.ml" "let x = 1\n"
  end;
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

(* A dune on PATH that records the pid of the server okitd starts and then
   becomes the real dune, so that a test can ask whether that server outlived
   the okitd which owned it. It goes in a directory of its own, because a file
   named dune in a workspace is a dune file.

   Only the passive server is recorded. okitd runs dune for a describe as well,
   in the same directory, and that dune has exited by the time anything is
   asked about it, so a file it had written would make the question answer
   itself. *)
let shim () =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "okitd_test" "shim" in
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

(* okitd, with its three streams as pipes. The parent's ends of the child's
   own sides are closed at once, or the child's stdin would never reach end of
   file however long the test waited. *)
let with_server ~sw ~proc ~dir f =
  let in_r, in_w = Eio_unix.pipe sw in
  let out_r, out_w = Eio_unix.pipe sw in
  let err_r, err_w = Eio_unix.pipe sw in
  let child =
    Eio.Process.spawn ~sw proc ~stdin:in_r ~stdout:out_w ~stderr:err_w
      [ exe; "okitd"; "--dir"; dir ]
  in
  Eio.Flow.close in_r;
  Eio.Flow.close out_w;
  Eio.Flow.close err_w;
  let r = Eio.Buf_read.of_flow out_r ~max_size:Agentkit.Line.max_line in
  Fun.protect
    ~finally:(fun () ->
      try Eio.Process.signal child Sys.sigkill
      with Eio.Io _ | Invalid_argument _ -> ())
    (fun () -> f ~child ~r ~stdin:in_w ~stderr:err_r)

(* A server that has stopped answering must fail the test rather than hang the
   suite. The window is long, since starting a dune server is allowed thirty
   seconds and a describe reloads the workspace. *)
let raw ~clock r =
  match
    Eio.Time.with_timeout clock 180. (fun () -> Ok (Eio.Buf_read.line r))
  with
  | Ok l -> Some l
  | Error `Timeout -> failwith "okitd said nothing within 180 seconds"
  | exception End_of_file -> None

(* Lines are taken whole and decoded one at a time, so that a test can look at
   what was on the wire as well as at what it meant. *)
let read ~clock r =
  match raw ~clock r with
  | None -> `Eof
  | Some line -> Proto.read_to_client (Eio.Buf_read.of_string (line ^ "\n"))

(* The greeting arrives after the session has started, so the traces of that
   start come first. They belong to no call, and the answer says how many were
   seen and whether every one of them was written without an id, which is what
   a peer reads as a step okit took on its own. *)
let greeting ~clock r =
  let outside = ref 0 in
  let idless = ref true in
  let rec go () =
    match raw ~clock r with
    | None -> failwith "okitd stopped before it greeted"
    | Some line -> (
        match Proto.read_to_client (Eio.Buf_read.of_string (line ^ "\n")) with
        | `Msg (Proto.Trace _) ->
            incr outside;
            if not (String.starts_with ~prefix:"{\"trace\":{\"line\":" line)
            then idless := false;
            go ()
        | `Msg (Proto.Hello h) -> (h, !outside, !idless)
        | `Msg (Proto.Result _) -> failwith "okitd answered before it greeted"
        | `Eof -> failwith "okitd stopped before it greeted"
        | `Bad l -> failwith ("okitd wrote a line that is not a message: " ^ l))
  in
  go ()

let call ~clock ~w ~r id op =
  Proto.write_to_server w (Proto.Call { id; op });
  Eio.Buf_write.flush w;
  let traces = ref [] in
  let rec await () =
    match read ~clock r with
    | `Msg (Proto.Trace t) ->
        traces := t :: !traces;
        await ()
    | `Msg (Proto.Result res) when res.id = id -> (res.output, List.rev !traces)
    | `Msg (Proto.Result _) -> failwith "okitd answered a call nobody made"
    | `Msg (Proto.Hello _) -> failwith "okitd greeted twice"
    | `Eof -> failwith "okitd stopped in the middle of a call"
    | `Bad l -> failwith ("okitd wrote a line that is not a message: " ^ l)
  in
  await ()

(* The status the child left, or a value no exit has when it is still there
   after a minute. A wedged okitd must fail the test rather than hang it. *)
let exited ~clock child =
  match
    Eio.Time.with_timeout clock 60. (fun () -> Ok (Eio.Process.await child))
  with
  | Ok (`Exited n) -> n
  | Ok (`Signaled n) -> -n
  | Error `Timeout -> -128

(* Whether a process is gone, polled: okitd asks its dune server to shut down
   and does not wait for it to have done so. *)
let rec gone ~clock pid n =
  match Unix.kill pid 0 with
  | () when n <= 0 -> false
  | () ->
      Eio.Time.sleep clock 0.2;
      gone ~clock pid (n - 1)
  | exception Unix.Unix_error (ESRCH, _, _) -> true
  | exception _ -> false

(* A workspace dune serves. One server answers every call of this test, since
   starting one is the slowest thing okitd does. *)
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
      with_server ~sw ~proc ~dir @@ fun ~child ~r ~stdin ~stderr:_ ->
      ( Eio.Buf_write.with_flow stdin @@ fun w ->
        let h, outside, idless = greeting ~clock r in
        check "a workspace with a dune-project gets a session" h.dune;
        check "a trace outside a call is written without an id"
          (outside > 0 && idless);
        check "the greeting carries the note the interface shows"
          (h.status
          =
          if h.merlin then "okit: dune tools active, ocamlmerlin found"
          else
            "okit: dune tools active, and no ocamlmerlin to answer for the \
             workspace");
        let out, traces = call ~clock ~w ~r 1 (Proto.Build { targets = "." }) in
        check "a build answers" (out = "build ok");
        check "a build streams traces carrying the call's id"
          (traces <> []
          && List.for_all (fun (t : Proto.trace) -> t.id = Some 1) traces);
        check "a build names itself on the trace"
          (List.exists (fun (t : Proto.trace) -> holds t.line "build") traces);
        let out, _ = call ~clock ~w ~r 2 (Proto.Project { module_ = "" }) in
        check "project describes the workspace" (holds out "fix");
        let out, _ =
          call ~clock ~w ~r 3 (Proto.Bash { command = "echo hello" })
        in
        check "bash runs a command" (out = "hello\n");
        (* A command that reads standard input must not be handed the pipe the
           protocol arrives on, which is okitd's own. One that were would take
           this call's successor for its input, and the call would never be
           answered. The bound in [call] is what would catch that, so the check
           is that this returns at all, and empty. *)
        let out, _ = call ~clock ~w ~r 4 (Proto.Bash { command = "cat" }) in
        check "a command that reads standard input is given an empty one"
          (out = "");
        let out, _ =
          call ~clock ~w ~r 5 (Proto.Bash { command = "echo after; cat" })
        in
        check "and the call after it is still the one answered" (out = "after\n");
        if not h.merlin then
          print_endline "no ocamlmerlin: the merlin calls are not run"
        else
          let out, _ =
            call ~clock ~w ~r 6
              (Proto.Outline { path = "lib/fix.ml"; source = "let x = 1\n" })
          in
          check "outline answers about the source in the call" (holds out "x")
      );
      (* End of file on stdin is shutdown, and the session goes with it. *)
      Eio.Flow.close stdin;
      check "closing stdin ends okitd" (exited ~clock child = 0);
      let pid =
        let ic = open_in (Filename.concat dir "pid") in
        let n = int_of_string (String.trim (input_line ic)) in
        close_in ic;
        n
      in
      check "the dune server okitd started is stopped" (gone ~clock pid 50))

(* A peer that gives up on okitd kills it. okitd catches the signal, so the
   dune server it started is stopped rather than left holding the workspace's
   build lock with nothing to answer to. Without the handler this exits on the
   signal and that server stays. *)
let terminated env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let dir = fixture "term" in
  let path = Sys.getenv "PATH" in
  let bin = shim () in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" path;
      remove [ dir; bin ])
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      with_server ~sw ~proc ~dir @@ fun ~child ~r ~stdin:_ ~stderr:_ ->
      let h, _, _ = greeting ~clock r in
      check "the okitd to be terminated has a session of its own" h.dune;
      Eio.Process.signal child Sys.sigterm;
      check "a terminated okitd stops rather than dying where it stands"
        (exited ~clock child = 0);
      let pid =
        let ic = open_in (Filename.concat dir "pid") in
        let n = int_of_string (String.trim (input_line ic)) in
        close_in ic;
        n
      in
      check "and the dune server it started is stopped with it"
        (gone ~clock pid 50))

(* A workspace with no dune-project. okitd still runs, and says why the dune
   tools are not there rather than refusing to start. *)
let without_dune env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let dir = fixture ~dune:false "bare" in
  Fun.protect
    ~finally:(fun () -> remove [ dir ])
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      with_server ~sw ~proc ~dir @@ fun ~child ~r ~stdin ~stderr:_ ->
      ( Eio.Buf_write.with_flow stdin @@ fun w ->
        let h, _, _ = greeting ~clock r in
        check "a workspace with no dune-project gets no session" (not h.dune);
        check "and no merlin either" (not h.merlin);
        check "the greeting says why there are no dune tools"
          (h.status
         = "okit: no dune tools, since this workspace has no dune-project");
        let out, _ = call ~clock ~w ~r 1 (Proto.Build { targets = "." }) in
        check "a dune call is refused in words rather than as a fault"
          (holds out "okit's dune session is not running"
          && holds out "no dune-project");
        let out, _ = call ~clock ~w ~r 2 (Proto.Bash { command = "echo hi" }) in
        check "bash answers without a session" (out = "hi\n");
        Proto.write_to_server w Proto.Shutdown );
      Eio.Flow.close stdin;
      check "a shutdown message ends okitd" (exited ~clock child = 0))

(* A line okitd cannot read is a fault of the peer, which is this same binary.
   It reports the line and leaves, rather than skipping it and going on to
   answer the wrong question. *)
let garbage env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let dir = fixture ~dune:false "garbage" in
  Fun.protect
    ~finally:(fun () -> remove [ dir ])
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      with_server ~sw ~proc ~dir @@ fun ~child ~r ~stdin ~stderr ->
      ( Eio.Buf_write.with_flow stdin @@ fun w ->
        ignore (greeting ~clock r);
        Eio.Buf_write.string w "not a message\n" );
      let status = exited ~clock child in
      check "a line that is not a message ends okitd nonzero"
        (status <> 0 && status <> -1);
      let said = Eio.Buf_read.(parse_exn take_all) ~max_size:100_000 stderr in
      check "and the offending line is on its standard error"
        (holds said "not a message"))

let () =
  Eio_main.run (fun env ->
      serves env;
      terminated env;
      without_dune env;
      garbage env);
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
