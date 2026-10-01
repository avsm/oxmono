(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The agentkit browser against a journal this test writes with the same
   writer the agents use. It is driven as the person drives it, through the
   binary this build produced, whose path arrives on the command line.

   The properties worth pinning: every run is listed, a record filter that
   names no known kind is refused rather than matching nothing, a shown run
   carries its prompt, its tool traffic and how it stopped, and a run the
   journal does not hold is an error naming where the runs are listed. *)

module Journal = Agentkit.Journal

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

let t0 = 1786180447. (* 2026-08-08T09:14:07Z *)

(* Run the browser and return its status with what it wrote. The output goes
   through temporary files, so a large journal cannot deadlock the pipe. *)
let run_browser ~env_root browser args =
  let out = Filename.temp_file "akit-out" "" in
  let err = Filename.temp_file "akit-err" "" in
  Fun.protect ~finally:(fun () ->
      Sys.remove out;
      Sys.remove err)
  @@ fun () ->
  let environment =
    Array.append
      (Array.of_list [ "XDG_STATE_HOME=" ^ env_root ])
      (Array.of_list
         (List.filter
            (fun s -> not (String.starts_with ~prefix:"XDG_STATE_HOME=" s))
            (Array.to_list (Unix.environment ()))))
  in
  let read p = In_channel.with_open_bin p In_channel.input_all in
  let fd_out = Unix.openfile out [ Unix.O_WRONLY ] 0o600 in
  let fd_err = Unix.openfile err [ Unix.O_WRONLY ] 0o600 in
  let pid =
    Unix.create_process_env browser
      (Array.of_list (browser :: args))
      environment Unix.stdin fd_out fd_err
  in
  Unix.close fd_out;
  Unix.close fd_err;
  (* Eio's signal handling interrupts a blocking wait, so the wait retries. *)
  let rec wait () =
    try Unix.waitpid [] pid with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  let _, status = wait () in
  let code = match status with Unix.WEXITED n -> n | _ -> 255 in
  (code, read out, read err)

let stats =
  {
    Agentkit.Agent.ctx_used = 2249;
    ctx_size = 32768;
    prompt_tokens = 2244;
    generated = 40;
    generate_seconds = 1.9;
    prefill_seconds = 6.3;
    tool_calls = 1;
    turns = 3;
    drafted = 0;
    total_generated = 40;
    total_generate_seconds = 1.9;
  }

let build ~clock dir =
  Eio.Switch.run @@ fun sw ->
  let j = Journal.create ~sw ~clock ~run:1 ~seq:1 dir in
  let append k = ignore (Journal.append j k) in
  append
    (Journal.Run_start
       {
         Journal.pid = 41;
         version = "0.1";
         backend = "cpu";
         model = "/models/GLM-5.3-Flash-Q2.gguf";
         ctx_size = 32768;
       });
  append (Journal.Prompt "read the notes and answer");
  append
    (Journal.Tool_call
       { Journal.call = 1; name = "read"; arguments = {|{"path":"n.txt"}|} });
  append
    (Journal.Tool_result
       {
         Journal.call = 1;
         name = "read";
         output = "the token is zx91";
         seconds = 0.1;
         truncated = false;
       });
  append (Journal.Content "the token is zx91");
  append (Journal.Stats stats);
  append (Journal.Run_stop "the exchange finished");
  Journal.close j;
  let next = Journal.recover dir in
  let j =
    Journal.create ~sw ~clock ~run:next.Journal.next_run
      ~seq:next.Journal.next_seq dir
  in
  let append k = ignore (Journal.append j k) in
  append
    (Journal.Run_start
       {
         Journal.pid = 42;
         version = "0.1";
         backend = "cpu";
         model = "/models/GLM-5.3-Flash-Q2.gguf";
         ctx_size = 32768;
       });
  append (Journal.Run_stop "timeout after 1s");
  Journal.close j

let run env browser =
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock t0;
  let tmp = Filename.temp_file "ds4-akit" "" in
  Sys.remove tmp;
  (* The fixture is laid out as a dumpty state directory, so the same journal
     answers both for a path source and for the agent the state root names. *)
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp / "dumpty" / "journal") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" tmp)))
  @@ fun () ->
  build ~clock dir;
  let path = Option.get (Eio.Path.native dir) in
  let browse args = run_browser ~env_root:tmp browser args in

  let code, out, _ = browse [ "runs"; path ] in
  check "runs exits zero" (code = 0);
  check "every run is listed"
    (contains out "the exchange finished" && contains out "timeout after 1s");
  check "a run says its model" (contains out "GLM-5.3-Flash-Q2.gguf");
  check "a run says its turns and calls" (contains out "    3     1");

  let code, out, _ = browse [ "list" ] in
  check "list exits zero" (code = 0);
  check "list finds the journal through the state root"
    (contains out "dumpty" && contains out "9");
  check "list counts the runs" (contains out " 2 ");

  let code, out, _ = browse [ "log"; path; "--kind"; "tool_call" ] in
  check "a kind filter keeps that kind alone"
    (code = 0 && contains out "#1 read" && not (contains out "run_stop"));

  let code, _, err = browse [ "log"; path; "--kind"; "tool_cal" ] in
  check "a kind this build does not write is refused"
    (code <> 0 && contains err "tool_call");

  let code, out, _ = browse [ "log"; path; "--run"; "2" ] in
  check "a run filter keeps that run alone"
    (code = 0
    && contains out "timeout after 1s"
    && not (contains out "the exchange finished"));

  let code, out, _ = browse [ "show"; path; "1" ] in
  check "show exits zero" (code = 0);
  check "show carries the prompt" (contains out "> read the notes and answer");
  check "show carries the call and its result"
    (contains out "#1 read" && contains out "the token is zx91");
  check "show says how the run stopped"
    (contains out "stopped: the exchange finished");

  let code, _, err = browse [ "show"; path; "9" ] in
  check "a run the journal does not hold is an error"
    (code <> 0 && contains err "no run 9" && contains err "agentkit runs");

  if !failures = 0 then print_string "\nAgentkit browser test passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end

let () =
  let browser = Sys.argv.(1) in
  Eio_main.run @@ fun env -> run env browser
