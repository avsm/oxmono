(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Prompt lines in [humpty expect], driven end to end against a real model.

   The spawned humpty is what loads the model, so this runner holds nothing
   and the Metal build alone registers it: generation on the CPU build would
   take longer than anyone would wait. The script mixes the forms the parser
   takes. A scripted call writes a file the model has not seen, the first
   prompt is the request that once hung the interface, and the second prompt
   can only be answered by reading the file with a tool, so a reply that
   names the token proves the model drove a tool through okitd.

   Every check is on the part of the transcript that the model produced.
   Searching the whole of it would pass on the scripted call's own echo,
   which is printed before the model is asked anything.

   A further run covers the expiry path, which is the failure the feature
   exists to report. One prompt bounded at a second cannot finish, so the
   transcript ends with the bound's own line and standard error names the
   script line that was running. It costs a second model load, which is what
   testing the path is worth.

   It needs a real model, so it is gated on DS4_LIVE and skips when no model
   is downloaded. *)

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

let index_from s sub start =
  let n = String.length sub in
  let rec at i =
    if i + n > String.length s then None
    else if String.sub s i n = sub then Some i
    else at (i + 1)
  in
  at start

let holds s sub = index_from s sub 0 <> None

let ends_with s suffix =
  let n = String.length s and m = String.length suffix in
  n >= m && String.sub s (n - m) m = suffix

let chomp s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = '\n' do
    decr n
  done;
  String.sub s 0 !n

let occurrences s sub =
  let n = String.length sub in
  let rec at i acc =
    if n = 0 then acc
    else
      match index_from s sub i with
      | None -> acc
      | Some j -> at (j + n) (acc + 1)
  in
  at 0 0

(* [after s sub k] is what follows the [k]th occurrence of [sub] in [s], and is
   the empty string when [s] holds fewer than [k] of them. A check made against
   it therefore fails on a transcript that stopped short, rather than falling
   back to the whole of it. *)
let after s sub k =
  let n = String.length sub in
  let rec skip i k =
    match index_from s sub i with
    | None -> ""
    | Some j when k <= 1 -> String.sub s (j + n) (String.length s - j - n)
    | Some j -> skip (j + n) (k - 1)
  in
  skip 0 k

(* The binary under test, passed by the dune rule. It is the Metal humpty,
   since the spawned process is the one that loads the model. *)
let exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else "humpty-metal"

(* The workspace goes under /tmp rather than TMPDIR, because dune sets TMPDIR
   to a path deep inside its own build directory when it runs a test. *)
let fixture ~fs =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "expect_prompts" "ws" in
  let root = Eio.Path.(fs / dir) in
  let save p s =
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(root / p) s
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / "lib");
  save "dune-project" "(lang dune 3.21)\n";
  save "lib/dune" "(library (name fix))\n";
  save "lib/fix.ml" "let x = 1\n";
  dir

(* What the note holds. An arbitrary string rather than a plausible answer,
   because a model that never opened the file can say "42" unprompted and
   could not say this. It is a fixed literal, so that two runs of the script
   are the same run. *)
let token = "9f3ac0de11"

let script =
  Printf.sprintf
    "write {\"cap\":\"\",\"path\":\"note.txt\",\"content\":\"the note token is \
     %s\\n\"}\n\
     ? describe this repository\n\
     ? read note.txt and tell me exactly what token it holds\n"
    token

(* The bound the expiry run gives one prompt. Neither the prefill nor the
   generation of an answer can finish inside it, so the exchange expires
   wherever it has got to. *)
let timeout_secs = 1
let timeout_script = "? describe this repository\n"

(* The expiry path. The run ends by leaving the process, so what it left
   behind is the whole diagnostic: the transcript's last line says the bound
   expired, and standard error names the script line that was running.
   Standard error is captured rather than inherited here, since it carries
   half of what this run checks. *)
let run_timeout ~fs =
  let dir = fixture ~fs in
  let script_file = dir ^ ".script" in
  let transcript = dir ^ ".transcript" in
  let errors = dir ^ ".stderr" in
  Fun.protect
    ~finally:(fun () ->
      ignore
        (Sys.command
           (Filename.quote_command "rm"
              [ "-rf"; dir; script_file; transcript; errors ])))
    (fun () ->
      Eio.Path.save ~create:(`Or_truncate 0o644)
        Eio.Path.(fs / script_file)
        timeout_script;
      Printf.printf "\nrunning %s expect --prompt-timeout %d ...\n%!" exe
        timeout_secs;
      let status =
        Sys.command
          (Filename.quote_command exe ~stdout:transcript ~stderr:errors
             [
               "expect";
               "--dir";
               dir;
               "--prompt-timeout";
               string_of_int timeout_secs;
               script_file;
             ])
      in
      let out = In_channel.with_open_text transcript In_channel.input_all in
      let err = In_channel.with_open_text errors In_channel.input_all in
      print_string out;
      print_string err;
      check "the expired run exited nonzero" (status <> 0);
      (* The last line of the transcript, since the process leaves as soon as
         the bound expires and nothing it had started can print after that. *)
      check "the transcript ends with the expiry"
        (ends_with (chomp out)
           (Printf.sprintf "= timeout after %ds" timeout_secs));
      check "standard error names the prompt that hung"
        (holds err "did not answer within" && holds err "the prompt on line 1"))

let run env =
  let fs = Eio.Stdenv.fs env in
  let dir = fixture ~fs in
  let script_file = dir ^ ".script" in
  let transcript = dir ^ ".transcript" in
  Fun.protect
    ~finally:(fun () ->
      ignore
        (Sys.command
           (Filename.quote_command "rm" [ "-rf"; dir; script_file; transcript ])))
    (fun () ->
      Eio.Path.save ~create:(`Or_truncate 0o644)
        Eio.Path.(fs / script_file)
        script;
      Printf.printf "running %s expect (the model loads first) ...\n%!" exe;
      (* Through the shell so that the transcript survives a nonzero exit,
         which is exactly the run worth reading. Standard error is inherited,
         so the engine's own report of a failure to load appears in the test
         log. *)
      let status =
        Sys.command
          (Filename.quote_command exe ~stdout:transcript
             [ "expect"; "--dir"; dir; "--prompt-timeout"; "300"; script_file ])
      in
      let out = In_channel.with_open_text transcript In_channel.input_all in
      print_string out;
      check "the run exited zero" (status = 0);
      check "no prompt timed out" (not (holds out "= timeout"));
      check "both prompts were answered" (occurrences out "= reply" = 2);
      (* From the first prompt echo onwards, since the scripted call on the
         script's first line echoes as a [> ] line before the model has been
         asked anything, and a search of the whole transcript would pass
         without a model at all. A tool result and the model's own prose both
         print verbatim, so a quoted line beginning "> " in either would
         satisfy this as well. That much is accepted, because pinning the tool
         by name would fail on a legitimate change of the model's mind and the
         transcript is printed for a reader to see which it chose. *)
      check "the model called a tool" (holds (after out "\n? " 1) "\n> ");
      (* After the second [= reply] alone, since the token is in the echo of
         the scripted write at the top of the transcript and in the result of
         whatever tool read the file. Only the final turn's text follows that
         marker, so this is the model saying the token rather than anything
         quoting it back. *)
      check "the second reply names the token"
        (holds (after out "= reply" 2) token));
  run_timeout ~fs;
  if !failures = 0 then print_string "\nLive expect prompts test passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end

let () =
  match Sys.getenv_opt "DS4_LIVE" with
  | None ->
      print_string "SKIP - live expect prompts test (set DS4_LIVE=1 to run)\n"
  | Some _ -> (
      Eio_main.run @@ fun env ->
      let xdg = Xdge.create (Eio.Stdenv.fs env) "ds4" in
      match Ds4_cli.Model.resolve ~dir:(Ds4_cli.Model.dir xdg) None with
      | Error e -> Printf.printf "SKIP - %s\n" e
      | Ok _ -> run env)
