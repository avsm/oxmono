(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* One real dumpty run, checked against the whole of its stream contract.

   The spawned dumpty is what loads the model, so this runner holds nothing and
   the Metal build alone registers it, as live_expect_prompts is registered:
   generation on the CPU build would take longer than anyone would wait.

   The workspace holds one file the model has not seen, and the prompt can only
   be answered by reading it, so a reply that names the token is a reply that
   drove a tool. What is checked is the contract rather than the answer:
   standard output is one line and one JSON object, standard error is journal
   records and nothing else, and the account runs from a run_start to a
   run_stop with the prompt and the turn statistics between them.

   The same exchange is then run under a bound it cannot meet, since the other
   half of the contract is what a run that could not finish leaves behind.

   It needs a real model, so it is gated on DS4_LIVE and skips when no model is
   downloaded. *)

module Journal = Agentkit.Journal

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

(* The binary under test, passed by the dune rule. It is the Metal dumpty,
   since the spawned process is the one that loads the model. *)
let exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else "dumpty-metal"
let read path = In_channel.with_open_bin path In_channel.input_all
let lines s = List.filter (fun l -> l <> "") (String.split_on_char '\n' s)

let holds s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

(* An arbitrary string rather than a plausible answer, because a model that
   never opened the file can say "42" unprompted and could not say this. *)
let token = "9f3ac0de11"
let prompt = "read note.txt and tell me exactly what token it holds"

(* Small, since the work is one file and one answer, and a context this size
   still leaves the run its own headroom to grow into. *)
let ctx = 8192

(* The workspace goes under /tmp rather than TMPDIR, because dune sets TMPDIR
   to a path deep inside its own build directory when it runs a test. *)
let fixture ~fs =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "live_dumpty" "ws" in
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(fs / dir / "note.txt")
    (Printf.sprintf "the note token is %s\n" token);
  dir

let remove paths =
  ignore (Sys.command (Filename.quote_command "rm" ("-rf" :: paths)))

(* What standard output carried, read as generic JSON rather than through a
   codec of this test's own, so that a member added later is not a failure
   here. *)
let reply_of out =
  match Jsont_bytesrw.decode_string Jsont.json out with
  | Error e -> Error e
  | Ok (Jsont.Object (mems, _)) -> (
      match Jsont.Json.find_mem "reply" mems with
      | Some (_, Jsont.String (s, _)) -> Ok s
      | Some _ -> Error "the reply member is not a string"
      | None -> Error "the object has no reply member")
  | Ok _ -> Error "standard output is not a JSON object"

let run env =
  let fs = Eio.Stdenv.fs env in
  let dir = fixture ~fs in
  let out_file = dir ^ ".out" and err_file = dir ^ ".err" in
  (* The run appends its account to a store under the state directory, and the
     sequence check below wants a journal this run alone wrote, so the state
     directory is this test's own. *)
  let state = dir ^ ".state" in
  Unix.putenv "XDG_STATE_HOME" state;
  Fun.protect
    ~finally:(fun () -> remove [ dir; out_file; err_file; state ])
    (fun () ->
      Printf.printf "running %s (the model loads first) ...\n%!" exe;
      let status =
        Sys.command
          (Filename.quote_command exe ~stdout:out_file ~stderr:err_file
             [
               "--quiet";
               "--dir";
               dir;
               "--ctx";
               string_of_int ctx;
               "--timeout";
               "900";
               prompt;
             ])
      in
      let out = read out_file and err = read err_file in
      print_string err;
      print_string out;
      check "the run exited zero" (status = 0);
      (* One line, and the line is the whole of what was printed. *)
      check "standard output is one line" (List.length (lines out) = 1);
      (match reply_of out with
      | Error e -> check ("standard output is one JSON object: " ^ e) false
      | Ok reply ->
          check "standard output is one JSON object with a reply" true;
          check "the reply is not empty" (String.trim reply <> "");
          check "the reply names the token" (holds reply token));
      (* The account. Every line is a record, since a diagnostic at the default
         verbosity is the exception and this run met none of them. *)
      let records = List.map (fun l -> (l, Journal.of_string l)) (lines err) in
      List.iter
        (function
          | l, Error e -> check (Printf.sprintf "%s: %s" l e) false
          | _, Ok _ -> ())
        records;
      let kinds =
        List.filter_map
          (function
            | _, Ok r -> Some (Journal.kind_name r.Journal.kind) | _ -> None)
          records
      in
      check "every line of standard error is a journal record"
        (List.length kinds = List.length records && kinds <> []);
      check "the account starts at a run_start"
        (match kinds with k :: _ -> k = "run_start" | [] -> false);
      check "the account ends at a run_stop"
        (match List.rev kinds with k :: _ -> k = "run_stop" | [] -> false);
      let between =
        match kinds with
        | [] -> []
        | _ :: rest -> List.filteri (fun i _ -> i < List.length rest - 1) rest
      in
      check "the prompt is on the record" (List.mem "prompt" between);
      check "and what the turns cost" (List.mem "stats" between);
      (* Gapless and in order, as a journal read forward is. *)
      let seqs =
        List.filter_map
          (function _, Ok r -> Some r.Journal.seq | _ -> None)
          records
      in
      check "the records are numbered from 1 without a gap"
        (List.mapi (fun i _ -> i + 1) seqs = seqs);
      (* The store is the stream. One append stamps both, so the account under
         the state directory is byte for byte what standard error carried. *)
      let stored =
        let jdir = Filename.concat (Filename.concat state "dumpty") "journal" in
        match Sys.readdir jdir with
        | entries ->
            Array.sort compare entries;
            String.concat ""
              (List.map
                 (fun f -> read (Filename.concat jdir f))
                 (Array.to_list entries
                 |> List.filter (String.ends_with ~suffix:".jsonl")))
        | exception Sys_error _ -> ""
      in
      check "the account is stored as it was streamed" (stored = err))

(* The same exchange under a bound of a second, which is less than the model
   takes over one turn, so the run is cut off part way through. A run that could
   not finish has nothing to print, so the whole of what it leaves is a nonzero
   status and an account whose last record says why it stopped. The model is
   loaded before the bound starts, so this costs a load and little else. *)
let timed_out env =
  let fs = Eio.Stdenv.fs env in
  let dir = fixture ~fs in
  let out_file = dir ^ ".out" and err_file = dir ^ ".err" in
  Fun.protect
    ~finally:(fun () -> remove [ dir; out_file; err_file ])
    (fun () ->
      Printf.printf "running %s again, bounded at a second ...\n%!" exe;
      let status =
        Sys.command
          (Filename.quote_command exe ~stdout:out_file ~stderr:err_file
             [
               "--quiet";
               "--dir";
               dir;
               "--ctx";
               string_of_int ctx;
               "--timeout";
               "1";
               prompt;
             ])
      in
      let out = read out_file and err = read err_file in
      print_string err;
      check "a run that outran its timeout exits nonzero" (status <> 0);
      check "and writes nothing at all on standard output" (out = "");
      let records =
        List.filter_map
          (fun l -> Result.to_option (Journal.of_string l))
          (lines err)
      in
      check "and its account ends at a run_stop naming the bound"
        (match List.rev records with
        | r :: _ -> (
            match r.Journal.kind with
            | Journal.Run_stop why -> holds why "timeout"
            | _ -> false)
        | [] -> false))

let () =
  match Sys.getenv_opt "DS4_LIVE" with
  | None -> print_string "SKIP - live dumpty test (set DS4_LIVE=1 to run)\n"
  | Some _ -> (
      Eio_main.run @@ fun env ->
      let xdg = Xdge.create (Eio.Stdenv.fs env) "ds4" in
      match Ds4_cli.Model.resolve ~dir:(Ds4_cli.Model.dir xdg) None with
      | Error e -> Printf.printf "SKIP - %s\n" e
      | Ok _ ->
          run env;
          timed_out env;
          if !failures = 0 then print_string "\nLive dumpty test passed.\n"
          else begin
            Printf.printf "\n%d check(s) failed.\n" !failures;
            exit 1
          end)
