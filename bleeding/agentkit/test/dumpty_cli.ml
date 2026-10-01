(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* dumpty's failure contract, driven against the binary this build produced.

   The property worth testing is the negative one. A run that could not do the
   work writes nothing on standard output and exits nonzero, so a consumer
   that reads a line and gets none knows the run failed without parsing
   anything, and never has to wonder whether a partial object is a whole one.
   A half-written object on a failed run would be the fault this checks
   against.

   A model path that names nothing is the cheapest way to that path: it fails
   before any process is spawned and needs no model on the machine. The
   exchange itself needs one, and is live_dumpty's. *)

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
   this build produced. The CPU build, since nothing here loads a model. *)
let exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else "dumpty-cpu"
let read path = In_channel.with_open_bin path In_channel.input_all

(* Under /tmp rather than TMPDIR, because dune sets TMPDIR to a path deep
   inside its own build directory when it runs a test. *)
let scratch name = Filename.temp_dir ~temp_dir:"/tmp" "dumpty_cli" name

let remove paths =
  ignore (Sys.command (Filename.quote_command "rm" ("-rf" :: paths)))

(* A run whose model is not on this machine. Nothing is generated, so nothing
   may be printed. *)
let no_model () =
  let dir = scratch "ws" in
  let out = dir ^ ".out" and err = dir ^ ".err" in
  Fun.protect
    ~finally:(fun () -> remove [ dir; out; err ])
    (fun () ->
      let status =
        Sys.command
          (Filename.quote_command exe ~stdout:out ~stderr:err
             [ "--model"; "/nonexistent/path.gguf"; "--dir"; dir; "say hello" ])
      in
      let err = read err in
      print_string err;
      check "a run whose model is not there exits nonzero" (status <> 0);
      check "and writes nothing at all on standard output" (read out = "");
      check "and names the model it could not find on standard error"
        (holds err "/nonexistent/path.gguf"))

(* A bound that is not a count of seconds. The refusal names the forms that are
   accepted, since a caller that arrived at a negative one by arithmetic has to
   be told which way removes the bound. It is refused while the arguments are
   read, so nothing is spawned and nothing is printed. *)
let bad_timeout () =
  let out = Filename.temp_file ~temp_dir:"/tmp" "dumpty_cli" "out" in
  let err = Filename.temp_file ~temp_dir:"/tmp" "dumpty_cli" "err" in
  Fun.protect
    ~finally:(fun () -> remove [ out; err ])
    (fun () ->
      let status =
        Sys.command
          (Filename.quote_command exe ~stdout:out ~stderr:err
             (* Joined to its option, since a negative number on its own reads
                as another option and never reaches the converter. *)
             [ "--timeout=-1"; "say hello" ])
      in
      let err = read err in
      print_string err;
      check "a negative timeout exits nonzero" (status <> 0);
      check "and writes nothing at all on standard output" (read out = "");
      (* Short enough not to depend on where cmdliner wraps the line. *)
      check "and says which forms a timeout takes"
        (holds err "--timeout"
        && holds err "removes the bound"
        && holds err "count of seconds"))

(* The prompt of [-] is read from standard input, and is read before anything
   else the run needs, so a prompt larger than the limit is refused where a
   model would otherwise have been loaded first. The limit is what stops a
   mistake such as a model file arriving on standard input. *)
let stdin_prompt_too_large () =
  let big = Filename.temp_file ~temp_dir:"/tmp" "dumpty_cli" "prompt" in
  let out = Filename.temp_file ~temp_dir:"/tmp" "dumpty_cli" "out" in
  let err = Filename.temp_file ~temp_dir:"/tmp" "dumpty_cli" "err" in
  Fun.protect
    ~finally:(fun () -> remove [ big; out; err ])
    (fun () ->
      Out_channel.with_open_bin big (fun oc ->
          Out_channel.output_string oc (String.make 1_000_001 'x'));
      let status =
        Sys.command
          (Filename.quote_command exe ~stdin:big ~stdout:out ~stderr:err
             [ "--model"; "/nonexistent/path.gguf"; "-" ])
      in
      let err = read err in
      print_string err;
      check "a prompt over the limit exits nonzero" (status <> 0);
      check "and writes nothing at all on standard output" (read out = "");
      (* The prompt was read first, so the refusal is the limit's rather than
         the model's. *)
      check "and is refused for its size rather than for the model"
        (not (holds err "/nonexistent/path.gguf")))

(* The manual page, which is the one thing a command must answer without a
   model. *)
let help () =
  let out = Filename.temp_file ~temp_dir:"/tmp" "dumpty_cli" "help" in
  Fun.protect
    ~finally:(fun () -> remove [ out ])
    (fun () ->
      let status =
        Sys.command (Filename.quote_command exe ~stdout:out [ "--help=plain" ])
      in
      check "--help exits zero" (status = 0);
      check "--help prints the page" (holds (read out) "PROMPT"))

let () =
  no_model ();
  bad_timeout ();
  stdin_prompt_too_large ();
  help ();
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
