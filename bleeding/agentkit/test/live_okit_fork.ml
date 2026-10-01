(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* What an okit call costs a process that holds an engine.

   A spawn is a fork, and an engine maps the whole model, so a process with one
   loaded forks tens of gigabytes of mapping. On macOS that can take minutes,
   and it blocks the domain that asked. okit therefore runs in okitd, a child
   spawned before the engine exists, and humpty forks nothing afterwards. This
   is the end-to-end pin of that arrangement: okitd is started first, as
   `humpty agent` starts it, the engine is created, and a build through okitd
   must then answer promptly, because nothing about it forks this process.

   The fork and exec made directly is measured beside it, since it is the cost
   the design exists to avoid and the number that makes the other meaningful.

   It is a measurement rather than a threshold. The durations are printed on the
   ok lines, and only a call that takes longer than a person would wait counts
   as a failure. It needs a real model, so it is gated on DS4_LIVE. *)

module V4 = Ds4.V4
module Client = Okit.Client
module Proto = Okit.Proto

(* What an operation may take before it is a hang rather than a cost. Chosen
   well above anything a working machine shows, since the point of the test is
   the number it prints. *)
let limit = 120.
let failures = ref 0

let check name ~seconds ok =
  let line = Printf.sprintf "%s: %.1fs" name seconds in
  if ok then Printf.printf "ok   - %s\n%!" line
  else begin
    incr failures;
    Printf.printf "FAIL - %s (over %.0fs)\n%!" line limit
  end

let timed f =
  let started = Unix.gettimeofday () in
  let r = f () in
  (r, Unix.gettimeofday () -. started)

(* The binary under test, passed by the dune rule so that the test runs the one
   this build produced. okitd loads no model, so the CPU build serves whichever
   backend this test was built for. *)
let exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else "humpty-cpu"

(* The workspace goes under /tmp rather than TMPDIR, because dune sets TMPDIR
   to a path deep inside its own build directory when it runs a test. *)
let fixture ~fs =
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "okit_fork" "ws" in
  let root = Eio.Path.(fs / dir) in
  let save p s =
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(root / p) s
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / "lib");
  save "dune-project" "(lang dune 3.21)\n";
  save "lib/dune" "(library (name fix))\n";
  save "lib/fix.ml" "let x = 1\n";
  dir

let run env xdg model_path =
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  let proc = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let cache = Xdge.cache_dir xdg in
  let dir = fixture ~fs in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
    (fun () ->
      (* Started before the engine exists, which is the whole of the ordering
         humpty keeps. Its own start is a fork of a process that holds
         nothing. *)
      let client, seconds =
        timed (fun () ->
            Client.start ~sw ~proc ~clock ~trace:ignore
              ~argv:[ exe; "okitd"; "--dir"; dir ])
      in
      match client with
      | Error e ->
          incr failures;
          Printf.printf "FAIL - okitd: %s\n%!" e
      | Ok client ->
          check "okitd started before the engine" ~seconds (seconds < limit);
          Printf.printf "note - %s\n%!" (Client.hello client).Proto.status;
          Printf.printf "loading %s …\n%!" (Filename.basename model_path);
          (* The engine as humpty agent creates it, on its own domain. *)
          let engine =
            V4.create ~sw
              ~domain_mgr:(Eio.Stdenv.domain_mgr env)
              ~cache
              ~model:Eio.Path.(fs / model_path)
              ()
          in
          (* Read from the engine, so that nothing above may be deferred past
             the measurements below. *)
          Printf.printf "engine holds %s, vocab %d\n%!" (V4.model_name engine)
            (V4.vocab_size engine);
          (* The cost okitd exists to avoid, paid once here so that the number
             below has something to be read against. *)
          let (), seconds =
            timed (fun () -> Eio.Process.run proc [ "/usr/bin/true" ])
          in
          check "fork+exec with engine" ~seconds (seconds < limit);
          let r, seconds =
            timed (fun () ->
                Client.call client ~timeout:limit
                  (Proto.Build { targets = "." })
                  ~on_trace:ignore)
          in
          check "build through okitd with engine" ~seconds (seconds < limit);
          (match r with
          | Ok "build ok" -> print_string "ok   - the build succeeded\n"
          | Ok out ->
              incr failures;
              Printf.printf "FAIL - build answered %S\n%!" out
          | Error e ->
              incr failures;
              Printf.printf "FAIL - build: %s\n%!" e);
          (* Named so that a reader knows what the numbers above are evidence
             about. *)
          print_string
            "note - humpty agent starts okitd before the engine, and forks \
             nothing after it\n";
          Client.stop client);
  if !failures = 0 then print_string "\nLive fork test passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end

let () =
  match Sys.getenv_opt "DS4_LIVE" with
  | None -> print_string "SKIP - live okit fork test (set DS4_LIVE=1 to run)\n"
  | Some _ -> (
      Eio_main.run @@ fun env ->
      let xdg = Xdge.create (Eio.Stdenv.fs env) "ds4" in
      match Ds4_cli.Model.resolve ~dir:(Ds4_cli.Model.dir xdg) None with
      | Error e -> Printf.printf "SKIP - %s\n" e
      | Ok model_path -> run env xdg model_path)
