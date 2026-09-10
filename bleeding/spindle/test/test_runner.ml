(* SPDX-License-Identifier: ISC *)
module Runner = Spindle__Runner
module Patch = Spindle__Patch
module J = Spindle__Json

let () =
  Eio_main.run @@ fun env ->
  Eio.Time.with_timeout_exn env#clock 15. @@ fun () ->
  let name = Filename.temp_file "spindle-runner-" "" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  Fun.protect ~finally:(fun () ->
      Eio.Process.run env#process_mgr [ "rm"; "-rf"; "--"; name ])
  @@ fun () ->
  let directory = Eio.Path.(env#fs / name) in
  let git args =
    Eio.Process.run env#process_mgr ~cwd:directory
      ("git" :: "-c" :: "user.name=Test" :: "-c"
     :: "user.email=test@example.test" :: args)
  in
  git [ "init"; "-q" ];
  git [ "commit"; "-q"; "--allow-empty"; "-m"; "fixture" ];
  let commit =
    Eio.Process.parse_out env#process_mgr Eio.Buf_read.line ~cwd:directory
      [ "git"; "rev-parse"; "HEAD" ]
  in
  let runner : Runner.env =
    { system = env; directory; slots = Eio.Semaphore.make 1 }
  in
  let run ~cancel id =
    let ready = name ^ "/" ^ id ^ ".ready" in
    let leaked = name ^ "/" ^ id ^ ".leaked" in
    let script =
      {|(sleep 0.6; echo leaked > "$2") & echo ready > "$1"; |}
      ^ if cancel then "wait" else "exit 0"
    in
    let job =
      Spindle.Job.v "children"
        [ Command [ "sh"; "-c"; script; "child-test"; ready; leaked ] ]
    in
    let workflow = Runner.v job in
    let input : Runner.input =
      {
        id;
        repo = "did:web:repo.test";
        source = name;
        commit;
        metadata = J.obj [];
      }
    in
    Eio.Fiber.both
      (fun () -> Runner.execute runner input ~persist:(fun () -> ()) workflow)
      (fun () ->
        let rec wait () =
          if not (Sys.file_exists ready) then (
            Eio.Time.sleep env#clock 0.01;
            wait ())
        in
        wait ();
        if cancel then Runner.cancel ~persist:(fun () -> ()) workflow);
    assert (workflow.status = if cancel then "cancelled" else "success");
    Eio.Time.sleep env#clock 0.8;
    assert (not (Sys.file_exists leaked));
    assert (not (Sys.file_exists (name ^ "/" ^ id ^ ".children.work")))
  in
  run ~cancel:true "cancel";
  run ~cancel:false "finish";
  (match Patch.revision env "not a gzip stream" with
  | _ -> failwith "invalid gzip was accepted"
  | exception J.Invalid _ -> ());
  let compressed =
    Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
      ~stdin:
        (Eio.Flow.string_source
           ("From " ^ commit ^ " Mon Sep 17 00:00:00 2001\n"))
      [ "gzip"; "-c" ]
  in
  assert (Patch.revision env compressed = commit);
  print_endline
    "runner: cancellation and completion clean up descendants; corrupt patches \
     are rejected"
