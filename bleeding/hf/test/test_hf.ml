let check message condition = if not condition then failwith message
let shell code = [ "/bin/sh"; "-c"; code ]

let main env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.mono_clock env in
  let within f =
    Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 3.) f
  in
  let root = Filename.temp_file "hf-test-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  let hf = Filename.concat root "hf" and uvx = Filename.concat root "uvx" in
  let script path source =
    Out_channel.with_open_bin path (fun out ->
        output_string out ("#!/bin/sh\n" ^ source));
    Unix.chmod path 0o700
  in
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Unix.unlink (Filename.concat root name))
        (Sys.readdir root);
      Unix.rmdir root)
    (fun () ->
      let child_env =
        Eio.Process.environment process_mgr
        |> Eio.Process.Env.override [ ("PATH", Some root); ("HF_TOKEN", None) ]
      in
      script hf
        "[ \"$HF_TOKEN\" = 'synthetic-secret' ] || exit 20\n\
         [ \"$#\" = 2 ] || exit 21\n\
         [ \"$1\" = download ] && [ \"$2\" = model ] || exit 22\n";
      within (fun () ->
          Hf.run ~process_mgr ~env:child_env ~token:"synthetic-secret"
            [ "download"; "model" ]);
      check "token did not enter parent environment"
        (Eio.Process.Env.get_opt "HF_TOKEN" child_env = None);
      script hf "exit 23\n";
      script uvx "exit 0\n";
      (match within (fun () -> Hf.run ~process_mgr ~env:child_env []) with
      | () -> failwith "unexpected retry after hf failure"
      | exception
          Eio.Io (Eio.Process.E (Eio.Process.Child_error (`Exited 23)), _) ->
          ());
      within (fun () ->
          Hf.run ~process_mgr ~env:child_env ~retry_failed:true []);
      Unix.chmod hf 0o600;
      (match within (fun () -> Hf.run ~process_mgr ~env:child_env []) with
      | () -> failwith "unexpected retry after hf permission failure"
      | exception Eio.Io (Eio.Process.E (Eio.Process.Permission_denied _), _) ->
          ());
      let fallback = Filename.concat root "fallback" in
      script uvx ("printf unexpected > " ^ Filename.quote fallback ^ "\n");
      script hf "/bin/sleep 60 & wait\n";
      (match
         within (fun () ->
             Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 0.05)
               (fun () -> Hf.run ~process_mgr ~env:child_env []))
       with
      | () -> failwith "expected downloader cancellation"
      | exception Eio.Time.Timeout -> ());
      check "cancellation does not invoke uvx" (not (Sys.file_exists fallback));
      script hf "kill -TERM $$\n";
      (match
         within (fun () ->
             Hf.run ~process_mgr ~env:child_env ~retry_failed:true [])
       with
      | () -> failwith "signal should not trigger fallback"
      | exception
          Eio.Io (Eio.Process.E (Eio.Process.Child_error (`Signaled _)), _) ->
          ());
      check "signal does not invoke uvx" (not (Sys.file_exists fallback));
      Unix.unlink hf;
      script uvx "[ \"$1\" = hf ] || exit 24\n";
      within (fun () -> Hf.run ~process_mgr ~env:child_env []));
  print_endline "HF credentials, executable selection and cancellation passed"

let () = Eio_main.run main
