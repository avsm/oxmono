let unavailable name f =
  match f () with
  | _ -> failwith (name ^ ": expected Apple_speech.Error Unavailable")
  | exception Apple_speech.Error Apple_speech.Unavailable -> ()

let () =
  assert (not (Apple_speech.available ()));
  assert (
    Apple_speech.text
      [
        { start = 0.; duration = 1.; text = " hello " };
        { start = 1.; duration = 1.; text = "world" };
      ]
    = "hello world");
  unavailable "supported locales" Apple_speech.supported_locales;
  unavailable "installed locales" Apple_speech.installed_locales;
  unavailable "status" (fun () -> Apple_speech.status ());
  unavailable "install" (fun () -> Apple_speech.install ());
  unavailable "transcribe" (fun () -> Apple_speech.transcribe "missing.wav");
  unavailable "duration" (fun () -> Apple_speech.duration "missing.wav");
  Eio_main.run (fun env ->
      let mgr = Eio.Stdenv.process_mgr env in
      unavailable "voices" (fun () -> Apple_speech.voices mgr);
      unavailable "synthesis" (fun () ->
          Apple_speech.synthesize mgr ~text:"hello" "unsupported.wav");
      assert (not (Sys.file_exists "unsupported.wav")))
