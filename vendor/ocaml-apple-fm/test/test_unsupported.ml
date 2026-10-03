let unsupported name f =
  match f () with
  | _ -> failwith (name ^ ": expected an unsupported platform error")
  | exception Eio.Io (Apple_fm.Error.E (`Unsupported_version message), _) ->
      assert (message = "Apple Foundation Models requires macOS")

let () =
  (match Apple_fm.Availability.get () with
  | `Unavailable "Apple Foundation Models requires macOS" -> ()
  | _ -> failwith "expected Foundation Models to be unavailable");
  unsupported "model info" (fun () -> Apple_fm.Model.info ());
  unsupported "locale support" (fun () ->
      Apple_fm.Model.supports_locale "en-GB");
  let transcript =
    match Apple_fm.Transcript.of_json "[]" with
    | Ok transcript -> transcript
    | Error message -> failwith message
  in
  unsupported "transcript compaction" (fun () ->
      Apple_fm.Model.compact_transcript ~summary:"summary" transcript);
  Eio_main.run (fun _ ->
      unsupported "token counting" (fun () ->
          Apple_fm.Model.count_text_tokens "hello");
      Eio.Switch.run (fun sw ->
          unsupported "session creation" (fun () ->
              Apple_fm.Session.create ~sw [])))
