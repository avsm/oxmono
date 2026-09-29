let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let no_arguments =
    Apple_fm.Codec.Invoke.map "nothing" () |> Apple_fm.Codec.Invoke.seal
  in
  let nothing =
    Apple_fm.Tool.v ~description:"Return an empty observation." no_arguments
      (fun () -> "")
  in
  ignore (Apple_fm.Model.info ());
  let schema_session = Apple_fm.Session.create ~sw [ nothing ] in
  let transcript = Apple_fm.Session.transcript schema_session in
  let transcript =
    Apple_fm.Transcript.to_json transcript
    |> Apple_fm.Transcript.of_json |> Result.get_ok
  in
  Apple_fm.Session.close schema_session;
  (match Apple_fm.Session.prewarm schema_session with
  | exception Eio.Io (Apple_fm.Error.E `Closed, _) -> ()
  | _ -> failwith "prewarming a closed session did not report Eio.Io");
  let restored = Apple_fm.Session.create ~sw ~transcript [ nothing ] in
  ignore (Apple_fm.Session.usage restored);
  Apple_fm.Session.close restored;
  let session = Apple_fm.Session.create ~sw [] in
  let options = Apple_fm.Generation.options ~maximum_response_tokens:1_000 () in
  let outcome =
    Eio.Fiber.first
      (fun () ->
        ignore
          (Apple_fm.Session.respond ~options session
             "Write a detailed one-thousand-word history of London.");
        `Finished)
      (fun () ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
        `Cancelled)
  in
  if outcome <> `Cancelled then
    failwith "generation completed before cancellation";
  (match Apple_fm.Session.respond session "This must fail" with
  | exception Eio.Io (Apple_fm.Error.E `Closed, _) -> ()
  | _ -> failwith "cancelled session remained open");
  let session = Apple_fm.Session.create ~sw [] in
  let answer =
    Apple_fm.Session.respond
      ~options:(Apple_fm.Generation.options ~maximum_response_tokens:20 ())
      session "Reply with exactly: reused"
  in
  if not (String.starts_with ~prefix:"reused" (String.trim answer)) then
    failwith ("new session failed after cancellation: " ^ answer);
  let session = Apple_fm.Session.create ~sw [] in
  let cancelled = ref false in
  (try
     Eio.Fiber.both
       (fun () ->
         ignore
           (Apple_fm.Session.respond ~options session
              "Write another detailed one-thousand-word history of London."))
       (fun () ->
         while not (Apple_fm.Session.is_responding session) do
           Eio.Fiber.yield ()
         done;
         Apple_fm.Session.cancel session)
   with Eio.Io (Apple_fm.Error.E _, _) -> cancelled := true);
  if not !cancelled then
    failwith "explicit cancellation did not stop the response";
  match Apple_fm.Session.respond session "This must also fail" with
  | exception Eio.Io (Apple_fm.Error.E `Closed, _) -> ()
  | _ -> failwith "explicitly cancelled session remained open"
