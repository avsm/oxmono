module O = Openrouter

let check name ok =
  if not ok then failwith name;
  Printf.printf "PASS %s\n%!" name

let () =
  if Array.length Sys.argv <> 3 then begin
    prerr_endline "Usage: probe BASE_URL MODEL";
    exit 2
  end;
  let base_url = Sys.argv.(1) and model = Sys.argv.(2) in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fetch =
    Fetch_curl.v ~sw ~timeout:(Duration.of_sec 90)
      ~connect_timeout:(Duration.of_sec 5) ()
  in
  (* This probe deliberately takes no credentials from the environment. *)
  let client = O.of_fetch ~base_url fetch in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 180. @@ fun () ->
  let models = O.Models.list client in
  check "model discovery"
    (List.exists (fun (m : O.Models.model) -> m.id = model) models);
  let request =
    O.Chat.request ~model ~max_tokens:256 ~temperature:0.
      ~messages:[ O.Message.user "Reply with the single word OK." ]
      ()
  in
  let completion = O.Chat.complete client request in
  check "ordinary completion"
    (List.exists
       (fun (c : O.Chat.choice) ->
         Option.fold ~none:false ~some:(fun s -> String.trim s <> "") c.text)
       completion.choices);
  check "ordinary usage"
    (Option.fold ~none:false
       ~some:(fun (u : O.Chat.usage) -> u.total_tokens > 0)
       completion.usage);
  let text = Buffer.create 32
  and usage = ref None
  and finished = ref false in
  let result =
    O.Chat.stream client request ~on_event:(fun event ->
        Eio.Fiber.yield ();
        (match event with
        | O.Chat.Text { choice = 0; text = s } -> Buffer.add_string text s
        | Finished { choice = 0; _ } -> finished := true
        | Usage u -> usage := Some u
        | _ -> ());
        `Continue)
  in
  check "stream completed" (result = `Complete && !finished);
  check "streamed text" (String.trim (Buffer.contents text) <> "");
  check "stream usage" (Option.is_some !usage);
  check "early stop"
    (O.Chat.stream client request ~on_event:(fun _ -> `Stop) = `Stopped);
  (match
     Eio.Cancel.sub (fun cc ->
         O.Chat.stream client request ~on_event:(fun _ ->
             Eio.Cancel.cancel cc Exit;
             Eio.Cancel.check cc;
             `Continue))
   with
  | _ -> failwith "cancellation was swallowed"
  | exception Eio.Cancel.Cancelled Exit -> check "fiber cancellation" true);
  check "client reusable after stop/cancel" (O.Models.list client <> []);
  let missing =
    O.Chat.request ~model:"openrouter-client-test-missing-model"
      ~messages:[ O.Message.user "Hello" ]
      ~max_tokens:1 ()
  in
  match O.Chat.complete client missing with
  | _ -> failwith "unknown model unexpectedly succeeded"
  | exception Eio.Io (O.E (O.Http_error { status; _ }), _) ->
      check "native HTTP error" (status >= 400 && status < 500)
