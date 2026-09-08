let () =
  if Array.length Sys.argv < 4 || Array.length Sys.argv > 5 then begin
    prerr_endline "Usage: chat BASE_URL MODEL PROMPT [--stream]";
    exit 2
  end;
  let base_url = Sys.argv.(1) and model = Sys.argv.(2) in
  let stream = Array.length Sys.argv = 5 && Sys.argv.(4) = "--stream" in
  if Array.length Sys.argv = 5 && not stream then begin
    prerr_endline "Expected --stream";
    exit 2
  end;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fetch =
    Fetch_curl.v ~sw ~timeout:(Duration.of_sec 90)
      ~connect_timeout:(Duration.of_sec 5) ()
  in
  let client =
    Openrouter.of_fetch ~base_url
      ?api_key:(Sys.getenv_opt "OPENROUTER_API_KEY")
      fetch
  in
  let request =
    Openrouter.Chat.request ~model ~max_tokens:256
      ~messages:[ Openrouter.Message.user Sys.argv.(3) ]
      ()
  in
  let output text = Eio.Flow.copy_string text (Eio.Stdenv.stdout env) in
  if stream then begin
    let result =
      Openrouter.Chat.stream client request ~on_event:(fun event ->
          (match event with
          | Openrouter.Chat.Text { choice = 0; text } -> output text
          | _ -> ());
          `Continue)
    in
    if result <> `Complete then failwith "stream stopped";
    output "\n"
  end
  else
    let response = Openrouter.Chat.complete client request in
    match
      List.find_opt
        (fun (c : Openrouter.Chat.choice) -> c.index = 0)
        response.choices
    with
    | Some { text = Some text; _ } -> output (text ^ "\n")
    | _ -> failwith "completion contains no text in choice zero"
