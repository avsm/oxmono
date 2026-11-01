module O = Openrouter

let check name text =
  let text = String.lowercase_ascii (String.trim text) in
  let rec contains_red i =
    i + 3 <= String.length text
    && (String.sub text i 3 = "red" || contains_red (i + 1))
  in
  if not (contains_red 0) then
    failwith (Printf.sprintf "%s: expected red, got %S" name text);
  Printf.printf "PASS %s: %s\n%!" name text

let () =
  if Array.length Sys.argv <> 4 then begin
    prerr_endline "Usage: vision BASE_URL MODEL RED_PNG_FILE";
    exit 2
  end;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fetch =
    Fetch_curl.v ~sw ~timeout:(Duration.of_sec 90)
      ~connect_timeout:(Duration.of_sec 5) ()
  in
  let client = O.of_fetch ~base_url:Sys.argv.(1) fetch in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 180. @@ fun () ->
  let image =
    Eio.Path.with_open_in
      Eio.Path.(Eio.Stdenv.fs env / Sys.argv.(3))
      (O.Image.of_flow ~format:Png)
  in
  let message =
    O.Message.user_parts
      [
        O.Content.text "What color is this square? Answer in one word.";
        O.Content.image image;
      ]
  in
  let request =
    O.Chat.request ~model:Sys.argv.(2) ~max_tokens:256 ~temperature:0.
      ~messages:[ message ] ()
  in
  let completion = O.Chat.complete client request in
  let text =
    List.filter_map (fun (c : O.Chat.choice) -> c.text) completion.choices
    |> String.concat ""
  in
  check "image completion" text;
  let text = Buffer.create 32 in
  let result =
    O.Chat.stream client request ~on_event:(fun event ->
        (match event with
        | O.Chat.Text { choice = 0; text = s } -> Buffer.add_string text s
        | _ -> ());
        `Continue)
  in
  if result <> `Complete then failwith "image stream did not complete";
  check "image stream" (Buffer.contents text)
