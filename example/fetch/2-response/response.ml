let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in
  Eio.Switch.run @@ fun sw ->
  let response = Fetch.get ~sw client (base ^ "/json") in
  Printf.printf "Status: %d\n" (Fetch.status response);
  Printf.printf "Fetched from: %s\n" (Fetch.url response);
  (match Fetch.header Fetch.Header.content_type response with
   | Some { media; _ } -> Printf.printf "Media type: %s\n" media
   | None -> print_endline "No media type given");
  print_endline "Headers:";
  Http.Header.to_list (Fetch.headers response)
  |> List.iter (fun (name, value) -> Printf.printf "  %s: %s\n" name value);
  print_string "Body: ";
  flush stdout;
  Eio.Flow.copy (Fetch.body response) (Eio.Stdenv.stdout env)
