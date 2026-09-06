let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in

  print_string (Fetch.read client (base ^ "/secret"));

  let trusted =
    Fetch.with_credentials ~scope:[ base ] ~allow_insecure:true
      Fetch.Credential.[ Bearer (fun () -> "letmein") ]
      client
  in
  print_string (Fetch.read trusted (base ^ "/secret"));

  let polite =
    Fetch.with_headers
      Fetch.Header.[ user_agent, "fetch-tutorial/1.0"; raw "X-Example" "7-credentials" ]
      trusted
  in
  print_string (Fetch.read polite (base ^ "/headers"))
