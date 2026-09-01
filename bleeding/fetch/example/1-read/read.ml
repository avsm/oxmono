let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in
  print_string (Fetch.read client (base ^ "/hello"))
