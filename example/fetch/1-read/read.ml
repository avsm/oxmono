let () =
  Eio_main.run @@ fun env ->
  let client = Fetch_httpz.std env in
  print_string (Fetch.read client "http://127.0.0.1:8765/")
