let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in
  match
    Fetch.read_as client
      (Fetch.Markdown.markdown ())
      (base ^ "/about")
  with
  | Ok document ->
    print_string
      (Fetch.Media.encode (Fetch.Markdown.html ()) document)
  | Error r -> Printf.printf "The server said %d\n" (Fetch.status r)
