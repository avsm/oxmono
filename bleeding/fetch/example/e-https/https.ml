let () =
  Eio_main.run @@ fun env ->
  let client = Fetch_httpz.std env in
  let url = if Array.length Sys.argv > 1 then Sys.argv.(1) else "https://example.com/" in
  print_string (Fetch.read client url)
