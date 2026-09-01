let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = Fetch_curl.std ~sw env in
  if Array.length Sys.argv > 1 then print_string (Fetch.read client Sys.argv.(1))
  else
    Localhost.run env @@ fun base ->
    print_string (Fetch.read client (base ^ "/hello"))
