let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->

  let client = Fetch_httpz.std env in
  print_string (Fetch.read client (base ^ "/flaky"));

  let impatient = Fetch_httpz.std ~retry:(Fetch.Retry.v ~max_retries:0 ()) env in
  print_string (Fetch.read impatient (base ^ "/flaky"))
