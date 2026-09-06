let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in
  Eio.Switch.run @@ fun sw ->

  let response = Fetch.get ~sw client (base ^ "/old") in
  Printf.printf "Status %d from %s\n%!" (Fetch.status response) (Fetch.url response);
  Eio.Flow.copy (Fetch.body response) (Eio.Stdenv.stdout env);

  let response = Fetch.get ~sw ~redirects:0 client (base ^ "/old") in
  Printf.printf "Status %d pointing at %s\n" (Fetch.status response)
    (Option.get (Fetch.header Fetch.Header.location response))
