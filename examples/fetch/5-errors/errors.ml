let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.v (Eio.Stdenv.net env) () in

  Eio.Switch.run (fun sw ->
    let response = Fetch.get ~sw client (base ^ "/nothing") in
    Printf.printf "A missing page is an ordinary response with status %d.\n"
      (Fetch.status response));

  (try ignore (Fetch.read client "http://127.0.0.1:1/")
   with Eio.Io (Fetch.E (Fetch.Connection_failure _), _) ->
     print_endline "Nothing is listening on port 1.");

  (try ignore (Fetch.read client "not a url")
   with Eio.Io (Fetch.E (Fetch.Invalid_url reason), _) ->
     Printf.printf "That is not a URL: %s.\n" reason);

  match
    Eio.Time.with_timeout (Eio.Stdenv.clock env) 1.0 (fun () ->
      Ok (Fetch.read client (base ^ "/slow")))
  with
  | Ok body -> print_string body
  | Error `Timeout -> print_endline "Gave up waiting after one second."
