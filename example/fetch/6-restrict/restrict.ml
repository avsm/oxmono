let denied f =
  try f () with Eio.Io (Fetch.E (Fetch.Denied reason), _) ->
    Printf.printf "Denied: %s\n" reason

let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in

  let local = Fetch.restrict ~under:[ base ] client in
  print_string (Fetch.read local (base ^ "/hello"));
  denied (fun () -> ignore (Fetch.read local "http://example.com/"));

  let read_only = Fetch.read_only local in
  print_string (Fetch.read read_only (base ^ "/hello"));
  denied (fun () ->
    Eio.Switch.run @@ fun sw ->
    ignore (Fetch.post ~sw read_only ~body:(String "hi") (base ^ "/echo")))
