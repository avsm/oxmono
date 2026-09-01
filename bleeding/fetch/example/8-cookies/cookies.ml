let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->

  let jar = Fetch_cookies.Jar.in_memory ~clock:(Eio.Stdenv.clock env) () in
  let client = Fetch_cookies.with_jar jar (Fetch_httpz.v (Eio.Stdenv.net env) ()) in
  print_string (Fetch.read client (base ^ "/login"));
  Printf.printf "The jar now holds: %s\n"
    (Option.value (Fetch_cookies.Jar.header_for jar base) ~default:"nothing");
  print_string (Fetch.read client (base ^ "/account"));

  let forgetful = Fetch_httpz.std ~cookies:`Off env in
  print_string (Fetch.read forgetful (base ^ "/account"))
