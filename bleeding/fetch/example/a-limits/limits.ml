let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let clock = Eio.Stdenv.clock env in
  let client = Fetch_httpz.std ~max_concurrent:2 ~min_interval:0.5 env in
  let started = Eio.Time.now clock in
  Eio.Fiber.List.iter
    (fun i ->
      ignore (Fetch.read client (base ^ "/hello"));
      Printf.printf "Request %d finished after %.1f seconds\n%!" i
        (Eio.Time.now clock -. started))
    [ 1; 2; 3; 4; 5; 6 ]
