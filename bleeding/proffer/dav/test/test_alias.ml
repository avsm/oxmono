let () = Eio_mock.Backend.run @@ fun () ->
  let calls = ref 0 in
  let backend = Fetch_mock.client (fun _ -> incr calls; raise Exit) in
  let client = Fetch_dav.v ~root:"https://example.test/dav/" backend in
  let condition : Proffer_dav.condition = Fetch_dav.If_absent in
  (try ignore (Proffer_dav.put ~condition client "file" (Fetch.String "data"))
   with Exit -> ());
  assert (!calls = 1);
  (try raise (Fetch_dav.Protocol_error "test")
   with Proffer_dav.Protocol_error "test" -> ());
  print_endline "proffer.dav shares Fetch client types and exceptions"
