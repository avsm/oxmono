module E = Retrieval.Embedding

let check name b = if not b then failwith name

let () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let response =
    ref
      {|{"data":[{"index":1,"embedding":[0,1]},{"index":0,"embedding":[1,0]}]}|}
  in
  let calls = ref 0 in
  let fetch =
    Fetch_mock.client (fun req ->
        incr calls;
        Fetch_mock.respond !response req)
  in
  let p =
    E.http ~sw ~fetch ~url:"https://embedding.example/v1/embeddings"
      ~model:"fixture" ~dimensions:2 ()
  in
  check "provider restores input order"
    (p.embed ~check:(fun () -> ()) [ "one"; "two" ] = [ [ 1.; 0. ]; [ 0.; 1. ] ]);
  response :=
    {|{"data":[{"index":0,"embedding":[1,0]},{"index":0,"embedding":[0,1]}]}|};
  check "duplicate response indices rejected"
    (try
       ignore (p.embed ~check:(fun () -> ()) [ "one"; "two" ]);
       false
     with Invalid_argument _ -> true);
  let before = !calls in
  check "oversized inputs rejected before network"
    (try
       ignore (p.embed ~check:(fun () -> ()) [ String.make 8193 'x' ]);
       false
     with Invalid_argument _ -> !calls = before);
  check "credentials require HTTPS"
    (try
       ignore
         (E.http ~sw ~fetch ~url:"http://embedding.example/" ~api_key:"secret"
            ~model:"fixture" ~dimensions:2 ());
       false
     with Invalid_argument _ -> true);
  print_endline "embedding HTTP tests passed"
