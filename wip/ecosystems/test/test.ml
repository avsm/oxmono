let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let c =
    Ecosystems.create ~sw env ~base_url:"https://packages.ecosyste.ms/api/v1"
  in
  assert (Ecosystems.base_url c = "https://packages.ecosyste.ms/api/v1");
  let c = Ecosystems_client.create ~sw env in
  assert (Ecosystems.base_url c = Ecosystems_client.default_base_url);
  let c =
    Ecosystems_client.create ~user_agent:"t/1" ~base_url:"http://x/" ~sw env
  in
  assert (Ecosystems.base_url c = "http://x")
