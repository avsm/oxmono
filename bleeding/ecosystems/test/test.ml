let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let c =
    Ecosystems.create ~sw env ~base_url:"https://packages.ecosyste.ms/api/v1"
  in
  assert (Ecosystems.base_url c = "https://packages.ecosyste.ms/api/v1")
