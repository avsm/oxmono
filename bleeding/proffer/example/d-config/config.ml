open Proffer
open Proffer.Route

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Good morning, world!\n") ]

let config =
  { Proffer_httpz.default_config with
    max_connections = 100;
    request_timeout = 5.0;
    idle_timeout = 10.0 }

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~port:0 ~config ~env:() site
