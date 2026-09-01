open Proffer
open Proffer.Route

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Hello fom Cambridge, world!\n") ]

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~env:() site
