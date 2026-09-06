open Proffer
open Proffer.Route

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Hello from Proffer!\n") ]

let () =
  Eio_main.run @@ fun env ->
  Proffer_httpz.run env ~env:() site
