open Proffer
open Proffer.Route

let api =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.media respond "application/json" "{\"version\": 1}\n");
      get (s "users" / str) (fun name () _request respond ->
        Resp.media respond "application/json"
          (Printf.sprintf "{\"name\": %S}\n" (Req.globalize name))) ]

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Good morning, world!\n") ]
  |> Site.mount ~at:[ "api"; "v1" ] api
  |> Site.with_headers [ ("Server", "proffer-tutorial") ]

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~env:() site
