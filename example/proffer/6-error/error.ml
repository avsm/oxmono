open Proffer
open Proffer.Route

let not_found () request respond =
  Resp.text respond ~status:Not_found
    ("Sorry! There is nothing at " ^ Req.globalize (Req.path request) ^ "\n")

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Good morning, world!\n");
      post (s "echo") (fun () request respond ->
        Resp.text respond (Req.globalize (Req.body request)));
      moved (s "old") "/" ]
  |> Site.with_fallback not_found
  |> Site.with_headers [ ("Server", "proffer-tutorial") ]

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~env:() site
