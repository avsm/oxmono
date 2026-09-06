open Proffer
open Proffer.Route

let rec join (segments @ local) =
  match segments with
  | [] -> ""
  | [part] -> Req.globalize part
  | part :: rest -> Req.globalize part ^ " / " ^ join rest

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Good morning, world!\n");

      get (s "echo" / str) (fun word () _request respond ->
        Resp.text respond (Req.globalize word ^ "\n"));

      get (s "square" / int) (fun n () _request respond ->
        Resp.text respond (string_of_int (n * n) ^ "\n"));

      get (s "files" / rest) (fun segments () _request respond ->
        Resp.text respond (join segments ^ "\n")) ]

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~env:() site
