open Proffer
open Proffer.Route

let greeting =
  Negotiate.v
    [ (`Html, fun () _request respond ->
        Resp.html respond "<!doctype html>\n<h1>Good morning, world!</h1>\n");
      (`Json, fun () _request respond ->
        Resp.media respond "application/json"
          "{\"greeting\": \"Good morning, world!\"}\n");
      (`Other "text/plain", fun () _request respond ->
        Resp.text respond "Good morning, world!\n") ]

let site =
  Site.of_routes [ get root greeting ]

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~env:() site
