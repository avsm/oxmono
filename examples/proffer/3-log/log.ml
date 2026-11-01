open Proffer
open Proffer.Route

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Good morning, world!\n");
      get (s "echo" / str) (fun word () _request respond ->
        Resp.text respond (Req.globalize word ^ "\n")) ]

(* [path] rather than [target]: the query is client-supplied and routinely
   carries tokens, which have no business in an access log. *)
let log (event : Proffer_httpz.event @ local) =
  Printf.printf "%s %s %s -> %d in %d us\n%!"
    (Req.globalize event.remote_addr)
    (Method.to_string event.meth)
    (Req.globalize event.path)
    (Status.code event.status)
    event.duration_us

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~on_event:log ~env:() site
