open Proffer
open Proffer.Route

type env = { count : int ref }

let site =
  Site.of_routes
    [ get root (fun env _request respond ->
        incr env.count;
        Resp.text respond (Printf.sprintf "Saw %d request(s)!\n" !(env.count))) ]

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~env:{ count = ref 0 } site
